"""Check Corner's fixed trace or run bounded SAT discovery with --search."""

import argparse
import json
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
from time import perf_counter

ROOT = Path(__file__).resolve().parents[2]


def prepare(directory):
    for name in ("src", "tech"):
        shutil.copytree(
            ROOT / name, directory / name, ignore=shutil.ignore_patterns("problem*.lisp", "*.fasl")
        )
    shutil.copy(ROOT / "wouldwork.asd", directory)
    (directory / "probs").mkdir()
    for name in ("blocks3", "corner-topo"):
        shutil.copy(ROOT / f"probs/problem-{name}.lisp", directory / "probs")
    driver = """
(require :asdf)
(push *default-pathname-defaults* asdf:*central-registry*)
(asdf:initialize-output-translations
  `(:output-translations (t ,(merge-pathnames "cache/" *default-pathname-defaults*))
    :ignore-inherited-configuration))
(asdf:load-asd (truename "wouldwork.asd"))
(assert (uiop:pathname-equal (asdf:system-source-directory :wouldwork) (uiop:getcwd)))
(asdf:load-system :wouldwork)
(in-package :ww)
(assert (%stage "corner-topo"))
(load "export.lisp")
"""
    shutil.copy(ROOT / "test/search/sat-corner-export.lisp", directory / "export.lisp")
    (directory / "run.lisp").write_text(driver)


def load_reference(directory):
    records = Path("doc/problems/corner-topo")
    (directory / records).mkdir(parents=True)
    shutil.copy(ROOT / records / "Actions.lisp", directory / records)
    return '(load "doc/problems/corner-topo/Actions.lisp")\n(assert *accepted-validators-p*)\n'


def export(directory, search=False, after=0):
    prepare(directory)
    if search and not after:
        operation = '(corner-search-input "reference.jsonl")'
    else:
        operation = load_reference(directory)
        operation += (f'(corner-search-input "reference.jsonl" (subseq *accepted-actions* 0 {after}))'
                      if search else '(corner-export "reference.jsonl")')
    run_lisp(directory, operation)
    return [json.loads(s) for s in (directory / "reference.jsonl").read_text().splitlines()]


def run_lisp(directory, expression):
    (directory / "operation.lisp").write_text('(in-package :ww)\n' + expression)
    run = subprocess.run(
        ["sbcl", "--dynamic-space-size", "4096", "--non-interactive", "--load", "run.lisp",
         "--load", "operation.lisp"],
        cwd=directory,
        capture_output=True,
        encoding="utf8",
        errors="replace",
        env={**os.environ, "WOULDWORK_INSTANCE": "sat-corner"},
    )
    if run.returncode:
        detail = run.stderr.partition("Backtrace for")[0]
        raise RuntimeError(run.stdout[-1200:] + detail[-4000:])
    return run.stdout


def normalize_state(rows):
    facts = {tuple(row) for row in rows}
    hands = [
        {f[2] for f in facts if f[:2] == (hand, "agent1")} for hand in ("holding1", "holding2")
    ]
    if hands[0] != hands[1]:
        raise ValueError(f"Corner hand slots disagree: {hands}")
    return {f for f in facts if f[0] not in ("holding1", "holding2")} | {
        ("holding", c) for c in hands[0]
    }


def normalize_action(name, args, state, geometry):
    assert args[0] == "agent1"
    location = next(p for rel, o, p in (f for f in state if f[0] == "has-location") if o == "agent1")
    if name == "move":
        (walk,) = args[1]
        mode, source, gates, destination = walk
        assert mode == "walk" and source == location
        assert gates == (["gate1"] if geometry.route[source, destination] else [])
        return name, None, destination, frozenset()
    assert args[-1] == location
    if name == "select-termini":
        return name, args[2], None, frozenset(args[1])
    if name == "put-connector":
        assert args[2] == "ground"
    return name, args[1], None, frozenset()


def peak_mib():
    if os.name != "nt":
        import resource
        import sys

        return resource.getrusage(resource.RUSAGE_SELF).ru_maxrss / (
            2**20 if sys.platform == "darwin" else 1024
        )
    import ctypes
    from ctypes import wintypes

    class Counters(ctypes.Structure):
        _fields_ = [
            ("cb", wintypes.DWORD),
            ("faults", wintypes.DWORD),
            ("peak", ctypes.c_size_t),
            ("unused", ctypes.c_size_t * 7),
        ]

    record = Counters()
    record.cb = ctypes.sizeof(record)
    if not ctypes.windll.psapi.GetProcessMemoryInfo(
        ctypes.c_void_p(-1), ctypes.byref(record), record.cb
    ):
        raise ctypes.WinError()
    return record.peak / 2**20


def compare(rows, receiver_rule=False, gate_rule=False, source_rule=False, one_color=False):
    import z3
    from sat_corner_model import CornerSAT, assignment, disjunction, values
    from sat_corner_beams import Geometry

    geometry = Geometry(rows)
    states = {}
    actions = {}
    children = {}
    for tag, *args in rows:
        if tag == "state":
            i, facts = args
            states[i] = normalize_state(facts)
            children[i] = {}
        elif tag in ("action", "child"):
            i, name, arguments, *outcome = args
            action = normalize_action(name, arguments, states[i], geometry)
            if tag == "action":
                actions[i] = action
            else:
                successor = normalize_state(outcome[0])
                if children[i].setdefault(action, successor) != successor:
                    raise ValueError(f"Wouldwork gives two outcomes after step {i} for {action}")
    actions = [actions[i] for i in range(len(actions))]
    states = [states[i] for i in range(len(states))]
    assert len(states) == 20 and len(actions) == 19

    def verify(model, solver, i, expected):
        actual = values(model, solver.states[i])
        if actual != expected:
            raise ValueError(f"State {i}: missing {expected - actual}, extra {actual - expected}")

    start = perf_counter()
    rules = dict(receiver_rule=receiver_rule, gate_rule=gate_rule, source_rule=source_rule,
                 one_color=one_color)
    trace = CornerSAT(geometry, states[0], **rules)
    for action in actions:
        trace.add_step(action)
    built = perf_counter() - start
    model = trace.check()
    if model is None:
        for end in range(1, len(actions) + 1):
            prefix = CornerSAT(geometry, states[0], **rules)
            for action in actions[:end]:
                prefix.add_step(action)
            if prefix.check() is None:
                raise ValueError(f"Recorded action {end} is rejected: {actions[end - 1]}")
    for i, expected in enumerate(states):
        verify(model, trace, i, expected)
    trace.solver.add(
        disjunction(
            z3.Not(assignment(bits, state))
            for bits, state in zip(trace.states, states, strict=True)
        )
    )
    if differing := trace.check():
        for i, expected in enumerate(states):
            verify(differing, trace, i, expected)
    print(
        f"19 fixed actions: SAT; any difference across 20 states: UNSAT. "
        f"{len(trace.states) * len(trace.keys)} state bits, {len(trace.solver.assertions())} assertions "
        f"including {len(trace.learned)} beam implications and the disagreement query. "
        f"Action model {built:.3f}s, beam evaluation/encoding {trace.beam_time:.3f}s, "
        f"Z3 checks {trace.solve_time:.3f}s, trace total {perf_counter() - start:.3f}s.",
        flush=True,
    )

    one_step_start = perf_counter()
    for i, expected in children.items():
        one = CornerSAT(geometry, states[i], **rules)
        one.add_step()
        seen = set()
        # Enumerate each (action, outcome) pair, so a second outcome for the same
        # action is also a failure. The final UNSAT check excludes extra actions.
        while (model := one.check()) is not None:
            action = one.read_action(0, model)
            if action not in expected:
                raise ValueError(f"After step {i}: additional SAT action {action}")
            verify(model, one, 1, expected[action])
            seen.add(action)
            one.solver.add(
                z3.Not(
                    z3.And(one.action_is(0, action), assignment(one.states[1], expected[action]))
                )
            )
        if seen != expected.keys():
            raise ValueError(f"After step {i}: missing actions {expected.keys() - seen}")
        print(f"After step {i}: {len(seen)} legal actions and outcomes agree.", flush=True)
    print(f"{sum(map(len, children.values()))} one-step comparisons in "
          f"{perf_counter()-one_step_start:.3f}s. Python + Z3 peak working set "
          f"{peak_mib():.1f} MiB; Z3 {z3.get_version_string()}.")


def discover(rows, seconds, receiver_rule=False, gate_rule=False, source_rule=False, one_color=False):
    import z3
    from sat_corner_beams import Geometry
    from sat_corner_model import CornerSAT

    start = perf_counter()
    (_, after, facts), = (row for row in rows if row[0] == "state")
    initial = normalize_state(facts)
    horizon = 19 - after
    goal = next(row[1] for row in rows if row[0] == "goal")
    assert {tuple(f) for f in goal} == {
        ("has-location", "agent1", "location4"), ("active", "receiver2"), ("active", "receiver3")}
    search = CornerSAT(Geometry(rows), initial, receiver_rule=receiver_rule, gate_rule=gate_rule,
                       source_rule=source_rule, one_color=one_color)
    for _ in range(horizon):
        search.add_step(allow_finish=True)
    search.solver.add(*(search.states[-1][tuple(f)] for f in goal))
    built = perf_counter() - start
    try:
        model = search.check(deadline=start + seconds)
        status = "SAT" if model is not None else "UNSAT"
    except TimeoutError:
        model, status = None, "TIMEOUT"
    print(f"After step {after}, at most {horizon} actions: {status}; wall {perf_counter()-start:.3f}s, build {built:.3f}s, "
          f"Z3 {search.solve_time:.3f}s, beam evaluation and encoding {search.beam_time:.3f}s, "
          f"{len(search.learned)} beam constraints, {len({k[1:] for k in search.learned})} "
          f"distinct arrangement/gate inputs, peak {peak_mib():.1f} MiB; Z3 {z3.get_version_string()}.")
    if status == "UNSAT":
        raise ValueError("Search excluded the known continuation")
    if model is not None:
        replay_model(search, model, after)


def replay_model(search, model, after=0):
    plan = []
    for i in range(len(search.steps)):
        kind, obj, dest, terms = search.read_action(i, model)
        if kind == "finished":
            break
        plan.append(f"({kind} {obj or dest} ({' '.join(sorted(terms))}))")
    with tempfile.TemporaryDirectory(prefix="wouldwork-corner-replay-") as scratch:
        directory = Path(scratch)
        prepare(directory)
        operation = load_reference(directory) if after else ""
        prefix = f"(subseq *accepted-actions* 0 {after})" if after else "nil"
        run_lisp(directory, operation + "(corner-replay-search '(" + " ".join(plan) + f") {prefix})")
    print(f"Wouldwork validated {after} prefix actions and {len(plan)} discovered actions:", *plan, sep="\n")


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    mode = parser.add_mutually_exclusive_group()
    mode.add_argument("--search", action="store_true", help="discover a plan within 19 actions")
    mode.add_argument("--ww-search", action="store_true", help="run serial Wouldwork forward search")
    parser.add_argument("--after", type=int, default=0, choices=range(19), metavar="N",
                        help="start discovery after N recorded actions with 19-N actions available")
    parser.add_argument("--receiver-rule", action="store_true",
                        help="require a placed, paired connector of the right color for every active receiver")
    parser.add_argument("--seconds", type=int, default=60, help="search budget including build")
    parser.add_argument("--gate-rule", action="store_true",
                        help="equate gate1 being open with receiver1 being active in each state")
    parser.add_argument("--source-rule", action="store_true",
                        help="require each lit connector to have a placed, paired path to its source")
    parser.add_argument("--one-color", action="store_true",
                        help="allow each connector to carry at most one color per state")
    args = parser.parse_args()
    if args.after and not (args.search or args.ww_search):
        parser.error("--after requires --search or --ww-search")
    if args.ww_search and any((args.receiver_rule, args.gate_rule, args.source_rule, args.one_color)):
        parser.error("the Boolean encoding options apply only to SAT")
    if args.seconds < 1:
        parser.error("--seconds must be positive")
    started = perf_counter()
    with tempfile.TemporaryDirectory(prefix="wouldwork-sat-corner-") as scratch:
        directory = Path(scratch)
        if args.ww_search:
            prepare(directory)
            operation = load_reference(directory) if args.after else ""
            prefix = f"(subseq *accepted-actions* 0 {args.after})" if args.after else "nil"
            output = run_lisp(directory, operation + f"(corner-forward-search {prefix} {args.seconds})")
            print(*(s for s in output.splitlines() if s.startswith("CORNER-WW")), sep="\n")
            print(f"Total elapsed including setup, search and replay {perf_counter()-started:.3f}s.")
            raise SystemExit(0)
        reference = export(directory, search=args.search, after=args.after)
        print(f"Temporary setup and export {perf_counter()-started:.3f}s.", flush=True)
        rules = dict(receiver_rule=args.receiver_rule, gate_rule=args.gate_rule,
                     source_rule=args.source_rule, one_color=args.one_color)
        if args.search:
            discover(reference, args.seconds, **rules)
        else:
            compare(reference, **rules)
