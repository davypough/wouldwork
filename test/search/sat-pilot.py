"""Compare bounded Talos planning with propositional SAT. See doc/sat-pilot.md."""
import argparse
from dataclasses import dataclass
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
from time import perf_counter

import z3
import sat_gate_sequence


ROOT = Path(__file__).resolve().parents[2]


@dataclass(frozen=True)
class Puzzle:
    name: str
    boxes: tuple
    shelves: int
    bounds: tuple
    pruning: bool = False
    plates: tuple = ("plate-a", "plate-b")
    symmetry: bool = False
    lower_bound: bool = False

    @property
    def plate_sites(self):
        return {plate: plate.replace("plate-", "site-") for plate in self.plates}

    @property
    def sites(self):
        return ("hub", *self.plate_sites.values(), *(f"shelf-{i}" for i in range(self.shelves)))


def fixture(puzzle, workers):
    plates = " ".join(puzzle.plates)
    facts = ["(has-location agent1 hub)", f"(controls (({plates})) gate1 normal)",
             "(traverse-via> hub ((gate1)) exit-site)"]
    for box in puzzle.boxes:
        facts.extend([f"(has-location {box} hub)", f"(has-height {box} 2)"])
    for site in puzzle.sites[1:]:
        facts.append(f"(reach-via hub () {site})")
    for plate, site in puzzle.plate_sites.items():
        facts.append(f"(has-position {plate} {site})")
    prune = ""
    if puzzle.pruning:
        prune = """(define-query prune-state? ()
                     (and (has-location agent1 hub)
                          (< (length (init-type-instances 'box))
                             (length (init-type-instances 'pressure-plate)))))"""
    return f"""(in-package :ww)
(ww-set *problem-name* {puzzle.name})
(ww-set *problem-type* planning)
(ww-set *solution-type* first)
(ww-set *tree-or-graph* graph)
(ww-set *threads* {workers})
(ww-set *symmetry-pruning* {'t' if puzzle.symmetry else 'nil'})
(define-types agent (agent1) location ({" ".join(puzzle.sites)} exit-site)
  box ({" ".join(puzzle.boxes)}) pressure-plate ({plates}) gate (gate1))
(include-tech plate)
(include-tech gate)
(include-tech box)
(include-tech walkability)
(include-tech reachability)
{'(include-tech topo-lower-bound)' if puzzle.lower_bound else ''}
(define-init {" ".join(facts)})
(define-init-action initialize-derived-state 0 () (always-true) ()
  (assert (propagate-changes!)))
{prune}
(define-goal (has-location agent1 exit-site))
"""


def gate_formula(solver, puzzle, bound):
    # Each box has exactly one position initially. Every changing action moves
    # one box from its old position to its new one. All other positions persist.
    positions = (*puzzle.sites, *puzzle.plates, "held")
    site_of = puzzle.plate_sites
    layers = [{(b, p): z3.Bool(f"{b}_{p}_{t}") for b in puzzle.boxes for p in positions}
              for t in range(bound + 1)]
    crossed = [z3.Bool(f"crossed_{t}") for t in range(bound + 1)]
    solver.add(z3.Not(crossed[0]), crossed[-1])
    for (box, place), bit in layers[0].items():
        solver.add(bit == (place == "hub"))
    if puzzle.pruning and len(puzzle.boxes) < len(puzzle.plates):
        solver.add(z3.Not(crossed[-1]))
    steps = []
    for t in range(bound):
        state, following = layers[t:t + 2]
        moves = {}
        enters = {key: [] for key in state}
        leaves = {key: [] for key in state}
        empty_hand = z3.Not(z3.Or([state[b, "held"] for b in puzzle.boxes]))
        occupied = {p: z3.Or([state[b, p] for b in puzzle.boxes]) for p in puzzle.plates}
        for b in puzzle.boxes:
            for p in positions[:-1]:
                site = site_of.get(p, p)
                pick = z3.Bool(f"pick_{b}_{p}_{t}")
                put = z3.Bool(f"put_{b}_{p}_{t}")
                solver.add(z3.Implies(pick, z3.And(state[b, p], empty_hand)))
                solver.add(z3.Implies(put, state[b, "held"]))
                if p in occupied:
                    solver.add(z3.Implies(put, z3.Not(occupied[p])))
                enters[b, "held"].append(pick)
                leaves[b, p].append(pick)
                enters[b, p].append(put)
                leaves[b, "held"].append(put)
                moves[pick] = f"(pickup-box agent1 {b} {site} hub)"
                moves[put] = f"(put-box agent1 {b} {p if p in site_of else 'ground'} {site})"
        walk = z3.Bool(f"walk_{t}")
        moves[walk] = "(move agent1 ((walk hub (gate1) exit-site)))"
        solver.add(z3.Implies(walk, z3.And(list(occupied.values()))))
        # No action is a padding step. A plan may end as soon as it crosses.
        solver.add(z3.AtMost(*moves, 1), z3.Implies(z3.Or(list(moves)), z3.Not(crossed[t])))
        solver.add(crossed[t + 1] == z3.Or(crossed[t], walk))
        for key in state:
            solver.add(following[key] == z3.Or(z3.Or(enters[key]),
                       z3.And(state[key], z3.Not(z3.Or(leaves[key])))))
        steps.append(moves)
    return steps


def switch_formula(solver, bound):
    # In switch-test the sole reachable toggle controls both gates with opposite
    # polarity. Holding is invariant, so the complete goal reduces to switch-on.
    on = z3.Bool("initially_on")
    solver.add(z3.Not(on))
    steps = []
    for t in range(bound):
        toggle, after = z3.Bools(f"toggle_{t} after_{t}")
        solver.add(after == z3.Xor(on, toggle))
        steps.append({toggle: "(toggle-switch agent1 main-switch)"})
        on = after
    solver.add(on)
    return steps


def sat_run(puzzle, bound, threads):
    start = perf_counter()
    solver = z3.Tactic("sat").solver()
    solver.set(threads=threads)
    if puzzle.name.startswith("sequence-"):
        steps = sat_gate_sequence.formula(solver, bound)
    elif puzzle.name == "switch-test":
        steps = switch_formula(solver, bound)
    else:
        steps = gate_formula(solver, puzzle, bound)
    built = perf_counter()
    remaining_ms = int((60 - (built - start)) * 1000)
    if remaining_ms <= 0:
        return "TIMEOUT", None, (built - start) * 1000, 0.0
    solver.set(timeout=remaining_ms)
    result = solver.check()
    end = perf_counter()
    if result == z3.unknown and solver.reason_unknown() not in ("timeout", "canceled"):
        raise RuntimeError(solver.reason_unknown())
    plan = None
    if result == z3.sat:
        model = solver.model()
        plan = [action for step in steps for bit, action in step.items()
                if z3.is_true(model.eval(bit))]
    status = "TIMEOUT" if result == z3.unknown else str(result).upper()
    return status, plan, (built - start) * 1000, (end - built) * 1000


def run(directory, args):
    for folder in ("src", "tech"):
        shutil.copytree(ROOT / folder, directory / folder,
                        ignore=shutil.ignore_patterns("problem*.lisp", "*.fasl"))
    (directory / "probs").mkdir()
    for source in (ROOT / "wouldwork.asd", ROOT / "probs/problem-blocks3.lisp"):
        shutil.copy2(source, directory / source.relative_to(ROOT))
    shutil.copy2(ROOT / "test/talos/problem-switch-test.lisp", directory / "probs")
    two = ("box1", "box2")
    puzzles = [Puzzle("switch-test", (), 0, (0, 1)),
               Puzzle("pilot-basic", two, 0, (4, 5)),
               Puzzle("pilot-extra", two, args.shelves, (4, 5)),
               Puzzle("pilot-shortage", two[:1], 0, (5,)),
               Puzzle("pilot-pruned", two[:1], 0, (5,), True)]
    if args.large or args.five_features:
        profiles = [("pilot-five", False, False)]
        if args.five_features:
            profiles.extend([("five-symmetry", True, False), ("five-bound", False, True),
                             ("five-both", True, True)])
        puzzles = [Puzzle(name, tuple(f"box{i}" for i in range(1, 6)), 6, (10, 11),
                          plates=tuple(f"plate-{c}" for c in "abcde"),
                          symmetry=symmetry, lower_bound=lower_bound)
                   for name, symmetry, lower_bound in profiles]
    if args.sequence:
        puzzles = [Puzzle(name, two, 0, (9, 10)) for name in ("sequence-base", "sequence-tuned")]
    driver = ['(require :asdf)',
              '(push *default-pathname-defaults* asdf:*central-registry*)',
              '(asdf:initialize-output-translations '
              '`(:output-translations (t ,(merge-pathnames "cache/" *default-pathname-defaults*)) '
              ':ignore-inherited-configuration))',
              '(asdf:load-asd (truename "wouldwork.asd"))',
              '(assert (uiop:pathname-equal (asdf:system-source-directory :wouldwork) '
              '(uiop:getcwd)))',
              '(asdf:load-system :wouldwork)', '(in-package :ww)',
              f'(load "{(ROOT / "test/search/sat-pilot.lisp").as_posix()}")']
    rows = []
    shared_results = {}
    for puzzle in puzzles:
        workers = 0 if args.threads == 1 else args.threads
        if args.sequence:
            spec = sat_gate_sequence.fixture(puzzle.name, workers, puzzle.name == "sequence-tuned")
            (directory / f"probs/problem-{puzzle.name}.lisp").write_text(spec)
        elif puzzle.name != "switch-test":
            (directory / f"probs/problem-{puzzle.name}.lisp").write_text(fixture(puzzle, workers))
        else:
            path = directory / "probs/problem-switch-test.lisp"
            path.write_text(path.read_text() + f"\n(ww-set *threads* {workers})\n")
        driver.append(f'(sat-pilot-stage "{puzzle.name}")')
        for bound in puzzle.bounds:
            key = (None if args.sequence or args.five_features else puzzle.name, bound)
            if key not in shared_results:
                shared_results[key] = sat_run(puzzle, bound, args.threads)
            status, plan, build, check = shared_results[key]
            if args.sequence:
                expected = bound >= 10
            elif puzzle.name == "switch-test":
                expected = bound >= 1
            else:
                expected = len(puzzle.boxes) >= len(puzzle.plates) and bound >= 2 * len(puzzle.plates) + 1
            if status != "TIMEOUT" and (plan is not None) != expected:
                raise ValueError(f"Unexpected SAT result for {puzzle.name} at {bound}: {plan}")
            actions = " ".join(plan or [])
            driver.append(f"(sat-pilot-check {bound} {'t' if expected else 'nil'} '({actions}))")
            rows.append((puzzle.name, bound, status, build, check))
    (directory / "run.lisp").write_text("\n".join(driver), encoding="utf8")
    proc = subprocess.run(["sbcl", "--dynamic-space-size", "4096", "--non-interactive",
                           "--load", "run.lisp"], cwd=directory, capture_output=True,
                          env={**os.environ, "WOULDWORK_INSTANCE": "sat-pilot"},
                          encoding="utf8", errors="replace")
    if proc.returncode:
        raise RuntimeError(proc.stdout[-2000:] + proc.stderr[-10000:])
    output = proc.stdout.splitlines()
    if args.sequence or args.five_features:
        for line in output:
            if line.startswith("PILOT-FEATURES "):
                print(line)
    stages = {line.split()[1]: float(line.split()[2]) for line in output
              if line.startswith("PILOT-STAGE ")}
    runs = [line.split() for line in output if line.startswith("PILOT-RUN ")]
    print(f"Z3 {z3.get_version_string()}, {args.threads} solver thread(s); times in ms")
    print("case             H  SAT       SAT build  SAT check  WW        WW stage  WW search  WW states")
    for (name, bound, status, build, check), (_, h, ww, search, states) in zip(rows, runs, strict=True):
        assert int(h) == bound
        print(f"{name:16} {bound:2} {status:7} {build:10.2f}"
              f" {check:10.2f} {ww:7} {stages[name]:9.1f} {float(search):10.1f} {states:10}")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--threads", type=int, choices=(1, 4), default=1)
    cases = parser.add_mutually_exclusive_group()
    cases.add_argument("--large", action="store_true", help="run the five-box, six-extra-site comparison")
    cases.add_argument("--five-features", action="store_true", help="compare four Wouldwork settings on five boxes")
    cases.add_argument("--sequence", action="store_true", help="compare baseline and tuned search across three gates")
    parser.add_argument("--shelves", type=int, choices=range(1, 11), default=3)
    with tempfile.TemporaryDirectory(prefix="wouldwork-sat-") as temp:
        run(Path(temp), parser.parse_args())
