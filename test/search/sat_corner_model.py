"""Boolean action model with exact Corner beam consequences added on demand."""

from time import perf_counter
import z3
from sat_corner_beams import CONNS, HUE, LOCS, TERMS

GATE = ("open", "gate1")
KINDS = (
    "move",
    "pickup-connector",
    "pickup-connector-retaining-pairings",
    "select-termini",
    "put-connector",
)


def conjunction(items):
    return z3.And(list(items))


def disjunction(items):
    return z3.Or(list(items))


def assignment(bits, facts):
    return conjunction(bit if key in facts else z3.Not(bit) for key, bit in bits.items())


def values(model, bits):
    return {k for k, b in bits.items() if z3.is_true(model.eval(b, model_completion=True))}


class CornerSAT:
    def __init__(self, geometry, initial, *, receiver_rule=False, gate_rule=False,
                 source_rule=False, one_color=False):
        self.geometry = geometry
        self.receiver_rule = receiver_rule
        self.gate_rule = gate_rule
        self.source_rule = source_rule
        self.one_color = one_color
        self.solver = z3.Solver()
        self.solver.set(timeout=10000)
        self.states = []
        self.steps = []
        self.learned = set()
        self.solve_time = self.beam_time = 0.0
        self.locations = [("has-location", o, p) for o in ("agent1", *CONNS) for p in LOCS]
        self.links = [(c, t) for c in CONNS for t in TERMS if c != t]
        self.derived = [
            GATE,
            *(("color", c, h) for c in CONNS for h in ("red", "blue")),
            *(("active", r) for r in ("receiver1", "receiver2", "receiver3")),
            *(("crossing-active", x) for x in geometry.cross),
        ]
        self.keys = [
            *self.locations,
            *(("holding", c) for c in CONNS),
            *((rel, c, t) for rel in ("paired", "selected") for c, t in self.links),
            *self.derived,
        ]
        if initial - set(self.keys):
            raise ValueError(f"Unknown initial facts: {initial - set(self.keys)}")
        self.solver.add(assignment(self.new_state(), initial))

    def new_state(self):
        bits = {k: z3.Bool(f"{len(self.states)}:{'.'.join(k)}") for k in self.keys}
        if self.one_color:
            for c in CONNS:
                self.solver.add(z3.Or(z3.Not(bits["color", c, "red"]), z3.Not(bits["color", c, "blue"])))
        if self.gate_rule:
            self.solver.add(bits[GATE] == bits["active", "receiver1"])
        if self.receiver_rule or self.source_rule:
            placed = {c: disjunction(bits["has-location", c, p] for p in LOCS) for c in CONNS}
        if self.source_rule:
            self.add_source_paths(bits, placed)
        if self.receiver_rule:
            for receiver in ("receiver1", "receiver2", "receiver3"):
                feed = disjunction(
                    z3.And(placed[c], bits["paired", c, receiver], bits["color", c, HUE[receiver]])
                    for c in CONNS
                )
                self.solver.add(z3.Implies(bits["active", receiver], feed))
        self.states.append(bits)
        return bits

    def add_source_paths(self, bits, placed):
        linked = {(c, d): z3.Or(bits["paired", c, d], bits["paired", d, c])
                  for c in CONNS for d in CONNS if c != d}
        for source in ("transmitter1", "transmitter2"):
            reachable = {c: z3.And(placed[c], bits["paired", c, source]) for c in CONNS}
            # A simple path visits at most every connector once.
            for _ in range(len(CONNS) - 1):
                reachable = {
                    c: z3.Or(reachable[c], z3.And(placed[c], disjunction(
                        z3.And(linked[c, d], reachable[d]) for d in CONNS if d != c)))
                    for c in CONNS
                }
            for c in CONNS:
                self.solver.add(z3.Implies(bits["color", c, HUE[source]], reachable[c]))

    def add_step(self, fixed=None, allow_finish=False):
        i = len(self.steps)
        s = self.states[-1]
        n = self.new_state()

        # The action fields are mutually exclusive kind, object, destination and a
        # subset of selected termini. Unused fields are false for a unique decoding.
        def fields(names):
            return {k: z3.Bool(f"a{i}:{k}") for k in names}

        kind = fields((*KINDS, "finished") if allow_finish else KINDS)
        obj, dest = fields(CONNS), fields(LOCS)
        chosen = {t: z3.Bool(f"a{i}:select:{t}") for t in TERMS}
        self.steps.append((kind, obj, dest, chosen))
        add = self.solver.add
        add(z3.PbEq([(x, 1) for x in kind.values()], 1))
        move, lift, retain, select, put = (kind[k] for k in KINDS)
        finished = kind.get("finished", False)
        if allow_finish:
            add(z3.Implies(finished, conjunction(n[k] == s[k] for k in self.derived)))
            if i:
                add(z3.Implies(self.steps[i-1][0]["finished"], finished))
        pickup = z3.Or(lift, retain)
        add(z3.PbLe([(x, 1) for x in obj.values()], 1),
            disjunction(obj.values()) == z3.Not(z3.Or(move, finished)))
        add(z3.PbLe([(x, 1) for x in dest.values()], 1), disjunction(dest.values()) == move)
        add(disjunction(chosen.values()) == select)
        at = {p: s["has-location", "agent1", p] for p in LOCS}
        empty = z3.Not(disjunction(s["holding", c] for c in CONNS))
        for p in LOCS:
            routes = [
                z3.And(at[a], s[GATE] if gated else True)
                for (a, b), gated in self.geometry.route.items()
                if b == p
            ]
            add(z3.Implies(dest[p], disjunction(routes)))
            add(n["has-location", "agent1", p] == z3.If(move, dest[p], at[p]))
        for c in CONNS:
            held = s["holding", c]
            colocated = disjunction(z3.And(at[p], s["has-location", c, p]) for p in LOCS)
            touched = obj[c]
            add(z3.Implies(z3.And(touched, pickup), z3.And(empty, colocated)))
            add(z3.Implies(z3.And(touched, z3.Or(put, select)), held))
            add(n["holding", c] == z3.If(touched, z3.Or(pickup, z3.And(held, z3.Not(put))), held))
            for p in LOCS:
                add(
                    n["has-location", c, p]
                    == z3.If(
                        z3.And(touched, pickup),
                        False,
                        z3.If(z3.And(touched, put), at[p], s["has-location", c, p]),
                    )
                )
            incident = [s["paired", a, b] for a, b in self.links if c in (a, b)]
            add(z3.Implies(z3.And(touched, retain), disjunction(incident)))
            add(z3.Implies(touched, z3.Not(chosen[c])))
            for t in TERMS:
                if t == c:
                    continue
                visible = []
                for p, q in self.geometry.selectable:
                    if t in CONNS and q in LOCS:
                        visible.append(z3.And(at[p], s["has-location", t, q]))
                    elif t == q:
                        visible.append(at[p])
                legal = [
                    disjunction(visible),
                    z3.Not(s["paired", c, t]),
                    z3.Not(s["selected", c, t]),
                ]
                if t in CONNS:
                    legal.append(z3.Not(s["paired", t, c]))
                add(z3.Implies(z3.And(touched, chosen[t]), conjunction(legal)))
        for c, t in self.links:
            cleared = z3.And(lift, z3.Or(obj[c], obj.get(t, False)))
            add(
                n["paired", c, t]
                == z3.And(
                    z3.Not(cleared),
                    z3.Or(s["paired", c, t], z3.And(put, obj[c], s["selected", c, t])),
                )
            )
            add(
                n["selected", c, t]
                == z3.And(
                    z3.Not(z3.And(put, obj[c])),
                    z3.Or(s["selected", c, t], z3.And(obj[c], chosen[t])),
                )
            )
        for c in CONNS:
            add(
                z3.PbLe(
                    [
                        (n[rel, a, b], 1)
                        for rel in ("paired", "selected")
                        for a, b in self.links
                        if c in (a, b)
                    ],
                    4,
                )
            )
        if fixed is not None:
            add(self.action_is(i, fixed))

    def action_is(self, i, action):
        kind, obj, dest, chosen = self.steps[i]
        name, c, p, terms = action
        return z3.And(
            kind[name], assignment(obj, {c}), assignment(dest, {p}), assignment(chosen, terms)
        )

    def read_action(self, i, model):
        kind, obj, dest, chosen = self.steps[i]
        return (
            next(iter(values(model, kind))),
            next(iter(values(model, obj)), None),
            next(iter(values(model, dest)), None),
            frozenset(values(model, chosen)),
        )

    def check(self, deadline=None):
        """Only accept a model once each structural arrangement has its actual beam result.
        A learned implication covers all action sequences inducing the same inputs at that time."""
        while True:
            start = perf_counter()
            if deadline is not None:
                if start >= deadline:
                    raise TimeoutError
                self.solver.set(timeout=max(1, int((deadline-start)*1000)))
            result = self.solver.check()
            self.solve_time += perf_counter() - start
            if result == z3.unknown:
                if deadline is not None and self.solver.reason_unknown() in ("timeout", "canceled"):
                    raise TimeoutError
                raise RuntimeError(self.solver.reason_unknown())
            if result == z3.unsat:
                return None
            model = self.solver.model()
            missing = False
            for i, (old, new) in enumerate(zip(self.states, self.states[1:])):
                if deadline is not None and perf_counter() >= deadline:
                    raise TimeoutError
                finished = self.steps[i][0].get("finished")
                if finished is not None and z3.is_true(model.eval(finished)):
                    continue
                structural = {k: v for k, v in new.items() if k[0] in ("has-location", "paired")}
                facts = frozenset(values(model, structural))
                opened = z3.is_true(model.eval(old[GATE]))
                key = i, facts, opened
                if key in self.learned:
                    continue
                start = perf_counter()
                places = {o: p for rel, o, p in facts if rel == "has-location"}
                pairs = {(c, t) for rel, c, t in facts if rel == "paired"}
                lighting = self.geometry.derive(places, pairs, opened)
                when = z3.And(assignment(structural, facts), old[GATE] == opened)
                if finished is not None:
                    when = z3.And(when, z3.Not(finished))
                derived = {k: new[k] for k in self.derived}
                consequence = False if lighting is None else assignment(derived, lighting)
                self.solver.add(z3.Implies(when, consequence))
                self.learned.add(key)
                self.beam_time += perf_counter() - start
                missing = True
            if not missing:
                return model
