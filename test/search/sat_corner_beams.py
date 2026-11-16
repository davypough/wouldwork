"""Corner's beam rules for grounding the SAT transition relation on demand."""

from collections import defaultdict

LOCS = tuple(f"location{i}" for i in range(1, 5))
CONNS = tuple(f"connector{i}" for i in range(1, 4))
HUE = {
    "transmitter1": "red",
    "transmitter2": "blue",
    "receiver1": "red",
    "receiver2": "red",
    "receiver3": "blue",
}
TERMS = (*CONNS, *HUE)


class Geometry:
    def __init__(self, rows):
        self.clear = defaultdict(set)
        self.closed_clear = set()
        self.selectable = set()
        self.route = {}
        self.cross = {}
        self.along = defaultdict(tuple)
        self.before_gate = {}
        self.coincide = set()
        for kind, *data in rows:
            if kind in ("visible", "prefix"):
                body, *beam = data
                self.clear[tuple(body)].add(tuple(beam))
            elif kind == "closed-visible":
                self.closed_clear.add(tuple(data))
            elif kind == "selectable":
                self.selectable.add(tuple(data))
            elif kind == "intersection":
                x, (a, b, c, d) = data
                self.cross[x] = ((a, b), (c, d))
            elif kind == "relay-order":
                (self.relay_order,) = data
            elif kind == "static":
                rel, *args = data[0]
                if rel == "crossings-along-beam>":
                    a, xs, b = args
                    self.along[a, b] = tuple(xs)
                elif rel == "beam-crossings-before-gate>":
                    a, xs, gate, b = args
                    assert gate == "gate1"
                    self.before_gate[a, b] = set(xs)
                elif rel == "crossings-coincide>":
                    self.coincide.add(tuple(args))
                elif rel == "traverse-via":
                    a, gates, b = args
                    assert gates in ([], [["gate1"]])
                    self.route[a, b] = bool(gates)
        assert len(self.route) == 12 and self.cross and set(self.relay_order) == set(CONNS)

    def derive(self, places, pairs, old_gate):
        bodies = [
            ("has-location", "agent1" if o == "agent1" else "connector1", p)
            for o, p in places.items()
        ]
        clear = self.clear[()].intersection(*(self.clear[b] for b in bodies))
        network = BeamNetwork(self, clear, places, pairs)
        gate = old_gate
        for _ in range(10):
            result = network.settle(gate)
            if result is None:
                return None
            facts, clash = result
            next_gate = ("active", "receiver1") in facts
            if gate == next_gate:
                if clash:
                    return None
                if gate:
                    facts.add(("open", "gate1"))
                return facts
            gate = next_gate
        return None


class BeamNetwork:
    def __init__(self, geometry, clear, places, pairs):
        self.g = geometry
        self.clear = clear
        self.places = places
        self.links = set(pairs) | {(b, a) for a, b in pairs}

    def lighting(self, active, gate):
        """First arrival wins; two hues arriving in the same round leave a relay dark."""
        frontier = {t: (HUE[t], 0) for t in HUE if t.startswith("transmitter")}
        visited = set()
        lit = {}
        occupied = set()
        clash = False
        while frontier:
            following = {}
            for c in self.g.relay_order:
                if c in visited or c not in self.places:
                    continue
                dest = self.places[c]
                arrivals = {
                    (h, depth + 1)
                    for src, (h, depth) in frontier.items()
                    if (src, c) in self.links
                    and self.visible(dest, self.places.get(src, src), gate, active, incoming=True)
                }
                if arrivals:
                    visited.add(c)
                if len(arrivals) == 1:
                    if dest in occupied:
                        clash = True
                    else:
                        occupied.add(dest)
                        following[c] = arrivals.pop()
            lit.update(following)
            frontier = following
        return lit, clash

    def visible(self, a, b, gate, active, incoming=False):
        if a == b:
            return True
        return (
            (a, b) in self.clear
            and (gate or (a, b) in self.g.closed_clear)
            and not active.intersection(self.g.along[(b, a) if incoming else (a, b)])
        )

    def crossing_inputs(self, active, gate):
        lit, _ = self.lighting(active, gate)
        live = set()
        for a, b in self.links:
            if a.startswith("transmitter") or a in lit:
                if b in self.places or b.startswith("receiver"):
                    live.add((self.places.get(a, a), self.places.get(b, b)))

        def reaches(beam, x):
            a, b = beam
            if (a, b) not in live:
                a, b = b, a
            if (a, b) not in live or (a, b, x) not in self.clear:
                return False
            before = self.g.before_gate.get((a, b))
            if not gate and before is not None and x not in before:
                return False
            for other in self.g.along[a, b]:
                if other == x:
                    break
                if other in active and (other, x) not in self.g.coincide:
                    return False
            return True

        distances = {self.places[c]: depth for c, (_, depth) in lit.items()}
        distances.update(transmitter1=0, transmitter2=0)
        return {
            x: max(distances.get(a, 10**9) for a, b in beams)
            for x, beams in self.g.cross.items()
            if all(reaches(b, x) for b in beams)
        }

    def settle(self, gate):
        active = set()
        previous = None
        for _ in range(10):
            upcoming = set(self.crossing_inputs(active, gate))
            if upcoming == active:
                break
            if upcoming == previous:
                candidates = active | upcoming
                active = candidates
                if set(self.crossing_inputs(active, gate)) != active:
                    active = set()
                    while candidates - active:
                        priorities = self.crossing_inputs(active, gate)
                        remaining = priorities.keys() & (candidates - active)
                        if not remaining:
                            break
                        active.add(min(remaining, key=lambda x: (priorities[x], x)))
                    if set(self.crossing_inputs(active, gate)) != active:
                        return None
                break
            previous, active = active, upcoming
        else:
            return None
        lit, clash = self.lighting(active, gate)
        facts = {("color", c, hue) for c, (hue, _) in lit.items()}
        facts.update(("crossing-active", x) for x in active)
        for c, (hue, _) in lit.items():
            for r in ("receiver1", "receiver2", "receiver3"):
                if (
                    HUE[r] == hue
                    and (c, r) in self.links
                    and self.visible(self.places[c], r, gate, active)
                ):
                    facts.add(("active", r))
        return facts, clash
