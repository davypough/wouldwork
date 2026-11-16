"""The boxes layout, using Talos walking, pickup and placement semantics."""
from collections import defaultdict

import z3


AREAS = ("area1", "area2", "area3", "area4")
BOX_START = {"box1": "area1", "box2": "area2"}
# source, destination, controlling plate, plate location (all gates bidirectional)
GATES = {"gate1": ("area1", "area2", "plate1", "area1"),
         "gate2": ("area1", "area3", "plate2", "area1"),
         "gate3": ("area3", "area4", "plate3", "area3")}


def fixture(name, workers, tuned):
    plates = []
    facts = ["(has-location agent1 area1)"]
    for box, site in BOX_START.items():
        facts.extend([f"(has-location {box} {site})", f"(has-height {box} 2)"])
    for gate, (a, b, plate, site) in GATES.items():
        plates.append(plate)
        facts.extend([f"(has-position {plate} {site})",
                      f"(controls (({plate})) {gate} normal)",
                      f"(traverse-via {a} (({gate})) {b})"])
    return f"""(in-package :ww)
(ww-set *problem-name* {name})
(ww-set *problem-type* planning)
(ww-set *solution-type* first)
(ww-set *tree-or-graph* graph)
(ww-set *threads* {workers})
(ww-set *symmetry-pruning* {'t' if tuned else 'nil'})
(define-types agent (agent1) location ({" ".join(AREAS)})
  box ({" ".join(BOX_START)}) pressure-plate ({" ".join(plates)}) gate ({" ".join(GATES)}))
(include-tech plate)
(include-tech gate)
(include-tech box)
(include-tech walkability)
{'(include-tech topo-lower-bound)' if tuned else ''}
(define-init {" ".join(facts)})
(define-init-action initialize-derived-state 0 () (always-true) ()
  (assert (propagate-changes!)))
(define-goal (has-location agent1 area4))
"""


def routes():
    """The area graph is a tree, so each ordered pair has one simple route."""
    neighbors = defaultdict(list)
    for gate, (a, b, plate, _) in GATES.items():
        neighbors[a].append((b, gate, plate))
        neighbors[b].append((a, gate, plate))
    for source in AREAS:
        todo = [(source, None, (), ())]
        while todo:
            here, parent, segments, required = todo.pop()
            for dest, gate, plate in neighbors[here]:
                if dest == parent:
                    continue
                path = (*segments, f"(walk {here} ({gate}) {dest})")
                controls = (*required, plate)
                yield source, dest, path, controls
                todo.append((dest, here, path, controls))


def formula(solver, bound):
    places = {area: area for area in AREAS}
    places.update({plate: site for _, _, plate, site in GATES.values()})
    initial = {("agent1", a): a == "area1" for a in AREAS}
    for box, start in BOX_START.items():
        initial.update({(box, p): p == start for p in (*places, "held")})
    layers = [{k: z3.Bool(f"{k[0]}_{k[1]}_{t}") for k in initial} for t in range(bound + 1)]
    for k, value in initial.items():
        solver.add(layers[0][k] == value)
    solver.add(layers[-1]["agent1", "area4"])
    steps = []
    for t in range(bound):
        state, after = layers[t:t + 2]
        moves, enters, leaves = {}, defaultdict(list), defaultdict(list)

        def move(label, obj, old, new, conditions):
            bit = z3.Bool(f"a_{t}_{len(moves)}")
            moves[bit] = label
            solver.add(z3.Implies(bit, z3.And(state[obj, old], *conditions)))
            enters[obj, new].append(bit)
            leaves[obj, old].append(bit)

        empty_hand = z3.Not(z3.Or([state[b, "held"] for b in BOX_START]))
        occupied = {p: z3.Or([state[b, p] for b in BOX_START]) for p in places if p not in AREAS}
        for b in BOX_START:
            for place, area in places.items():
                at = state["agent1", area]
                move(f"(pickup-box agent1 {b} {area} {area})", b, place, "held", [at, empty_hand])
                support = place if place in occupied else "ground"
                clear = [z3.Not(occupied[place])] if place in occupied else []
                move(f"(put-box agent1 {b} {support} {area})", b, "held", place, [at, *clear])
        for source, dest, path, controls in routes():
            move(f"(move agent1 ({' '.join(path)}))", "agent1", source, dest,
                 [occupied[p] for p in controls])
        solver.add(z3.AtMost(*moves, 1))
        for k in state:
            solver.add(after[k] == z3.Or(z3.Or(enters[k]), z3.And(state[k], z3.Not(z3.Or(leaves[k])))))
        steps.append(moves)
    return steps
