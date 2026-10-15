"""Exact-distance table pre-check for tiles7a (independent of wouldwork).

Reduced puzzle: the yellow tile Y plus the named pieces; every other piece
(and every single square not named) is removed, which only frees cells, so
the reduced distance never overestimates the real one.  The script builds
the reduced puzzle's reachable states from the start, searches breadth-first
backward from every state with Y on the goal cell, and prints the table size
and the start state's exact reduced distance (the bound such a table gives).

Usage (from the wouldwork root):
  python doc/problems/tiles7a/distance-table-check.py ROW COL PIECE ...
  e.g.  ... 3 3 LELL 3HOR DTEE
Pieces: 2HOR 3HOR CANE LELL DELL RTEE LTEE DTEE UTEE LKEY RKEY, and the
single squares S0..S4 in the order of the spec's (squares ...) list.
"""
import re
import sys
import time
from collections import deque

SPEC = "probs/problem-tiles7a-heuristic-1.lisp"
DIRS = {"right": (0, 1), "left": (0, -1), "down": (1, 0), "up": (-1, 0)}
BOARD = {(r, c) for r in range(8) for c in range(8)}


def main():
    goal = (int(sys.argv[1]), int(sys.argv[2]))
    pieces = ["Y"] + sys.argv[3:]
    shapes, loc = read_spec(open(SPEC).read())
    start = time.time()
    size, dist = reduced_distance(pieces, shapes, loc, goal)
    manhattan = abs(loc["Y"][0] - goal[0]) + abs(loc["Y"][1] - goal[1])
    print(f"goal {goal} pieces {pieces[1:]}: table {size} states, "
          f"bound {dist}, manhattan {manhattan}, {time.time() - start:.1f} s")


def read_spec(src):
    """Shapes from each tile's 'right' template; start cells from define-init."""
    coords = lambda s: [tuple(map(int, x)) for x in re.findall(r"\((-?\d+) \. (-?\d+)\)", s)]
    templates = {}
    for m in re.finditer(r"\(pre-post-emptys> (\S+) right\s+(\(\(.*?\)\))\s+(\(\(.*?\)\))\)", src):
        templates[m.group(1)] = (coords(m.group(2)), coords(m.group(3)))
    shapes = {}
    for tile, (pre, post) in templates.items():
        rows = {}
        for r, c in post:
            rows.setdefault(r, [None, None])[0] = c
        for r, c in pre:
            rows.setdefault(r, [None, None])[1] = c
        shapes[tile] = [(r, c) for r, (a, b) in rows.items() for c in range(a, b)]
    loc = {m.group(1): (int(m.group(2)), int(m.group(3)))
           for m in re.finditer(r"\(loc (\S+) (\d) (\d)\)", src)}
    squares = coords(re.search(r"\(squares (\(\(.*?\)\))\)", src).group(1))
    for i, cell in enumerate(squares):
        shapes[f"S{i}"] = [(0, 0)]
        loc[f"S{i}"] = cell
    return shapes, loc


def reduced_distance(pieces, shapes, loc, goal):
    """Reachable reduced states from the start, then exact distances to the goal set."""
    yi = pieces.index("Y")
    s0 = tuple(loc[p] for p in pieces)
    seen = {s0}
    queue = deque([s0])
    while queue:
        for n in successors(queue.popleft(), pieces, shapes):
            if n not in seen:
                seen.add(n)
                queue.append(n)
    goals = [s for s in seen if s[yi] == goal]
    dist = {s: 0 for s in goals}
    queue = deque(goals)
    while queue:
        s = queue.popleft()
        for n in successors(s, pieces, shapes):
            if n not in dist:
                dist[n] = dist[s] + 1
                queue.append(n)
    return len(seen), dist.get(s0)


def successors(state, pieces, shapes):
    """Moves are reversible, so the same successors serve both searches."""
    occupied = {}
    for p, (r, c) in zip(pieces, state):
        for a, b in shapes[p]:
            occupied[(r + a, c + b)] = p
    for i, (p, (r, c)) in enumerate(zip(pieces, state)):
        for dr, dc in DIRS.values():
            q = (r + dr, c + dc)
            cells = [(q[0] + a, q[1] + b) for a, b in shapes[p]]
            if all(x in BOARD and occupied.get(x, p) == p for x in cells):
                yield state[:i] + (q,) + state[i + 1:]


if __name__ == "__main__":
    main()
