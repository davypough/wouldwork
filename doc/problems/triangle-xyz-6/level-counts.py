# level-counts.py -- distinct boards per level for the triangle peg puzzle
# (occupancy model of probs/problem-triangle-xyz.lisp), forward from the
# starting board (one hole at coordinates (1,1,N)) and backward from each
# position where a single last peg can end.  Independent of wouldwork.
# Usage: python level-counts.py 6
import sys
N = int(sys.argv[1])
pos = [(x, y, N + 2 - x - y) for x in range(1, N + 1) for y in range(1, N + 2 - x)]
idx = {p: i for i, p in enumerate(pos)}
dirs = [(0, 1, -1), (0, -1, 1), (1, 0, -1), (-1, 0, 1), (1, -1, 0), (-1, 1, 0)]
lines = []
for p in pos:
    for d in dirs:
        o = tuple(a + b for a, b in zip(p, d))
        t = tuple(a + 2 * b for a, b in zip(p, d))
        if t in idx:
            lines.append((1 << idx[p], 1 << idx[o], 1 << idx[t]))
full = (1 << len(pos)) - 1
start = full & ~(1 << idx[(1, 1, N)])
def fwd(b):  # jump: from and over occupied, to empty
    for f, o, t in lines:
        if b & f and b & o and not b & t:
            yield b ^ f ^ o ^ t
def bwd(b):  # un-jump: from and over empty, to occupied
    for f, o, t in lines:
        if not b & f and not b & o and b & t:
            yield b ^ f ^ o ^ t
lvl = {start}; F = [1]
for k in range(len(pos) - 2):
    lvl = {n for b in lvl for n in fwd(b)}; F.append(len(lvl))
ends = sorted(pos[b.bit_length() - 1] for b in lvl)
print("N", N, "forward per level", F, "total", sum(F), "ends", ends)
for e in ends:
    lvl = {1 << idx[e]}; B = [1]
    for k in range(len(pos) - 2):
        lvl = {n for b in lvl for n in bwd(b)}; B.append(len(lvl))
    print(" backward from", e, B, "total", sum(B), "reaches start:", start in lvl)
