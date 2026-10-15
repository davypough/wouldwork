# relaxed-bound-check.py -- independent check of the delete-relaxation bounds (h-max, LM-cut)
# for tiles7a, without wouldwork.  Reads the move templates, piece positions, squares and
# empty cells from a spec's define-init section, and the yellow tile's target from the
# command line.  Facts: piece at a reference position, cell empty, square at a cell; a move
# needs its piece's place and pre-move cells empty and adds the new place and vacated cells.
# Usage (from the wouldwork root):
#   python doc/problems/tiles7a/relaxed-bound-check.py probs/problem-tiles7a-heuristic-1.lisp 3 3
# To check another state, give a file holding a define-init section with that state's
# (loc ...), (squares ...) and (emptys ...) and the pre-post-emptys> templates.
import re, sys, heapq
src = open(sys.argv[1]).read().split('(define-goal')[0].split('(define-init')[1]
goal = (int(sys.argv[2]), int(sys.argv[3]))
pp = {}
for m in re.finditer(r'\(pre-post-emptys> (\S+) (\S+)\s+(\(.*?\)\)|\(\))\s+(\(.*?\)\))\)', src):
    t,d,pre,post = m.groups()
    cells = lambda s: [(int(a),int(b)) for a,b in re.findall(r'\((-?\d+) \. (-?\d+)\)', s)]
    pp[(t,d)] = (cells(pre), cells(post))
locs = {t:(int(r),int(c)) for t,r,c in re.findall(r'^\s*\(loc (\S+) (\d) (\d)\)', src, re.M)}
sq = re.search(r'\(squares \((.*?)\)\)\n', src).group(1)
squares = [(int(a),int(b)) for a,b in re.findall(r'\((\d) \. (\d)\)', sq)]
em = re.search(r'\(emptys \((.*?)\)\)\)', src).group(1)
emptys = [(int(a),int(b)) for a,b in re.findall(r'\((\d) \. (\d)\)', em)]
D = {'right':(0,1),'left':(0,-1),'down':(1,0),'up':(-1,0)}
on = lambda c: 0<=c[0]<8 and 0<=c[1]<8
ops = []  # (pre, add, cost)
for (t,d),(pre,post) in pp.items():
    for r in range(-4,12):
        for c in range(-4,12):
            pc = [(r+a,c+b) for a,b in pre]; qc=[(r+a,c+b) for a,b in post]
            if not all(map(on,pc)): continue
            dr,dc = D[d]
            ops.append(([('at',t,r,c)]+[('e',)+x for x in pc], [('at',t,r+dr,c+dc)]+[('e',)+x for x in qc], 1))
for r in range(8):
    for c in range(8):
        for dr,dc in D.values():
            t=(r+dr,c+dc)
            if on(t): ops.append(([('sq',r,c),('e',)+t],[('sq',)+t,('e',r,c)],1))
init = [('at',t)+p for t,p in locs.items()] + [('sq',)+s for s in squares] + [('e',)+e for e in emptys]
G = [('at','Y')+goal]
# index
facts={}
def fid(f):
    if f not in facts: facts[f]=len(facts)
    return facts[f]
I=fid('I'); GF=fid('G')
OPS=[]
for pre,add,c in ops: OPS.append(([fid(f) for f in pre] or [I],[fid(f) for f in add],c))
OPS.append(([fid(f) for f in G],[GF],0))
initids=[I]+[fid(f) for f in init]
deps={}
for i,(pre,add,c) in enumerate(OPS):
    for p in set(pre): deps.setdefault(p,[]).append(i)
def hmax(cost):
    h=[None]*len(facts); pq=[]
    for f in initids: h[f]=0; heapq.heappush(pq,(0,f))
    cnt=[len(set(p)) for p,_,_ in OPS]; done=[False]*len(facts)
    while pq:
        v,f=heapq.heappop(pq)
        if done[f]: continue
        done[f]=True
        for o in deps.get(f,[]):
            cnt[o]-=1
            if cnt[o]==0:
                nv=v+cost[o]
                for a in OPS[o][1]:
                    if h[a] is None or nv<h[a]: h[a]=nv; heapq.heappush(pq,(nv,a))
    return h
cost=[c for _,_,c in OPS]
h=hmax(cost); print('h-max', h[GF])
total=0
while True:
    h=hmax(cost)
    if h[GF] is None: print('unreachable'); break
    if h[GF]==0: break
    sup=[max(p,key=lambda f:h[f]) if all(h[f] is not None for f in p) else None for p,_,_ in OPS]
    # goal zone backward via zero-cost arcs
    ach={}
    for o,(p,a,c) in enumerate(OPS):
        if sup[o] is None: continue
        for f in a: ach.setdefault(f,[]).append(o)
    gz={GF}; st=[GF]
    while st:
        f=st.pop()
        for o in ach.get(f,[]):
            if cost[o]==0 and sup[o] not in gz: gz.add(sup[o]); st.append(sup[o])
    bz=set(); st=list(initids)
    for f in st: bz.add(f)
    outs={}
    for o,(p,a,c) in enumerate(OPS):
        if sup[o] is not None: outs.setdefault(sup[o],[]).append(o)
    while st:
        f=st.pop()
        for o in outs.get(f,[]):
            for a in OPS[o][1]:
                if a not in gz and a not in bz: bz.add(a); st.append(a)
    cut=[o for o in range(len(OPS)) if sup[o] is not None and sup[o] in bz and any(a in gz for a in OPS[o][1])]
    m=min(cost[o] for o in cut); total+=m
    for o in cut: cost[o]-=m
print('LM-cut', total, 'ops', len(OPS), 'facts', len(facts))
