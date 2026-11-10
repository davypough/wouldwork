"""intersect beam-dynamics sketch (solving advisor, 2026-11-09).  NOT the engine.

A synchronous-tick approximation of Talos beam behaviour on the intersect geometry
(spec revision 3 coordinates): each tick, a beam stops at its first crossing with a beam
that reached that point on the previous tick; relays light from incoming beams (two hues ->
unlit); bodies (connectors, agent) block a beam passing within 1/2; walls block; windows
pass; gate1 open, gates 2-4 closed.  It reproduces D's in-game rotating triangle
(connector1 L1: T1 R1 R3; connector2 L2: R2 T2; connector3 L3: T2 R4).

Usage:
  python3 beam-sketch.py triangle        # reproduce the triangle (receivers lit per tick)
  python3 beam-sketch.py search [M]      # oscillation search, beams clear wall corners by M
See doc/problems/intersect/Briefing.md (SG3 dialogue) for findings and the in-game test.
"""
import math, itertools, time, sys
WALLS=[(5,0,5,6),(8,3,8,6),(8,6,14,6),(17,0,17,6),(23,4,23,6),(23,10,23,15),(17,15,23,15),(5,15,13,15),(5,10,5,15),(5,10,8,10),
       (0,0,29,0),(29,0,29,20),(29,20,0,20),(0,20,0,0),
       (23,0,23,4),(18,15,18,20),(0,6,5,6)]          # gates 2,3,4 treated as closed; gate1 open
FIX={'T1':((15,0.1),'blue','N'),'T2':((7,14.9),'red','S'),
     'R1':((13,5.9),'blue','S'),'R2':((22.9,5),'red','W'),'R3':((15,19.9),'blue','S'),'R4':((0.1,8),'red','E')}
TOL=0.5
def orient(p,q,r): return (q[0]-p[0])*(r[1]-p[1])-(q[1]-p[1])*(r[0]-p[0])
def seg_touch(p,q,a,b):
    d1,d2,d3,d4=orient(a,b,p),orient(a,b,q),orient(p,q,a),orient(p,q,b)
    return d1*d2<=0 and d3*d4<=0
def clear(p,q):
    return not any(seg_touch(p,q,(w[0],w[1]),(w[2],w[3])) for w in WALLS)
def facing_ok(name,other):
    if name not in FIX: return True
    (x,y),_,f=FIX[name]
    return {'N':other[1]>y,'S':other[1]<y,'E':other[0]>x,'W':other[0]<x}[f]
def inter(p,q,a,b):
    # proper intersection params (t on pq, u on ab), excluding endpoints
    d=(q[0]-p[0])*(b[1]-a[1])-(q[1]-p[1])*(b[0]-a[0])
    if abs(d)<1e-12: return None
    t=((a[0]-p[0])*(b[1]-a[1])-(a[1]-p[1])*(b[0]-a[0]))/d
    u=((a[0]-p[0])*(q[1]-p[1])-(a[1]-p[1])*(q[0]-p[0]))/d
    if 1e-9<t<1-1e-9 and 1e-9<u<1-1e-9: return t,u
    return None
def body_param(p,q,c):
    dx,dy=q[0]-p[0],q[1]-p[1]; L2=dx*dx+dy*dy
    t=((c[0]-p[0])*dx+(c[1]-p[1])*dy)/L2
    if t<=1e-9 or t>=1-1e-9: return None
    if math.hypot(p[0]+t*dx-c[0],p[1]+t*dy-c[1])<=TOL: return t
    return None
def simulate(conn, pairs, agent, ticks=60):
    """conn: name->pos; pairs: list of (a,b) names; agent pos or None.
       returns list per tick of set of lit receivers."""
    pos={k:v[0] for k,v in FIX.items()}; pos.update(conn)
    # BFS distances (ignoring cuts) to orient relay-relay beams
    dist={k:0 for k in FIX if k[0]=='T'}
    changed=True
    while changed:
        changed=False
        for a,b in pairs:
            for x,y in ((a,b),(b,a)):
                if x in dist and y in conn and dist.get(y,99)>dist[x]+1:
                    dist[y]=dist[x]+1; changed=True
    beams=[]
    for a,b in pairs:
        if a in dist and (b not in dist or dist[a]<=dist[b]): s,e=a,b
        elif b in dist: s,e=b,a
        else: continue
        if e in FIX and e[0]=='T': continue
        p,q=pos[s],pos[e]
        if not clear(p,q) or not facing_ok(s,q) or not facing_ok(e,p): continue
        bodies=[body_param(p,q,c) for k,c in list(conn.items())+([('agent',agent)] if agent else []) if k not in (s,e)]
        bodies=[t for t in bodies if t is not None]
        beams.append(dict(s=s,e=e,p=p,q=q,stop=min(bodies) if bodies else 1.0))
    n=len(beams)
    X=[[None]*n for _ in range(n)]
    for i in range(n):
        for j in range(n):
            if i!=j:
                r=inter(beams[i]['p'],beams[i]['q'],beams[j]['p'],beams[j]['q'])
                if r: X[i][j]=r   # t on i, u on j
    reach=[0.0]*n; lit={}
    hist=[]
    for tick in range(ticks):
        # relay hues from previous reach
        hues={}
        for i,b in enumerate(beams):
            if reach[i]>=1.0 and b['e'] in conn:
                src=b['s']; h=FIX[src][1] if src in FIX else lit.get(src)
                if h: hues.setdefault(b['e'],set()).add(h)
        newlit={k:(next(iter(v)) if len(v)==1 else None) for k,v in hues.items()}
        newreach=[]
        for i,b in enumerate(beams):
            src=b['s']
            if src in conn and not lit.get(src): newreach.append(0.0); continue
            r=b['stop']
            for j in range(n):
                if X[i][j] and reach[j]>X[i][j][1]-1e-12: r=min(r,X[i][j][0])
            newreach.append(r)
        reach=newreach; lit=newlit
        rec=set()
        for i,b in enumerate(beams):
            if reach[i]>=1.0 and b['e'] in FIX and b['e'][0]=='R':
                src=b['s']; h=FIX[src][1] if src in FIX else lit.get(src)
                if h==FIX[b['e']][1]: rec.add(b['e'])
        hist.append(rec)
    return hist


def triangle():
    h=simulate({'C1':(15,5),'C2':(21,6),'C3':(24,8)},
               [('C1','T1'),('C1','R1'),('C1','R3'),('C2','R2'),('C2','T2'),('C3','T2'),('C3','R4')],(24,8))
    print("triangle:",[sorted(x) for x in h[20:32]])


def search(argv):
    global clear
    M=float(argv[0]) if argv else 0.5
    CORNERS=set()
    for w in WALLS[:10]: CORNERS.add((w[0],w[1])); CORNERS.add((w[2],w[3]))
    def pt_seg(c,p,q):
        dx,dy=q[0]-p[0],q[1]-p[1]; L=dx*dx+dy*dy
        t=max(0,min(1,((c[0]-p[0])*dx+(c[1]-p[1])*dy)/L))
        return math.hypot(p[0]+t*dx-c[0],p[1]+t*dy-c[1])
    _clear=clear
    def clear_m(p,q):
        if not _clear(p,q): return False
        return all(pt_seg(c,p,q)>=M for c in CORNERS if math.hypot(c[0]-p[0],c[1]-p[1])>M and math.hypot(c[0]-q[0],c[1]-q[1])>M)
    clear=clear_m
    def frange(a,b,s):
        x=a
        while x<=b+1e-9: yield round(x,3); x+=s
    def central(p):
        x,y=p
        return (5<x<23 and 6<y<15 and not (5<=x<=8 and y<10)) or (17<x<23 and 0<y<6)
    def west(p):
        x,y=p
        return (0<x<5 and 6<y<20) or (5<=x<18 and 15<y<20)
    T2=FIX['T2'][0]; R2=FIX['R2'][0]
    Xs=[(x,y) for x in frange(5.5,22.5,0.5) for y in frange(0.5,14.5,0.5) if central((x,y))
        and clear_m((x,y),T2) and facing_ok('T2',(x,y)) and clear_m((x,y),R2) and facing_ok('R2',(x,y))]
    Ws=[(x,y) for x in frange(0.5,17.5,0.5) for y in frange(6.5,19.5,0.5) if west((x,y))]
    print(len(Xs),len(Ws),flush=True)
    def frac(h,r): return sum(r in x for x in h)/len(h)
    L1=(15,5); hits=[]
    t0=time.time()
    for X in Xs:
        for W in Ws:
            for extra in ([],[('C3','R2')],[('C3','R4')],[('C3','R2'),('C3','R4')],[('C3','T2')],[('C3','T2'),('C3','R2')]):
                pairs=[('C1','T1'),('C1','R1'),('C1','R3'),('C2','T2'),('C2','R2'),('C2','C3')]+extra
                h=simulate({'C1':L1,'C2':X,'C3':W},pairs,(24,8),60)[30:]
                f3=frac(h,'R3')
                if f3<=0: continue
                best=0;bs=None
                for sy in (6.1,6.25,6.5,6.75,7,7.5,8,8.5,9,10,11):
                    he=simulate({'C1':L1,'C2':X,'C3':W},pairs,(15,sy),60)[30:]
                    f2=frac(he,'R2')
                    if f2>best: best,bs=f2,sy
                if best>0: hits.append((X,W,[e[1] for e in extra],round(f3,2),bs,round(best,2)))
    print("hits",len(hits),"time",round(time.time()-t0))
    for hh in hits[:40]: print(hh)


if __name__=='__main__':
    import sys
    if sys.argv[1:2]==['triangle']: triangle()
    else: search(sys.argv[2:])
