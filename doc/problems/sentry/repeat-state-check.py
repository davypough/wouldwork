# repeat-state-check.py -- independent model of probs/problem-sentry.lisp
# (spec's wait action, tree search), written 2026-10-15 to measure how often
# states repeat and what failed-state memory would save.  Not wouldwork code.
# Run from the wouldwork root:  python doc/problems/sentry/repeat-state-check.py
# Model: every action and each wait take 1 time unit; the sentry cycles
# area7, area6, area5, area6 at times 1-4 and freezes (index kept) while jammed;
# a move or happening that leaves me sharing an area with the active sentry, as
# seen after the happening with me either at my old or my new area, is refused;
# no two waits in a row.  It omits wouldwork's on-current-path cycle check.
import sys
sys.setrecursionlimit(10000)
ADJ = {1:[2],2:[1,3,4],3:[2],4:[2,5],5:[4,6],6:[5,7],7:[6,8],8:[7]}
VIS = {(5,6),(5,7),(5,8),(6,7),(6,8),(7,8)}
for a,bs in ADJ.items():
    for b in bs: VIS.add((a,b))
VIS |= {(b,a) for (a,b) in VIS}
EV = [7,6,5,6]          # sentry locs at event index 0..3 (times 1,2,3,4, repeat)
DIST = {8:0,7:1,6:2,5:3,4:4,2:5,1:6,3:6}
# state: (me, jammer loc|'H', box loc|'H', green, jammed None|'gun'|'sentry',
#         sentry loc, sentry event index, time, last action was wait)
START = (1,1,4,False,None,6,0,0,False)

def lb(s):
    me,j,b,g,jd,sl,si,t,w = s
    h = DIST[me]
    if me in (7,8) or jd=='sentry': return h
    return h + (1 if j=='H' else 2)

def gun_active(g,jd): return not (g or jd=='gun')

def succ(s):
    me,j,b,g,jd,sl,si,t,w = s
    out = []
    free = j!='H' and b!='H'
    if j=='H':                                                  # jam
        for tgt,tl in (('gun',2),('sentry',sl)):
            if (me,tl) in VIS: out.append((me,me,b,g,tgt))
    if free and me==3: out.append((me,j,b,not g,jd))            # throw
    if free:
        if j==me: out.append((me,'H',b,g,None))                  # pickup jammer (unjams)
        if b==me: out.append((me,j,'H',g,jd))                    # pickup box
    if j=='H': out.append((me,me,b,g,jd))                        # drop jammer
    if b=='H': out.append((me,j,me,g,jd))                        # drop box
    for a2 in ADJ[me]:                                           # move
        if a2==2 and gun_active(g,jd): continue
        out.append((a2,j,b,g,jd))
    res = [happen(s,a,False) for a in out]
    if not w: res.append(happen(s,(me,j,b,g,jd),True))           # wait (not twice)
    return [r for r in res if r]

def happen(s,a,iswait):
    me,j,b,g,jd,sl,si,t,w = s
    me2,j2,b2,g2,jd2 = a
    if jd2=='sentry': sl2,si2 = sl,si                            # interrupted
    else: sl2,si2 = EV[si],(si+1)%4
    if me==sl2 and jd!='sentry': return None                     # happening-state constraint
    if me2==sl2 and jd2!='sentry': return None                   # net-state constraint
    return (me2,j2,b2,g2,jd2,sl2,si2,t+1,iswait)

def key(s,mode):
    if mode=='time': return s
    return s[:7]+s[8:]   # drop absolute time: the phase is in the event index

class Run:
    def __init__(s,cut,use_lb,mode):
        s.cut,s.lb,s.mode=cut,use_lb,mode; s.exp=0; s.paths=0; s.memo={}
    def dfs(s,st,d):
        if st[0]==8: s.paths+=1; return True
        if d>=s.cut: return False
        if s.lb and d+lb(st)>s.cut: return False
        if s.mode:
            k=key(st,s.mode)
            if s.memo.get(k,-1)>=s.cut-d: return False
        s.exp+=1
        found=False
        for c in succ(st):
            if s.dfs(c,d+1): found=True
        if s.mode and not found:
            k=key(st,s.mode); s.memo[k]=max(s.memo.get(k,-1),s.cut-d)
        return found

def distinct(cut,use_lb):
    seen_t=set(); seen_p=set()
    def go(st,d):
        if st[0]==8 or d>=cut: return
        if use_lb and d+lb(st)>cut: return
        seen_t.add(st); seen_p.add(key(st,'phase'))
        for c in succ(st): go(c,d+1)
    go(START,0); return len(seen_t),len(seen_p)

for cut in (15,16,18):
    for use_lb in (False,True):
        base=Run(cut,use_lb,None); base.dfs(START,0)
        dt,dp=distinct(cut,use_lb)
        line=(f"cut {cut} lb {use_lb!s:5}: expanded {base.exp:>9} paths {base.paths:>5} "
              f"distinct(state,time) {dt:>7} distinct(state,phase) {dp:>6}")
        for mode in ('time','phase'):
            r=Run(cut,use_lb,mode); r.dfs(START,0)
            line+=f" | memo-{mode}: exp {r.exp:>7} memo {len(r.memo):>6}"
        print(line, flush=True)
