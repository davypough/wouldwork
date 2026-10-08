# triangle-xyz-6 pilot: S8 liveness check (A, 2026-10-01). Needs static-analysis.py's definitions; run from this folder.
exec(open('static-analysis.py').read().split('# GF(2)')[0])
from scipy.optimize import linprog
nm=lambda p:f"{p[0]}{p[1]}"
board=lambda empt:set(p for p in pos if nm(p) not in empt)
moves=lambda s:[(n,f,o,t) for n,f,o,t in jumps if f in s and o in s and t not in s]
def unmoves(s): return [(n,f,o,t) for n,f,o,t in jumps if t in s and f not in s and o not in s]
def fwd(s,k):
  L={frozenset(s)}
  for d in range(k): L={frozenset((x-{f,o})|{t}) for x in L for n,f,o,t in moves(x)}
  return L
def bwd(final,k):
  L={frozenset({final})}
  for d in range(k): L={frozenset((x-{t})|{f,o}) for x in L for n,f,o,t in unmoves(x)}
  return L
def live(s,final):
  r=len(s)-1; a=r//2; return bool(fwd(s,a)&bwd(final,r-a))
cls1=[p for p in pos if (p[1]-p[2])%3==1]
boards={'SG1':{'13','15','16','22','31'},'SG2':{'13','15','16','22','33','42','51','61'},
        'SG3':set(nm(p) for p in pos)-{'11','12','14','21','23','24','32','41','43'}}
for k,e in boards.items():
  s=board(e); print(k,len(s),"pegs; can finish at:",[nm(f) for f in cls1 if live(s,f)])
