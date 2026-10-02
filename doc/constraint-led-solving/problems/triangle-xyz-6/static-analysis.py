# triangle-xyz-6 pilot: hand static analysis (A, 2026-10-01). Run: python3 static-analysis.py (needs numpy, scipy).
import itertools, numpy as np
N=6
pos=[(x,y,N+2-x-y) for x in range(1,N+1) for y in range(1,N+2-x)]
P={p:i for i,p in enumerate(pos)}
# jumps from the six actions (from, over, to)
dirs={'LD':(0,1,-1),'RU':(0,-1,1),'RD':(1,0,-1),'LU':(-1,0,1),'RH':(1,-1,0),'LH':(-1,1,0)}
jumps=[]
for (x,y,z) in pos:
  for n,(dx,dy,dz) in dirs.items():
    o=(x+dx,y+dy,z+dz); t=(x+2*dx,y+2*dy,z+2*dz)
    if o in P and t in P: jumps.append((n,(x,y,z),o,t))
print("positions",len(pos),"jumps",len(jumps),"lines",len(jumps)//2)
# spec guard check: compare engine guards
def guard(n,x,y,z):
  return {'LD':y<=N-2 and z>=3,'RU':z<=N-2 and y>=3,'RD':x<=N-2 and z>=3,'LU':z<=N-2 and x>=3,'RH':x<=N-2 and y>=3,'LH':y<=N-2 and x>=3}[n]
cnt=0
for (x,y,z) in pos:
  for n in dirs:
    g=guard(n,x,y,z); dx,dy,dz=dirs[n]; t=(x+2*dx,y+2*dy,z+2*dz)
    if g!=(t in P): cnt+=1; print("guard mismatch",n,(x,y,z))
print("guard mismatches",cnt)
# GF(2) invariants: vectors v s.t. v.(delta)=0 mod 2 for all jumps; delta = from+over+to mod2
M=np.zeros((len(jumps),len(pos)),dtype=int)
for k,(n,f,o,t) in enumerate(jumps):
  for p in (f,o,t): M[k,P[p]]=1
# nullspace mod 2
def nullspace_mod2(A):
  A=A.copy()%2; r,c=A.shape; piv=[];row=0
  for col in range(c):
    pr=next((i for i in range(row,r) if A[i,col]),None)
    if pr is None: continue
    A[[row,pr]]=A[[pr,row]]
    for i in range(r):
      if i!=row and A[i,col]: A[i]^=A[row]
    piv.append(col); row+=1
  free=[j for j in range(c) if j not in piv]; basis=[]
  for f in free:
    v=np.zeros(c,dtype=int); v[f]=1
    for i,pc in enumerate(piv): v[pc]=A[i,f]
    basis.append(v)
  return basis
B=nullspace_mod2(M)
print("GF2 invariant dimension",len(B))
col=lambda p:(p[1]-p[2])%3
classes={c:[p for p in pos if col(p)==c] for c in range(3)}
for c in classes: print("class",c,len(classes[c]),[f"{p[0]}{p[1]}" for p in classes[c]])
# check colour-pair indicators are in span: indicator of class a + class b
def inspan(v):
  return all(((M@v)%2)==0)
for a,b in [(0,1),(0,2),(1,2)]:
  v=np.array([1 if col(p) in (a,b) else 0 for p in pos]); print("class",a,"+",b,"invariant:",inspan(v))
start=[0 if p==(1,1,N) else 1 for p in pos]
start=np.array(start)
# final single-peg positions consistent with all GF2 invariants
ok=[]
for p in pos:
  f=np.zeros(len(pos),dtype=int); f[P[p]]=1
  if all((v@start)%2==(v@f)%2 for v in B): ok.append(p)
print("final positions allowed by GF2 invariants:",[f"{p[0]}{p[1]}" for p in ok])
print("hole class",col((1,1,N)))
mids={p:0 for p in pos}; frm={p:0 for p in pos}
for n,f,o,t in jumps: mids[o]+=1; frm[f]+=1
print("never jumped over:",[f"{p[0]}{p[1]}" for p in pos if mids[p]==0])
print("first moves:",[(n,f"{f[0]}{f[1]}",f"{t[0]}{t[1]}") for n,f,o,t in jumps if t==(1,1,N)])
from scipy.optimize import linprog
print("--- pagoda LP: maximize w(final)-w(start), s.t. w(t)<=w(f)+w(o) for all jumps, -1<=w<=1")
for p in ok:
  c=np.array(start,dtype=float); c[P[p]]-=1   # minimize w(start)-w(final)
  A=[];b=[]
  for n,f,o,t in jumps:
    r=np.zeros(len(pos)); r[P[t]]+=1; r[P[f]]-=1; r[P[o]]-=1; A.append(r); b.append(0)
  res=linprog(c,A_ub=A,b_ub=b,bounds=[(-1,1)]*len(pos),method='highs')
  print(f"{p[0]}{p[1]}", "EXCLUDED by pagoda" if res.fun< -1e-9 else "not excluded", round(res.fun,3))
nm=lambda p:f"{p[0]}{p[1]}"
for c in [(1,1,6),(1,6,1),(6,1,1)]:
  print("corner",nm(c),"leaves by:",[(n,nm(o),nm(t)) for n,f,o,t in jumps if f==c],"filled by:",[(n,nm(f),nm(o)) for n,f,o,t in jumps if t==c])
refl=lambda p:(p[1],p[0],p[2])
S=set((f,o,t) for n,f,o,t in jumps)
print("x<->y reflection preserves jumps:",all((refl(f),refl(o),refl(t)) in S for f,o,t in S), "fixes hole:",refl((1,1,6))==(1,1,6))
