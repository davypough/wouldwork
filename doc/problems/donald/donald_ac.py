# Donald: spec's forward checking (FC) search vs the same search with arc consistency (MAC)
import itertools, time
L = "DONALGERBT"
cols = [  # (carry_in, x, y, out, carry_out) : cin + x + y = out + 10*cout, in spec action order
 ("c6","D","D","T","c5"),("c5","L","L","R","c4"),("c4","A","A","E","c3"),
 ("c3","N","R","B","c2"),("c2","O","E","O","c1"),("c1","D","G","R","c0")]
def init():
    d = {l:set(range(10)) for l in L}
    d.update({"c0":{0},"c6":{0}}); d.update({c:{0,1} for c in ["c1","c2","c3","c4","c5"]})
    return d
checks = 0
def ac(d):
    """GAC on column sums + pairwise all-different on letters. Returns reduced copy or None."""
    global checks
    d = {k:set(v) for k,v in d.items()}
    changed = True
    while changed:
        changed = False
        for (ci,x,y,o,co) in cols:
            vs = [ci,x,y,o,co]; uniq = list(dict.fromkeys(vs))
            sup = {v:set() for v in uniq}
            for combo in itertools.product(*[d[v] for v in uniq]):
                checks += 1
                a = dict(zip(uniq,combo))
                letters = [v for v in uniq if v in L]
                if len({a[v] for v in letters}) != len(letters): continue
                if a[ci]+a[x]+a[y] == a[o]+10*a[co]:
                    for v in uniq: sup[v].add(a[v])
            for v in uniq:
                if sup[v] != d[v]:
                    d[v] = sup[v]; changed = True
                    if not d[v]: return None
        for l in L:
            if len(d[l]) == 1:
                (x,) = d[l]
                for m in L:
                    if m != l and x in d[m]:
                        checks += 1
                        d[m].discard(x); changed = True
                        if not d[m]: return None
    return d
def children(d, k):
    """The spec's action k: product over the column's variables, column test, then FC."""
    global checks
    ci,x,y,o,co = cols[k]
    uniq = list(dict.fromkeys([x,y,o,ci,co]))
    for combo in itertools.product(*[sorted(d[v]) for v in uniq]):
        checks += 1
        a = dict(zip(uniq,combo))
        letters = [v for v in uniq if v in L]
        if len({a[v] for v in letters}) != len(letters): continue
        if a[ci]+a[x]+a[y] != a[o]+10*a[co]: continue
        n = {kk:set(vv) for kk,vv in d.items()}
        for v in uniq:
            if v in L:
                for m in L:
                    if m != v: n[m].discard(a[v])
            n[v] = {a[v]}
        if any(not n[m] for m in L): continue
        yield n
def goal(d):
    if any(len(d[l])!=1 for l in L): return False
    w = lambda s: int("".join(str(next(iter(d[c]))) for c in s))
    return w("DONALD")+w("GERALD")==w("ROBERT")
def search(use_ac):
    global checks; checks = 0
    stats = dict(nodes=0, sols=0, removed=0, dead_early=0)
    root = init()
    if use_ac: root = ac(root)
    stack = [(root,0)]
    while stack:
        d,k = stack.pop(); stats["nodes"] += 1
        if k == 6:
            stats["sols"] += goal(d); continue
        for n in children(d,k):
            if use_ac:
                before = sum(len(v) for v in n.values())
                m = ac(n)
                if m is None: stats["dead_early"] += 1; continue
                stats["removed"] += before - sum(len(v) for v in m.values()); n = m
            stack.append((n,k+1))
    stats["checks"] = checks
    return stats
for use in (False, True):
    t=time.time(); s=search(use); print("AC" if use else "FC", s, "%.2fs"%(time.time()-t))
