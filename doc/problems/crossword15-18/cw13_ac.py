# crossword13: spec search (no look-ahead) vs forward checking vs arc consistency
import re, time, sys
src = open(sys.argv[1]).read()
words = re.search(r"word \(([^)]*)\)", src).group(1).split()
contents = dict((f, len(s)) for f,s in re.findall(r'\(contents (\w+)\s+"([^"]*)"\)', src))
cuts = {}
for f, body in re.findall(r"\(crosscuts (\w+)\s+\(([^)]*)\)\)", src):
    t = body.split(); cuts[f] = [(t[i], int(t[i+1]), int(t[i+2])) for i in range(0,len(t),3)]
order = re.search(r"\(open-fields \(([^)]*)\)\)", src.split("define-init")[1]).group(1).split()
W = [w.upper() for w in words]
def fits(w, pat): return len(w)==len(pat) and all(p in (None,c) for p,c in zip(pat,w))
def pattern(f, a):  # letters fixed in f by assigned crossings
    p = [None]*contents[f]
    for (g, gi, fi) in cuts[f]:
        if g in a: p[fi] = a[g][gi]
    if f in a: return list(a[f])
    return p
def domains(a):
    used = set(a.values())
    return {f: {w for w in W if w not in used and fits(w, pattern(f,a))} for f in contents if f not in a}
checks = 0
def ac(dom, a):
    global checks
    dom = {f:set(s) for f,s in dom.items()}
    queue = list(dom)
    while queue:
        f = queue.pop()
        for (g, gi, fi) in cuts[f]:   # g's letter gi meets f's letter fi
            if g not in dom: continue
            letters = {w[fi] for w in dom[f]}
            new = {w for w in dom[g] if w[gi] in letters}; checks += len(dom[g])
            if new != dom[g]:
                dom[g] = new
                if not new: return None
                if g not in queue: queue.append(g)
        # all-different: a field with one word left takes it from the others
        if len(dom[f]) == 1:
            (w,) = dom[f]
            for g in dom:
                if g != f and w in dom[g]:
                    dom[g].discard(w)
                    if not dom[g]: return None
                    if g not in queue: queue.append(g)
    return dom
def search(mode):
    global checks; checks = 0
    st = dict(nodes=0, removed=0, dead_early=0)
    def rec(a, k):
        global checks
        st["nodes"] += 1
        if k == len(order): return True
        f = order[k]; used = set(a.values())
        for w in W:
            if w in used or not fits(w, pattern(f, a)): continue
            checks += 1
            b = dict(a); b[f] = w
            if mode != "plain":
                dom = domains(b)
                if any(not s for s in dom.values()): st["dead_early"] += 1; continue
                if mode == "ac":
                    before = sum(len(s) for s in dom.values())
                    r = ac(dom, b)
                    if r is None: st["dead_early"] += 1; continue
                    st["removed"] += before - sum(len(s) for s in r.values())
            if rec(b, k+1): return True
        return False
    ok = rec({}, 0); st["solved"] = ok; st["checks"] = checks
    return st
d0 = domains({}); r0 = ac(d0, {})
print("start: FC values", sum(map(len,d0.values())), " AC values", sum(map(len,r0.values())))
for m in ("plain","fc","ac"):
    t = time.time(); print(m, search(m), "%.2fs" % (time.time()-t))
