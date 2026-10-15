# Crossword family with dictionary fill: along random paths of the spec's search (placements the
# spec's crossing check accepts), how many does arc consistency reject, and how many are truly
# uncompletable (exact completion search)?  Domains: dictionary words + unused listed words.
import re, sys, random, time
spec, dictfile, seed, paths, per_step = sys.argv[1], sys.argv[2], int(sys.argv[3]), int(sys.argv[4]), int(sys.argv[5])
random.seed(seed)
src = open(spec).read()
fields = [(f, int(n)) for f, n in re.findall(r"\((\w+) (\d+)\)", re.search(r"defparameter \*fields\*.*?'\((.*?)\)\)\s", src, re.S).group(1) + ")")]
FL = dict(fields)
listed = [w.upper() for w in re.search(r"defparameter \*words\*[^']*'\(([^)]*)\)", src, re.S).group(1).split()]
cuts = {}
cbody = src.split("(defparameter *crosscuts*")[1].split("(def")[0]
for f, body in re.findall(r"\((\w+)\s+\(([^()]*)\)\)", cbody):
    t = body.split(); cuts[f] = [(t[i], int(t[i+1]), int(t[i+2])) for i in range(0, len(t), 3)]
def crossing(f): return [g for g, _, _ in cuts[f]]
def next_field(ordered, remaining):
    best, bk = None, None
    for f in remaining:
        k = (sum(1 for g in crossing(f) if g in ordered), FL[f])
        if bk is None or k > bk: best, bk = f, k
    return best
names = [f for f, _ in fields]
order = [next_field(names, names)]
while len(order) < len(names): order.append(next_field(order, [f for f in names if f not in order]))
lengths = set(FL.values())
# word lists per length: dictionary forwards and reversed, then listed words
words = {L: [] for L in lengths}
dset = set()
for w in open(dictfile).read().split("\n"):
    if len(w) in lengths: words[len(w)] += [w, w[::-1]]; dset.add(w); dset.add(w[::-1])
nd = {L: len(words[L]) for L in lengths}
for w in listed:
    if len(w) in lengths: words[len(w)].append(w)
listed_idx = {w: len(words[len(w)]) - 1 - words[len(w)][::-1].index(w) for w in listed if len(w) in lengths}
letset = {}
for L in lengths:
    for p in range(L):
        acc = {}
        for i, w in enumerate(words[L]):
            acc[w[p]] = acc.get(w[p], 0) | (1 << i)
        letset[(L, p)] = acc
full = {L: (1 << len(words[L])) - 1 for L in lengths}
dictmask = {L: (1 << nd[L]) - 1 for L in lengths}
def match(pat, used):   # pat: string with '?'
    L = len(pat); m = full[L]
    for p, c in enumerate(pat):
        if c != '?': m &= letset[(L, p)].get(c, 0)
    for w in used:          # used listed words are no longer available as listed words
        if len(w) == L and w not in dset: m &= ~(1 << listed_idx[w])
    return m
def spec_check(texts, f, w):   # the spec's crossing check: each changed crossing fits a dictionary word
    for g, gi, fi in cuts[f]:
        t = texts[g]
        if t[gi] != w[fi]:
            nt = t[:gi] + w[fi] + t[gi+1:]
            if '?' in nt:
                if not (match(nt, []) & dictmask[len(nt)]): return False
            elif nt not in dset: return False
    return True
def place(texts, f, w):
    t = dict(texts); t[f] = w
    for g, gi, fi in cuts[f]: t[g] = t[g][:gi] + w[fi] + t[g][gi+1:]
    return t
def full_ok(t, used):
    return all('?' in s or s in dset or s in used for s in t.values())
def letters(dom, L, p):
    return {c for c, m in letset[(L, p)].items() if dom & m}
def ac(dom, queue):
    dom = dict(dom); queue = list(queue)
    while queue:
        f = queue.pop()
        for g, gi, fi in cuts[f]:
            if g not in dom: continue
            ok = 0
            for c in letters(dom[f], FL[f], fi): ok |= letset[(FL[g], gi)].get(c, 0)
            nd_ = dom[g] & ok
            if nd_ != dom[g]:
                if not nd_: return None
                dom[g] = nd_
                if g not in queue: queue.append(g)
    return dom
def domains(t, used):
    return {f: match(s, used) for f, s in t.items() if '?' in s}
def popcount(x): return bin(x).count("1")
class Budget(Exception): pass
def complete(dom, budget):
    tries = [0]
    def rec(dom):
        open_ = [f for f in dom if popcount(dom[f]) > 1]
        if not open_: return True
        f = min(open_, key=lambda f: popcount(dom[f]))
        d = dom[f]
        while d:
            b = d & -d; d ^= b
            tries[0] += 1
            if tries[0] > budget: raise Budget
            nd2 = dict(dom); nd2[f] = b
            r = ac(nd2, [f])
            if r is not None and rec(r): return True
        return False
    try: return "complete" if rec(dom) else "impossible"
    except Budget: return "unknown"
import os
dump = open(os.environ["DUMP"], "w") if os.environ.get("DUMP") else None
stats = dict(accepted=0, ac_reject=0, ac_pass_impossible=0, ac_pass_complete=0, ac_pass_unknown=0,
             fc_values=0, ac_values=0, open_fields=0, ac_time=0.0, n_ac=0)
by_depth = {}
t0 = time.time()
for path in range(paths):
    texts = {f: '?' * FL[f] for f in names}; used = []
    for k, f in enumerate(order):
        cands = [w for w in listed if len(w) == FL[f] and w not in used
                 and all(c in ('?', x) for c, x in zip(texts[f], w)) and spec_check(texts, f, w)]
        random.shuffle(cands); good = []
        for w in cands[:per_step]:
            t = place(texts, f, w); u = used + [w]
            if not full_ok(t, u):
                continue
            stats["accepted"] += 1
            d = domains(t, u)
            stats["fc_values"] += sum(popcount(x) for x in d.values()); stats["open_fields"] += len(d)
            ta = time.time(); r = ac(d, list(d)); stats["ac_time"] += time.time() - ta; stats["n_ac"] += 1
            bd = by_depth.setdefault(k, [0, 0, 0])
            bd[0] += 1
            if r is None:
                stats["ac_reject"] += 1; bd[1] += 1
                if dump: dump.write("(:ac-reject " + " ".join('(%s "%s")' % (g, s) for g, s in t.items()) + ")\n")
                continue
            stats["ac_values"] += sum(popcount(x) for x in r.values())
            res = complete(r, 2000)
            stats["ac_pass_" + res] += 1
            if dump: dump.write("(:" + res + " " + " ".join('(%s "%s")' % (g, s) for g, s in t.items()) + ")\n")
            if res == "impossible": bd[2] += 1
            if res == "complete": good.append(w)
        if good and random.random() < 0.7:
            w = good[0]; texts = place(texts, f, w); used.append(w)
    print("path", path, "placed", len(used), "%.0fs" % (time.time() - t0), {k: stats[k] for k in ("accepted","ac_reject","ac_pass_impossible","ac_pass_complete","ac_pass_unknown")}, "ac_ms %.1f" % (1000*stats["ac_time"]/stats["n_ac"]), flush=True)
stats["ac_ms"] = round(1000 * stats["ac_time"] / max(1, stats["n_ac"]), 1)
print(stats)
print("by step: [accepted, AC rejects, AC passes but impossible]")
print({k: v for k, v in sorted(by_depth.items())})
