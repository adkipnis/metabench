#!/usr/bin/env python3
"""
Paired rank resolution: how many adjacent leaderboard ranks can the FULL item set
distinguish, and how many can the metabench (reduced) item set distinguish?

Complements Table 1 of the paper, which reports Spearman correlation between the
full and reconstructed rankings. Rank correlation measures whether two
measurements agree; it does not say whether the gap between two neighbouring
models exceeds the sampling noise of the items. That second question is what a
paired test answers, and it can be asked of the full set and the reduced set
alike.

Method
------
Every subject (LLM) is scored on the same items, so the comparison of two
subjects is a paired binary comparison. Items both got right or both got wrong
carry no information about which is better; only DISCORDANT items do. The exact
McNemar test is the sign test on the discordant items:

    b = # items only A solved,  c = # items only B solved
    p = two-sided exact binomial P under H0: b ~ Bin(b + c, 1/2)

Two subjects are "separable" at level alpha if p < alpha. This is a property of
the item set AND of the pair: with d discordant items the smallest resolvable
gap is roughly 2.8 * sqrt(d) / n_items at alpha = 0.05.

What is reported per benchmark
------------------------------
  n_subjects, n_items_full, n_items_reduced
  spearman(full score, reduced score)             -- the paper's quantity
  adjacent pairs separable, full items             -- rank i vs i+1 under full ranking
  adjacent pairs separable, reduced items          -- rank i vs i+1 under reduced ranking
  adjacent pairs separable, reduced items, full ranking order
                                                    -- same neighbours as full, scored on reduced
  top tie group (ranks not separable from #1) under both
  all-pairs separable fraction within the top K   -- default K = 100

Score = mean accuracy over the item set (raw, no IRT reconstruction). This is
deliberate: the question is what the ITEMS can resolve, before any model.

Data layout (as in the rest of this repo)
-----------------------------------------
  data/{bm}.csv                 long format: source,item,correct   (Zenodo data.tgz)
  data/mmlu_{scenario}.csv      one file per MMLU scenario, merged like preprocess.R
  items/items-{bm}-A.csv        the metabench item list, column `item`
                                (ids like "arc.866" / "mmlu.college_biology.3")

Usage
-----
  python analysis/paired_resolution.py                    # all six benchmarks
  python analysis/paired_resolution.py --bm arc gsm8k
  python analysis/paired_resolution.py --selftest         # no data needed
  python analysis/paired_resolution.py --out results.json

Only numpy + pandas are required.
"""
from __future__ import annotations

import argparse
import glob
import json
import math
import os
import sys
from functools import lru_cache

import numpy as np
import pandas as pd

BENCHMARKS = ["arc", "gsm8k", "hellaswag", "mmlu", "truthfulqa", "winogrande"]
HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.abspath(os.path.join(HERE, os.pardir))


# ----------------------------------------------------------------------------
# exact McNemar (sign test on discordant items)
# ----------------------------------------------------------------------------
@lru_cache(maxsize=None)
def _log_binom_cdf(k: int, n: int) -> float:
    """log P(X <= k) for X ~ Bin(n, 1/2), stable for n in the thousands."""
    if k < 0:
        return -math.inf
    if k >= n:
        return 0.0
    logs = [math.lgamma(n + 1) - math.lgamma(i + 1) - math.lgamma(n - i + 1) - n * math.log(2.0)
            for i in range(k + 1)]
    m = max(logs)
    return m + math.log(sum(math.exp(x - m) for x in logs))


def mcnemar_exact(b: int, c: int) -> float:
    """Two-sided exact McNemar p-value from the discordant counts b, c."""
    n = b + c
    if n == 0:
        return 1.0
    k = min(b, c)
    p = 2.0 * math.exp(_log_binom_cdf(k, n))
    return min(1.0, p)


def pair_p(x: np.ndarray, y: np.ndarray) -> tuple[float, int, int]:
    """x, y: 0/1 vectors on the same items. Returns (p, b, c)."""
    b = int(np.count_nonzero(x & ~y))
    c = int(np.count_nonzero(~x & y))
    return mcnemar_exact(b, c), b, c


# ----------------------------------------------------------------------------
# data loading
# ----------------------------------------------------------------------------
def _pivot_csv(path: str) -> pd.DataFrame:
    """Long CSV (source,item,correct) -> subjects x items int8 frame (-1 = missing).

    Streams the file in batches, dictionary-encodes `source` per batch and reads
    `item` as an integer, so hellaswag (52M rows, 2.1 GB) fits in well under
    1 GB; a pandas pivot_table on the same file needs several GB and fails on a
    16 GB machine. Subjects are ordered alphabetically, items numerically, so
    tie-breaks in the ranking are deterministic."""
    src_index: dict = {}
    src_parts, itm_parts, cor_parts = [], [], []
    try:
        import pyarrow as pa
        import pyarrow.csv as pacsv
        reader = pacsv.open_csv(
            path,
            read_options=pacsv.ReadOptions(block_size=32 << 20),
            convert_options=pacsv.ConvertOptions(
                include_columns=["source", "item", "correct"],
                column_types={"source": pa.string(), "item": pa.int64(), "correct": pa.float32()},
            ),
        )
        for batch in reader:
            d = batch.column("source").dictionary_encode()
            names = d.dictionary.to_pylist()
            local = np.fromiter((src_index.setdefault(x, len(src_index)) for x in names),
                                dtype=np.int32, count=len(names))
            src_parts.append(local[d.indices.to_numpy(zero_copy_only=False)])
            itm_parts.append(batch.column("item").to_numpy(zero_copy_only=False).astype(np.int32))
            cor_parts.append(batch.column("correct").to_numpy(zero_copy_only=False).astype(np.float32))
    except ImportError:  # slower fallback
        for chunk in pd.read_csv(path, usecols=["source", "item", "correct"],
                                 dtype={"item": np.int64, "correct": np.float32}, chunksize=2_000_000):
            codes, names = pd.factorize(chunk["source"])
            local = np.fromiter((src_index.setdefault(x, len(src_index)) for x in names),
                                dtype=np.int32, count=len(names))
            src_parts.append(local[codes])
            itm_parts.append(chunk["item"].to_numpy())
            cor_parts.append(chunk["correct"].to_numpy())
    src_codes = np.concatenate(src_parts)
    itm_all = np.concatenate(itm_parts)
    cor_all = np.concatenate(cor_parts)
    del src_parts, itm_parts, cor_parts
    # item ids are small non-negative ints: build the code table without a 52M-element sort
    lo, hi = int(itm_all.min()), int(itm_all.max())
    assert lo >= 0 and hi < 10_000_000, (lo, hi)
    present = np.zeros(hi + 1, dtype=bool)
    present[itm_all] = True
    itm_uni = np.flatnonzero(present)
    lut = np.full(hi + 1, -1, dtype=np.int32)
    lut[itm_uni] = np.arange(len(itm_uni), dtype=np.int32)
    itm_codes = lut[itm_all]
    del itm_all
    names_in = np.array(list(src_index.keys()), dtype=object)
    order = np.argsort(names_in, kind="stable")           # alphabetical subjects
    remap = np.empty(len(order), dtype=np.int32)
    remap[order] = np.arange(len(order), dtype=np.int32)
    M = np.full((len(names_in), len(itm_uni)), -1, dtype=np.int8)
    M[remap[src_codes], itm_codes] = (cor_all > 0.5).astype(np.int8)
    return pd.DataFrame(M, index=names_in[order], columns=itm_uni)  # -1 marks missing


def load_full_matrix(bm: str, data_dir: str) -> pd.DataFrame:
    """subjects x items, values 0/1 (subjects with any missing item are dropped)."""
    if bm == "mmlu":
        files = sorted(f for f in glob.glob(os.path.join(data_dir, "mmlu_*.csv")) if "prompts" not in f)
        if not files:
            raise FileNotFoundError(f"no mmlu_*.csv under {data_dir}")
        parts = []
        for f in files:
            scenario = os.path.basename(f)[len("mmlu_"):-len(".csv")]
            w = _pivot_csv(f)
            w.columns = [f"{scenario}.{c}" for c in w.columns]
            parts.append(w)
        wide = pd.concat(parts, axis=1, join="inner")
    else:
        wide = _pivot_csv(os.path.join(data_dir, f"{bm}.csv"))
    n0 = len(wide)
    wide = wide[(wide.to_numpy() >= 0).all(axis=1)]
    if len(wide) < n0:
        print(f"  [{bm}] dropped {n0 - len(wide)} subjects with missing items", file=sys.stderr)
    return wide.astype(np.int8)


def _norm_item(bm: str, item: str) -> str:
    """Make ids from items-{bm}-A.csv comparable to data column names."""
    s = str(item)
    if s.startswith(bm + "."):
        s = s[len(bm) + 1:]
    return s


def load_reduced_items(bm: str, items_dir: str, version: str = "A") -> list[str]:
    f = os.path.join(items_dir, f"items-{bm}-{version}.csv")
    ids = pd.read_csv(f)["item"].astype(str).tolist()
    return [_norm_item(bm, i) for i in ids]


def align_reduced(bm: str, wide: pd.DataFrame, reduced_ids: list[str]) -> list:
    cols = {str(c): c for c in wide.columns}
    norm_cols = {_norm_item(bm, str(c)): c for c in wide.columns}
    keep = []
    for rid in reduced_ids:
        if rid in cols:
            keep.append(cols[rid])
        elif rid in norm_cols:
            keep.append(norm_cols[rid])
    return keep


# ----------------------------------------------------------------------------
# analysis
# ----------------------------------------------------------------------------
def adjacent_stats(M: np.ndarray, order: np.ndarray, alpha: float) -> dict:
    """M: subjects x items (bool). order: subject indices sorted best->worst.
    Tests each rank i vs i+1 on M's items."""
    ps, gaps, disc, reversed_sep = [], [], [], 0
    for i in range(len(order) - 1):
        a, b_ = order[i], order[i + 1]
        p, b, c = pair_p(M[a], M[b_])
        ps.append(p)
        gaps.append((M[a].mean() - M[b_].mean()) * 100.0)
        disc.append(b + c)
        if p < alpha and c > b:
            reversed_sep += 1  # separable, but these items favour the LOWER-ranked one
    ps = np.array(ps)
    sep = ps < alpha
    top = 1
    for j in range(1, len(order)):
        p, _, _ = pair_p(M[order[0]], M[order[j]])
        if p < alpha:
            break
        top += 1
    return {
        "n_pairs": int(len(ps)),
        "separable": int(sep.sum()),
        "separable_frac": float(sep.mean()) if len(ps) else float("nan"),
        "median_p": float(np.median(ps)) if len(ps) else float("nan"),
        "median_gap_pp": float(np.median(gaps)) if gaps else float("nan"),
        "median_discordant": float(np.median(disc)) if disc else float("nan"),
        "separable_reversed": int(reversed_sep),
        "top_tie_group": int(top),
        "n_items": int(M.shape[1]),
    }


def topk_allpairs(M: np.ndarray, order: np.ndarray, k: int, alpha: float) -> dict:
    idx = order[:k]
    n = len(idx)
    sep = tot = 0
    for i in range(n):
        for j in range(i + 1, n):
            p, _, _ = pair_p(M[idx[i]], M[idx[j]])
            tot += 1
            sep += p < alpha
    return {"k": int(n), "n_pairs": int(tot), "separable": int(sep),
            "separable_frac": float(sep / tot) if tot else float("nan")}


def spearman(a: np.ndarray, b: np.ndarray) -> float:
    ra = pd.Series(a).rank().to_numpy()
    rb = pd.Series(b).rank().to_numpy()
    return float(np.corrcoef(ra, rb)[0, 1])


def analyse_benchmark(bm: str, wide: pd.DataFrame, reduced_cols: list,
                      alpha: float, topk: int) -> dict:
    Mf = wide.to_numpy().astype(bool)
    Mr = wide[reduced_cols].to_numpy().astype(bool)
    sf = Mf.mean(axis=1)
    sr = Mr.mean(axis=1)
    order_f = np.argsort(-sf, kind="stable")
    order_r = np.argsort(-sr, kind="stable")
    return {
        "benchmark": bm,
        "n_subjects": int(Mf.shape[0]),
        "n_items_full": int(Mf.shape[1]),
        "n_items_reduced": int(Mr.shape[1]),
        "spearman_full_vs_reduced_score": spearman(sf, sr),
        "adjacent_full_items_full_order": adjacent_stats(Mf, order_f, alpha),
        "adjacent_reduced_items_reduced_order": adjacent_stats(Mr, order_r, alpha),
        "adjacent_reduced_items_full_order": adjacent_stats(Mr, order_f, alpha),
        f"top{topk}_allpairs_full_items": topk_allpairs(Mf, order_f, topk, alpha),
        f"top{topk}_allpairs_reduced_items": topk_allpairs(Mr, order_r, topk, alpha),
    }


def print_report(res: list, alpha: float, topk: int) -> None:
    print(f"\nPaired resolution at alpha={alpha}  (exact McNemar on discordant items)\n")
    hdr = (f"{'benchmark':<11}{'subj':>6}{'items':>7}{'meta':>6}{'rho':>7} | "
           f"{'adj.sep full':>13}{'adj.sep meta':>13}{'meta@full-ord (rev)':>20} | "
           f"{'tie#1 full':>10}{'tie#1 meta':>10} | top{topk} full  top{topk} meta")
    print(hdr)
    print("-" * len(hdr))
    for r in res:
        af = r["adjacent_full_items_full_order"]
        ar = r["adjacent_reduced_items_reduced_order"]
        arf = r["adjacent_reduced_items_full_order"]
        tf = r[f"top{topk}_allpairs_full_items"]
        tr = r[f"top{topk}_allpairs_reduced_items"]
        print(f"{r['benchmark']:<11}{r['n_subjects']:>6}{r['n_items_full']:>7}{r['n_items_reduced']:>6}"
              f"{r['spearman_full_vs_reduced_score']:>7.3f} | "
              f"{af['separable']:>5}/{af['n_pairs']:<7}"
              f"{ar['separable']:>5}/{ar['n_pairs']:<7}"
              f"{arf['separable']:>4}/{arf['n_pairs']:<5}(rev {arf['separable_reversed']:>3}) | "
              f"{af['top_tie_group']:>10}{ar['top_tie_group']:>10} | "
              f"{tf['separable_frac']:>10.3f}  {tr['separable_frac']:>10.3f}")
    print("\nadj.sep = adjacent ranks (i vs i+1) whose difference is separable; "
          "tie#1 = ranks not separable from #1;")
    print("meta@full-ord = full-set neighbours re-tested on the metabench items; (rev) = of those, separable "
          "but favouring the lower-ranked one, i.e. the reduced set confidently disagrees with the full order;")
    print(f"top{topk} = fraction of all pairs among the top {topk} subjects that are separable; "
          "rho = Spearman(full score, metabench score).")


# ----------------------------------------------------------------------------
# self-tests (run before any headline number; abort if they fail)
# ----------------------------------------------------------------------------
def selftest() -> None:
    # 1. known value: b=5, c=10 -> two-sided exact p = 2*P(X<=5 | n=15) = 0.30176
    p = mcnemar_exact(5, 10)
    assert abs(p - 0.30175781) < 1e-6, p
    # 2. no discordance -> p = 1
    assert mcnemar_exact(0, 0) == 1.0
    # 3. identity: a subject against itself
    rng = np.random.default_rng(0)
    x = rng.random(500) < 0.6
    assert pair_p(x, x.copy())[0] == 1.0
    # 4. planted signal: 60 one-directional flips -> p tiny (else test 3 would pass vacuously)
    y = x.copy()
    y[np.flatnonzero(~x)[:60]] = True
    p, b, c = pair_p(y, x)
    assert (b, c) == (60, 0) and p < 1e-6, (p, b, c)
    # 5. symmetric noise: same accuracy, 36 discordant split 18/18 -> p = 1
    z = x.copy()
    z[np.flatnonzero(x)[:18]] = False
    z[np.flatnonzero(~x)[:18]] = True
    p, b, c = pair_p(x, z)
    assert (b, c) == (18, 18) and p == 1.0, (p, b, c)
    # 6. large-n stability
    assert 0.0 < mcnemar_exact(2400, 2600) < 0.01
    # 7. synthetic board: 50 subjects, 1000 items, abilities 0.5 pp apart
    #    -> adjacent pairs mostly NOT separable; rank 1 vs rank 50 separable
    n, m = 50, 1000
    ab = 0.5 + 0.005 * np.arange(n)[::-1]
    M = rng.random((n, m)) < ab[:, None]
    order = np.argsort(-M.mean(axis=1), kind="stable")
    st = adjacent_stats(M, order, 0.05)
    assert st["separable_frac"] < 0.5, st
    assert pair_p(M[order[0]], M[order[-1]])[0] < 1e-6
    # 8. a 100-item subset of the same board resolves no more than the full 1000
    Mr = M[:, :100]
    order_r = np.argsort(-Mr.mean(axis=1), kind="stable")
    st_r = adjacent_stats(Mr, order_r, 0.05)
    assert st_r["separable"] <= st["separable"] + 3, (st_r, st)
    print("selftest: 8/8 passed")


def main() -> None:
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--data", default=os.path.join(ROOT, "data"), help="dir with {bm}.csv (Zenodo data.tgz)")
    ap.add_argument("--items", default=os.path.join(ROOT, "items"), help="dir with items-{bm}-A.csv")
    ap.add_argument("--bm", nargs="+", default=BENCHMARKS, choices=BENCHMARKS)
    ap.add_argument("--version", default="A", choices=["A", "B"], help="metabench item list version")
    ap.add_argument("--alpha", type=float, default=0.05)
    ap.add_argument("--topk", type=int, default=100)
    ap.add_argument("--out", default=None, help="write JSON results here")
    ap.add_argument("--selftest", action="store_true", help="run self-tests only")
    args = ap.parse_args()

    selftest()
    if args.selftest:
        return

    results = []
    for bm in args.bm:
        print(f"[{bm}] loading ...", file=sys.stderr)
        wide = load_full_matrix(bm, args.data)
        red_ids = load_reduced_items(bm, args.items, args.version)
        red_cols = align_reduced(bm, wide, red_ids)
        if len(red_cols) < len(red_ids):
            print(f"  [{bm}] WARNING: {len(red_ids) - len(red_cols)} of {len(red_ids)} metabench items "
                  f"not found in the full matrix", file=sys.stderr)
        if not red_cols:
            raise SystemExit(f"[{bm}] no metabench items matched the data columns; check id format")
        r = analyse_benchmark(bm, wide, red_cols, args.alpha, args.topk)
        r["reduced_items_matched"] = len(red_cols)
        r["reduced_items_listed"] = len(red_ids)
        results.append(r)

    print_report(results, args.alpha, args.topk)
    if args.out:
        with open(args.out, "w") as fh:
            json.dump({"alpha": args.alpha, "topk": args.topk, "version": args.version,
                       "results": results}, fh, indent=2)
        print(f"\nwrote {args.out}")


if __name__ == "__main__":
    main()
