"""Print the full-sample results for all 12 comparisons, computed independently
with scipy (unweighted Welch) and numpy (weighted, design-based), so you can
compare them with the page and with R.

Run from the comparing-couples folder:  python3 tests/check_full_sample.py
Needs Python 3 with numpy and scipy.
"""
import base64
import json
from pathlib import Path

import numpy as np
from scipy import stats

data = json.loads((Path(__file__).resolve().parent.parent / "data" / "hcmst_data.json").read_text())
col = {k: np.frombuffer(base64.b64decode(v), dtype=np.float32).astype(float) for k, v in data["cols"].items()}
N = data["n"]

# (outcome, group variable, code for the group R lists first, code for the second, labels)
COMPS = [
    ("rel_quality", "how_met", 0, 1, "offline", "online"),
    ("rel_quality", "marital", 1, 0, "married", "not_married"),
    ("rel_quality", "living", 0, 1, "apart", "together"),
    ("rel_quality", "couple_type", 0, 1, "different_sex", "same_sex"),
    ("rel_quality", "met_friends", 1, 0, "friends", "other_ways"),
    ("age_met", "how_met", 0, 1, "offline", "online"),
    ("years_to_couple", "how_met", 0, 1, "offline", "online"),
    ("years_to_movein", "how_met", 0, 1, "offline", "online"),
    ("age_gap", "couple_type", 0, 1, "different_sex", "same_sex"),
    ("age_gap", "marital", 1, 0, "married", "not_married"),
    ("age", "app_use", 0, 1, "no_app", "used_app"),
    ("rel_years", "how_met", 0, 1, "offline", "online"),
]

w = col["weight"]
for y, g, code_a, code_b, lab_a, lab_b in COMPS:
    ok = ~np.isnan(col[y]) & ~np.isnan(col[g])
    in_a, in_b = ok & (col[g] == code_a), ok & (col[g] == code_b)
    a, b = col[y][in_a], col[y][in_b]
    # unweighted Welch t-test, as t.test(y ~ g): first group minus second
    r = stats.ttest_ind(a, b, equal_var=False)
    ci = r.confidence_interval()
    # weighted, as survey::svyttest with svydesign(ids = ~1, weights = ~w1_weight_combo):
    # second group minus first, linearization SE, df = n - 2, CI on n - 1 df
    wa, wb = w[in_a], w[in_b]
    ma, mb = np.sum(wa * a) / wa.sum(), np.sum(wb * b) / wb.sum()
    var = (np.sum(wa**2 * (a - ma) ** 2) / wa.sum() ** 2 + np.sum(wb**2 * (b - mb) ** 2) / wb.sum() ** 2) * N / (N - 1)
    se, diff, n = np.sqrt(var), mb - ma, len(a) + len(b)
    t = diff / se
    p = 2 * stats.t.sf(abs(t), n - 2)
    q = stats.t.ppf(0.975, n - 1)
    print(f"{y} ~ {g}  ({lab_a} n={len(a)}, {lab_b} n={len(b)})")
    print(f"  t.test:   t = {r.statistic:.5g}, df = {r.df:.5g}, p = {r.pvalue:.4g}, "
          f"95% CI {ci.low:.7g} to {ci.high:.7g}, means {a.mean():.7g} and {b.mean():.7g}")
    print(f"  svyttest: t = {t:.5g}, df = {n - 2}, p = {p:.4g}, "
          f"95% CI {diff - q * se:.7g} to {diff + q * se:.7g}, difference in mean {diff:.7g}")
