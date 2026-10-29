"""Rebuild data/hcmst_data.json from the HCMST public data file.

Usage (from the comparing-couples folder):
    python3 data/build_data.py path/to/HCMST_2017_to_2022_small_public_version_2.2.dta

Needs Python 3 with pandas and numpy. Uses the 2017 wave (w1_ variables).
Each variable is stored as base64-encoded float32 values, one per respondent
(3,510 rows), with NaN for missing. Float32 keeps the .dta file's own precision,
so the page's numbers match what R gets from the same file.
"""
import base64
import json
import sys
import warnings
from pathlib import Path

import numpy as np
import pandas as pd

warnings.filterwarnings("ignore")  # the .dta has a few latin-1 labels

if len(sys.argv) != 2:
    raise SystemExit(__doc__)

df = pd.read_stata(sys.argv[1], convert_categoricals=False)

cols = {}
# Q34 is coded 1 = excellent ... 5 = very poor; flip it so higher = better
q34 = df.w1_q34.where(df.w1_q34.between(1, 5))
cols["rel_quality"] = 6 - q34
cols["how_met"] = df.w1_either_internet_adjusted                   # 0 offline, 1 online
cols["marital"] = df.w1_partnership_status.map({1: 1.0, 2: 0.0})   # 1 married, 0 not married (currently partnered only)
cols["living"] = df.w1_q19.map({1: 1.0, 2: 0.0})                   # 1 together, 0 apart
cols["couple_type"] = df.w1_same_sex_couple                        # 0 different-sex, 1 same-sex
cols["met_friends"] = df.w1_q24_met_through_friend                 # 1 through friends, 0 other ways
cols["age_met"] = df.w1_age_when_met
cols["years_to_couple"] = df.w1_time_from_met_to_rel
cols["years_to_movein"] = df.w1_time_from_rel_to_cohab
partner_age = df.w1_q9.where(df.w1_q9 >= 18)                        # -1 = refused
cols["age_gap"] = (df.w1_ppage - partner_age).abs()
cols["age"] = df.w1_ppage.astype(float)
cols["app_use"] = df.w1_otherdate_app_all.map({1: 0.0, 2: 1.0})   # 1 used an app, 0 didn't (met someone new last year)
rel_years = df.w1_relate_duration_in2017_years
cols["rel_years"] = rel_years.where(rel_years >= 0)
cols["weight"] = df.w1_weight_combo                                 # survey weight for the 2017 wave

out = {"n": len(df), "cols": {}}
for name, series in cols.items():
    values = np.asarray(series, dtype=np.float32)
    out["cols"][name] = base64.b64encode(values.tobytes()).decode()

target = Path(__file__).resolve().parent / "hcmst_data.json"
target.write_text(json.dumps(out), encoding="utf-8")
print(f"Wrote {target} ({len(out['cols'])} variables, {len(df):,} respondents)")
