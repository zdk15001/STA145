# Comparing Couples

A browser-based review page for STA 145, module 3.2 (two-sample t-tests), built on real data from the How Couples Meet and Stay Together survey (HCMST 2017).

Students can:

- **Explore** 12 comparisons (box plots, the difference in means with its 95% CI, a clickable R printout, a clickable summary table, and an assumptions check).
- **Read the output**: answer questions about fresh random-sample printouts.
- **Write it up**: write an interpretation in the Extra Practice format and get feedback from a checker.
- **Pick the test**: choose between t-tests, paired t-tests, ANOVA and chi-square, and spot assumption problems.

The bottom of the page also has **See weighted results!** (a `svyttest()` printout with the survey weights), **What questions can't these data answer?**, and **About the data**.

Everything runs in the student's browser. There is no server, no tracking, and nothing students type leaves the page.

## What's in this folder

| Path | What it is |
|---|---|
| `index.html` | The finished page. This is the only file students need; GitHub Pages serves it. |
| `build.py` | Rebuilds `index.html` from the files in `src/` and `data/`. |
| `src/template.html` | The page's layout and styles (HTML + CSS). |
| `src/engine.js` | The statistics: Welch t-test, weighted t-test, and R-style printing. |
| `src/app.js` | Everything students see and click: comparisons, wording, questions, the checker. |
| `data/hcmst_data.json` | The 13 HCMST variables the page uses (plus the survey weight), already built. |
| `data/build_data.py` | Rebuilds `hcmst_data.json` from the HCMST `.dta` file. |
| `tests/test_engine.js` | Checks the engine against R's own `t.test()` example. |
| `tests/check_full_sample.py` | Prints all 12 full-sample results, computed separately with scipy. |
| `r/check_in_r.R` | Reproduces the full-sample printouts in R (unweighted and weighted). |

## Host it from your `sta145` repository with GitHub Pages

1. **Add the folder.** Put this whole `comparing-couples` folder inside the supplemental folder of your `sta145` repository, so the page ends up at `supplemental/comparing-couples/index.html`.
   - On github.com: open the supplemental folder, click **Add file → Upload files**, drag in the `comparing-couples` folder, and click **Commit changes**.
   - Or with git: copy the folder in, then `git add`, `git commit`, `git push`.
2. **Turn on GitHub Pages** (skip this if the repository already publishes a site). Go to the repository's **Settings → Pages**. Under "Build and deployment," choose **Deploy from a branch**, then branch **main** and folder **/ (root)**, and click **Save**.
   - If Pages is already set to publish from a `/docs` folder, the page has to live inside `docs/` instead.
   - On a free GitHub account, Pages only works for **public** repositories.
3. **Open the page.** After a minute or two it's live at:
   `https://<your-username>.github.io/sta145/supplemental/comparing-couples/`
   (Use your folder's exact name in the address, including its spelling and capitalization.)
4. **Share it.** Paste that address into Canvas. Students don't need any accounts.

## Change the page

Edit the files in `src/`, then rebuild and upload the new `index.html`:

```
python3 build.py
```

Where to find things in `src/app.js` (search for these names):

| To change | Look for |
|---|---|
| The 12 comparisons, research questions, hypotheses, and who's included | `var COMPS` |
| How each outcome is described (labels, units, Direction/Magnitude wording) | `var OUT` |
| The "So what?" sentences in the Write it up example answers | `var SOWHAT` |
| The "Pick the test" scenarios | `var BANK` |
| The "What questions can't these data answer?" list | `var LIMITS` |
| What each part of the R printout means when clicked | `function partExplain` |
| What each summary-table cell means when clicked | `function cellExplain` |
| The assumptions checks | `function assumptions` |
| The write-up checker | `function checkWriteup` |

Text on the page that doesn't change with the data (headings, intros, About the data) is in `src/template.html`.

## Rebuild the data (only if you change which variables are used)

Needs Python 3 with pandas and numpy, and the HCMST public file `HCMST_2017_to_2022_small_public_version_2.2.dta`:

```
python3 data/build_data.py path/to/HCMST_2017_to_2022_small_public_version_2.2.dta
python3 build.py
```

The `.dta` file isn't included here; download it from the HCMST project at Stanford.

## Check the numbers

- `node tests/test_engine.js` checks the engine against R's documented `t.test(extra ~ group, data = sleep)` output and R's number formatting.
- `python3 tests/check_full_sample.py` prints all 12 comparisons, unweighted and weighted, computed separately with scipy (needs numpy and scipy).
- `r/check_in_r.R` runs the same comparisons in R with `t.test()` and `survey::svyttest()`. The weighted printout on the page reproduces how `svyttest()` computes its results. If R's last printed digit ever differs, the confidence interval is the most likely place.

## About the data

All examples come from the first wave (2017) of How Couples Meet and Stay Together (HCMST 2017–2022 public data, version 2.2, produced by Michael J. Rosenfeld, Stanford University). See the HCMST user's guide for the preferred citation.

- The tests on the Explore, Read, and Write tabs are **unweighted**, like the `t.test()` students run in class. HCMST includes an oversample of lesbian, gay, and bisexual respondents, so "See weighted results!" shows the same comparison with the `w1_weight_combo` survey weight.
- Relationship quality (`w1_q34`) is flipped so that 5 = excellent and 1 = very poor.
- The page's printouts use current R's wording ("true difference in means between group … and group …"). Older versions of R leave the group names out of that line.
- Fonts load from Google Fonts. If they're blocked, the page falls back to the computer's own fonts and still works.
