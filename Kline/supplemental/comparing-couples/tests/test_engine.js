// Checks the t-test engine against R's own documented example and R's number formatting.
// Run from the comparing-couples folder:  node tests/test_engine.js
const TT = require("../src/engine.js");
let failed = 0;
const check = (label, got, want) => {
  const ok = got === want;
  if (!ok) failed++;
  console.log((ok ? "ok    " : "FAIL  ") + label + (ok ? "" : `\n      got:  ${JSON.stringify(got)}\n      want: ${JSON.stringify(want)}`));
};

// R: t.test(extra ~ group, data = sleep)
const g1 = [0.7, -1.6, -0.2, -1.2, -0.1, 3.4, 3.7, 0.8, 0.0, 2.0];
const g2 = [1.9, 0.8, 1.1, 0.1, -0.1, 4.4, 5.5, 1.6, 4.6, 3.4];
const want = [
  "",
  "\tWelch Two Sample t-test",
  "",
  "data:  extra by group",
  "t = -1.8608, df = 17.776, p-value = 0.07939",
  "alternative hypothesis: true difference in means between group 1 and group 2 is not equal to 0",
  "95 percent confidence interval:",
  " -3.3654832  0.2054832",
  "sample estimates:",
  "mean in group 1 mean in group 2 ",
  "           0.75            2.33 ",
  ""
].join("\n");
check("R's sleep example printout", TT.printWelch(TT.welch(g1, g2), "extra", "group", "1", "2"), want);

// R's format.pval / format() behaviour
check("small p in scientific notation", TT.formatPval(1.613e-05, 4), "1.613e-05");
check("p kept in fixed notation", TT.formatPval(0.0001631, 4), "0.0001631");
check("p below machine epsilon", TT.formatPval(1e-30, 4), "< 2.2e-16");
check("df to 5 significant digits", TT.fmt(89.2139, 5), "89.214");
check("vector shares decimals", TT.formatReal([0.75, 2.33], 7).join(" "), "0.75 2.33");

// the clickable printout must be the same text as the plain printout
const r = TT.welch(g1, g2);
for (const w of [80, 40, 20]) {
  check(`clickable parts match plain text (width ${w})`,
    TT.printWelchParts(r, "extra", "group", "1", "2", w).map(p => p.t).join(""),
    TT.printWelch(r, "extra", "group", "1", "2", w));
}

console.log(failed ? `\n${failed} check(s) failed` : "\nAll checks passed");
process.exit(failed ? 1 : 0);
