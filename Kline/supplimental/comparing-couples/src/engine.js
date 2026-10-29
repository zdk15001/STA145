// Welch two-sample t-test engine with R-style printing.
// Mirrors R's t.test(y ~ g) (var.equal = FALSE) and print.htest formatting.
(function (root) {
  "use strict";

  // ---------- special functions ----------
  var LANCZOS = [0.99999999999980993, 676.5203681218851, -1259.1392167224028,
    771.32342877765313, -176.61502916214059, 12.507343278686905,
    -0.13857109526572012, 9.9843695780195716e-6, 1.5056327351493116e-7];

  function lgamma(x) {
    if (x < 0.5) return Math.log(Math.PI / Math.abs(Math.sin(Math.PI * x))) - lgamma(1 - x);
    x -= 1;
    var a = LANCZOS[0], t = x + 7.5;
    for (var i = 1; i < 9; i++) a += LANCZOS[i] / (x + i);
    return 0.5 * Math.log(2 * Math.PI) + (x + 0.5) * Math.log(t) - t + Math.log(a);
  }

  // continued fraction for the incomplete beta (modified Lentz)
  function betacf(x, a, b) {
    var FPMIN = 1e-300, EPS = 1e-16, qab = a + b, qap = a + 1, qam = a - 1;
    var c = 1, d = 1 - qab * x / qap;
    if (Math.abs(d) < FPMIN) d = FPMIN;
    d = 1 / d;
    var h = d;
    for (var m = 1; m <= 10000; m++) {
      var m2 = 2 * m, aa = m * (b - m) * x / ((qam + m2) * (a + m2));
      d = 1 + aa * d; if (Math.abs(d) < FPMIN) d = FPMIN;
      c = 1 + aa / c; if (Math.abs(c) < FPMIN) c = FPMIN;
      d = 1 / d; h *= d * c;
      aa = -(a + m) * (qab + m) * x / ((a + m2) * (qap + m2));
      d = 1 + aa * d; if (Math.abs(d) < FPMIN) d = FPMIN;
      c = 1 + aa / c; if (Math.abs(c) < FPMIN) c = FPMIN;
      d = 1 / d;
      var del = d * c; h *= del;
      if (Math.abs(del - 1) < EPS) break;
    }
    return h;
  }

  // regularized incomplete beta I_x(a, b)
  function ibeta(x, a, b) {
    if (x <= 0) return 0;
    if (x >= 1) return 1;
    var lbt = lgamma(a + b) - lgamma(a) - lgamma(b) + a * Math.log(x) + b * Math.log(1 - x);
    if (x < (a + 1) / (a + b + 2)) return Math.exp(lbt) * betacf(x, a, b) / a;
    return 1 - Math.exp(lbt) * betacf(1 - x, b, a) / b;
  }

  // two-sided p-value P(|T| >= |t|) for Student t with df degrees of freedom
  function pTwoSided(t, df) {
    if (!isFinite(t)) return 0;
    return ibeta(df / (df + t * t), df / 2, 0.5);
  }

  // upper quantile: t such that P(|T| >= t) = alpha
  function qtTwoSided(alpha, df) {
    var lo = 0, hi = 1;
    while (pTwoSided(hi, df) > alpha) hi *= 2;
    for (var i = 0; i < 200; i++) {
      var mid = (lo + hi) / 2;
      if (pTwoSided(mid, df) > alpha) lo = mid; else hi = mid;
      if (hi - lo < 1e-15 * hi) break;
    }
    return (lo + hi) / 2;
  }

  // ---------- descriptive ----------
  function mean(a) { var s = 0; for (var i = 0; i < a.length; i++) s += a[i]; var m = s / a.length;
    // second pass correction, as R does
    var c = 0; for (i = 0; i < a.length; i++) c += a[i] - m; return m + c / a.length; }
  function variance(a) { var m = mean(a), s = 0, c = 0;
    for (var i = 0; i < a.length; i++) { var d = a[i] - m; s += d * d; c += d; }
    return (s - c * c / a.length) / (a.length - 1); }

  function welch(x, y) {
    var nx = x.length, ny = y.length, mx = mean(x), my = mean(y), vx = variance(x), vy = variance(y);
    var sx = vx / nx, sy = vy / ny, se = Math.sqrt(sx + sy);
    var df = (sx + sy) * (sx + sy) / (sx * sx / (nx - 1) + sy * sy / (ny - 1));
    var t = (mx - my) / se, p = pTwoSided(t, df), q = qtTwoSided(0.05, df);
    var diff = mx - my;
    return { nx: nx, ny: ny, mx: mx, my: my, sdx: Math.sqrt(vx), sdy: Math.sqrt(vy), se: se,
      t: t, df: df, p: p, diff: diff, lo: diff - q * se, hi: diff + q * se, tcrit: q };
  }

  // ---------- R-style number formatting ----------
  // significant-digit info for one value at `digits` (R's scientific())
  function sigInfo(x, digits) {
    if (x === 0) return { neg: 0, kp: 0, nsig: 1 };
    var s = Math.abs(x).toExponential(digits - 1); // "d.ddddde+k"
    var parts = s.split("e"), mant = parts[0].replace(".", "").replace(/0+$/, "");
    return { neg: x < 0 ? 1 : 0, kp: parseInt(parts[1], 10), nsig: Math.max(1, mant.length) };
  }

  // R's formatReal for a vector with common formatting. Returns array of strings (common width).
  function formatReal(xs, digits) {
    var infos = xs.map(function (x) { return sigInfo(x, digits); });
    var neg = 0, mxsl = 1, rgt = 0, mxe = 1, mxns = 1, mF = 0;
    infos.forEach(function (f) {
      if (f.neg) neg = 1;
      var left = f.kp + 1; if (left < 1) left = 1;
      var r = f.nsig - f.kp - 1; if (r < 0) r = 0;
      if (left > mxsl) mxsl = left;
      if (r > rgt) rgt = r;
      if (f.nsig > mxns) mxns = f.nsig;
      var ae = Math.abs(f.kp); if (ae >= 100 && mxe < 3) mxe = 3;
    });
    if (rgt > 15) rgt = 15; // R caps decimals at DBL_DIG
    var wF = neg + mxsl + (rgt ? rgt + 1 : 0);
    var wE = neg + (mxns > 1 ? mxns + 1 : mxns) + 2 + (mxe === 3 ? 3 : 2);
    var out;
    if (wF <= wE) {
      out = xs.map(function (x) { return x.toFixed(rgt); });
    } else {
      var e = mxns - 1;
      out = xs.map(function (x) {
        var s = x.toExponential(e), p = s.split("e"), k = parseInt(p[1], 10);
        var ks = String(Math.abs(k)); if (ks.length < 2) ks = "0" + ks;
        return p[0] + "e" + (k < 0 ? "-" : "+") + ks;
      });
    }
    var w = Math.max.apply(null, out.map(function (s) { return s.length; }));
    return out.map(function (s) { return pad(s, w); });
  }
  function pad(s, w) { while (s.length < w) s = " " + s; return s; }
  function fmt(x, digits) { return formatReal([x], digits)[0]; }

  function formatPval(p, digits) {
    var eps = 2.220446049250313e-16;
    if (p < eps) return "< " + fmt(eps, Math.max(1, digits - 2));
    return fmt(p, digits);
  }

  // Printout as R prints a Welch t-test from the formula interface, split into
  // segments. Each segment is {t: text, k: key or null}; keys mark the parts a
  // reader can ask about. `width` is the console width in characters (R wraps
  // named vectors to fit it).
  function printWelchParts(res, yName, gName, labA, labB, width) {
    var D = 7, out = [];
    width = width || 80;
    function seg(t, k) { out.push({ t: t, k: k || null }); }
    seg("\n\t"); seg("Welch Two Sample t-test", "method"); seg("\n\n");
    seg("data:  " + yName + " by " + gName, "data"); seg("\n");
    var fp = formatPval(res.p, D - 3);
    seg("t = " + fmt(res.t, D - 2), "t"); seg(", ");
    seg("df = " + fmt(res.df, D - 2), "df"); seg(", ");
    seg("p-value " + (fp.charAt(0) === "<" ? fp : "= " + fp), "p"); seg("\n");
    seg("alternative hypothesis: true difference in means between group " + labA +
      " and group " + labB + " is not equal to 0", "alt"); seg("\n");
    seg("95 percent confidence interval:", "ci"); seg("\n");
    var ci = formatReal([res.lo, res.hi], D), ciLine = " " + ci.join(" ");
    var lead = ciLine.match(/^\s*/)[0];
    seg(lead); seg(ciLine.slice(lead.length), "ci"); seg("\n");
    seg("sample estimates:", "est"); seg("\n");
    var names = ["mean in group " + labA, "mean in group " + labB], keys = ["meanA", "meanB"];
    var vals = formatReal([res.mx, res.my], D).map(function (v) { return v.trim(); });
    var w = Math.max(vals[0].length, vals[1].length, names[0].length, names[1].length);
    var per = Math.max(1, Math.floor(width / (w + 1)));
    for (var i = 0; i < names.length; i += per) {
      [names, vals].forEach(function (arr) {
        for (var j = i; j < Math.min(i + per, arr.length); j++) {
          var padded = pad(arr[j], w);
          seg(padded.slice(0, padded.length - arr[j].length)); seg(arr[j], keys[j]); seg(" ");
        }
        seg("\n");
      });
    }
    return out;
  }
  function printWelch(res, yName, gName, labA, labB, width) {
    return printWelchParts(res, yName, gName, labA, labB, width).map(function (s) { return s.t; }).join("");
  }


  // Weighted, design-based two-group t-test as survey::svyttest() reports it for
  // svydesign(ids = ~1, weights = ~w): the estimate is the svyglm coefficient,
  // second group minus first, with a linearization (sandwich) standard error.
  // nTotal is the number of rows in the full design (domain estimation keeps them).
  function svyttest(a, wa, b, wb, nTotal) {
    function wsum(v, w) { var s = 0; for (var i = 0; i < v.length; i++) s += w[i] * v[i]; return s; }
    function sum(w) { var s = 0; for (var i = 0; i < w.length; i++) s += w[i]; return s; }
    var Wa = sum(wa), Wb = sum(wb), ma = wsum(a, wa) / Wa, mb = wsum(b, wb) / Wb, va = 0, vb = 0, i;
    for (i = 0; i < a.length; i++) va += wa[i] * wa[i] * (a[i] - ma) * (a[i] - ma);
    for (i = 0; i < b.length; i++) vb += wb[i] * wb[i] * (b[i] - mb) * (b[i] - mb);
    va /= Wa * Wa; vb /= Wb * Wb;
    var n = a.length + b.length, se = Math.sqrt(nTotal / (nTotal - 1) * (va + vb)), diff = mb - ma;
    var df = n - 2, t = diff / se, q = qtTwoSided(0.05, n - 1);
    return { ma: ma, mb: mb, diff: diff, se: se, t: t, df: df, p: pTwoSided(t, df), lo: diff - q * se, hi: diff + q * se, n: n };
  }
  function printSvyttest(res, yName, gName) {
    var D = 7, lines = [], fp = formatPval(res.p, D - 3);
    lines.push("", "\tDesign-based t-test", "", "data:  " + yName + " ~ " + gName);
    lines.push("t = " + fmt(res.t, D - 2) + ", df = " + fmt(res.df, D - 2) + ", p-value " + (fp.charAt(0) === "<" ? fp : "= " + fp));
    lines.push("alternative hypothesis: true difference in mean is not equal to 0");
    lines.push("95 percent confidence interval:");
    lines.push(" " + formatReal([res.lo, res.hi], D).join(" "));
    lines.push("sample estimates:");
    var name = "difference in mean", v = fmt(res.diff, D).trim(), w = Math.max(name.length, v.length);
    lines.push(pad(name, w) + " ", pad(v, w) + " ");
    return lines.join("\n") + "\n";
  }

  // pieces of the printout as the student sees them (strings exactly as printed)
  function printedPieces(res) {
    var D = 7, ci = formatReal([res.lo, res.hi], D).map(function (s) { return s.trim(); });
    var est = formatReal([res.mx, res.my], D).map(function (s) { return s.trim(); });
    var fp = formatPval(res.p, D - 3);
    return { t: fmt(res.t, D - 2).trim(), df: fmt(res.df, D - 2).trim(), p: fp,
      lo: ci[0], hi: ci[1], mA: est[0], mB: est[1] };
  }

  var api = { lgamma: lgamma, ibeta: ibeta, pTwoSided: pTwoSided, qtTwoSided: qtTwoSided,
    mean: mean, variance: variance, welch: welch, formatReal: formatReal, fmt: fmt,
    formatPval: formatPval, printWelch: printWelch, printWelchParts: printWelchParts, printedPieces: printedPieces,
    svyttest: svyttest, printSvyttest: printSvyttest };
  if (typeof module !== "undefined" && module.exports) module.exports = api;
  else root.TT = api;
})(this);
