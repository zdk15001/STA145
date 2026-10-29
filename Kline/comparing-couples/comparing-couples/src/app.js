(function () {
  "use strict";
  var TT = window.TT;

  /* =========================================================
     Data
     ========================================================= */
  var RAW = JSON.parse(document.getElementById("hcmst-data").textContent);
  function decode(b64) {
    var bin = atob(b64), bytes = new Uint8Array(bin.length);
    for (var i = 0; i < bin.length; i++) bytes[i] = bin.charCodeAt(i);
    return new Float32Array(bytes.buffer);
  }
  var COL = {};
  Object.keys(RAW.cols).forEach(function (k) { COL[k] = decode(RAW.cols[k]); });

  /* Outcomes: how each quantitative variable is described in words */
  var OUT = {
    rel_quality: {
      label: "Relationship quality", axis: "Relationship quality (1 = very poor, 5 = excellent)",
      desc: "“How would you describe the quality of your relationship?” scored 1 = very poor, 2 = poor, 3 = fair, 4 = good, 5 = excellent.",
      noun: "relationship quality rating", unit: "points", up: "higher", down: "lower",
      ci: { pre: "rate their relationships", up: "higher", down: "lower", post: "" },
      more: "rate their relationships higher", less: "rate their relationships lower",
      mag: function (v, up) { return "rate their relationships " + v + " points " + (up ? "higher" : "lower") + " on average, on a 1-to-5 scale"; },
      verb: function (v) { return "rated their relationship " + v + " out of 5 on average"; },
      kw: ["quality", "rate", "rating", "relationship", "satisf", "happy", "happi"],
      domain: [1, 5], shift: [2, 0.05]
    },
    age_met: {
      label: "Age when they met their partner", axis: "Age when they first met their partner (years)",
      desc: "Age in years when they first met their partner.",
      noun: "age when they met their partner", unit: "years", up: "higher", down: "lower",
      ci: { pre: "were", up: "older", down: "younger", post: " when they met their partner" },
      more: "were older when they met their partner", less: "were younger when they met their partner",
      mag: function (v, up) { return "were " + v + " years " + (up ? "older" : "younger") + " on average when they met their partner"; },
      verb: function (v) { return "were " + v + " years old on average when they met their partner"; },
      kw: ["age", "old", "young"], shift: [12, 0.5]
    },
    years_to_couple: {
      label: "Years from meeting to becoming a couple", axis: "Years from first meeting to becoming a couple",
      desc: "Years between first meeting their partner and the start of the romantic relationship.",
      noun: "time from meeting to becoming a couple", unit: "years", up: "longer", down: "shorter",
      ci: { pre: "took", up: "longer", down: "less", post: " to go from meeting to becoming a couple" },
      more: "took longer to go from meeting to becoming a couple", less: "became a couple sooner after meeting",
      mag: function (v, up) { return "took " + v + " years " + (up ? "longer" : "less") + " on average to go from meeting to becoming a couple"; },
      verb: function (v) { return "took " + v + " years on average to go from meeting to becoming a couple"; },
      kw: ["year", "time", "took", "fast", "slow", "quick", "long", "sooner", "couple", "dating"], shift: [4, 0.1]
    },
    years_to_movein: {
      label: "Years from becoming a couple to moving in", axis: "Years from becoming a couple to moving in together",
      desc: "Years between the start of the romantic relationship and first living together.",
      noun: "time from becoming a couple to moving in together", unit: "years", up: "longer", down: "shorter",
      ci: { pre: "took", up: "longer", down: "less", post: " to move in together after becoming a couple" },
      more: "took longer to move in together", less: "moved in together sooner",
      mag: function (v, up) { return "took " + v + " years " + (up ? "longer" : "less") + " on average to move in together after becoming a couple"; },
      verb: function (v) { return "took " + v + " years on average to move in together after becoming a couple"; },
      kw: ["year", "time", "took", "move", "moving", "moved", "live", "living", "cohabit", "sooner", "fast"], shift: [4, 0.1]
    },
    age_gap: {
      label: "Age gap between partners", axis: "Age gap between partners (years)",
      desc: "Absolute difference in years between the respondent's age and their partner's age.",
      noun: "age gap between partners", unit: "years", up: "larger", down: "smaller",
      ci: { pre: "have age gaps that are", up: "bigger", down: "smaller", post: "" },
      more: "have bigger age gaps with their partner", less: "have smaller age gaps with their partner",
      mag: function (v, up) { return "have age gaps that are " + v + " years " + (up ? "bigger" : "smaller") + " on average"; },
      verb: function (v) { return "had an average age gap of " + v + " years with their partner"; },
      kw: ["gap", "age difference", "difference in age", "apart in age", "older", "younger", "age"], shift: [6, 0.25]
    },
    age: {
      label: "Age", axis: "Age (years)",
      desc: "Age in years in 2017.",
      noun: "age", unit: "years", up: "higher", down: "lower",
      ci: { pre: "are", up: "older", down: "younger", post: "" },
      more: "are older", less: "are younger",
      mag: function (v, up) { return "are " + v + " years " + (up ? "older" : "younger") + " on average"; },
      verb: function (v) { return "were " + v + " years old on average"; },
      kw: ["age", "old", "young"], shift: [12, 0.5]
    },
    rel_years: {
      label: "Years together", axis: "Years together (as of 2017)",
      desc: "Years since the romantic relationship started, as of July 2017.",
      noun: "relationship length", unit: "years", up: "longer", down: "shorter",
      ci: { pre: "have been with their partner", up: "longer", down: "less", post: "" },
      more: "have been with their partner longer", less: "have been with their partner for less time",
      mag: function (v, up) { return "have been with their partner " + v + " years " + (up ? "longer" : "less") + " on average"; },
      verb: function (v) { return "had been with their partner for " + v + " years on average"; },
      kw: ["year", "together", "long", "length", "last", "duration"], shift: [20, 0.5]
    }
  };

  /* Group definitions. In every pair, A is the label R sorts first (alphabetically). */
  var HOW = {
    g: "how_met", desc: "how they met their partner",
    A: { code: 0, lab: "offline", who: "respondents who met their partner offline", short: "people who met offline",
      kw: ["offline", "off-line", "in person", "in-person", "not online", "face to face", "face-to-face", "traditional", "other ways"] },
    B: { code: 1, lab: "online", who: "respondents who met their partner online", short: "people who met online",
      kw: ["online", "on-line", "internet", "app", "website", "web site", "dating site"] }
  };
  var MARITAL = {
    g: "marital", desc: "whether they are married",
    A: { code: 1, lab: "married", who: "married respondents", short: "married people", kw: ["married", "spouse", "marriage"] },
    B: { code: 0, lab: "not_married", who: "respondents in unmarried relationships", short: "people in unmarried relationships",
      kw: ["not married", "unmarried", "non-married", "nonmarried", "aren't married", "are not married", "dating", "cohabit"] }
  };
  var LIVING = {
    g: "living", desc: "whether they live with their partner",
    A: { code: 0, lab: "apart", who: "respondents who don't live with their partner", short: "people who live apart from their partner",
      kw: ["apart", "separately", "separate", "not living", "don't live", "do not live", "not live", "doesn't live", "don’t live"] },
    B: { code: 1, lab: "together", who: "respondents who live with their partner", short: "people who live with their partner",
      kw: ["together", "cohabit", "live with", "living with", "lives with", "same home", "same household"] }
  };
  var COUPLE = {
    g: "couple_type", desc: "couple type (same-sex or different-sex)",
    A: { code: 0, lab: "different_sex", who: "respondents in different-sex couples", short: "people in different-sex couples",
      kw: ["different-sex", "different sex", "opposite-sex", "opposite sex", "straight", "heterosexual", "mixed-sex"] },
    B: { code: 1, lab: "same_sex", who: "respondents in same-sex couples", short: "people in same-sex couples",
      kw: ["same-sex", "same sex", "gay", "lesbian", "queer"] }
  };
  var FRIENDS = {
    g: "met_friends", desc: "whether friends introduced them",
    A: { code: 1, lab: "friends", who: "respondents who met their partner through friends", short: "people who met through friends", kw: ["friend"] },
    B: { code: 0, lab: "other_ways", who: "respondents who met their partner some other way", short: "people who met some other way",
      kw: ["other way", "other ways", "another way", "some other", "not through friends", "didn't meet through", "did not meet through", "without friends", "otherwise"] }
  };
  var APP = {
    g: "app_use", desc: "whether they used a dating app",
    A: { code: 0, lab: "no_app", who: "people who didn't use a dating app", short: "people who didn't use an app",
      kw: ["no app", "not use", "didn't use", "did not use", "without", "non-app", "non app", "not using", "no dating app", "didn’t use"] },
    B: { code: 1, lab: "used_app", who: "people who used a dating app", short: "app users", kw: ["app"] }
  };

  var PARTNERED = "People currently in a relationship.";
  var EVER = "People with a current or past partner.";
  var COMPS = [
    { id: "quality-howmet", y: "rel_quality", grp: HOW, pop: PARTNERED,
      rq: "Do people who met their partner online rate their relationship differently from people who met offline?",
      hyp: "People who met their partner online will rate their relationship quality differently than people who met offline." },
    { id: "quality-married", y: "rel_quality", grp: MARITAL, pop: PARTNERED,
      rq: "Do married people rate their relationship higher than people in unmarried relationships?",
      hyp: "Married respondents will report higher relationship quality than respondents in unmarried relationships." },
    { id: "quality-living", y: "rel_quality", grp: LIVING, pop: PARTNERED,
      rq: "Do people who live with their partner rate their relationship differently from people who don't?",
      hyp: "Respondents who live with their partner will report higher relationship quality than those who live apart." },
    { id: "quality-couple", y: "rel_quality", grp: COUPLE, pop: PARTNERED,
      rq: "Do people in same-sex couples rate their relationship differently from people in different-sex couples?",
      hyp: "Relationship quality will differ between same-sex and different-sex couples." },
    { id: "quality-friends", y: "rel_quality", grp: FRIENDS, pop: PARTNERED,
      rq: "Do couples who were introduced by friends have better relationships?",
      hyp: "Respondents who met their partner through friends will report higher relationship quality than those who met some other way." },
    { id: "agemet-howmet", y: "age_met", grp: HOW, pop: EVER,
      rq: "Are people who meet their partner online older when they meet?",
      hyp: "People who met their partner online were older when they met than people who met offline." },
    { id: "couple-howmet", y: "years_to_couple", grp: HOW, pop: EVER,
      rq: "Do couples who meet online become a couple faster?",
      hyp: "Couples who met online will go from meeting to dating faster than couples who met offline.",
      note: "Time to become a couple is very right-skewed: most couples got together within a year, but a few took decades. With large samples the t-test still works because of the Central Limit Theorem; with small samples, be more cautious." },
    { id: "movein-howmet", y: "years_to_movein", grp: HOW, pop: "People who have lived with their current or most recent partner.",
      rq: "Do couples who meet online move in together sooner?",
      hyp: "Couples who met online will move in together sooner after becoming a couple than couples who met offline." },
    { id: "gap-couple", y: "age_gap", grp: COUPLE, pop: EVER,
      rq: "Are age gaps bigger in same-sex couples?",
      hyp: "Same-sex couples will have larger age gaps than different-sex couples." },
    { id: "gap-married", y: "age_gap", grp: MARITAL, pop: PARTNERED,
      rq: "Do married couples have smaller age gaps than unmarried couples?",
      hyp: "Married respondents will have smaller age gaps with their partner than respondents in unmarried relationships." },
    { id: "age-app", y: "age", grp: APP, pop: "People who met someone new for dating, romance, or sex in the past year.",
      rq: "Are dating app users younger than other people who are dating?",
      hyp: "People who used a dating app in the past year will be younger than people who met someone new without an app." },
    { id: "years-howmet", y: "rel_years", grp: HOW, pop: PARTNERED,
      rq: "Have couples who met online been together for less time?",
      hyp: "Couples who met online will have been together for less time than couples who met offline.",
      note: "Online dating only became common recently, so most couples who met online simply haven't had as many years together yet. A t-test shows that a difference exists; it doesn't tell you why." }
  ];
  var GROUP_FOR = {};
  COMPS.forEach(function (c) {
    if (!(c.grp.A.lab < c.grp.B.lab)) console.warn("group order not alphabetical", c.id);
    var y = COL[c.y], g = COL[c.grp.g], a = [], b = [];
    for (var i = 0; i < y.length; i++) {
      var yi = y[i], gi = g[i];
      if (yi !== yi || gi !== gi) continue;
      if (gi === c.grp.A.code) a.push(yi); else if (gi === c.grp.B.code) b.push(yi);
    }
    GROUP_FOR[c.id] = { a: a, b: b };
  });
  var BY_ID = {}; COMPS.forEach(function (c) { BY_ID[c.id] = c; });

  /* =========================================================
     Helpers
     ========================================================= */
  function $(id) { return document.getElementById(id); }
  function el(tag, attrs, kids) {
    var n = document.createElement(tag);
    if (attrs) Object.keys(attrs).forEach(function (k) {
      if (k === "text") n.textContent = attrs[k];
      else if (k === "class") n.className = attrs[k];
      else n.setAttribute(k, attrs[k]);
    });
    (kids || []).forEach(function (c) { if (c != null) n.appendChild(typeof c === "string" ? document.createTextNode(c) : c); });
    return n;
  }
  function svgEl(tag, attrs) {
    var n = document.createElementNS("http://www.w3.org/2000/svg", tag);
    Object.keys(attrs || {}).forEach(function (k) { n.setAttribute(k, attrs[k]); });
    return n;
  }
  function cap(s) { return s.charAt(0).toUpperCase() + s.slice(1); }
  function fmtInt(n) { return Math.round(n).toLocaleString("en-US"); }
  function r2(x) {
    var s = x.toFixed(2);
    if (Math.abs(x) > 0 && Math.abs(x) < 0.01) s = x.toPrecision(2);
    if (s === "-0.00") s = "0.00";
    return s;
  }
  function sig3(x) { return String(parseFloat(x.toPrecision(3))); }
  function shuffle(a) {
    for (var i = a.length - 1; i > 0; i--) { var j = Math.floor(Math.random() * (i + 1)); var t = a[i]; a[i] = a[j]; a[j] = t; }
    return a;
  }
  function sampleOf(arr, k) {
    if (k >= arr.length) return arr.slice();
    var c = arr.slice();
    for (var i = 0; i < k; i++) { var j = i + Math.floor(Math.random() * (c.length - i)); var t = c[i]; c[i] = c[j]; c[j] = t; }
    return c.slice(0, k);
  }
  function pick(a) { return a[Math.floor(Math.random() * a.length)]; }
  function quantile(sorted, p) {
    var h = (sorted.length - 1) * p, lo = Math.floor(h), hi = Math.ceil(h);
    return sorted[lo] + (sorted[hi] - sorted[lo]) * (h - lo);
  }
  function boxStats(values) {
    var s = values.slice().sort(function (x, y) { return x - y; });
    var q1 = quantile(s, 0.25), med = quantile(s, 0.5), q3 = quantile(s, 0.75), iqr = q3 - q1;
    var lof = q1 - 1.5 * iqr, hif = q3 + 1.5 * iqr, wlo = q1, whi = q3, out = [];
    for (var i = 0; i < s.length; i++) {
      var v = s[i];
      if (v < lof || v > hif) out.push(v);
      else { if (v < wlo) wlo = v; if (v > whi) whi = v; }
    }
    return { n: s.length, q1: q1, med: med, q3: q3, wlo: wlo, whi: whi, out: out, min: s[0], max: s[s.length - 1],
      mean: TT.mean(values), sd: Math.sqrt(TT.variance(values)) };
  }
  function niceStep(span, count) {
    var raw = span / Math.max(1, count), pow = Math.pow(10, Math.floor(Math.log10(raw))), f = raw / pow;
    var nf = f < 1.5 ? 1 : f < 3 ? 2 : f < 7 ? 5 : 10;
    return nf * pow;
  }
  function ticks(lo, hi, count) {
    var step = niceStep(hi - lo, count), t = [], start = Math.ceil(lo / step - 1e-9) * step;
    for (var v = start; v <= hi + step * 1e-9; v += step) t.push(Math.abs(v) < step * 1e-9 ? 0 : v);
    return { values: t, step: step };
  }
  function tickLabel(v, step) {
    var d = step >= 1 ? 0 : Math.min(4, Math.ceil(-Math.log10(step) - 1e-9));
    return v.toFixed(d);
  }
  function textWidth(s, fs) { return s.length * fs * 0.56; }

  /* class guide for p-values (workbook 3.2) */
  function verdict(p) {
    if (p < 0.01) return { key: "confident", pill: "strong", short: "Confident there's a difference (p < .01)" };
    if (p < 0.05) return { key: "fairly", pill: "strong", short: "Fairly confident there's a difference (p < .05)" };
    if (p < 0.10) return { key: "marginal", pill: "marginal", short: "Marginally significant (p < .10)" };
    return { key: "none", pill: "none", short: "Not enough evidence of a difference" };
  }
  function pText(res) { // p as a reader would say it
    var pp = TT.printedPieces(res).p;
    if (res.p < 0.001) return "less than 0.001";
    return pp.replace(/^0\./, "0.");
  }
  function pFreq(p) {
    if (p < 1e-6) return "fewer than 1 in a million";
    if (p < 0.001) return "fewer than 1 in 1,000";
    return "about " + pctStr(p);
  }
  function pctStr(p) {
    if (p < 0.001) return "less than 0.1%";
    var pct = p * 100; return String(parseFloat(pct.toPrecision(pct < 10 ? 2 : 3))) + "%";
  }

  function conclusionSentence(c, res) {
    var g = c.grp, o = OUT[c.y], v = verdict(res.p);
    var groups = g.A.short + " and " + g.B.short;
    if (v.key === "confident") return "Because p is below .01, we're confident that " + groups + " differ in their average " + o.noun + ".";
    if (v.key === "fairly") return "Because p is below .05, we're fairly confident that " + groups + " differ in their average " + o.noun + ".";
    if (v.key === "marginal") return "Because p is between .05 and .10, this is marginally significant. It hints at a difference worth more research, but we don't have enough evidence to say " + groups + " differ in their average " + o.noun + ".";
    return "Because p is above .10, we don't have enough evidence to say that " + groups + " differ in their average " + o.noun + ".";
  }
  // "We're 95 percent confident that, on average, X ... somewhere between L and H ... than Y."
  function ciStatement(c, res) {
    var g = c.grp, o = OUT[c.y], k = o.ci, body;
    if (res.lo > 0) body = k.pre + " somewhere between " + r2(res.lo) + " and " + r2(res.hi) + " " + o.unit + " " + k.up + k.post;
    else if (res.hi < 0) body = k.pre + " somewhere between " + r2(-res.hi) + " and " + r2(-res.lo) + " " + o.unit + " " + k.down + k.post;
    else body = k.pre + " somewhere between " + r2(-res.lo) + " " + o.unit + " " + k.down + " and " + r2(res.hi) + " " + o.unit + " " + k.up + k.post;
    return "We're 95 percent confident that, on average, " + g.A.short + " " + body + " than " + g.B.short + ".";
  }
  function ciSentence(c, res) {
    return ciStatement(c, res) + (res.lo > 0 || res.hi < 0
      ? " Since this range doesn't include zero, it suggests a real difference between the groups."
      : " Since this range includes zero, the true difference could be zero.");
  }
  function directionSentence(c, res) {
    var g = c.grp, o = OUT[c.y];
    return cap(g.A.short) + " " + (res.diff >= 0 ? o.more : o.less) + " than " + g.B.short + ".";
  }
  function magnitudeSentence(c, res) {
    var g = c.grp, o = OUT[c.y];
    return cap(g.A.short) + " " + o.mag(r2(Math.abs(res.diff)), res.diff >= 0) + ".";
  }
  function pShort(res) { return res.p < 0.001 ? "p < 0.001" : "p = " + TT.formatPval(res.p, 4); }
  function significanceSentence(c, res) {
    var g = c.grp, o = OUT[c.y];
    if (res.p < 0.05) return "We're fairly confident that " + g.A.short + " " + (res.diff >= 0 ? o.more : o.less) + " than " + g.B.short + " (" + pShort(res) + ").";
    return "We do not have enough evidence to say that " + g.A.short + " and " + g.B.short + " differ in their average " + o.noun + " (" + pShort(res) + ").";
  }
  function meansSentence(c, res) {
    var g = c.grp, o = OUT[c.y];
    return cap(g.A.who) + " " + o.verb(r2(res.mx)) + ", compared with " + r2(res.my) + " for " + g.B.who + ".";
  }
  function pSentence(c, res) {
    var g = c.grp;
    return "The p-value is " + (res.p < 0.001 ? "less than 0.001" : pText(res)) + ". If there were really no difference between " + g.A.short + " and " + g.B.short +
      " in the population, a gap this big or bigger would show up in " + pFreq(res.p) + " of random samples. " + conclusionSentence(c, res);
  }
  function sizeOf(res) {
    var ratio = Math.abs(res.diff) / Math.sqrt((res.sdx * res.sdx + res.sdy * res.sdy) / 2);
    return ratio < 0.2 ? "small" : ratio < 0.5 ? "modest" : ratio < 0.8 ? "moderate" : "large";
  }
  function practicalSentence(c, res) {
    var o = OUT[c.y], size = sizeOf(res);
    return "For scale: the averages differ by " + r2(Math.abs(res.diff)) + " " + o.unit + ", while individual answers within each group typically sit about " +
      r2(res.sdx) + " and " + r2(res.sdy) + " " + o.unit + " from their group's mean (the standard deviations). So the gap is " + size + " compared with the spread inside each group.";
  }
  function sampleSentence(c, na, nb, full) {
    var g = c.grp;
    if (full) return "All HCMST respondents who answered both questions: " + fmtInt(na) + " " + g.A.who + " and " + fmtInt(nb) + " " + g.B.who + ".";
    return "A random sample of " + na + " " + g.A.who + " and " + nb + " " + g.B.who + ", drawn from HCMST.";
  }
  function commandLine(c, full) {
    return "t.test(" + c.y + " ~ " + c.grp.g + ", data = " + (full ? "hcmst" : "my_sample") + ")";
  }
  function consoleWidth(pre) {
    var w = pre.clientWidth; if (!w) return 80;
    var cs = getComputedStyle(pre), probe = document.createElement("span");
    probe.textContent = "0000000000"; probe.style.cssText = "position:absolute;visibility:hidden;white-space:pre";
    pre.appendChild(probe); var cw = probe.getBoundingClientRect().width / 10; pre.removeChild(probe);
    var inner = w - parseFloat(cs.paddingLeft) - parseFloat(cs.paddingRight);
    return Math.max(20, Math.min(80, Math.floor(inner / cw)));
  }
  var consoles = [];
  function refreshConsoles() { consoles.forEach(function (pre) { if (pre._args && pre.clientWidth) renderConsole.apply(null, [pre].concat(pre._args)); }); }
  function makeHot(node, key) {
    node.classList.add("hot"); node.setAttribute("role", "button"); node.setAttribute("tabindex", "0");
    node.setAttribute("data-k", key); node.setAttribute("aria-pressed", "false");
    return node;
  }
  function renderConsole(pre, c, res, full, interactive) {
    pre._args = [c, res, full, interactive];
    if (consoles.indexOf(pre) < 0) consoles.push(pre);
    var width = consoleWidth(pre);
    pre.textContent = "";
    var cmdB = el("b", { text: commandLine(c, full) });
    pre.appendChild(el("span", { class: "cmd" }, ["> ", interactive ? makeHot(cmdB, "cmd") : cmdB]));
    pre.appendChild(document.createTextNode("\n"));
    var parts = TT.printWelchParts(res, c.y, c.grp.g, c.grp.A.lab, c.grp.B.lab, width);
    if (!interactive) { pre.appendChild(document.createTextNode(parts.map(function (x) { return x.t; }).join(""))); return; }
    parts.forEach(function (x) {
      pre.appendChild(x.k ? makeHot(el("span", { text: x.t }), x.k) : document.createTextNode(x.t));
    });
    if (pre._after) pre._after();
  }
  function bindHot(pre, onPick) {
    pre.addEventListener("click", function (e) { var h = e.target.closest(".hot"); if (h) onPick(h.getAttribute("data-k")); });
    pre.addEventListener("keydown", function (e) {
      if ((e.key === "Enter" || e.key === " ") && e.target.classList && e.target.classList.contains("hot")) { e.preventDefault(); onPick(e.target.getAttribute("data-k")); }
    });
  }

  /* a random practice item: comparison + sample + result */
  var N_CHOICES = [15, 20, 25, 30, 40, 50, 60, 80, 100, 150, 250, Infinity, Infinity];
  function makeItem(avoidId) {
    for (var tries = 0; tries < 50; tries++) {
      var c = pick(COMPS);
      if (c.id === avoidId && tries < 10) continue;
      var n = pick(N_CHOICES), G = GROUP_FOR[c.id];
      var full = !isFinite(n);
      var a = full ? G.a : sampleOf(G.a, n), b = full ? G.b : sampleOf(G.b, n);
      if (b.length >= G.b.length && a.length >= G.a.length) full = true;
      var res = TT.welch(a, b);
      if (!(res.se > 0) || !isFinite(res.t)) continue;
      return { c: c, res: res, full: full, na: a.length, nb: b.length, a: a, b: b };
    }
    return null;
  }

  /* =========================================================
     Charts
     ========================================================= */
  function drawBoxes(host, groups, axisTitle, domainHint, describedBy) {
    var W = host.clientWidth; if (!W) return;
    host.textContent = "";
    var fs = parseFloat(getComputedStyle(host).fontSize) || 16;
    var stats = groups.map(function (g) { return boxStats(g.values); });
    var lo = Math.min.apply(null, stats.map(function (s) { return s.min; }));
    var hi = Math.max.apply(null, stats.map(function (s) { return s.max; }));
    if (domainHint) { lo = Math.min(lo, domainHint[0]); hi = Math.max(hi, domainHint[1]); }
    if (hi === lo) { hi += 1; lo -= 1; }
    var pad = (hi - lo) * 0.03; lo -= pad; hi += pad;
    var mL = fs * 0.4, mR = fs * 0.9;
    var labelH = fs * 1.6, boxH = Math.max(26, fs * 1.7), rowGap = fs * 1.1;
    var axisH = fs * 1.8, titleH = 0;
    var H = groups.length * (labelH + boxH) + (groups.length - 1) * rowGap + fs * 0.6 + axisH + titleH;
    var x = function (v) { return mL + (v - lo) / (hi - lo) * (W - mL - mR); };
    var svg = svgEl("svg", { width: W, height: H, viewBox: "0 0 " + W + " " + H, role: "group", "aria-label": "Box plots of " + axisTitle });
    if (describedBy) svg.setAttribute("aria-describedby", describedBy);
    var plotBottom = groups.length * (labelH + boxH) + (groups.length - 1) * rowGap + fs * 0.3;
    var tk = ticks(lo, hi, Math.max(3, Math.floor((W - mL - mR) / (fs * 4.2))));
    var gridG = svgEl("g", { "aria-hidden": "true" });
    tk.values.forEach(function (v) {
      if (v < lo || v > hi) return;
      gridG.appendChild(svgEl("line", { x1: x(v), x2: x(v), y1: 0, y2: plotBottom, class: "grid", "stroke-width": 1 }));
      var tlab = tickLabel(v, tk.step), tw = textWidth(tlab, fs * 0.85), tx = x(v), ta = "middle";
      if (tx + tw / 2 > W) { tx = W; ta = "end"; } else if (tx - tw / 2 < 0) { tx = 0; ta = "start"; }
      var tm = svgEl("line", { x1: x(v), x2: x(v), y1: plotBottom, y2: plotBottom + fs * 0.35, "stroke-width": 1 }); tm.style.stroke = "var(--muted)"; gridG.appendChild(tm);
      var t = svgEl("text", { x: tx, y: plotBottom + fs * 1.45, "text-anchor": ta, "font-size": fs * 0.85 });
      t.textContent = tlab; t.setAttribute("class", "tick-label");
      t.style.fill = "var(--muted)";
      gridG.appendChild(t);
    });
    var base = svgEl("line", { x1: mL, x2: W - mR, y1: plotBottom, y2: plotBottom, "stroke-width": 1 }); base.style.stroke = "var(--muted)"; gridG.appendChild(base);
    svg.appendChild(gridG);

    var tip = el("div", { class: "tooltip", hidden: "" });
    groups.forEach(function (g, i) {
      var s = stats[i], top = i * (labelH + boxH + rowGap);
      var yc = top + labelH + boxH / 2, color = g.color;
      var row = svgEl("g", { class: "box-row", tabindex: "0", role: "img",
        "aria-label": g.label + ", " + fmtInt(s.n) + " people: middle half from " + r2(s.q1) + " to " + r2(s.q3) + ", median " + r2(s.med) + ", mean " + r2(s.mean) + "." });
      row.appendChild(svgEl("rect", { class: "row-hit", x: 0, y: top, width: W, height: labelH + boxH, rx: 6, fill: "transparent" }));
      // label with swatch (legend + direct label)
      row.appendChild(svgEl("rect", { x: mL, y: top + fs * 0.35, width: fs * 0.8, height: fs * 0.8, rx: 2, fill: color }));
      var avail = W - mL - fs * 1.15, nTxt = "   n = " + fmtInt(s.n), lfs = fs * 0.95;
      var fitsAll = textWidth(g.label + nTxt, lfs) <= avail;
      if (!fitsAll && textWidth(g.label, lfs) > avail) lfs = Math.max(10, avail / (g.label.length * 0.56));
      var lt = svgEl("text", { x: mL + fs * 1.15, y: top + fs * 1.1, "font-size": lfs, "font-weight": 700 });
      lt.textContent = g.label; lt.style.fill = "var(--ink)";
      if (fitsAll) { var ln = svgEl("tspan", { "font-weight": 400 }); ln.textContent = nTxt; ln.style.fill = "var(--muted)"; lt.appendChild(ln); }
      row.appendChild(lt);
      // whiskers
      row.appendChild(svgEl("line", { x1: x(s.wlo), x2: x(s.q1), y1: yc, y2: yc, stroke: color, "stroke-width": 2 }));
      row.appendChild(svgEl("line", { x1: x(s.q3), x2: x(s.whi), y1: yc, y2: yc, stroke: color, "stroke-width": 2 }));
      [s.wlo, s.whi].forEach(function (w) { row.appendChild(svgEl("line", { x1: x(w), x2: x(w), y1: yc - boxH * 0.25, y2: yc + boxH * 0.25, stroke: color, "stroke-width": 2 })); });
      // box
      var bw = Math.max(2, x(s.q3) - x(s.q1));
      row.appendChild(svgEl("rect", { x: x(s.q1), y: yc - boxH / 2, width: bw, height: boxH, rx: 3, fill: color, "fill-opacity": 0.22, stroke: color, "stroke-width": 2 }));
      // outliers (dedupe identical values to keep the DOM small)
      var seen = {};
      s.out.forEach(function (v) {
        var k = Math.round(x(v) * 2); if (seen[k]) return; seen[k] = 1;
        row.appendChild(svgEl("circle", { cx: x(v), cy: yc, r: 3.2, fill: "none", stroke: color, "stroke-width": 1.5, "stroke-opacity": 0.7 }));
      });
      // median
      var mline = svgEl("line", { x1: x(s.med), x2: x(s.med), y1: yc - boxH / 2, y2: yc + boxH / 2, "stroke-width": 3, "stroke-linecap": "round" });
      mline.style.stroke = "var(--ink)"; row.appendChild(mline);
      // mean diamond
      var d = Math.max(5, fs * 0.42), mx = x(s.mean);
      row.appendChild(svgEl("path", { class: "mean-mark", d: "M" + mx + " " + (yc - d) + " L" + (mx + d) + " " + yc + " L" + mx + " " + (yc + d) + " L" + (mx - d) + " " + yc + " Z" }));

      function showTip(px, py) {
        tip.textContent = "";
        var head = el("div", null, [el("span", { class: "key", style: "background:" + color }), el("span", { text: g.label })]);
        tip.appendChild(head);
        [["Mean", r2(s.mean)], ["Median", r2(s.med)], ["Middle half", r2(s.q1) + " to " + r2(s.q3)], ["People", fmtInt(s.n)]].forEach(function (r) {
          tip.appendChild(el("div", null, [el("b", { text: r[1] }), " " + r[0].toLowerCase()]));
        });
        tip.hidden = false;
        var tw = tip.offsetWidth, left = Math.min(Math.max(0, px + 12), W - tw);
        tip.style.left = left + "px"; tip.style.top = Math.max(0, py - tip.offsetHeight - 10) + "px";
      }
      row.addEventListener("pointermove", function (e) { var r = host.getBoundingClientRect(); showTip(e.clientX - r.left, e.clientY - r.top); });
      row.addEventListener("pointerleave", function () { tip.hidden = true; });
      row.addEventListener("focus", function () { showTip(x(s.q3), top + labelH); });
      row.addEventListener("blur", function () { tip.hidden = true; });
      svg.appendChild(row);
    });
    host._domain = [lo, hi];
    host.appendChild(svg);
    host.appendChild(el("p", { class: "axis-title", text: axisTitle }));
    host.appendChild(tip);
    return stats;
  }

  // Difference and its 95% CI, drawn with the same units-per-pixel as the box plots
  // (an axis as wide as the outcome's own range, centered on 0), so the size of the
  // gap reads honestly against the scale of the outcome.
  function drawCI(host, res, span, axisTitle) {
    var W = host.clientWidth; if (!W) return;
    host.textContent = "";
    var fs = parseFloat(getComputedStyle(host).fontSize) || 16;
    var hi = span / 4, ext = Math.max(Math.abs(res.lo), Math.abs(res.hi));
    if (ext > hi * 0.95) hi = ext * 1.08;
    var lo = -hi, mL = fs * 0.5, mR = fs * 0.5;
    var x = function (v) { return mL + (v - lo) / (hi - lo) * (W - mL - mR); };
    var yDiff = fs * 1.15, yLine = fs * 2.3, yEnds = fs * 3.65, yAxis = fs * 4.35, yTick = yAxis + fs * 1.45, H = yTick + fs * 0.55;
    var svg = svgEl("svg", { width: W, height: H, viewBox: "0 0 " + W + " " + H, role: "img",
      "aria-label": "Difference in means " + r2(res.diff) + ", 95% confidence interval from " + r2(res.lo) + " to " + r2(res.hi) + ", drawn at twice the scale of the box plots." });
    host.appendChild(svg); // in the document early so labels can be measured
    function text(s, xp, y, anchor, weight, color) {
      var t = svgEl("text", { x: xp, y: y, "text-anchor": anchor, "font-size": fs * 0.9, "font-weight": weight || 400 });
      t.textContent = s; t.style.fill = color || "var(--ink-2)"; svg.appendChild(t); return t;
    }
    function label(s, xp, y, weight) {
      var w = textWidth(s, fs * 0.9), anchor = "middle";
      if (xp - w / 2 < 0) { anchor = "start"; xp = 0; } else if (xp + w / 2 > W) { anchor = "end"; xp = W; }
      return text(s, xp, y, anchor, weight);
    }
    // axis with ticks
    var axis = svgEl("line", { x1: mL, x2: W - mR, y1: yAxis, y2: yAxis, "stroke-width": 1 }); axis.style.stroke = "var(--muted)"; svg.appendChild(axis);
    var tk = ticks(lo, hi, Math.max(3, Math.floor((W - mL - mR) / (fs * 4.2))));
    tk.values.forEach(function (v) {
      if (v < lo || v > hi) return;
      var g = svgEl("line", { x1: x(v), x2: x(v), y1: fs * 0.2, y2: yAxis, "stroke-width": 1 }); g.style.stroke = "var(--hair)"; svg.appendChild(g);
      var tm = svgEl("line", { x1: x(v), x2: x(v), y1: yAxis, y2: yAxis + fs * 0.35, "stroke-width": 1 }); tm.style.stroke = "var(--muted)"; svg.appendChild(tm);
      var tlab = tickLabel(v, tk.step), tw = textWidth(tlab, fs * 0.85), tx = x(v), ta = "middle";
      if (tx + tw / 2 > W) { tx = W; ta = "end"; } else if (tx - tw / 2 < 0) { tx = 0; ta = "start"; }
      var tl = text(tlab, tx, yTick, ta, v === 0 ? 700 : 400, v === 0 ? "var(--ink-2)" : "var(--muted)");
      tl.setAttribute("font-size", fs * 0.85);
    });
    var z = svgEl("line", { x1: x(0), x2: x(0), y1: fs * 0.2, y2: yAxis, "stroke-width": 2 }); z.style.stroke = "var(--ink-2)"; svg.appendChild(z);
    var seg = svgEl("line", { x1: x(res.lo), x2: x(res.hi), y1: yLine, y2: yLine, "stroke-width": 4, "stroke-linecap": "round" });
    seg.style.stroke = "var(--accent)"; svg.appendChild(seg);
    [res.lo, res.hi].forEach(function (v) {
      var c = svgEl("line", { x1: x(v), x2: x(v), y1: yLine - fs * 0.45, y2: yLine + fs * 0.45, "stroke-width": 3, "stroke-linecap": "round" });
      c.style.stroke = "var(--accent)"; svg.appendChild(c);
    });
    var dot = svgEl("circle", { cx: x(res.diff), cy: yLine, r: Math.max(5, fs * 0.38), "stroke-width": 2 });
    dot.style.fill = "var(--ink)"; dot.style.stroke = "var(--surface)"; svg.appendChild(dot);
    // labels sit on the difference's side of the zero line; a surface-colored halo keeps them legible if they must cross it
    function sideLabel(s, y, weight) {
      var w = textWidth(s, fs * 0.9), xc = x(res.diff), left;
      if (res.lo > 0) left = Math.max(xc - w / 2, x(0) + fs * 0.4);
      else if (res.hi < 0) left = Math.min(xc + w / 2, x(0) - fs * 0.4) - w;
      else left = xc - w / 2;
      left = Math.max(0, Math.min(W - w, left));
      var t = text(s, left, y, "start", weight);
      var len = t.getComputedTextLength ? t.getComputedTextLength() : 0;
      if (len > W) { t.setAttribute("font-size", fs * 0.9 * W / len * 0.97); len = t.getComputedTextLength(); }
      if (len && left + len > W) t.setAttribute("x", Math.max(0, W - len));
      t.style.paintOrder = "stroke"; t.style.stroke = "var(--surface)"; t.style.strokeWidth = "5px"; t.style.strokeLinejoin = "round";
      return t;
    }
    sideLabel("difference " + r2(res.diff), yDiff, 700);
    sideLabel("95% CI " + r2(res.lo) + " to " + r2(res.hi), yEnds);
    if (axisTitle) host.appendChild(el("p", { class: "axis-title", text: axisTitle }));
  }

  /* =========================================================
     Explore
     ========================================================= */
  var ex = { c: COMPS[0], a: null, b: null, full: true, res: null, stats: null, selKey: null, cell: null };

  /* full-survey direction and significance, used to flag samples that flip the pattern */
  var FULL = {};
  COMPS.forEach(function (c) { var G = GROUP_FOR[c.id], r = TT.welch(G.a, G.b); FULL[c.id] = { aHigher: r.diff > 0, sig: r.p < 0.05 }; });

  /* "So what?" sentences: what the result means for the question people actually care about */
  var SOWHAT = {
    "quality-howmet": {
      yes: function (x) {
        return x.size === "small"
          ? "How a couple met says little about how happy they are. The gap is real, but it's a sliver of a point on a 5-point scale, so worries that dating apps produce unhappier couples aren't backed by a meaningful difference."
          : "How a couple met is linked to how they rate their relationship, which is worth explaining: online couples may be newer, younger, or different in other ways.";
      },
      no: function (x) { return "This " + x.where + " gives no clear sign that meeting online goes with better or worse relationships, so worries that dating apps produce unhappier couples don't find support here."; }
    },
    "quality-married": {
      yes: function () { return "Married people rate their relationships higher, but that doesn't mean marriage makes relationships better: happier couples may be more likely to marry, and unhappy marriages may already have ended."; },
      no: function (x) { return "This " + x.where + " doesn't give clear evidence that married and unmarried partners rate their relationships differently."; }
    },
    "quality-living": {
      yes: function () { return "People who live with their partner rate their relationships higher. Living apart often goes with newer or less settled relationships, so the gap may say more about the stage of the relationship than about sharing an address."; },
      no: function (x) { return "This " + x.where + " doesn't give clear evidence that living together goes with higher or lower relationship quality."; }
    },
    "quality-couple": {
      yes: function (x) { return "In this " + x.where + ", same-sex and different-sex couples rate their relationships differently" + (x.size === "small" ? ", though the gap is small next to the spread within each group." : "."); },
      no: function (x) { return "This " + x.where + " gives no clear evidence that same-sex and different-sex couples rate their relationships differently, so it offers no support for the idea that same-sex relationships are lower in quality."; }
    },
    "quality-friends": {
      yes: function (x) {
        return x.aHigher
          ? "Couples introduced by friends rate their relationships higher, which fits the idea that friends screen for good matches and that a shared social circle supports a couple."
          : "Couples introduced by friends rated their relationships lower here, which goes against the idea that friends make the best matchmakers.";
      },
      no: function (x) { return "This " + x.where + " gives no clear evidence that being introduced by friends goes with a better relationship, so the idea that friends make the best matchmakers isn't supported here."; }
    },
    "agemet-howmet": {
      yes: function () { return "People who met online were older when they met. That fits the idea that online dating matters most once school and college social circles have thinned out and fewer potential partners are close at hand."; },
      no: function (x) { return "This " + x.where + " doesn't give clear evidence that people who meet online are a different age when they meet" + (x.full ? "." : ". The full survey, with far more people, does."); }
    },
    "couple-howmet": {
      yes: function () { return "Couples who met online got together faster. People on a dating site are usually there looking for a partner, while people who meet as classmates or coworkers may know each other for years first."; },
      no: function (x) { return "This " + x.where + " doesn't give clear evidence that meeting online changes how fast people become a couple. These times are very skewed, which makes a gap harder to detect in a small sample."; }
    },
    "movein-howmet": {
      yes: function () { return "Couples who met online moved in together sooner after they started dating, which may reflect people who went online already looking for something serious."; },
      no: function (x) { return "This " + x.where + " doesn't give clear evidence that meeting online changes how soon couples move in together."; }
    },
    "gap-couple": {
      yes: function () { return "Same-sex couples tend to have bigger age gaps. One explanation researchers offer is that a smaller pool of potential partners makes people more open to partners further from their own age."; },
      no: function (x) { return "This " + x.where + " doesn't give clear evidence that age gaps differ between same-sex and different-sex couples."; }
    },
    "gap-married": {
      yes: function () { return "Married couples tend to be closer in age. Couples with big age gaps may be less likely to marry, or marriages with big gaps may be less likely to last; this test doesn't show which."; },
      no: function (x) { return "This " + x.where + " doesn't give clear evidence that age gaps differ between married and unmarried couples."; }
    },
    "age-app": {
      yes: function () { return "Dating app users are younger than other people who are dating, so an outreach program that relies on apps would reach younger daters better than older ones."; },
      no: function (x) { return "This " + x.where + " doesn't give clear evidence that app users differ in age from other people who are dating" + (x.full ? "." : ". The whole survey has only 98 app users, so small samples have little power to detect a gap."); }
    },
    "years-howmet": {
      yes: function () { return "Couples who met online have been together for less time, but that's mostly history: online dating took off recently, so online couples are newer. A real difference between groups isn't automatically a cause."; },
      no: function (x) { return "This " + x.where + " doesn't give clear evidence of a difference in how long couples have been together."; }
    }
  };
  function soWhat(c, res, full) {
    var x = { full: full, where: full ? "survey" : "sample", aHigher: res.diff > 0, size: sizeOf(res) }, S = SOWHAT[c.id], F = FULL[c.id];
    if (res.p >= 0.05) return S.no(x);
    if (!full && F.sig && x.aHigher !== F.aHigher)
      return "The gap in this sample runs the opposite way from the full survey, a reminder that small samples bounce around, even when p is below .05.";
    return S.yes(x);
  }

  /* ---------- assumptions (workbook 3.2, step 5) ---------- */
  function skewness(v) {
    var m = TT.mean(v), s2 = 0, s3 = 0;
    for (var i = 0; i < v.length; i++) { var d = v[i] - m; s2 += d * d; s3 += d * d * d; }
    s2 /= v.length; s3 /= v.length;
    return s2 > 0 ? s3 / Math.pow(s2, 1.5) : 0;
  }
  function skewWords(k) {
    var a = Math.abs(k);
    if (a < 0.5) return "roughly symmetric";
    return (a > 1 ? "strongly" : "somewhat") + (k > 0 ? " skewed right, with a tail of high values" : " skewed left, with a tail of low values");
  }
  function assumptions(c, a, b, full) {
    var o = OUT[c.y], g = c.grp, out = [];
    var na = a.length, nb = b.length, sa = Math.sqrt(TT.variance(a)), sb = Math.sqrt(TT.variance(b));
    var ka = skewness(a), kb = skewness(b), nmin = Math.min(na, nb), kmax = Math.max(Math.abs(ka), Math.abs(kb));
    out.push({ s: "pass", label: "Looks fine", title: "Independence of observations",
      body: "Each respondent is a different person, and each person is in only one group (" + g.A.lab + " or " + g.B.lab + "). HCMST asked people about their partner but didn't survey the partners themselves, so no couple shows up twice." });
    var shape = g.A.lab + " answers are " + skewWords(ka) + "; " + g.B.lab + " answers are " + skewWords(kb);
    if (nmin >= 30)
      out.push({ s: "pass", label: "Looks fine", title: "Normality",
        body: "The answers themselves aren't perfectly normal (" + shape + "). But both groups have at least 30 people (n = " + fmtInt(na) + " and " + fmtInt(nb) + "), so the Central Limit Theorem makes the sample means close to normal, which is what the t-test needs." });
    else if (kmax > 1)
      out.push({ s: "fix", label: "Concern", title: "Normality",
        body: "With only " + nmin + " people in the smaller group and strongly skewed answers (" + shape + "), the sample means may not be close to normal. The p-value could be off in either direction, so treat it with caution." });
    else
      out.push({ s: "tip", label: "Worth noting", title: "Normality",
        body: "With fewer than 30 people in a group, the t-test leans on the answers being roughly normal. Here " + shape + ", so this is a modest worry." });
    var ratio = Math.max(sa, sb) / Math.min(sa, sb);
    if (!(ratio < 1.5))
      out.push({ s: "pass", label: "Handled by Welch", title: "Equal variances",
        body: "The groups are spread out differently (SD " + r2(sa) + " for " + g.A.lab + " vs. " + r2(sb) + " for " + g.B.lab + "). A classic t-test assumes equal spreads, but R's default Welch test doesn't, which is why the printout says “Welch.”" });
    else
      out.push({ s: "pass", label: "Looks fine", title: "Equal variances",
        body: "The standard deviations are similar (" + r2(sa) + " for " + g.A.lab + " and " + r2(sb) + " for " + g.B.lab + "), so the groups are about equally spread out. R's Welch test doesn't require this anyway." });
    out.push({ s: "tip", label: "Worth noting", title: "Random sampling",
      body: "HCMST recruited people at random into a national survey panel, which is much closer to a simple random sample than a convenience sample. But it deliberately includes extra lesbian, gay, and bisexual respondents, and this page doesn't use the survey weights, so these averages describe HCMST respondents more than all US adults." +
        (full ? "" : " The " + fmtInt(na) + " and " + fmtInt(nb) + " people here are then drawn at random from HCMST.") });
    if (c.y === "rel_quality")
      out.push({ s: "tip", label: "Worth noting", title: "Kind of variables",
        body: "The outcome is a 1-to-5 rating, an ordinal variable that we're treating as quantitative (like conservatism in Activity 3.2A). That's common practice, but a one-point step may not mean the same thing everywhere on the scale." });
    else
      out.push({ s: "pass", label: "Looks fine", title: "Kind of variables",
        body: o.label + " is quantitative (measured in " + o.unit + "), and " + g.desc + " splits people into exactly two groups, so a two-sample t-test fits." });
    var concern = out.some(function (x) { return x.s === "fix"; });
    var bottom = concern
      ? "Bottom line: at least one assumption is shaky here, so treat the p-value with caution. A violation like this can make a Type I or Type II error more likely than the p-value suggests."
      : "Bottom line: no serious violations. The main caveat is generalizing: with an oversample and no weights, these numbers describe HCMST respondents better than all US adults.";
    return { items: out, bottom: bottom };
  }

  /* ---------- plain-English notes for each part of the printout ---------- */
  function partExplain(key) {
    var c = ex.c, res = ex.res, g = c.grp, o = OUT[c.y], P = TT.printedPieces(res), v = verdict(res.p);
    var na = ex.a.length, nb = ex.b.length, d = res.diff;
    switch (key) {
      case "cmd": return { title: "> " + commandLine(c, ex.full), paras: [
        "This is the R code that made the printout. It compares " + c.y + " (the outcome, before the ~) across the groups in " + c.grp.g + " (after the ~), using " +
        (ex.full ? "the data set hcmst, which holds everyone in the survey." : "my_sample, a random sample of " + na + " and " + nb + " people from the survey.")] };
      case "method": return { title: "Welch Two Sample t-test", paras: [
        "R ran a two-sample t-test, which compares the averages of two separate groups: " + g.A.who + " and " + g.B.who + ".",
        "“Welch” is R's default version. It doesn't assume the two groups are equally spread out."] };
      case "data": return { title: "data:  " + c.y + " by " + c.grp.g, paras: [
        "The outcome is " + o.label.toLowerCase() + " (" + c.y + "), split up by " + g.desc + " (" + c.grp.g + "). Read “by” as “broken down by.”"] };
      case "t": return { title: "t = " + P.t, paras: [
        "The t statistic is the difference between the two averages (" + sig3(d) + " " + o.unit + ") divided by its standard error (" + sig3(res.se) + "), which is how much a difference like this would bounce around from sample to sample. Here the gap is about " + Math.abs(res.t).toFixed(1) + " standard errors away from 0.",
        "The farther t is from 0, in either direction, the stronger the evidence of a real difference. It's " + (res.t >= 0 ? "positive" : "negative") + " because " + g.A.lab + " (listed first) has the " + (res.t >= 0 ? "higher" : "lower") + " average."] };
      case "df": return { title: "df = " + P.df, paras: [
        "Degrees of freedom. R uses this number to pick the right t distribution when it calculates the p-value. Welch's version adjusts it for unequal spreads and group sizes, so it usually isn't a whole number.",
        "You rarely need to interpret it. Bigger samples give bigger df" + (res.df > 100 ? ", and with df this large the t distribution is almost the same as the normal curve." : ".")] };
      case "p": return { title: "p-value " + (P.p.charAt(0) === "<" ? P.p : "= " + P.p), pill: v, paras: [pSentence(c, res)].concat(
        P.p.charAt(0) === "<" ? ["R prints “< 2.2e-16” when the p-value is too small to show. It's essentially zero."] :
        /e/.test(P.p) ? ["R writes very small numbers in scientific notation: " + P.p + " means " + res.p.toFixed(Math.min(12, 2 - Math.floor(Math.log10(res.p)))).replace(/0+$/, "") + "."] : []) };
      case "alt": return { title: "alternative hypothesis", paras: [
        "The alternative hypothesis is that the true difference in average " + o.noun + " (" + g.A.lab + " minus " + g.B.lab + ") is not 0. The null hypothesis is that it is 0: no difference in the population.",
        "“Not equal to” makes this a two-sided test, so it looks for a difference in either direction. R lists the groups alphabetically, so " + g.A.lab + " comes first and is the one subtracted from. Older versions of R leave the group names out of this line."] };
      case "ci": return { title: "95 percent confidence interval: " + P.lo + " to " + P.hi, paras: [ciStatement(c, res)] };
      case "est": return { title: "sample estimates", paras: [
        "The average " + o.noun + " in each group in this " + (ex.full ? "survey" : "sample") + ". These two numbers are what the t-test compares."] };
      case "meanA": case "meanB": {
        var isA = key === "meanA", G = isA ? g.A : g.B, O2 = isA ? g.B : g.A, m = isA ? res.mx : res.my, gap = isA ? d : -d;
        return { title: "mean in group " + G.lab + ": " + (isA ? P.mA : P.mB), paras: [
          cap(G.who) + " " + o.verb(r2(m)) + " (n = " + fmtInt(isA ? na : nb) + "). That's " + r2(Math.abs(gap)) + " " + o.unit + " " + (gap >= 0 ? o.up : o.down) + " than " + O2.short + "."] };
      }
    }
    return { title: "", paras: [] };
  }
  function renderExplain(box, info) {
    box.textContent = "";
    box.appendChild(el("p", { class: "what", text: info.title }));
    if (info.pill) box.appendChild(el("p", null, [el("span", { class: "verdict " + info.pill.pill, text: info.pill.short })]));
    info.paras.forEach(function (t) { box.appendChild(el("p", { text: t })); });
  }
  function exMarkSel() {
    Array.prototype.forEach.call($("ex-out").querySelectorAll(".hot"), function (h) {
      var on = h.getAttribute("data-k") === ex.selKey;
      h.classList.toggle("sel", on); h.setAttribute("aria-pressed", on ? "true" : "false");
    });
  }
  function exExplain() { if (ex.selKey && ex.res && ex.res.se > 0) renderExplain($("ex-explain"), partExplain(ex.selKey)); }

  /* ---------- summary table with explanations ---------- */
  var COLS = [["group", "Group"], ["n", "n"], ["mean", "Mean"], ["sd", "SD"], ["q1", "Q1"], ["med", "Median"], ["q3", "Q3"]];
  var COL_NAME = { group: "Group", n: "n", mean: "Mean", sd: "SD", q1: "Q1", med: "Median", q3: "Q3" };
  function cellBtn(text, col, row, label) {
    var b = el("button", { type: "button", class: "cellbtn", "data-col": col, "data-row": String(row), "aria-pressed": "false" });
    if (label) b.setAttribute("aria-label", label);
    if (text) b.textContent = text;
    return b;
  }
  function exTable(sa, sb) {
    var g = ex.c.grp, tbl = $("ex-table"); tbl.textContent = "";
    var tr = el("tr");
    COLS.forEach(function (col) { var th = el("th", { scope: "col" }); th.appendChild(cellBtn(col[1], col[0], -1, "About " + col[1])); tr.appendChild(th); });
    var tbody = el("tbody");
    [[g.A, "var(--series-a)", sa], [g.B, "var(--series-b)", sb]].forEach(function (r, i) {
      var s = r[2], vals = { n: fmtInt(s.n), mean: r2(s.mean), sd: r2(s.sd), q1: r2(s.q1), med: r2(s.med), q3: r2(s.q3) };
      var row = el("tr"), th = el("th", { scope: "row" }), gb = cellBtn("", "group", i, "About the " + r[0].lab + " group");
      gb.appendChild(el("span", { class: "swatch", style: "background:" + r[1] })); gb.appendChild(document.createTextNode(r[0].lab));
      th.appendChild(gb); row.appendChild(th);
      ["n", "mean", "sd", "q1", "med", "q3"].forEach(function (k) {
        var td = el("td"); td.appendChild(cellBtn(vals[k], k, i, COL_NAME[k] + " for " + r[0].lab + ": " + vals[k])); row.appendChild(td);
      });
      tbody.appendChild(row);
    });
    tbl.appendChild(el("thead", null, [tr])); tbl.appendChild(tbody);
  }
  function cellExplain(col, row) {
    var c = ex.c, g = c.grp, o = OUT[c.y], u = o.unit;
    if (row < 0) {
      var H = {
        group: "The two groups being compared, labeled the way R labels them. R puts them in alphabetical order.",
        n: "n is the number of people in each group.",
        mean: "The mean is the average: add up everyone's answers and divide by n. It's the number the t-test compares, and the diamond in the box plot.",
        sd: "The standard deviation (SD) measures spread: roughly, how far a typical answer sits from its group's mean. A bigger SD means answers are more spread out.",
        q1: "Q1, the lower quartile, is the 25th percentile: a quarter of answers are at or below it. It's the left edge of the box.",
        med: "The median is the middle answer, the 50th percentile: half the answers are at or below it. It's the dark line inside the box.",
        q3: "Q3, the upper quartile, is the 75th percentile: three quarters of answers are at or below it. It's the right edge of the box. The middle half of the answers falls between Q1 and Q3 (the interquartile range, or IQR)."
      };
      return { title: COL_NAME[col], paras: [H[col]] };
    }
    var G = row === 0 ? g.A : g.B, O2 = row === 0 ? g.B : g.A, s = ex.stats[row], so = ex.stats[1 - row];
    switch (col) {
      case "group": return { title: G.lab, paras: [cap(G.who) + ". Who's included: " + c.pop] };
      case "n": return { title: "n for " + G.lab + " = " + fmtInt(s.n), paras: [
        fmtInt(s.n) + " " + G.who + (ex.full ? " answered both questions in HCMST." : " were drawn at random from the survey for this sample.")] };
      case "mean": return { title: "Mean for " + G.lab + " = " + r2(s.mean), paras: [
        cap(G.who) + " " + o.verb(r2(s.mean)) + ". Compare that with " + r2(so.mean) + " for " + O2.short + ", a gap of " + r2(Math.abs(s.mean - so.mean)) + " " + u + ". That gap is what the t-test asks about."] };
      case "sd": {
        var ratio = Math.max(s.sd, so.sd) / Math.min(s.sd, so.sd);
        return { title: "SD for " + G.lab + " = " + r2(s.sd), paras: [
          "Answers for " + G.short + " typically sit about " + r2(s.sd) + " " + u + " away from their mean of " + r2(s.mean) + ".",
          ratio < 1.5 ? "That's similar to the spread for " + O2.short + " (" + r2(so.sd) + "), so the two groups are about equally spread out."
            : "That's " + (s.sd > so.sd ? "more" : "less") + " spread out than " + O2.short + " (" + r2(so.sd) + "). Unequal spreads are why R's Welch test, which allows them, is a good choice."] };
      }
      case "q1": return { title: "Q1 for " + G.lab + " = " + r2(s.q1), paras: [
        "A quarter of " + G.short + " are at or below " + r2(s.q1) + " " + u + " on this measure, and three quarters are above it."] };
      case "med": {
        var gap = s.mean - s.med, note;
        if (Math.abs(gap) < 0.1 * s.sd) note = "The mean (" + r2(s.mean) + ") is close to the median, so the answers are fairly balanced around the middle.";
        else if (gap < 0) note = "The mean (" + r2(s.mean) + ") is below the median, a sign of a tail of low values pulling the average down (skewed left).";
        else note = "The mean (" + r2(s.mean) + ") is above the median, a sign of a tail of high values pulling the average up (skewed right).";
        return { title: "Median for " + G.lab + " = " + r2(s.med), paras: ["Half of " + G.short + " are at or below " + r2(s.med) + " " + u + ", and half are at or above it.", note] };
      }
      case "q3": return { title: "Q3 for " + G.lab + " = " + r2(s.q3), paras: [
        "Three quarters of " + G.short + " are at or below " + r2(s.q3) + " " + u + ".",
        s.q3 === s.q1 ? "Q1 and Q3 are the same here, so the whole middle half of answers is exactly " + r2(s.q1) + "."
          : "The middle half of answers falls between " + r2(s.q1) + " and " + r2(s.q3) + ", an IQR of " + r2(s.q3 - s.q1) + " " + u + "."] };
    }
    return { title: "", paras: [] };
  }
  function exCellSel() {
    Array.prototype.forEach.call($("ex-table").querySelectorAll(".cellbtn"), function (b) {
      var on = !!ex.cell && b.getAttribute("data-col") === ex.cell.col && +b.getAttribute("data-row") === ex.cell.row;
      b.classList.toggle("sel", on); b.setAttribute("aria-pressed", on ? "true" : "false");
    });
  }
  function exCellExplain() { if (ex.cell && ex.stats) renderExplain($("ex-table-explain"), cellExplain(ex.cell.col, ex.cell.row)); }

  /* ---------- explore view ---------- */
  function exBuildSelect() {
    var sel = $("ex-comp"), groups = {};
    COMPS.forEach(function (c) {
      var o = OUT[c.y];
      if (!groups[c.y]) { groups[c.y] = el("optgroup", { label: o.label }); sel.appendChild(groups[c.y]); }
      groups[c.y].appendChild(el("option", { value: c.id, text: c.rq }));
    });
  }
  function exDraw() {
    var G = GROUP_FOR[ex.c.id];
    ex.a = G.a; ex.b = G.b; ex.full = true;
  }
  function exRender(announce) {
    var c = ex.c, o = OUT[c.y], g = c.grp, a = ex.a, b = ex.b;
    $("ex-rq").textContent = c.rq;
    $("ex-who").textContent = "Who's included: " + c.pop + " Everyone in HCMST with answers to both questions.";
    $("ex-leg-a").textContent = "Group 1: " + g.A.lab;
    $("ex-leg-b").textContent = "Group 2: " + g.B.lab;
    $("ex-chart-title").textContent = o.label + " by group";

    var res = TT.welch(a, b); ex.res = res;
    var stats = drawBoxes($("ex-box"), [
      { label: g.A.lab, color: "var(--series-a)", values: a },
      { label: g.B.lab, color: "var(--series-b)", values: b }
    ], o.axis, o.domain, "ex-reading-text");
    var sa = stats ? stats[0] : boxStats(a), sb = stats ? stats[1] : boxStats(b);
    ex.stats = [sa, sb];

    var pre = $("ex-out");
    if (!(res.se > 0)) {
      pre.textContent = "> " + commandLine(c, ex.full) + "\nError in t.test.default(x = DATA[[1L]], y = DATA[[2L]], ...) :\n  data are essentially constant";
      $("ex-reading-text").textContent = "Every value in both groups is the same, so there's nothing to test. Move the slider to draw a new sample.";
      $("ex-ci").textContent = ""; $("ex-ci-sentence").textContent = "";
      return;
    }
    renderConsole(pre, c, res, ex.full, true);
    $("ex-console-note").textContent = ex.full ? "Everyone in HCMST" : "Random sample: " + a.length + " + " + b.length;
    $("ex-ci-caption").textContent = "Group 1 minus group 2 (" + g.A.lab + " − " + g.B.lab + "), in " + o.unit + ". Drawn at twice the scale of the box plots above, so small differences are easier to see. The dark vertical line marks 0, no difference.";
    var dom = $("ex-box")._domain || [0, 1];
    drawCI($("ex-ci"), res, dom[1] - dom[0], "Difference in average " + o.noun + ", " + g.A.lab + " minus " + g.B.lab + " (" + o.unit + ")");
    $("ex-ci-sentence").textContent = ciStatement(c, res);

    // plain English: direction, magnitude, significance
    var box = $("ex-reading-text"); box.textContent = "";
    var part = function (label, text) { box.appendChild(el("p", null, [el("b", { text: label + ": " }), text])); };
    part("Direction", directionSentence(c, res));
    part("Magnitude", magnitudeSentence(c, res) + " Is that magnitude practically significant? You tell me!");
    part("Significance", significanceSentence(c, res));

    exTable(sa, sb); exCellSel(); exCellExplain();

    var A = assumptions(c, a, b, ex.full), ul = $("ex-assume"); ul.textContent = "";
    A.items.forEach(function (x) {
      ul.appendChild(el("li", null, [el("span", { class: "tag " + x.s, text: x.label }), el("span", { class: "ck-title", text: x.title }), el("span", { class: "ck-body", text: x.body })]));
    });
    $("ex-assume-bottom").textContent = A.bottom;

    exWeighted();
    exExplain();
    if (announce) $("ex-live").textContent = "p = " + TT.printedPieces(res).p + ". Difference " + r2(res.diff) + ", 95% confidence interval " + r2(res.lo) + " to " + r2(res.hi) + ".";
  }
  function exWeighted() {
    var c = ex.c, g = c.grp, o = OUT[c.y], y = COL[c.y], gr = COL[c.grp.g], wt = COL.weight;
    var a = [], wa = [], b = [], wb = [];
    for (var i = 0; i < y.length; i++) {
      var yi = y[i], gi = gr[i];
      if (yi !== yi || gi !== gi) continue;
      if (gi === g.A.code) { a.push(yi); wa.push(wt[i]); } else if (gi === g.B.code) { b.push(yi); wb.push(wt[i]); }
    }
    var w = TT.svyttest(a, wa, b, wb, y.length), u = ex.res;
    $("wt-label").textContent = "For the comparison selected on the Explore tab: " + o.label + ", " + g.A.lab.replace(/_/g, " ") + " vs. " + g.B.lab.replace(/_/g, " ") + ".";
    var pre = $("wt-out"); pre.textContent = "";
    pre.appendChild(el("span", { class: "cmd" }, ["> ", el("b", { text: "hcmst_design <- svydesign(ids = ~1, weights = ~w1_weight_combo, data = hcmst)" }), "\n> ", el("b", { text: "svyttest(" + c.y + " ~ " + c.grp.g + ", design = hcmst_design)" })]));
    pre.appendChild(document.createTextNode("\n" + TT.printSvyttest(w, c.y, c.grp.g)));
    // summary of how the weighted result differs, in the same A-minus-B terms as t.test()
    var wd = -w.diff, wlo = -w.hi, whi = -w.lo, box = $("wt-summary"); box.textContent = "";
    var sigU = u.p < 0.05, sigW = w.p < 0.05;
    var dirSame = (wd >= 0) === (u.diff >= 0);
    box.appendChild(el("p", { text: "Weighted: " + g.A.short + " " + o.mag(r2(Math.abs(wd)), wd >= 0) + " (95% CI " + r2(wlo) + " to " + r2(whi) + ", " + pShort(w) + ")." }));
    box.appendChild(el("p", { text: "Unweighted: " + g.A.short + " " + o.mag(r2(Math.abs(u.diff)), u.diff >= 0) + " (95% CI " + r2(u.lo) + " to " + r2(u.hi) + ", " + pShort(u) + ")." }));
    var change;
    if (!dirSame) change = "Weighting flips the direction of the difference.";
    else if (sigU && !sigW) change = "Weighting changes the conclusion: with the weights, we do not have enough evidence to say that " + g.A.short + " and " + g.B.short + " differ in their average " + o.noun + ".";
    else if (!sigU && sigW) change = "Weighting changes the conclusion: with the weights, we're fairly confident that " + g.A.short + " " + (wd >= 0 ? o.more : o.less) + " than " + g.B.short + ".";
    else change = "Weighting " + (Math.abs(Math.abs(wd) - Math.abs(u.diff)) < 0.05 * Math.abs(u.diff) ? "barely moves the estimate" : "moves the estimate " + (Math.abs(wd) < Math.abs(u.diff) ? "closer to 0" : "further from 0")) + ", but the conclusion is the same: " + (sigW ? "we're fairly confident the groups differ." : "we do not have enough evidence to say the groups differ.");
    box.appendChild(el("p", { class: "bottom-line", text: change }));
    box.appendChild(el("p", { class: "note", text: "svyttest() subtracts the other way (" + g.B.lab + " minus " + g.A.lab + "), so its numbers have the opposite sign from t.test()." }));
  }
  function exInit() {
    exBuildSelect();
    $("ex-comp").addEventListener("change", function () { ex.c = BY_ID[this.value]; exDraw(); exRender(true); });
    var pre = $("ex-out");
    pre._after = exMarkSel;
    bindHot(pre, function (k) { ex.selKey = k; exMarkSel(); exExplain(); });
    $("ex-table").addEventListener("click", function (e) {
      var bt = e.target.closest(".cellbtn"); if (!bt) return;
      ex.cell = { col: bt.getAttribute("data-col"), row: +bt.getAttribute("data-row") };
      exCellSel(); exCellExplain();
    });
    exDraw();
  }

  /* =========================================================
     Read the output
     ========================================================= */
  var rd = { item: null };
  var Q_ORDER = ["mean", "higher", "pval", "pmean", "order", "zero", "class"];

  function uniqOptions(opts) {
    var seen = {}, out = [];
    opts.forEach(function (o) { if (!seen[o.text]) { seen[o.text] = 1; out.push(o); } });
    return out;
  }
  function buildQuestions(item) {
    var c = item.c, o = OUT[c.y], g = c.grp, res = item.res, P = TT.printedPieces(res);
    var Q = {};
    var askB = Math.random() < 0.5, G1 = askB ? g.B : g.A, mG = askB ? P.mB : P.mA, mO = askB ? P.mA : P.mB;
    Q.mean = { prompt: "What was the average " + o.noun + " for " + G1.who + "?",
      options: [{ text: mG, ok: true }, { text: mO }, { text: P.hi }, { text: P.t }],
      explain: "Look under “sample estimates”: “mean in group " + G1.lab + "” is " + mG + "." };
    var pOpt = P.p.charAt(0) === "<" ? P.p + "  (less than 2.2e-16)" : P.p;
    var pNote = "";
    if (P.p.charAt(0) === "<") pNote = " R prints “< 2.2e-16” when the p-value is too small to show. It's essentially zero.";
    else if (/e/.test(P.p)) pNote = " R uses scientific notation for very small numbers: " + P.p + " means " + res.p.toFixed(Math.min(12, 2 - Math.floor(Math.log10(res.p)))).replace(/0+$/, "") + ".";
    Q.pval = { prompt: "Which number is the p-value?",
      options: [{ text: pOpt, ok: true }, { text: P.t }, { text: P.df }, { text: P.lo }],
      explain: "It's on the third line, right after “p-value”. The other numbers are the t statistic, the degrees of freedom (df), and the lower end of the confidence interval." + pNote };
    Q.order = { prompt: "The confidence interval estimates which difference?",
      options: [{ text: "The average for " + g.A.lab + " minus the average for " + g.B.lab, ok: true },
        { text: "The average for " + g.B.lab + " minus the average for " + g.A.lab },
        { text: "The median for " + g.A.lab + " minus the median for " + g.B.lab },
        { text: "The range that 95% of individual answers fall in" }],
      explain: "R puts the groups in alphabetical order, so “" + g.A.lab + "” comes first and every difference is " + g.A.lab + " minus " + g.B.lab + ". The “alternative hypothesis” line names them in that order. Here " + P.mA + " − " + P.mB + " = " + r2(res.diff) + ", the center of the interval." };
    var includes = res.lo <= 0 && res.hi >= 0;
    Q.zero = { prompt: "Does the 95% confidence interval include 0?", fixed: true,
      options: [{ text: "Yes", ok: includes }, { text: "No", ok: !includes }],
      explain: "The interval runs from " + P.lo + " to " + P.hi + ". " + (includes
        ? "It crosses 0, so “no difference” is still a plausible value. That matches a p-value above .05."
        : "Both ends are on the same side of 0, so “no difference” isn't a plausible value. That matches a p-value below .05.") };
    var v = verdict(res.p);
    Q["class"] = { prompt: "Using our class guide for p-values, which conclusion fits?", fixed: true,
      options: [{ text: "We're confident there's a difference (p is below .01)", ok: v.key === "confident" },
        { text: "We're fairly confident there's a difference (p is between .01 and .05)", ok: v.key === "fairly" },
        { text: "Marginally significant, worth more research (p is between .05 and .10)", ok: v.key === "marginal" },
        { text: "We don't have enough evidence to say the groups differ (p is .10 or higher)", ok: v.key === "none" }],
      explain: "p = " + P.p + ". Our guide: below .01, confident; below .05, fairly confident; below .10, marginally significant; otherwise, not enough evidence. Never conclude the groups are the same." };
    var hiG = res.diff >= 0 ? g.A : g.B, loG = res.diff >= 0 ? g.B : g.A;
    var d = r2(Math.abs(res.diff)), far = r2(Math.max(Math.abs(res.lo), Math.abs(res.hi))), near = r2(Math.min(Math.abs(res.lo), Math.abs(res.hi)));
    Q.higher = { prompt: "Which group had the higher average " + o.noun + ", and by about how much?",
      options: [{ text: hiG.lab + ", by about " + d + " " + o.unit, ok: true }, { text: loG.lab + ", by about " + d + " " + o.unit },
        { text: hiG.lab + ", by about " + far + " " + o.unit }, { text: loG.lab + ", by about " + near + " " + o.unit }],
      explain: P.mA + " − " + P.mB + " = " + r2(res.diff) + ", so " + hiG.lab + " is higher by about " + d + " " + o.unit + ". The ends of the confidence interval are a range for that difference, not the difference itself." };
    Q.pmean = { prompt: "What does p = " + P.p + " mean here?",
      options: [{ text: "If there were really no difference between these groups in the population, a gap this big or bigger would show up in " + pFreq(res.p) + " of random samples.", ok: true },
        { text: "There is " + (res.p < 0.001 ? "less than a 0.1%" : "a " + pctStr(res.p)) + " chance that there's no difference between the groups." },
        { text: "There is " + (res.p < 0.001 ? "more than a 99.9%" : "a " + pctStr(1 - res.p)) + " chance that the hypothesis is true." },
        { text: "The two groups' averages differ by " + P.p + " " + o.unit + "." }],
      explain: "That's the definition from 3.2: the probability of a result at least this extreme if there were truly no difference. It is not the chance that either hypothesis is true." };
    var others = shuffle(["mean", "higher", "pval", "pmean", "order", "zero"]).slice(0, 4).concat(["class"]);
    others.sort(function (x, y) { return Q_ORDER.indexOf(x) - Q_ORDER.indexOf(y); });
    return others.map(function (k) {
      var q = Q[k]; q.key = k; q.options = uniqOptions(q.options);
      if (!q.fixed) shuffle(q.options);
      return q;
    });
  }
  function renderQuestion(q, idx, prefix, onAnswer) {
    var fs = el("fieldset", { class: "q" });
    fs.appendChild(el("legend", null, [el("span", { class: "qnum", text: (idx + 1) + "." }), q.prompt]));
    var fb = el("div", { class: "fb", role: "status", hidden: "" });
    q.options.forEach(function (o, j) {
      var id = prefix + "-" + idx + "-" + j;
      var inp = el("input", { type: "radio", name: prefix + "-" + idx, id: id, value: String(j) });
      var lab = el("label", { class: "choice", for: id }, [inp, el("span", { text: o.text })]);
      inp.addEventListener("change", function () {
        Array.prototype.forEach.call(fs.querySelectorAll(".choice"), function (l) { l.classList.remove("is-right", "is-wrong"); });
        lab.classList.add(o.ok ? "is-right" : "is-wrong");
        fb.hidden = false; fb.className = "fb " + (o.ok ? "right" : "wrong"); fb.textContent = "";
        fb.appendChild(el("strong", { text: o.ok ? "Correct." : "Not quite." }));
        fb.appendChild(document.createTextNode(o.ok ? q.explain : "Try another answer, or read this hint: " + q.explain));
        if (onAnswer) onAnswer(idx, !!o.ok);
      });
      fs.appendChild(lab);
    });
    fs.appendChild(fb);
    return fs;
  }
  function rdNew() {
    var item = makeItem(rd.item && rd.item.c.id); rd.item = item;
    var c = item.c, o = OUT[c.y];
    $("rd-context").textContent = sampleSentence(c, item.na, item.nb, item.full);
    $("rd-measure").textContent = "Measured: " + o.desc + " Who's included: " + c.pop;
    renderConsole($("rd-out"), c, item.res, item.full);
    var qs = buildQuestions(item), box = $("rd-questions"), state = {};
    box.textContent = "";
    $("rd-tally").textContent = "";
    qs.forEach(function (q, i) {
      box.appendChild(renderQuestion(q, i, "rdq", function (idx, ok) {
        state[idx] = ok;
        var answered = Object.keys(state).length, right = Object.keys(state).filter(function (k) { return state[k]; }).length;
        $("rd-tally").textContent = right + " of " + qs.length + " correct" + (answered < qs.length ? " so far." : (right === qs.length ? ". Nice work. Try a new printout." : ". Change any answer to try again."));
      }));
    });
  }

  /* =========================================================
     Write it up: generator + checker
     ========================================================= */
  var wr = { item: null };
  function wrNew() {
    var item = makeItem(wr.item && wr.item.c.id); wr.item = item;
    var c = item.c, o = OUT[c.y], g = c.grp;
    $("wr-rq").textContent = c.rq;
    $("wr-hyp").textContent = c.hyp;
    $("wr-data").textContent = sampleSentence(c, item.na, item.nb, item.full) + " Each person reported their " + o.noun + ". " + o.desc;
    $("wr-analysis").textContent = "An independent samples t-test (Welch) was conducted to compare " + o.noun + " between " + g.A.short + " and " + g.B.short + ".";
    renderConsole($("wr-out"), c, item.res, item.full);
    $("wr-text").value = "";
    $("wr-feedback").textContent = "";
    $("wr-model").hidden = true;
  }
  function modelAnswer(item) {
    var c = item.c, res = item.res, box = $("wr-model");
    box.textContent = "";
    box.appendChild(el("h3", { text: "An example answer" }));
    box.appendChild(el("ul", null, [
      el("li", null, [el("b", { text: "Mean for each group: " }), meansSentence(c, res)]),
      el("li", null, [el("b", { text: "Confidence interval of the difference: " }), ciSentence(c, res)]),
      el("li", null, [el("b", { text: "Interpretation of the p-value: " }), pSentence(c, res)]),
      el("li", null, [el("b", { text: "Going further, practical significance: " }), practicalSentence(c, res)]),
      el("li", null, [el("b", { text: "So what? " }), soWhat(c, res, item.full)])
    ]));
    if (c.note) box.appendChild(el("p", { class: "note", text: c.note }));
    box.hidden = false;
  }

  function normalize(t) {
    return " " + t.toLowerCase().replace(/[−]/g, "-").replace(/[‘’]/g, "'").replace(/[“”]/g, '"').replace(/\s+/g, " ") + " ";
  }
  function numbers(t) {
    var re = /(^|[^\d.\w])(-?)((?:\d{1,3}(?:,\d{3})+|\d*\.?\d+)(?:e-?\d+)?)(\s*%)?/g, out = [], m;
    while ((m = re.exec(t))) {
      var s = m[3].replace(/,/g, ""), sci = /e/.test(s), val = parseFloat(s);
      if (!isFinite(val)) continue;
      var mant = s.split("e")[0], dec = mant.indexOf(".") >= 0 ? mant.length - mant.indexOf(".") - 1 : 0;
      if (m[2] === "-") val = -val;
      if (m[4]) { val /= 100; dec += 2; }
      out.push({ v: val, d: sci ? 12 : dec, sci: sci });
      re.lastIndex = m.index + m[0].length;
    }
    return out;
  }
  function numMatches(tok, target, absolute) {
    var v = absolute ? Math.abs(tok.v) : tok.v, tg = absolute ? Math.abs(target) : target;
    if (tok.sci) return Math.abs(v - tg) <= Math.abs(tg) * 0.06;
    var unit = Math.pow(10, -tok.d);
    if (!(tok.d >= 2 || unit <= Math.abs(tg) / 10)) return false;
    return Math.abs(v - tg) < unit + 1e-12;
  }
  function twoDistinct(toks, x, y, absolute) {
    var ix = [], iy = [];
    toks.forEach(function (t, i) { if (numMatches(t, x, absolute)) ix.push(i); if (numMatches(t, y, absolute)) iy.push(i); });
    for (var i = 0; i < ix.length; i++) for (var j = 0; j < iy.length; j++) if (ix[i] !== iy[j]) return true;
    return false;
  }
  function hasAny(t, list) { return list.some(function (k) { return t.indexOf(k) >= 0; }); }

  function checkWriteup(text, item) {
    var c = item.c, o = OUT[c.y], g = c.grp, res = item.res, P = TT.printedPieces(res), t = normalize(text);
    var toks = numbers(t), out = [];
    var add = function (status, title, body) { out.push({ status: status, title: title, body: body }); };
    var words = text.trim().split(/\s+/).filter(Boolean).length;

    // 1. means
    if (twoDistinct(toks, res.mx, res.my, false))
      add("pass", "Group means", "You reported the average for both groups.");
    else
      add("fix", "Group means", "Give the average " + o.noun + " for each group (" + P.mA + " for " + g.A.lab + " and " + P.mB + " for " + g.B.lab + " under “sample estimates”). Rounding to two decimals is fine.");

    // 2. confidence interval
    if (twoDistinct(toks, res.lo, res.hi, true)) {
      add("pass", "Confidence interval", "You gave both ends of the 95% confidence interval for the difference.");
      if (!/(includ|contain|cross|span|overlap|cover|go(es)? through|has)\w*\s+(\w+\s+){0,3}?(zero|0(?!\.?\d))/.test(t) && !/(zero|0(?!\.?\d))\s+(is|isn't|is not|was|wasn't)\s+(\w+\s+){0,2}?(in|inside|within|included|outside)/.test(t))
        add("tip", "Does the interval include 0?", "Say whether the interval includes 0. That tells the reader whether “no difference” is still plausible (here it " + (res.lo <= 0 && res.hi >= 0 ? "is" : "isn't") + ").");
    } else {
      add("fix", "Confidence interval", "Report the 95% confidence interval of the difference: from " + P.lo + " to " + P.hi + " (" + g.A.lab + " minus " + g.B.lab + "). Rounding to two decimals is fine.");
    }

    // 3. p-value reported
    var pExact = toks.some(function (tk) { return numMatches(tk, res.p, false); });
    var claims = [], pre = /(\bp\b|p-value|p value)/g, m;
    while ((m = pre.exec(t))) {
      var after = t.slice(m.index + m[0].length, m.index + m[0].length + 45);
      var cm = after.match(/(<=|>=|<|>|≤|≥|less than|lower than|below|under|smaller than|greater than|more than|above|over|bigger than|larger than|higher than)\s*(?:the\s+)?(?:alpha\s+(?:of\s+)?|cutoff\s+(?:of\s+)?|threshold\s+(?:of\s+)?)?(-?\d*\.?\d+(?:e-?\d+)?)/);
      if (cm) {
        var num = parseFloat(cm[2]), less = /<|≤|less|lower|below|under|smaller/.test(cm[1]);
        claims.push({ text: cm[0].trim(), ok: less ? res.p < num : res.p > num });
      }
    }
    var badClaim = claims.filter(function (x) { return !x.ok; })[0], goodClaim = claims.some(function (x) { return x.ok; });
    if (badClaim) add("fix", "p-value", "You wrote “p " + badClaim.text + "”, but the printout says p-value " + (P.p.charAt(0) === "<" ? P.p : "= " + P.p) + ".");
    else if (pExact || goodClaim) add("pass", "p-value", "You reported the p-value.");
    else add("fix", "p-value", "Report the p-value. It's on the third line of the printout, after “p-value” (" + P.p + ").");

    // (computed early so the conclusion row doesn't double up with it)
    var nullRe = /there (is|was|'s|are|were) no (real |actual |true |meaningful )?(difference|relationship|effect|association|link)|\bno difference\b|(are|were|is|was) (basically |essentially |about |roughly )?the same|(are|were) (equal|identical)|accept(ed|ing|s)? the null|null (hypothesis )?(is|was) (true|correct|confirmed|right)|(groups?|means?|averages?) (are|were) (equal|the same)/g;
    var nullHit = null;
    while ((m = nullRe.exec(t))) {
      var before = t.slice(Math.max(0, m.index - 45), m.index), after = t.slice(m.index + m[0].length, m.index + m[0].length + 40);
      if (/\b(if|assum\w*|suppos\w*|rule out|ruled out|under|whether|unless|consistent with|chance|probability|likelihood)\b[^.]*$/.test(before)) continue;
      if (/["']\s*$/.test(before) || /^\s*["']/.test(after)) continue;
      if (/^\W*(is|remains|would be|was|could be|might be|can't be|cannot be)\s+(still\s+|also\s+)?(a\s+)?(plausible|possible|consistent|within|in the|ruled out|excluded|included)/.test(after)) continue;
      nullHit = m[0]; break;
    }

    // 4. conclusion matches the class guide
    var v = verdict(res.p);
    var notEnough = /(not|n't|no)\s+(have\s+)?(enough|sufficient|strong enough|adequate|much)\s+(statistical\s+)?evidence|insufficient evidence|lack(s|ing)? (of )?(sufficient |enough )?evidence|not (statistically )?significant|no (statistically )?significant|n't (statistically )?significant|can(not|'t) (say|conclude|tell)|unable to (say|conclude)|(not|n't) (be )?(very |fairly )?confident/.test(t);
    var t2 = t.replace(/(not|n't)\s+(be\s+)?(very\s+|fairly\s+|that\s+)?confident/g, " ").replace(/\b(90|95|99)\s*(%|percent)\s+(confiden\w*)/g, " ").replace(/confidence interval/g, " ");
    var confident = /\bconfident\b/.test(t2);
    var t3 = t.replace(/(not|n't|no)\s+(statistically\s+)?significant(ly)?/g, " ");
    var signif = /significant/.test(t3) && !/marginal\w*\s+significant/.test(t3);
    var marginal = /marginal|suggest|more research|further research|further study|more data|weak evidence|some evidence|borderline|hint/.test(t);
    var pTxt = "p = " + P.p;
    if (v.key === "confident" || v.key === "fairly") {
      if ((confident || signif) && !notEnough) {
        add("pass", "Conclusion", "Your conclusion fits the class guide: with " + pTxt + ", we're " + (v.key === "confident" ? "confident" : "fairly confident") + " there's a difference.");
        if (v.key === "fairly" && confident && !/fairly confident/.test(t)) add("tip", "How confident?", "With p between .01 and .05, the class guide says “fairly confident.” Save plain “confident” for p below .01.");
      }
      else if (notEnough) add("fix", "Conclusion", "With " + pTxt + " (below .05), the class guide says we're " + (v.key === "confident" ? "confident" : "fairly confident") + " the groups differ, but your write-up says there isn't enough evidence.");
      else add("fix", "Conclusion", "Add a conclusion in words. With " + pTxt + ", say that we're " + (v.key === "confident" ? "confident" : "fairly confident") + " " + g.A.short + " and " + g.B.short + " differ.");
    } else if (v.key === "marginal") {
      if (marginal || notEnough) add("pass", "Conclusion", "Your conclusion fits the class guide for " + pTxt + ": marginally significant, not enough to be fairly confident.");
      else if (confident || signif) add("fix", "Conclusion", pTxt + " is above .05, so that's not enough to be fairly confident. Call it marginally significant, or say we don't have enough evidence to say the groups differ.");
      else add("fix", "Conclusion", "Add a conclusion in words. With " + pTxt + ", call it marginally significant: it suggests a difference worth more research.");
    } else {
      if (notEnough && !(confident && !notEnough)) add("pass", "Conclusion", "Your conclusion fits the class guide: with " + pTxt + ", we don't have enough evidence to say the groups differ.");
      else if (confident || signif) add("fix", "Conclusion", pTxt + " is above .10, so the class guide says we don't have enough evidence of a difference.");
      else if (!nullHit) add("fix", "Conclusion", "Add a conclusion in words. With " + pTxt + ", say we don't have enough evidence to say " + g.A.short + " and " + g.B.short + " differ.");
    }

    // 5. never conclude the null; never "prove"
    var proves = /\bprov(e|es|ed|en|ing)\b/.test(t);
    if (nullHit) add("fix", "Don't conclude the null is true", "“" + nullHit.trim() + "” says the groups are the same, and a hypothesis test can never show that. Say we don't have enough evidence to say they differ.");
    else if (proves) add("fix", "Don't say “prove”", "A hypothesis test can't prove anything. Use “we're fairly confident” or “the evidence suggests.”");
    else add("pass", "No overclaiming", "You didn't claim the groups are the same or that anything was proven.");

    // 6. p-value misreadings
    var mis = [
      /(chance|probability|likelihood|likely)\s+(that\s+)?(the\s+)?(null( hypothesis)?|h0)\s+(is|was)\s+(true|correct|right|false|wrong)/,
      /(chance|probability|likelihood)\s+(that\s+)?(the\s+|our\s+|my\s+)?(alternative( hypothesis)?|hypothesis|h1|research hypothesis)\s+(is|was)\s+(true|correct|right|wrong|false)/,
      /(chance|probability|likelihood)\s+(that\s+)?there\s+(is|was|'s)\s+(no|a)\s+(real\s+|true\s+)?(difference|relationship|effect)/,
      /(chance|probability|likelihood)[^.]{0,40}(due to|by|just|purely|from)\s+(random\s+)?chance/,
      /(chance|probability|likelihood)\s+(that\s+)?(the\s+)?(results?|findings?|difference)\s+(is|are|was|were)\s+(wrong|false|a fluke|real|true)/
    ];
    var misHit = mis.some(function (re) { return re.test(t); });
    var oneMinus = false;
    var om = /(\d+(?:\.\d+)?)\s*(%|percent)\s+(sure|confident|certain|chance|likely|probability)/g;
    while ((m = om.exec(t))) {
      var val = parseFloat(m[1]);
      if (Math.abs(val - 95) < 0.01) continue;
      if (Math.abs(val - 100 * (1 - res.p)) < 1.5) oneMinus = true;
    }
    if (misHit) add("fix", "What the p-value means", "The p-value isn't the chance that a hypothesis is true or that the result is “due to chance.” It's how often you'd see a gap this big or bigger if there were really no difference.");
    else if (oneMinus) add("fix", "What the p-value means", "1 minus the p-value isn't how sure you can be. Use the class language instead (“fairly confident,” “not enough evidence”).");
    else if (pExact || goodClaim) add("pass", "What the p-value means", "Your p-value statement avoids the common misreadings.");

    // 7. context and plain English
    var hasA = hasAny(t, g.A.kw), hasB = hasAny(t, g.B.kw), hasO = hasAny(t, o.kw);
    if (!(hasA && hasB && hasO)) {
      var miss = [];
      if (!hasA) miss.push(g.A.short); if (!hasB) miss.push(g.B.short); if (!hasO) miss.push("what was measured (" + o.noun + ")");
      add("fix", "Put it in context", "Name " + miss.join(", ") + ". A reader should know who was compared and on what, without seeing the printout.");
    } else add("pass", "In context", "You named both groups and what was measured.");
    if (/\bh0\b|\bh1\b|\bh_?[01]\b|null hypothesis|alternative hypothesis|reject(ed|ing|s)?\s+the\s+null|fail(ed|s|ing)?\s+to\s+reject|\bt\s*=|\bdf\b|welch/.test(t))
      add("tip", "Plain English", "Someone without stats training won't know “reject the null,” “H0,” or “t =”. Say it in everyday words, like “we're fairly confident the groups differ.”");
    if (words < 35) add("tip", "Length", "Short is fine, but give each of the three parts (means, interval, p-value) its own sentence.");
    return out;
  }
  function renderChecks(list) {
    var box = $("wr-feedback"); box.textContent = "";
    var fixes = list.filter(function (x) { return x.status === "fix"; }).length;
    box.appendChild(el("h3", { text: fixes ? fixes + (fixes === 1 ? " thing" : " things") + " to fix" : "Looks solid" }));
    var ul = el("ul", { class: "checks" });
    var label = { pass: "Done", fix: "Fix", tip: "Tip" };
    list.forEach(function (x) {
      ul.appendChild(el("li", null, [el("span", { class: "tag " + x.status, text: label[x.status] }), el("span", { class: "ck-title", text: x.title }), el("span", { class: "ck-body", text: x.body })]));
    });
    box.appendChild(ul);
    box.appendChild(el("p", { class: "note", text: "The checker matches numbers and phrases, so it can miss things a person would catch. Compare with the example answer too." }));
  }
  function wrInit() {
    $("wr-new").addEventListener("click", wrNew);
    $("wr-show").addEventListener("click", function () { modelAnswer(wr.item); $("wr-model").scrollIntoView({ behavior: "smooth", block: "nearest" }); });
    $("wr-check").addEventListener("click", function () {
      var text = $("wr-text").value;
      if (!text.trim()) { var b = $("wr-feedback"); b.textContent = ""; b.appendChild(el("p", { class: "fb wrong", text: "Write your interpretation in the box first, then check it." })); return; }
      renderChecks(checkWriteup(text, wr.item));
    });
  }

  /* =========================================================
     Pick the test
     ========================================================= */
  var TESTS = ["Two-sample t-test (independent groups)", "Paired t-test", "ANOVA", "Chi-square test"];
  function testItem(scenario, answer, explain) {
    return { scenario: scenario, fixed: true, options: TESTS.map(function (t, i) { return { text: t, ok: i === answer }; }), explain: explain, prompt: "Which test fits best?" };
  }
  var BANK = [
    testItem("You want to know whether relationship quality (1 to 5) differs between people who met their partner online and people who met offline.", 0,
      "One categorical variable with two groups (online, offline) and one quantitative outcome. Each person is in only one group, so the groups are independent."),
    testItem("For each different-sex couple in the survey, you compare the man's age with the woman's age to see whether men tend to be older.", 1,
      "Each couple gives you two related numbers. A paired t-test looks at the difference within each couple."),
    testItem("You compare relationship quality across four ways of meeting: through friends, at work, at a bar, and online.", 2,
      "There are more than two groups. ANOVA compares them all at once instead of running several t-tests."),
    testItem("You want to know whether people who met online are more or less likely to be married (yes or no) than people who met offline.", 3,
      "Both variables are categorical (met online yes/no, married yes/no), so you compare proportions in a contingency table, as in module 3.1."),
    testItem("HCMST asked the same people about their relationship quality in 2017 and again in 2022. You want to know whether quality changed for couples who stayed together.", 1,
      "Same people measured twice: the two sets of answers are linked person by person, so a paired t-test fits."),
    testItem("You compare respondents' years of education with their own partner's years of education.", 1,
      "Each respondent is matched with one specific partner, so the two measurements are paired."),
    testItem("You compare the age gap between partners for married couples and for unmarried couples.", 0,
      "Two separate groups (married, unmarried) and a quantitative outcome (age gap in years)."),
    testItem("You compare how many years couples took to move in together across the four US census regions.", 2,
      "One quantitative outcome compared across more than two groups calls for ANOVA."),
    testItem("You want to know whether men and women differ in whether they used a dating app in the past year (yes or no).", 3,
      "Gender and app use are both categorical, so this is a chi-square question about proportions."),
    { prompt: "What's the problem?", scenario: "A classmate compares relationship quality for couples who met through friends, at work, and online by running three separate t-tests (friends vs. work, friends vs. online, work vs. online).",
      options: [{ text: "Each extra test adds another chance of a false positive, so the overall Type I error rate climbs above .05.", ok: true },
        { text: "t-tests can't be used when the outcome is a rating from 1 to 5." },
        { text: "Three t-tests always give the same answer as one t-test, so it's wasted effort." },
        { text: "Nothing. Running every pair is the standard approach." }],
      explain: "This is why 3.2 introduces ANOVA: one test across all the groups keeps the Type I error rate at alpha." },
    { prompt: "Which t-test assumption is most at risk?", scenario: "You draw 12 people who met online and 12 who met offline. Years from meeting to becoming a couple is very right-skewed: most couples get together within a year, but a few take 20 years or more.",
      options: [{ text: "Normality", ok: true }, { text: "Independence of observations" }, { text: "Equal variances" }, { text: "None of them" }],
      explain: "With only 12 per group, the Central Limit Theorem can't rescue a strongly skewed variable. With hundreds per group (as in the full HCMST sample) it's much less of a worry." },
    { prompt: "Which assumption does this bear on, and what does R do about it?", scenario: "Relationship quality has a standard deviation of 0.92 for people who live apart from their partner and 0.68 for people who live together.",
      options: [{ text: "Equal variances. R's t.test() runs Welch's version by default, which doesn't assume equal variances.", ok: true },
        { text: "Normality. R automatically switches to a nonparametric test." },
        { text: "Independence. R pairs up the observations." },
        { text: "None. Standard deviations don't matter for a t-test." }],
      explain: "That's why the printout says “Welch Two Sample t-test.” In this class you can compare the two standard deviations to check this assumption." },
    { prompt: "What's wrong with this analysis?", scenario: "A researcher surveys both partners in 200 couples, puts all the men in one group and all the women in the other, and runs a two-sample t-test on relationship quality.",
      options: [{ text: "The groups aren't independent: each man is linked to a woman in the other group, so a paired t-test fits better.", ok: true },
        { text: "The sample is too small for a t-test." },
        { text: "Relationship quality isn't a quantitative variable." },
        { text: "Nothing. Men and women are different groups." }],
      explain: "Partners in the same couple share a relationship, so their answers are related. Independence of observations is violated." },
    { prompt: "Which statement fits best?", scenario: "In the full HCMST sample (2,458 people who met offline, 389 who met online), the average relationship quality was 4.50 vs. 4.39 on a 1-to-5 scale, with p = 0.015.",
      options: [{ text: "It's statistically significant, but a 0.1-point gap on a 5-point scale is small, so it may not matter much in practice.", ok: true },
        { text: "It's not statistically significant because the difference is small." },
        { text: "It proves that meeting online makes relationships worse." },
        { text: "It's practically significant because the p-value is small." }],
      explain: "Large samples can detect tiny differences. Statistical significance tells you a difference is unlikely to be zero, not that it's big enough to matter." },
    { prompt: "Which statement fits best?", scenario: "Suppose a small study with 15 people per group found that couples who met through friends rated their relationship 0.6 points higher, with p = 0.12.",
      options: [{ text: "We don't have enough evidence to say the groups differ, but the gap could matter in practice, and a bigger sample might detect it.", ok: true },
        { text: "There is no difference between the groups." },
        { text: "The difference is statistically significant." },
        { text: "The study proves meeting through friends doesn't matter." }],
      explain: "A result can be practically meaningful but not statistically significant when the sample is small (a possible Type II error). And never conclude the null is true." },
    { prompt: "What's the best answer?", scenario: "In HCMST, people who met their partner online had been together 6.5 years on average, compared with 23.9 years for people who met offline (p < 2.2e-16). Does this mean relationships that start online don't last?",
      options: [{ text: "No. Online dating only became common recently, so most online couples simply haven't had as much time together. The t-test shows a difference, not its cause.", ok: true },
        { text: "Yes. The p-value is tiny, so online relationships end sooner." },
        { text: "Yes, because the confidence interval doesn't include 0." },
        { text: "No, because the sample is too small to tell." }],
      explain: "A t-test compares averages. Explaining why they differ takes thinking about what else is going on, like when each group of couples met." },
    { prompt: "Which assumption is most clearly violated?", scenario: "A dating app emails a satisfaction survey to its users and compares people who've used the app for less than a year with people who've used it longer. Only 8% of users answer.",
      options: [{ text: "Random sampling. The people who chose to answer may differ from those who didn't, so the results may not generalize.", ok: true },
        { text: "Normality" }, { text: "Equal variances" }, { text: "None. Email surveys are simple random samples." }],
      explain: "As in 3.2's discussion of assumptions: a self-selected sample is further from a simple random sample than a probability-based design, so interpret with caution." }
  ];
  var ch = { order: [], pos: 0, round: 1 };
  function chRender() {
    var item = BANK[ch.order[ch.pos]];
    $("ch-progress").textContent = "Scenario " + (ch.pos + 1) + " of " + BANK.length + (ch.round > 1 ? " · round " + ch.round : "");
    var box = $("ch-card"); box.textContent = "";
    var q = { prompt: item.prompt, options: item.fixed ? item.options.slice() : shuffle(item.options.slice()), explain: item.explain };
    var card = el("div", { class: "stack" });
    card.appendChild(el("p", { class: "scenario", text: item.scenario }));
    card.appendChild(renderQuestion(q, 0, "chq-" + ch.pos, null));
    box.appendChild(card);
    $("ch-next").textContent = ch.pos === BANK.length - 1 ? "Start over with a new order" : "Next scenario";
  }
  function chInit() {
    ch.order = shuffle(BANK.map(function (_, i) { return i; }));
    $("ch-next").addEventListener("click", function () {
      ch.pos++;
      if (ch.pos >= BANK.length) { ch.pos = 0; ch.round++; ch.order = shuffle(ch.order); }
      chRender();
      var lg = document.querySelector("#ch-card legend"); if (lg) lg.scrollIntoView({ behavior: "smooth", block: "nearest" });
    });
    chRender();
  }

  /* =========================================================
     What questions can't these data answer?
     ========================================================= */
  var LIMITS = [
    { q: "Does meeting online make relationships less happy?", tag: "Cause and effect",
      why: "HCMST is a survey, not an experiment. Nobody was assigned to meet their partner online or offline, so the two groups differ in other ways too: online couples are newer, met when they were older, and are more often same-sex couples. A t-test shows whether the averages differ, not what caused the difference.",
      need: "An experiment (not practical for dating), or a model that holds those other differences constant, like a regression with several other variables. You'll learn to do this in future classes like SOC 302 or POL 200! Or if you want to explore being a statistics minor :)" },
    { q: "Do couples who met online break up sooner?", tag: "One moment in time",
      why: "This page uses only the 2017 survey, a snapshot of couples at one moment. People whose relationships already ended aren't in the relationship-quality data, and “years together” only counts couples who are still together, so it mostly reflects how recently online dating took off.",
      need: "The same couples followed over time. HCMST did re-survey people in 2020 and 2022, but those waves aren't used here." },
    { q: "Is the online vs. offline gap different for same-sex couples?", tag: "Two grouping variables",
      why: "A two-sample t-test compares two groups on one grouping variable at a time. This question needs two at once (how they met and couple type) to see whether one difference depends on the other, which is called an interaction.",
      need: "A two-way ANOVA or a regression with both variables, which 3.2 mentions for future classes." },
    { q: "Which dating app produces the happiest couples?", tag: "Not measured that way",
      why: "Here, how couples met is just online or offline. The survey asked what kind of service a couple used (a dating site, a phone app, a social network), but not which app. App names were asked only of the 98 people who met someone new in the past year, so they can't be linked to how a relationship is going.",
      need: "A variable recording the specific platform for each couple, enough couples per app, and ANOVA to compare more than two groups." },
    { q: "Do couples who met at work, at a bar, through friends, or online differ in relationship quality?", tag: "Collapsed categories",
      why: "On this page, how couples met is boiled down to yes/no variables (online or not, through friends or not), and a t-test only compares two groups. The full HCMST file keeps more detailed codes for where couples met.",
      need: "A several-category “where they met” variable and an ANOVA, which compares all the groups at once instead of stacking up t-tests." },
    { q: "How happy are American couples overall?", tag: "Who the sample represents",
      why: "The tests here are unweighted, and HCMST deliberately includes extra lesbian, gay, and bisexual respondents, so raw averages over-represent some groups. And a t-test compares two groups; it isn't built to estimate one national average.",
      need: "The survey weights (see “See weighted results!”) and a weighted mean with its confidence interval." },
    { q: "Did relationship quality go up or down for the same couples over time?", tag: "One time point",
      why: "Each person rated their relationship once, in 2017. Comparing different people can't show change within a couple.",
      need: "The same people asked again later (HCMST's later waves) and a paired t-test on each person's before and after ratings." },
    { q: "Do both partners rate their relationship the same way?", tag: "One partner only",
      why: "HCMST surveyed one person per couple and asked them about their partner. We never hear from the partner directly.",
      need: "Ratings from both partners, then a paired t-test comparing the two ratings within each couple." },
    { q: "Will my relationship last if I met my partner online?", tag: "Groups, not individuals",
      why: "A t-test describes group averages. Even when the averages differ, the box plots overlap a lot, so a group average says very little about any one couple.",
      need: "No survey can answer this for one person. Data on many couples over time can give odds for a group, not a prediction for you." },
    { q: "Why do same-sex couples have bigger age gaps?", tag: "“Why” questions",
      why: "The data show that the gap exists, not what causes it. Possible explanations, like a smaller pool of potential partners, aren't measured.",
      need: "Measures of the proposed reasons, or interviews asking people how they chose their partners." },
    { q: "Has online dating changed couples since 2017?", tag: "Dated snapshot",
      why: "Everything here comes from 2017. Apps, and who uses them, have changed since then, so these results may not describe couples meeting today.",
      need: "Newer survey data asked the same way, so different years can be compared." },
    { q: "Are interracial couples as happy as same-race couples?", tag: "Not in this page's data",
      why: "None of the 12 comparisons here include race. HCMST does record race for respondents and their partners, but that information isn't part of this page's data.",
      need: "An interracial-couple variable added to the data (a two-group t-test could then compare them), plus care about what any difference does and doesn't mean." }
  ];
  var limSel = -1;
  function limRender() {
    var list = $("lim-list"); list.textContent = "";
    LIMITS.forEach(function (item, i) {
      var b = el("button", { type: "button", class: "lim-q", "aria-pressed": i === limSel ? "true" : "false", "aria-controls": "lim-explain" }, [item.q]);
      if (i === limSel) b.classList.add("sel");
      b.addEventListener("click", function () {
        limSel = i; limMark(); limExplain();
        var panel = $("lim-explain"), r = panel.getBoundingClientRect();
        if (r.top > window.innerHeight || r.bottom < 0) panel.scrollIntoView({ behavior: "smooth", block: "nearest" });
      });
      list.appendChild(el("li", null, [b]));
    });
  }
  function limMark() {
    Array.prototype.forEach.call($("lim-list").querySelectorAll(".lim-q"), function (b, i) {
      b.classList.toggle("sel", i === limSel); b.setAttribute("aria-pressed", i === limSel ? "true" : "false");
    });
  }
  function limExplain() {
    var item = LIMITS[limSel], box = $("lim-explain"); box.textContent = "";
    box.appendChild(el("p", { class: "lim-title", text: item.q }));
    box.appendChild(el("p", null, [el("span", { class: "tag tip", text: item.tag })]));
    box.appendChild(el("p", null, [el("b", { text: "Why these data can't answer it: " }), item.why]));
    box.appendChild(el("p", null, [el("b", { text: "What it would take: " }), item.need]));
  }

  /* =========================================================
     Tabs, startup
     ========================================================= */
  var TABS = ["explore", "read", "write", "choose"];
  var started = {};
  function showTab(name, focus) {
    if (TABS.indexOf(name) < 0) name = "explore";
    TABS.forEach(function (t) {
      var on = t === name, btn = $("tab-" + t);
      btn.setAttribute("aria-selected", on ? "true" : "false");
      btn.tabIndex = on ? 0 : -1;
      $(t).hidden = !on;
    });
    if (!started[name]) {
      started[name] = true;
      if (name === "read") rdNew();
      if (name === "write") wrNew();
    }
    if (name === "explore") exRender(false);
    refreshConsoles();
    if (focus) $("tab-" + name).focus();
  }
  function tabsInit() {
    TABS.forEach(function (t, i) {
      var btn = $("tab-" + t);
      btn.addEventListener("click", function () {
        showTab(t);
        try { if (location.hash !== "#" + t) history.replaceState(null, "", "#" + t); } catch (e) { location.hash = t; }
      });
      btn.addEventListener("keydown", function (e) {
        var k = e.key, j = null;
        if (k === "ArrowRight") j = (i + 1) % TABS.length;
        else if (k === "ArrowLeft") j = (i - 1 + TABS.length) % TABS.length;
        else if (k === "Home") j = 0; else if (k === "End") j = TABS.length - 1;
        if (j !== null) { e.preventDefault(); showTab(TABS[j], true); try { history.replaceState(null, "", "#" + TABS[j]); } catch (er) {} }
      });
    });
    window.addEventListener("hashchange", function () { showTab(location.hash.slice(1)); });
  }

  function init() {
    exInit();
    $("rd-new").addEventListener("click", rdNew);
    wrInit();
    chInit();
    limRender();
    tabsInit();
    showTab((location.hash || "#explore").slice(1));
    var lastW = 0, lastFs = 0;
    var ro = new ResizeObserver(function () {
      var host = $("ex-box"), w = host.clientWidth, fs = parseFloat(getComputedStyle(host).fontSize);
      if (w && (w !== lastW || fs !== lastFs)) { lastW = w; lastFs = fs; exRender(false); }
    });
    ro.observe($("ex-box"));
    var rt = null;
    window.addEventListener("resize", function () { clearTimeout(rt); rt = setTimeout(refreshConsoles, 150); });
    if (document.fonts && document.fonts.ready) document.fonts.ready.then(function () { exRender(false); refreshConsoles(); });
  }
  init();

  // exposed for testing
  function modelText(item) { return [meansSentence(item.c, item.res), ciSentence(item.c, item.res), pSentence(item.c, item.res), practicalSentence(item.c, item.res), soWhat(item.c, item.res, item.full)].join(" "); }
  window.__couples = { modelText: modelText, checkWriteup: checkWriteup, makeItem: makeItem, COMPS: COMPS, GROUP_FOR: GROUP_FOR, buildQuestions: buildQuestions };
})();
