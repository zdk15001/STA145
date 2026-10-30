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
      scale: " (on a 1-to-5 scale)",
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
  // "1.138e-14" -> "0.00000000000001138", built from the printed digits so nothing is rounded away
  function expandSci(str) {
    var m = /^(\d)(?:\.(\d+))?e-(\d+)$/.exec(str); if (!m) return str;
    var digits = m[1] + (m[2] || ""), zeros = +m[3] - 1;
    return "0." + new Array(zeros + 1).join("0") + digits;
  }
  function pNote(P) {
    if (P.p.charAt(0) === "<") return "R prints “< 2.2e-16” when the p-value is smaller than it will display. It isn't zero, but it's tiny: report it as p < .001.";
    if (/e/.test(P.p)) return "R writes very small numbers in scientific notation: " + P.p + " means " + expandSci(P.p) + ".";
    return "";
  }
  function sig3(x) { return String(parseFloat(x.toPrecision(3))); }
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
  /* class wording for p-values: fairly confident below .05, otherwise not enough evidence */
  function verdict(p) {
    if (p < 0.05) return { key: "fairly", pill: "strong", short: "Fairly confident there's a difference (p < .05)" };
    return { key: "none", pill: "none", short: "Not enough evidence of a difference (p ≥ .05)" };
  }
  function pText(res) { // p as a reader would say it
    var pp = TT.printedPieces(res).p;
    if (res.p < 0.001) return "less than 0.001";
    return pp.replace(/^0\./, "0.");
  }
  function pFreq(p) {
    if (p < 1e-6) return "fewer than 1 in every million random samples";
    if (p < 0.001) return "fewer than 1 in every 1,000 random samples";
    return "about " + pctStr(p) + " of random samples";
  }
  function pctStr(p) {
    if (p < 0.001) return "less than 0.1%";
    var pct = p * 100; return String(parseFloat(pct.toPrecision(pct < 10 ? 2 : 3))) + "%";
  }

  // Significance never states a direction: the test is two-sided, so it only asks whether the averages are different.
  function differentClause(c) { return "the average " + OUT[c.y].noun + " is different for " + c.grp.A.short + " and " + c.grp.B.short; }
  function fairlyConfident(c) { return "We're fairly confident that " + differentClause(c); }
  function notEnough(c) { return "We do not have enough evidence to say that " + differentClause(c); }
  function conclusionSentence(c, res) {
    if (res.p < 0.05) return "Because p is below .05, we're fairly confident that " + differentClause(c) + ".";
    return "Because p is .05 or higher, we do not have enough evidence to say that " + differentClause(c) + ".";
  }
  // "We're 95 percent confident that, on average, X ... somewhere between L and H ... than Y."
  function ciStatement(c, res) {
    var g = c.grp, o = OUT[c.y], k = o.ci, body;
    if (res.lo > 0) body = k.pre + ", on average, somewhere between " + r2(res.lo) + " and " + r2(res.hi) + " " + o.unit + " " + k.up + k.post;
    else if (res.hi < 0) body = k.pre + ", on average, somewhere between " + r2(-res.hi) + " and " + r2(-res.lo) + " " + o.unit + " " + k.down + k.post;
    else body = k.pre + ", on average, somewhere between " + r2(-res.lo) + " " + o.unit + " " + k.down + " and " + r2(res.hi) + " " + o.unit + " " + k.up + k.post;
    return "We're 95 percent confident that, in the population, " + g.A.short + " " + body + " than " + g.B.short + ".";
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
    return (res.p < 0.05 ? fairlyConfident(c) : notEnough(c)) + " (" + pShort(res) + ").";
  }
  function pSentence(c, res) {
    var g = c.grp;
    return "The p-value is " + (res.p < 0.001 ? "less than 0.001" : pText(res)) + ". If there were really no difference between " + g.A.short + " and " + g.B.short +
      " in the population, a gap this big or bigger (in either direction) would show up in " + pFreq(res.p) + ". " + conclusionSentence(c, res);
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
        pNote(P) ? [pNote(P)] : []) };
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
    $("ex-ci-caption").textContent = "Group 2 minus group 1 (" + g.B.lab + " − " + g.A.lab + "), in " + o.unit + ", so a higher group 2 average lands to the right of 0. Drawn at twice the scale of the box plots above. The dark vertical line marks 0, no difference. R's output subtracts the other way (" + g.A.lab + " − " + g.B.lab + "), so its interval has the opposite sign.";
    var dom = $("ex-box")._domain || [0, 1];
    var flip = { diff: -res.diff, lo: -res.hi, hi: -res.lo };
    drawCI($("ex-ci"), flip, dom[1] - dom[0], "Difference in average " + o.noun + ", " + g.B.lab + " minus " + g.A.lab + " (" + o.unit + ")");
    $("ex-ci-sentence").textContent = ciStatement(c, res);

    // plain English: direction, magnitude, significance
    var box = $("ex-reading-text"); box.textContent = ""; tipState.pinned = false;
    var part = function (label, text) { box.appendChild(el("p", null, [el("b", { text: label + ": " }), text])); };
    part("Direction", directionSentence(c, res));
    var mag = el("p", null, [el("b", { text: "Magnitude: " }), magnitudeSentence(c, res) + " Is that magnitude practically significant? "]);
    var termBtn = el("button", { type: "button", class: "term", id: "ps-term", "aria-expanded": "false", "aria-describedby": "ps-tip" }, ["You tell me!"]);
    mag.appendChild(termBtn);
    box.appendChild(mag);
    box.appendChild(practicalTip(c, res, sa, sb));
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
  /* "You tell me!" tooltip: practical significance, in the workbook's language, for this comparison */
  function practicalTip(c, res, sa, sb) {
    var g = c.grp, o = OUT[c.y];
    var tip = el("div", { class: "term-tip", id: "ps-tip", role: "tooltip", hidden: "" });
    tip.appendChild(el("p", { class: "term-tip-title", text: "Practical significance" }));
    tip.appendChild(el("p", { text: "Practical significance considers whether a difference is large or meaningful enough to have real-world implications, regardless of the p-value. It addresses the question: “Is this result important in a real-world context?”" }));
    tip.appendChild(el("p", { text: "Here, ask: is a difference of " + r2(Math.abs(res.diff)) + " " + o.unit + " in the average " + o.noun + (o.scale || "") +
      ", between " + g.A.short + " and " + g.B.short + ", important in a real-world context?" }));
    tip.appendChild(el("p", { text: "To help you decide, compare it with the spread inside each group: individual people typically sit about " + r2(sa.sd) + " " + o.unit + " (" + g.A.lab.replace(/_/g, " ") +
      ") and " + r2(sb.sd) + " " + o.unit + " (" + g.B.lab.replace(/_/g, " ") + ") from their group's average. Those are the standard deviations." }));
    tip.appendChild(el("p", { class: "term-tip-note", text: "Statistical significance is a separate question: whether the difference is unlikely to be due to random chance alone. A result can be one without the other." }));
    return tip;
  }
  var tipState = { pinned: false, hideTimer: null };
  function tipEls() { return { btn: $("ps-term"), tip: $("ps-tip") }; }
  function showTip() {
    var e = tipEls(); if (!e.btn || !e.tip) return;
    clearTimeout(tipState.hideTimer);
    e.tip.hidden = false; e.btn.setAttribute("aria-expanded", "true");
    var host = e.tip.offsetParent || e.tip.parentNode, hr = host.getBoundingClientRect(), br = e.btn.getBoundingClientRect();
    var left = Math.max(8, Math.min(br.left - hr.left, hr.width - e.tip.offsetWidth - 8));
    e.tip.style.left = left + "px"; e.tip.style.top = (br.bottom - hr.top + 6) + "px";
  }
  function hideTip(force) {
    var e = tipEls(); if (!e.tip) return;
    if (tipState.pinned && !force) return;
    if (force) tipState.pinned = false;
    e.tip.hidden = true; if (e.btn) e.btn.setAttribute("aria-expanded", "false");
  }
  function laterHideTip() { clearTimeout(tipState.hideTimer); tipState.hideTimer = setTimeout(function () { hideTip(false); }, 150); }
  function tipInit() {
    var box = $("ex-reading-text");
    box.addEventListener("mouseover", function (e) { if (e.target.closest("#ps-term, #ps-tip")) showTip(); });
    box.addEventListener("mouseout", function (e) {
      var to = e.relatedTarget;
      if (e.target.closest("#ps-term, #ps-tip") && !(to && to.closest && to.closest("#ps-term, #ps-tip"))) laterHideTip();
    });
    box.addEventListener("focusin", function (e) { if (e.target.id === "ps-term") showTip(); });
    box.addEventListener("focusout", function (e) { if (e.target.id === "ps-term") laterHideTip(); });
    box.addEventListener("click", function (e) {
      if (!e.target.closest("#ps-term")) return;
      tipState.pinned = !tipState.pinned;
      if (tipState.pinned) showTip(); else hideTip(true);
    });
    document.addEventListener("keydown", function (e) { if (e.key === "Escape") hideTip(true); });
    document.addEventListener("click", function (e) { if (tipState.pinned && !e.target.closest("#ps-term, #ps-tip")) hideTip(true); });
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
    else if (sigU && !sigW) change = "Weighting changes the conclusion: with the weights, we do not have enough evidence to say that " + differentClause(c) + ".";
    else if (!sigU && sigW) change = "Weighting changes the conclusion: with the weights, we're fairly confident that " + differentClause(c) + ".";
    else change = "Weighting " + (Math.abs(Math.abs(wd) - Math.abs(u.diff)) < 0.05 * Math.abs(u.diff) ? "barely moves the estimate" : "moves the estimate " + (Math.abs(wd) < Math.abs(u.diff) ? "closer to 0" : "further from 0")) + ", but the conclusion is the same: " + (sigW ? "we're fairly confident the averages are different." : "we do not have enough evidence to say the averages are different.");
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
     Quiz myself: questions about the comparison being explored
     ========================================================= */
  function shuffle(a) {
    for (var i = a.length - 1; i > 0; i--) { var j = Math.floor(Math.random() * (i + 1)); var t = a[i]; a[i] = a[j]; a[j] = t; }
    return a;
  }
  function uniqOptions(opts) {
    var seen = {}, out = [];
    opts.forEach(function (o) { if (!seen[o.text]) { seen[o.text] = 1; out.push(o); } });
    return out;
  }
  function renderQuestion(q, label, prefix, onAnswer) {
    var fs = el("fieldset", { class: "q" });
    fs.appendChild(el("legend", null, [el("span", { class: "qnum", text: label }), q.prompt]));
    var fb = el("div", { class: "fb", role: "status", hidden: "" });
    q.options.forEach(function (o, j) {
      var id = prefix + "-" + j;
      var inp = el("input", { type: "radio", name: prefix, id: id, value: String(j) });
      var lab = el("label", { class: "choice", for: id }, [inp, el("span", { text: o.text })]);
      inp.addEventListener("change", function () {
        Array.prototype.forEach.call(fs.querySelectorAll(".choice"), function (l) { l.classList.remove("is-right", "is-wrong"); });
        lab.classList.add(o.ok ? "is-right" : "is-wrong");
        fb.hidden = false; fb.className = "fb " + (o.ok ? "right" : "wrong"); fb.textContent = "";
        fb.appendChild(el("strong", { text: o.ok ? "Correct." : "Not quite." }));
        fb.appendChild(document.createTextNode(o.ok ? q.explain : "Try another answer, or use this hint: " + q.explain));
        if (onAnswer) onAnswer(!!o.ok);
      });
      fs.appendChild(lab);
    });
    fs.appendChild(fb);
    return fs;
  }

  var QZ_KEYS = ["mean", "median", "spread", "higher", "pval", "pmean", "order", "zero", "signif", "ci", "normal", "welch"];
  function quizQuestion(key) {
    var c = ex.c, o = OUT[c.y], g = c.grp, res = ex.res, P = TT.printedPieces(res), sa = ex.stats[0], sb = ex.stats[1];
    var askB = Math.random() < 0.5, G = askB ? g.B : g.A, sG = askB ? sb : sa, sO = askB ? sa : sb;
    var mG = askB ? P.mB : P.mA, mO = askB ? P.mA : P.mB;
    var includes = res.lo <= 0 && res.hi >= 0;
    switch (key) {
      case "mean": return { prompt: "In the R output, what is the average " + o.noun + " for " + G.who + "?",
        options: [{ text: mG, ok: true }, { text: mO }, { text: P.hi }, { text: P.t }],
        explain: "Under “sample estimates,” “mean in group " + G.lab + "” is " + mG + "." };
      case "median": return { prompt: "According to the summary table, what is the median " + o.noun + " for " + G.who + "?",
        options: [{ text: r2(sG.med), ok: true }, { text: r2(sG.mean) }, { text: r2(sG.q1) }, { text: r2(sG.q3) }, { text: r2(sO.med) }],
        explain: "The median is the middle answer once everyone is sorted: half are at or below it. For " + G.lab + " it's " + r2(sG.med) + " (the mean is " + r2(sG.mean) + ")." +
          (sG.med === sG.q3 ? " It equals Q3 because so many people gave the same answer, so on the box plot the median line sits on the box's right edge."
            : sG.med === sG.q1 ? " It equals Q1 because so many people gave the same answer, so on the box plot the median line sits on the box's left edge."
            : " On the box plot, it's the dark line inside the box.") };
      case "spread": {
        var tie = r2(sa.sd) === r2(sb.sd), aBig = sa.sd > sb.sd, bigG = aBig ? g.A : g.B;
        var boxSame = r2(sa.q1) === r2(sb.q1) && r2(sa.q3) === r2(sb.q3);
        return { prompt: "According to the summary table, which group has the larger standard deviation (SD)?", fixed: true,
          options: [{ text: g.A.lab, ok: !tie && aBig }, { text: g.B.lab, ok: !tie && !aBig }].concat(tie ? [{ text: "They're the same", ok: true }] : []),
          explain: "Read the SD column: " + r2(sa.sd) + " for " + g.A.lab + " and " + r2(sb.sd) + " for " + g.B.lab + "." +
            (tie ? " They're the same to two decimals." : " So the answers of " + bigG.short + " are more spread out around their average.") +
            (boxSame ? " The boxes on the box plot look the same because the box shows a different measure of spread, the middle half of the answers (the IQR), which is the same for both groups." : "") +
            " R's Welch test doesn't need the SDs to be equal." };
      }
      case "higher": {
        var hiG = res.diff >= 0 ? g.A : g.B, loG = res.diff >= 0 ? g.B : g.A;
        var d = r2(Math.abs(res.diff)), far = r2(Math.max(Math.abs(res.lo), Math.abs(res.hi))), near = r2(Math.min(Math.abs(res.lo), Math.abs(res.hi)));
        return { prompt: "In this sample, which group has the higher average " + o.noun + ", and by about how much?",
          options: [{ text: hiG.lab + ", by about " + d + " " + o.unit, ok: true }, { text: loG.lab + ", by about " + d + " " + o.unit },
            { text: hiG.lab + ", by about " + far + " " + o.unit }, { text: loG.lab + ", by about " + near + " " + o.unit }],
          explain: P.mA + " − " + P.mB + " ≈ " + r2(res.diff) + ", so in this sample " + hiG.lab + " is higher by about " + d + " " + o.unit + "." +
            (res.p >= 0.05 ? " That's only true of this sample: with p = " + TT.formatPval(res.p, 4) + " and a confidence interval that includes 0, we do not have enough evidence to say the averages are different in the population." : "") +
            " The ends of the confidence interval are a range of plausible values for the true difference, not the sample difference itself." };
      }
      case "pval": {
        var note = pNote(P) ? " " + pNote(P) : "";
        return { prompt: "Which number in the R output is the p-value?",
          options: [{ text: P.p.charAt(0) === "<" ? P.p + "  (less than 2.2e-16)" : P.p, ok: true }, { text: P.t }, { text: P.df }, { text: P.lo }],
          explain: "It's on the third line, right after “p-value.” The others are the t statistic, the degrees of freedom (df), and the lower end of the confidence interval." + note };
      }
      case "pmean": return { prompt: "What does the p-value (" + pShort(res) + ") mean here?",
        options: [{ text: "If there were really no difference between these groups in the population, a gap this big or bigger (in either direction) would show up in " + pFreq(res.p) + ".", ok: true },
          { text: "There is " + (res.p < 0.001 ? "less than a 0.1%" : "a " + pctStr(res.p)) + " chance that there's no difference between the groups." },
          { text: "There is " + (res.p < 0.001 ? "more than a 99.9%" : "a " + pctStr(1 - res.p)) + " chance that the hypothesis is true." },
          { text: "The two groups' averages differ by " + P.p + " " + o.unit + "." }],
        explain: "That's the definition from 3.2: the probability of a result at least this extreme if there were truly no difference. Because the test is two-sided, a gap in either direction counts. It is not the chance that either hypothesis is true." };
      case "order": return { prompt: "The confidence interval in the R output estimates which difference?",
        options: [{ text: "The true (population) average for " + g.A.lab + " minus the true average for " + g.B.lab, ok: true },
          { text: "The true (population) average for " + g.B.lab + " minus the true average for " + g.A.lab },
          { text: "The median for " + g.A.lab + " minus the median for " + g.B.lab },
          { text: "The range that 95% of individual answers fall in" }],
        explain: "R puts the groups in alphabetical order, so “" + g.A.lab + "” comes first and the t statistic and confidence interval are both " + g.A.lab + " minus " + g.B.lab + ". The sample difference is " + P.mA + " − " + P.mB + " ≈ " + r2(res.diff) + "; the interval is a range of plausible values for the true difference." };
      case "zero": return { prompt: "Does the 95% confidence interval include 0?", fixed: true,
        options: [{ text: "Yes", ok: includes }, { text: "No", ok: !includes }],
        explain: "The interval runs from " + P.lo + " to " + P.hi + ". " + (includes
          ? "It crosses 0, so “no difference” is still a plausible value. That matches a p-value above .05."
          : "Both ends are on the same side of 0, so “no difference” isn't a plausible value. That matches a p-value below .05.") };
      case "signif": return { prompt: "Which conclusion fits this p-value (" + pShort(res) + ")?", fixed: true,
        options: [{ text: fairlyConfident(c) + ".", ok: res.p < 0.05 },
          { text: notEnough(c) + ".", ok: res.p >= 0.05 },
          { text: "This proves that " + differentClause(c) + "." },
          { text: "This shows that the average " + o.noun + " is the same for " + g.A.short + " and " + g.B.short + "." }],
        explain: (res.p < 0.05
          ? "p is below .05, so we're fairly confident the averages are different. The test is two-sided: it asks whether the groups are different, not which one is higher. The means and the confidence interval show the direction."
          : "p is .05 or higher, so we do not have enough evidence to say the averages are different. That isn't the same as showing they're the same.") +
          " A t-test can't prove anything, and it can never show that the groups are the same." };
      case "ci": return { prompt: "Which sentence correctly interprets the 95% confidence interval?",
        options: [{ text: ciStatement(c, res), ok: true },
          { text: "95% of individual people differ by somewhere between " + r2(res.lo) + " and " + r2(res.hi) + " " + o.unit + "." },
          { text: "There's a 95% chance that the difference in this sample is between " + r2(res.lo) + " and " + r2(res.hi) + " " + o.unit + "." },
          { text: "The difference between the groups in the population is exactly " + r2(res.diff) + " " + o.unit + "." }],
        explain: "The interval is about the true average difference in the population. It isn't about individual people, and it isn't about this sample, whose difference we already know (" + r2(res.diff) + ")." };
      case "normal": {
        var nmin = Math.min(sa.n, sb.n), kmax = Math.max(Math.abs(skewness(ex.a)), Math.abs(skewness(ex.b))), big = nmin >= 30;
        return { prompt: "Is normality a concern for this t-test?",
          options: [{ text: "No. Each group has at least 30 people, so by the Central Limit Theorem the sampling distribution of each group's average is close to normal, even if individual answers are skewed.", ok: big },
            { text: "Yes. With fewer than 30 people in a group, the t-test relies on the answers themselves being roughly normal.", ok: !big },
            { text: "Yes. Normality requires the two groups to be the same size." },
            { text: "No. Normality only matters for chi-square tests." }],
          explain: "Here the groups have " + fmtInt(sa.n) + " and " + fmtInt(sb.n) + " people." +
            (big ? " The usual rule of thumb is at least 30 per group." + (kmax > 2
                ? " These answers are very skewed (skewness " + kmax.toFixed(1) + "), and very skewed variables need more than 30, but groups this large are plenty."
                : " With groups this large, the averages behave normally even though individual answers aren't perfectly normal.")
              : " That's fewer than 30, so check whether the answers in each group look roughly normal.") +
            " Chi-square tests don't assume normality at all. See “Checking the assumptions” on the Explore tab." };
      }
      case "welch": return { prompt: "Why does the R output say “Welch Two Sample t-test”?",
        options: [{ text: "R's default t-test doesn't assume the two groups are equally spread out.", ok: true },
          { text: "The data were weighted to match the US population." },
          { text: "The two groups have the same number of people." },
          { text: "Welch is the name of the survey." }],
        explain: "Welch's version allows unequal standard deviations, so the equal-variances assumption is handled. It also adjusts the degrees of freedom for that, which is why df (" + P.df + ") isn't a whole number. Here the SDs are " + r2(sa.sd) + " and " + r2(sb.sd) + "." };
    }
    return null;
  }

  var qz = { cid: null, order: [], pos: 0, round: 1, answered: 0, right: 0, q: null };
  function qzReset() {
    qz.cid = ex.c.id; qz.order = shuffle(QZ_KEYS.slice()); qz.pos = 0; qz.round = 1; qz.answered = 0; qz.right = 0; qz.q = null;
  }
  function qzTable(sa, sb) {
    var g = ex.c.grp, tbl = $("qz-table"); tbl.textContent = "";
    var head = el("tr", null, ["Group", "n", "Mean", "SD", "Q1", "Median", "Q3"].map(function (h) { return el("th", { scope: "col", text: h }); }));
    var body = el("tbody");
    [[g.A.lab, "var(--series-a)", sa], [g.B.lab, "var(--series-b)", sb]].forEach(function (r) {
      var s = r[2];
      body.appendChild(el("tr", null, [el("th", { scope: "row" }, [el("span", { class: "swatch", style: "background:" + r[1] }), r[0]]),
        el("td", { text: fmtInt(s.n) }), el("td", { text: r2(s.mean) }), el("td", { text: r2(s.sd) }),
        el("td", { text: r2(s.q1) }), el("td", { text: r2(s.med) }), el("td", { text: r2(s.q3) })]));
    });
    tbl.appendChild(el("thead", null, [head])); tbl.appendChild(body);
  }
  function qzRender() {
    if (qz.cid !== ex.c.id) qzReset();
    var c = ex.c;
    $("qz-comp").value = c.id;
    $("qz-rq").textContent = c.rq;
    $("qz-who").textContent = "Who's included: " + c.pop + " Everyone in HCMST with answers to both questions.";
    renderConsole($("qz-out"), c, ex.res, true, false);
    qzTable(ex.stats[0], ex.stats[1]);
    qzQuestion();
  }
  function qzQuestion() {
    var key = qz.order[qz.pos], q = quizQuestion(key);
    q.options = uniqOptions(q.options);
    if (!q.fixed) shuffle(q.options);
    qz.q = q;
    $("qz-progress").textContent = "Question " + (qz.pos + 1) + " of " + qz.order.length + (qz.round > 1 ? " · round " + qz.round : "");
    var box = $("qz-card"), first = true; box.textContent = "";
    box.appendChild(renderQuestion(q, (qz.pos + 1) + ".", "qz-" + qz.round + "-" + qz.pos, function (ok) {
      if (!first) return;
      first = false; qz.answered++; if (ok) qz.right++;
      $("qz-score").textContent = qz.right + " of " + qz.answered + " right on the first try for this comparison.";
    }));
    $("qz-next").textContent = qz.pos === qz.order.length - 1 ? "Start over with a new order" : "Next question";
  }
  function qzInit() {
    var sel = $("qz-comp");
    Array.prototype.forEach.call($("ex-comp").children, function (node) { sel.appendChild(node.cloneNode(true)); });
    sel.addEventListener("change", function () {
      ex.c = BY_ID[this.value]; $("ex-comp").value = this.value;
      exDraw(); exRender(false); qzReset(); qzRender(); $("qz-score").textContent = "";
    });
    $("qz-next").addEventListener("click", function () {
      qz.pos++;
      if (qz.pos >= qz.order.length) { qz.pos = 0; qz.round++; qz.order = shuffle(qz.order); }
      qzQuestion();
      var lg = document.querySelector("#qz-card legend"); if (lg) lg.scrollIntoView({ behavior: "smooth", block: "nearest" });
    });
  }

  /* =========================================================
     Tabs
     ========================================================= */
  var TABS = ["explore", "quiz"];
  function showTab(name, focus) {
    if (TABS.indexOf(name) < 0) name = "explore";
    TABS.forEach(function (t) {
      var on = t === name, btn = $("tab-" + t);
      btn.setAttribute("aria-selected", on ? "true" : "false");
      btn.tabIndex = on ? 0 : -1;
      $(t).hidden = !on;
    });
    if (name === "explore") exRender(false);
    if (name === "quiz") qzRender();
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

  /* =========================================================
     Startup
     ========================================================= */
  function init() {
    exInit();
    tipInit();
    qzInit();
    limRender();
    tabsInit();
    exRender(false); // the quiz reads the explore results, so compute them even if the page opens on #quiz
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
  window.__couples = { COMPS: COMPS, GROUP_FOR: GROUP_FOR };
})();
