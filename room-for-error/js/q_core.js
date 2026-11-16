/* Question engine core: random helpers, question builders, small chart figures. */
const QZ = (() => {
  const rnd = (a, b) => Math.floor(Math.random() * (b - a + 1)) + a;
  const pick = (a) => a[Math.floor(Math.random() * a.length)];
  const shuffle = (a) => { a = a.slice(); for (let i = a.length - 1; i > 0; i--) { const j = Math.floor(Math.random() * (i + 1)); [a[i], a[j]] = [a[j], a[i]]; } return a; };
  const sample = (a, k) => shuffle(a).slice(0, k);
  const sum = (a) => a.reduce((s, v) => s + v, 0);
  const mean = (a) => sum(a) / a.length;
  const r2 = (x, d = 2) => { const p = Math.pow(10, d); return Math.round((x + Number.EPSILON) * p) / p; };
  const fmt = (x, d = 2) => { const v = r2(x, d); return String(v); };
  const money = (x) => '$' + Number(x).toLocaleString('en-US', { minimumFractionDigits: Number.isInteger(x) ? 0 : 2, maximumFractionDigits: 2 });
  const commas = (x) => Number(x).toLocaleString('en-US');
  const NAMES = ['Maya', 'Jalen', 'Sofia', 'Arjun', 'Chloe', 'Diego', 'Amara', 'Noah', 'Priya', 'Elijah', 'Mei', 'Tariq', 'Leila', 'Owen', 'Imani', 'Lucas'];

  /* Builders. A generator returns one of these "raw" questions; finalize() turns it into plain JSON for the app. */
  const mc = (q, right, wrongs, why, x = {}) => ({ type: 'mc', q, right, wrongs, why, ...x });
  /* cat: options are a fixed, ordered label set (not shuffled) */
  const cat = (q, right, labels, why, x = {}) => ({ type: 'mc', q, right, wrongs: labels.filter((l) => l !== right), order: labels, why, ...x });
  const num = (q, ans, why, x = {}) => ({ type: 'num', q, ans, why, tol: 0.011, ...x });

  function finalize(raw) {
    const out = { type: raw.type, q: raw.q, why: raw.why };
    for (const k of ['table', 'code', 'svg', 'hint', 'mono', 'unit']) if (raw[k] !== undefined) out[k] = raw[k];
    if (raw.type === 'mc') {
      const opts = raw.order ? raw.order.slice() : shuffle([raw.right, ...raw.wrongs]);
      out.opts = opts.map(String);
      out.ans = out.opts.indexOf(String(raw.right));
    } else {
      out.ans = raw.ans; out.tol = raw.tol;
    }
    return out;
  }

  /* ---------- Figures (inline SVG, themed through CSS classes) ---------- */
  const esc = (s) => String(s).replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;').replace(/"/g, '&quot;');

  function barChart(labels, vals, o = {}) {
    const W = 340, H = 200, L = 36, B = 44, T = 12, R = 8;
    const step = o.step || 5, ymax = o.ymax || Math.ceil(Math.max(...vals) / (step * 2)) * step * 2;
    const pw = W - L - R, ph = H - T - B, bw = pw / labels.length;
    const y = (v) => T + ph - (v / ymax) * ph;
    let s = `<svg class="fig" viewBox="0 0 ${W} ${H}" role="img" aria-label="${esc('Bar chart. ' + (o.ylab || 'Count') + ' by category. ' + labels.map((l, i) => l + ': ' + vals[i]).join(', ') + '.')}">`;
    for (let v = 0; v <= ymax; v += step) {
      const major = v % (step * 2) === 0;
      s += `<line class="${major ? 'grid' : 'grid minor'}" x1="${L}" x2="${W - R}" y1="${y(v)}" y2="${y(v)}"/>`;
      if (major) s += `<text class="tick" x="${L - 6}" y="${y(v) + 4}" text-anchor="end">${v}</text>`;
    }
    labels.forEach((l, i) => {
      const x = L + i * bw + bw * 0.18, w = bw * 0.64;
      s += `<rect class="bar" x="${x}" y="${y(vals[i])}" width="${w}" height="${T + ph - y(vals[i])}" rx="2"/>`;
      s += `<text class="tick" x="${x + w / 2}" y="${H - B + 16}" text-anchor="middle">${esc(l)}</text>`;
    });
    s += `<line class="ax" x1="${L}" x2="${W - R}" y1="${T + ph}" y2="${T + ph}"/>`;
    s += `<text class="lab" x="${(L + W - R) / 2}" y="${H - 6}" text-anchor="middle">${esc(o.xlab || '')}</text>`;
    return s + '</svg>';
  }

  function histShape(h) {
    const W = 300, H = 150, L = 14, B = 22, T = 8, R = 8, mx = Math.max(...h);
    const pw = W - L - R, ph = H - T - B, bw = pw / h.length;
    let s = `<svg class="fig" viewBox="0 0 ${W} ${H}" role="img" aria-label="${esc('Histogram with ' + h.length + ' bars. Bar heights from left to right: ' + h.join(', ') + '.')}">`;
    h.forEach((v, i) => { const bh = (v / mx) * ph; s += `<rect class="bar" x="${L + i * bw + 1}" y="${T + ph - bh}" width="${bw - 2}" height="${bh}"/>`; });
    s += `<line class="ax" x1="${L}" x2="${W - R}" y1="${T + ph}" y2="${T + ph}"/>`;
    s += `<text class="lab" x="${L}" y="${H - 5}">low values</text><text class="lab" x="${W - R}" y="${H - 5}" text-anchor="end">high values</text>`;
    return s + '</svg>';
  }

  function boxPlot(f, o) { /* f = [min,q1,med,q3,max]; o = {lo,hi,step,label} */
    const W = 340, H = 120, L = 16, R = 16, pw = W - L - R;
    const x = (v) => L + ((v - o.lo) / (o.hi - o.lo)) * pw, cy = 42, bh = 34;
    let s = `<svg class="fig" viewBox="0 0 ${W} ${H}" role="img" aria-label="${esc('Box plot of ' + o.label + '. Minimum ' + f[0] + ', first quartile ' + f[1] + ', median ' + f[2] + ', third quartile ' + f[3] + ', maximum ' + f[4] + '.')}">`;
    for (let v = o.lo; v <= o.hi + 1e-9; v += o.step) {
      s += `<line class="grid" x1="${x(v)}" x2="${x(v)}" y1="14" y2="78"/><text class="tick" x="${x(v)}" y="94" text-anchor="middle">${v}</text>`;
    }
    s += `<line class="whisk" x1="${x(f[0])}" x2="${x(f[1])}" y1="${cy}" y2="${cy}"/><line class="whisk" x1="${x(f[3])}" x2="${x(f[4])}" y1="${cy}" y2="${cy}"/>`;
    s += `<line class="whisk" x1="${x(f[0])}" x2="${x(f[0])}" y1="${cy - 9}" y2="${cy + 9}"/><line class="whisk" x1="${x(f[4])}" x2="${x(f[4])}" y1="${cy - 9}" y2="${cy + 9}"/>`;
    s += `<rect class="box" x="${x(f[1])}" y="${cy - bh / 2}" width="${x(f[3]) - x(f[1])}" height="${bh}" rx="2"/>`;
    s += `<line class="med" x1="${x(f[2])}" x2="${x(f[2])}" y1="${cy - bh / 2}" y2="${cy + bh / 2}"/>`;
    s += `<line class="ax" x1="${L}" x2="${W - R}" y1="78" y2="78"/><text class="lab" x="${W / 2}" y="114" text-anchor="middle">${esc(o.label)}</text>`;
    return s + '</svg>';
  }

  function scatter(pts, o) { /* pts in 0..1 unit square */
    const W = 300, H = 200, L = 30, B = 30, T = 10, R = 10, pw = W - L - R, ph = H - T - B;
    let s = `<svg class="fig" viewBox="0 0 ${W} ${H}" role="img" aria-label="${esc('Scatterplot of ' + o.y + ' against ' + o.x + '. ' + o.alt)}">`;
    s += `<line class="ax" x1="${L}" x2="${W - R}" y1="${T + ph}" y2="${T + ph}"/><line class="ax" x1="${L}" x2="${L}" y1="${T}" y2="${T + ph}"/>`;
    pts.forEach((p) => { s += `<circle class="pt" cx="${r2(L + p[0] * pw, 1)}" cy="${r2(T + ph - p[1] * ph, 1)}" r="3.4"/>`; });
    s += `<text class="lab" x="${L + pw / 2}" y="${H - 8}" text-anchor="middle">${esc(o.x)}</text>`;
    s += `<text class="lab" transform="translate(12 ${T + ph / 2}) rotate(-90)" text-anchor="middle">${esc(o.y)}</text>`;
    return s + '</svg>';
  }

  /* gaussian-ish noise from uniform draws */
  const noise = () => (Math.random() + Math.random() + Math.random() + Math.random() - 2) / 2;

  const LEVELS = [];
  const level = (meta, gens) => { LEVELS.push({ ...meta, n: LEVELS.length + 1, gens }); };

  /* Difficulty tier of every template within its own level, in pool order: 1 = foundational, 2 = middle, 3 = hardest. */
  const TIERS = {
    1: [1, 2, 3, 1, 1, 2, 3, 2, 1], 2: [1, 1, 2, 1, 3, 2, 3, 1, 2], 3: [1, 2, 3, 2, 1, 1, 1, 2, 3], 4: [3, 2, 2, 3, 1, 1, 1, 1, 2, 2],
    5: [1, 1, 3, 1, 2, 2, 3, 1, 2, 2, 1], 6: [2, 2, 1, 2, 1, 3, 1, 1, 3, 1, 2, 1], 7: [1, 2, 3, 1, 2, 1, 2, 1, 2, 1, 3, 2], 8: [1, 1, 3, 2, 1, 3, 1, 2, 3, 2, 1],
    9: [2, 1, 3, 2, 1, 2, 1, 2, 1, 3, 1], 10: [1, 2, 1, 1, 2, 3, 2, 3, 1, 1, 2, 1], 11: [1, 2, 2, 1, 3, 2, 1, 3, 1, 2], 12: [2, 3, 3, 2, 2, 3, 1, 2, 1, 1, 1, 1],
    13: [3, 2, 1, 1, 2, 3, 2, 1, 1, 3, 3], 14: [1, 2, 2, 1, 3, 2, 3, 2, 2, 1, 1, 1, 1], 15: [2, 3, 1, 3, 2, 1, 1, 2, 1, 2, 1], 16: [2, 2, 3, 2, 2, 3, 1, 2, 3, 1, 2, 1, 1],
    17: [1, 3, 1, 1, 2, 2, 2, 3, 1, 2, 2, 3, 1], 18: [1, 2, 2, 1, 2, 2, 3, 1, 2, 3, 1, 1, 2], 19: [1, 2, 1, 1, 3, 2, 3, 2, 2, 1, 3, 2], 20: [1, 2, 1, 3, 2, 3, 2, 2, 2, 2, 2, 1, 1, 2]
  };
  /* How many [foundational, middle, hardest] questions a set holds at each ease step (0 = standard set). */
  const MIX = [[1, 2, 2], [2, 2, 1], [3, 2, 0], [4, 1, 0]];
  const MAX_EASE = MIX.length - 1;
  /* From ease step 2 on, a multiple-choice question loses one wrong answer (never below three choices). */
  function trim(q) {
    if (q.type !== 'mc' || q.opts.length < 4) return q;
    const drop = pick(q.opts.map((_, i) => i).filter((i) => i !== q.ans));
    q.opts.splice(drop, 1); if (drop < q.ans) q.ans--;
    return q;
  }
  /* Draw a challenge: k different templates from a level's pool. Each failed attempt raises `ease` by one step. */
  function draw(n, k = 5, ease = 0) {
    const L = LEVELS[n - 1], tiers = TIERS[n] || [], e = Math.max(0, Math.min(MAX_EASE, Math.floor(ease) || 0));
    const pools = [1, 2, 3].map((t) => shuffle(L.gens.map((_, i) => i).filter((i) => (tiers[i] || 2) === t)));
    const want = MIX[e].slice(), chosen = [];
    [0, 1, 2].forEach((t) => { while (want[t] > 0 && pools[t].length && chosen.length < k) { chosen.push([pools[t].pop(), t + 1]); want[t]--; } });
    (e === 0 ? [2, 1, 0] : [0, 1, 2]).forEach((t) => { while (chosen.length < k && pools[t].length) chosen.push([pools[t].pop(), t + 1]); });
    chosen.sort((a, b) => a[1] - b[1]); /* warm up: foundational questions come first */
    return chosen.map(([i, t]) => { const q = finalize(L.gens[i]()); q.tier = t; return e >= 2 ? trim(q) : q; });
  }

  /* Check a typed numeric answer. Accepts "1,250", "$3.50", "45%", "-0.5", ".25". */
  function parseNum(s) {
    const t = String(s).trim().replace(/[\s,$%]/g, '').replace(/^\+/, '').replace(/−/g, '-');
    if (!/^-?(\d+\.?\d*|\.\d+)$/.test(t)) return null;
    return parseFloat(t);
  }
  const checkNum = (q, s) => { const v = parseNum(s); return v !== null && Math.abs(v - q.ans) <= q.tol + 1e-9; };

  return { rnd, pick, shuffle, sample, sum, mean, r2, fmt, money, commas, NAMES, mc, cat, num, finalize, barChart, histShape, boxPlot, scatter, noise, LEVELS, TIERS, MIX, MAX_EASE, level, draw, trim, parseNum, checkNum, esc };
})();
if (typeof module !== 'undefined') module.exports = QZ;
