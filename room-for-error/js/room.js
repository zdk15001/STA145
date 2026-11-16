/* The bedroom: one SVG scene, 20 decoration slots (one per level), three choices each. */
const ROOM = (() => {
  const Y = '#FFD84A', C = '#F2647A', TL = '#1FA79A', B = '#2F5BEA', V = '#7A5AF8', O = '#F59E42', G = '#3FA66B', INK = '#1B2440', W = '#FFFFFF';
  const WALL = '#DDE4F6', FLOOR = '#CDA77F', WOOD = '#A8744A', WOOD2 = '#8B5E3A';
  const t = (x, y, s, size, fill, extra = '') => `<text x="${x}" y="${y}" font-size="${size}" fill="${fill}" text-anchor="middle" font-weight="800" ${extra}>${s}</text>`;
  const bell = (x0, x1, yb, yt) => { const w = x1 - x0, m = (x0 + x1) / 2; return `M${x0},${yb} C${x0 + w * 0.3},${yb} ${m - w * 0.16},${yt} ${m},${yt} C${m + w * 0.16},${yt} ${x1 - w * 0.3},${yb} ${x1},${yb}`; };
  const lcg = (seed) => () => (seed = (seed * 1664525 + 1013904223) % 4294967296) / 4294967296;
  const star = (cx, cy, r) => { let p = ''; for (let i = 0; i < 10; i++) { const a = -Math.PI / 2 + i * Math.PI / 5, rr = i % 2 ? r * 0.45 : r; p += (i ? 'L' : 'M') + (cx + rr * Math.cos(a)).toFixed(1) + ',' + (cy + rr * Math.sin(a)).toFixed(1); } return p + 'Z'; };
  const pieSlices = (cx, cy, r, cuts, cols) => { let s = '', a0 = -Math.PI / 2; cuts.forEach((f, i) => { const a1 = a0 + f * 2 * Math.PI; s += `<path d="M${cx},${cy} L${(cx + r * Math.cos(a0)).toFixed(1)},${(cy + r * Math.sin(a0)).toFixed(1)} A${r},${r} 0 ${f > 0.5 ? 1 : 0} 1 ${(cx + r * Math.cos(a1)).toFixed(1)},${(cy + r * Math.sin(a1)).toFixed(1)} Z" fill="${cols[i]}"/>`; a0 = a1; }); return s; };
  const wire = 'M10,8 Q67,30 125,10 Q182,30 240,10 Q297,30 355,10 Q412,30 470,8';
  const wireY = (x) => { const seg = [[10, 125, 8, 10], [125, 240, 10, 10], [240, 355, 10, 10], [355, 470, 10, 8]].find((s) => x <= s[1]) || [355, 470, 10, 8]; const u = (x - seg[0]) / (seg[1] - seg[0]); return (1 - u) * (1 - u) * seg[2] + 2 * u * (1 - u) * 30 + u * u * seg[3]; };
  const duvet = (fill) => `<path d="M62,196 Q62,188 72,188 H198 V232 Q130,242 62,232 Z" fill="${fill}" stroke="${INK}" stroke-opacity=".18"/>`;
  const frame = (x, y, w, h, fill) => `<rect x="${x}" y="${y}" width="${w}" height="${h}" rx="2" fill="${fill}" stroke="${W}" stroke-width="3"/><rect x="${x - 1.5}" y="${y - 1.5}" width="${w + 3}" height="${h + 3}" rx="3" fill="none" stroke="${INK}" stroke-opacity=".25"/>`;
  const mug = (fill, txt, tc) => `<path d="M224,197 q7,0 7,4.5 t-7,4.5" fill="none" stroke="${fill === W ? '#C9CEDD' : fill}" stroke-width="2"/><rect x="211" y="193" width="14" height="13" rx="2" fill="${fill}" stroke="${INK}" stroke-opacity=".25"/>${t(218, 203, txt, 8, tc)}`;
  const cup = (fill, txt) => `<path d="M444,104 h16 v6 a8,8 0 0 1 -16,0 Z" fill="${fill}" stroke="${INK}" stroke-opacity=".3"/><path d="M444,106 q-5,0 -4,5 q1,4 5,4 M460,106 q5,0 4,5 q-1,4 -5,4" fill="none" stroke="${fill}" stroke-width="2"/><rect x="450.5" y="117" width="3" height="5" fill="${fill}"/><rect x="445" y="122" width="14" height="6" rx="1" fill="${INK}"/>${t(452, 114, txt, 8, INK)}`;
  const neon = (txt, col, size) => `<rect x="198" y="30" width="84" height="24" rx="6" fill="${INK}"/><text x="240" y="47" font-size="${size}" text-anchor="middle" font-weight="800" fill="none" stroke="${col}" stroke-width="5" stroke-opacity=".28" stroke-linejoin="round">${txt}</text>${t(240, 47, txt, size, col)}`;

  const SLOTS = [
    { name: 'Wallpaper', bg: WALL, box: [16, 30, 168, 112], opts: [
      { name: 'Graph Paper Wallpaper', d: () => { let s = ''; for (let x = 0; x <= 480; x += 16) s += `<line x1="${x}" y1="0" x2="${x}" y2="226" stroke="#B9C4E8" stroke-width="${x % 80 ? 0.6 : 1.3}"/>`; for (let y = 2; y <= 226; y += 16) s += `<line x1="0" y1="${y}" x2="480" y2="${y}" stroke="#B9C4E8" stroke-width="${(y - 2) % 80 ? 0.6 : 1.3}"/>`; return s; } },
      { name: 'Scatterplot Speckle Wallpaper', d: () => { const r = lcg(145); let s = ''; for (let i = 0; i < 130; i++) s += `<circle cx="${(r() * 480).toFixed(0)}" cy="${(r() * 222).toFixed(0)}" r="${(1.6 + r() * 1.6).toFixed(1)}" fill="${[B, C, Y, TL][i % 4]}" opacity=".5"/>`; return s; } },
      { name: 'Bell Curve Wallpaper', d: () => { let s = ''; for (let row = 0; row < 5; row++) for (let x = -30 + (row % 2) * 30; x < 480; x += 60) s += `<path d="${bell(x, x + 52, 44 + row * 44, 18 + row * 44)}" fill="none" stroke="${V}" stroke-opacity=".38" stroke-width="1.6"/>`; return s; } }
    ] },
    { name: 'Rug', bg: FLOOR, box: [140, 258, 220, 66], opts: [
      { name: 'Pie Chart Rug', d: () => `<g transform="translate(250 294) scale(1 .27)">${pieSlices(0, 0, 94, [0.4, 0.25, 0.2, 0.15], [C, Y, TL, B])}<circle r="94" fill="none" stroke="${W}" stroke-width="5"/></g>` },
      { name: 'Histogram Runner', d: () => { let s = `<rect x="158" y="270" width="184" height="48" rx="5" fill="#F7F3EA" stroke="${INK}" stroke-opacity=".2"/>`; [8, 16, 26, 36, 40, 34, 24, 14, 8].forEach((h, i) => { s += `<rect x="${168 + i * 18.5}" y="${312 - h}" width="15" height="${h}" fill="${i % 2 ? B : TL}"/>`; }); return s; } },
      { name: 'Normal Curve Rug', d: () => `<path d="${bell(146, 354, 318, 262)} Z" fill="${V}"/><path d="${bell(160, 340, 314, 272)}" fill="none" stroke="${W}" stroke-width="2" stroke-dasharray="5 4"/><line x1="250" y1="268" x2="250" y2="316" stroke="${Y}" stroke-width="2.5"/>` }
    ] },
    { name: 'Bedding', bg: WALL, box: [52, 176, 156, 70], opts: [
      { name: 'Scatterplot Duvet', d: () => { const r = lcg(7); let s = duvet('#FFF7DC'); for (let i = 0; i < 22; i++) { const x = 72 + r() * 118, y = 226 - (x - 72) * 0.24 + (r() - 0.5) * 12; s += `<circle cx="${x.toFixed(0)}" cy="${y.toFixed(0)}" r="2.6" fill="${B}"/>`; } return s + `<line x1="70" y1="227" x2="192" y2="197" stroke="${C}" stroke-width="2.2"/>`; } },
      { name: 'Histogram Quilt', d: () => { let s = duvet('#BFE9E3'); [6, 12, 20, 28, 32, 26, 18, 10, 5].forEach((h, i) => { s += `<rect x="${70 + i * 14}" y="${230 - h}" width="11" height="${h}" fill="${TL}"/>`; }); return s; } },
      { name: 'Error Bar Blanket', d: () => { let s = duvet('#DDD5FD'); [[78, 212], [100, 204], [122, 216], [144, 202], [166, 210], [186, 200]].forEach(([x, y]) => { s += `<path d="M${x},${y - 9} V${y + 9} M${x - 4},${y - 9} H${x + 4} M${x - 4},${y + 9} H${x + 4}" stroke="${INK}" stroke-width="1.8" fill="none"/><circle cx="${x}" cy="${y}" r="3" fill="${V}"/>`; }); return s; } }
    ] },
    { name: 'Poster over the bed', bg: WALL, box: [50, 60, 80, 98], opts: [
      { name: '"Correlation ≠ Causation" Poster', d: () => frame(62, 72, 56, 74, INK) + t(90, 90, 'CORRELATION', 6.6, Y) + t(90, 118, '≠', 30, C) + t(90, 136, 'CAUSATION', 6.6, Y) },
      { name: 'Normal Curve Poster', d: () => frame(62, 72, 56, 74, W) + `<path d="${bell(66, 114, 132, 86)} Z" fill="${B}" fill-opacity=".22"/><path d="${bell(66, 114, 132, 86)}" fill="none" stroke="${B}" stroke-width="2"/><line x1="80" y1="106" x2="80" y2="132" stroke="${C}" stroke-width="1.4"/><line x1="100" y1="106" x2="100" y2="132" stroke="${C}" stroke-width="1.4"/><line x1="66" y1="132" x2="114" y2="132" stroke="${INK}" stroke-width="1.2"/>` + t(90, 126, '68%', 8, INK) },
      { name: 'Box Plot Art Print', d: () => frame(62, 72, 56, 74, Y) + [[86, 72, 96, 18, INK], [108, 78, 104, 22, C], [130, 68, 92, 14, TL]].map(([y, x, x2, bw, col]) => `<line x1="${x - 4}" y1="${y}" x2="${x2 + 12}" y2="${y}" stroke="${INK}" stroke-width="1.4"/><rect x="${x + 4}" y="${y - 6}" width="${bw}" height="12" fill="${col}" stroke="${INK}"/><line x1="${x + 4 + bw * 0.45}" y1="${y - 6}" x2="${x + 4 + bw * 0.45}" y2="${y + 6}" stroke="${W}" stroke-width="1.6"/>`).join('') }
    ] },
    { name: 'Pillow', bg: WALL, box: [14, 164, 60, 46], opts: [
      { name: 'μ Pillow', d: () => `<rect x="26" y="177" width="36" height="22" rx="8" fill="${C}" stroke="${INK}" stroke-opacity=".2"/>` + t(44, 194, 'μ', 15, W) },
      { name: 'σ Pillow', d: () => `<rect x="26" y="177" width="36" height="22" rx="8" fill="${B}" stroke="${INK}" stroke-opacity=".2"/>` + t(44, 193, 'σ', 15, W) },
      { name: 'x̄ Pillow', d: () => `<rect x="26" y="177" width="36" height="22" rx="8" fill="${Y}" stroke="${INK}" stroke-opacity=".2"/>` + t(44, 193, 'x', 14, INK) + `<line x1="39.5" y1="182.5" x2="48.5" y2="182.5" stroke="${INK}" stroke-width="1.6"/>` }
    ] },
    { name: 'Window', bg: WALL, box: [178, 46, 124, 120], opts: [
      { name: 'Dot Plot Curtains', d: () => { let s = `<line x1="184" y1="57" x2="296" y2="57" stroke="${WOOD2}" stroke-width="3" stroke-linecap="round"/>`; [[186, 0], [270, 1]].forEach(([x, k]) => { s += `<path d="M${x},58 h24 q-3,50 0,98 h-24 q3,-50 0,-98 Z" fill="#FFF3D1" stroke="${INK}" stroke-opacity=".18"/>`; [3, 5, 2].forEach((n, i) => { for (let j = 0; j < n; j++) s += `<circle cx="${x + 5 + i * 7}" cy="${150 - j * 7}" r="2.6" fill="${k ? B : C}"/>`; }); }); return s; } },
      { name: 'Histogram Drapes', d: () => { let s = `<line x1="184" y1="57" x2="296" y2="57" stroke="${WOOD2}" stroke-width="3" stroke-linecap="round"/>`; [186, 270].forEach((x) => { s += `<path d="M${x},58 h24 q-3,50 0,98 h-24 q3,-50 0,-98 Z" fill="${B}"/>`; [14, 30, 22, 10].forEach((h, i) => { s += `<rect x="${x + 2.5 + i * 5}" y="${154 - h}" width="4" height="${h}" fill="${Y}"/>`; }); }); return s; } },
      { name: 'Trend Line Blinds', d: () => { let s = `<rect x="196" y="58" width="88" height="6" fill="${W}" stroke="${INK}" stroke-opacity=".25"/>`; for (let y = 68; y < 148; y += 8) s += `<rect x="198" y="${y}" width="84" height="4.5" fill="${W}" fill-opacity=".82"/>`; return s + `<polyline points="202,140 222,128 240,132 258,104 278,80" fill="none" stroke="${C}" stroke-width="3" stroke-linejoin="round" stroke-linecap="round"/><circle cx="278" cy="80" r="3.6" fill="${C}"/>`; } }
    ] },
    { name: 'Desk lamp', bg: WALL, box: [314, 130, 54, 62], opts: [
      { name: 'Bell Curve Lamp', d: () => `<path d="${bell(324, 358, 164, 141)} Z" fill="${Y}" stroke="${INK}" stroke-opacity=".3"/><line x1="341" y1="164" x2="341" y2="182" stroke="${INK}" stroke-width="2.4"/><ellipse cx="341" cy="184" rx="9" ry="2.6" fill="${INK}"/><path d="M329,166 L323,184 H359 L353,166 Z" fill="${Y}" fill-opacity=".22"/>` },
      { name: 'Error Bar Lamp', d: () => `<ellipse cx="341" cy="184" rx="9" ry="2.6" fill="${INK}"/><rect x="333" y="145" width="16" height="36" rx="8" fill="${Y}" fill-opacity=".3"/><path d="M341,147 V181 M335,147 H347 M335,181 H347" stroke="${O}" stroke-width="3" stroke-linecap="round" fill="none"/><circle cx="341" cy="164" r="4.6" fill="${Y}" stroke="${O}" stroke-width="1.5"/>` },
      { name: 'Lava Lamp of Random Variables', d: () => `<path d="M336,146 h10 l4,34 h-18 Z" fill="${V}" stroke="${INK}" stroke-opacity=".3"/><circle cx="341" cy="170" r="4" fill="${C}"/><circle cx="339" cy="158" r="2.6" fill="${Y}"/><circle cx="344" cy="152" r="1.8" fill="${C}"/><path d="M335,142 h12 l-1,4 h-10 Z" fill="${INK}"/><path d="M331,180 h20 l2,6 h-24 Z" fill="${INK}"/>` }
    ] },
    { name: 'Plant', bg: FLOOR, box: [418, 250, 62, 88], opts: [
      { name: 'Stem-and-Leaf Plant', d: () => { let s = `<line x1="451" y1="306" x2="451" y2="262" stroke="${G}" stroke-width="3" stroke-linecap="round"/>`; [[268, 1], [278, -1], [288, 1], [298, -1]].forEach(([y, k]) => { s += `<ellipse cx="${451 + k * 10}" cy="${y}" rx="10" ry="4.6" fill="${G}" transform="rotate(${-k * 24} ${451 + k * 10} ${y})"/>`; }); return s + `<path d="M436,306 h30 l-4,26 h-22 Z" fill="${C}" stroke="${INK}" stroke-opacity=".25"/>` + t(451, 323, '3 | 1 4 7', 5.6, W); } },
      { name: 'Box-and-Whisker Cactus', d: () => `<rect x="444" y="272" width="14" height="36" rx="7" fill="${G}"/><path d="M444,292 h-5 a4,4 0 0 1 -4,-4 v-6 M458,286 h5 a4,4 0 0 0 4,-4 v-5" fill="none" stroke="${G}" stroke-width="5" stroke-linecap="round"/><circle cx="451" cy="270" r="3" fill="${C}"/><line x1="426" y1="320" x2="476" y2="320" stroke="${INK}" stroke-width="1.6"/><line x1="426" y1="314" x2="426" y2="326" stroke="${INK}" stroke-width="1.6"/><line x1="476" y1="314" x2="476" y2="326" stroke="${INK}" stroke-width="1.6"/><rect x="436" y="308" width="30" height="24" fill="${Y}" stroke="${INK}" stroke-width="1.4"/><line x1="449" y1="308" x2="449" y2="332" stroke="${INK}" stroke-width="2"/>` },
      { name: 'Probability Tree Bonsai', d: () => { let s = `<path d="M451,308 V290 M451,290 L438,276 M451,290 L464,276 M438,276 L431,264 M438,276 L445,264 M464,276 L457,264 M464,276 L471,264" stroke="${WOOD2}" stroke-width="2.6" fill="none" stroke-linecap="round"/>`; [431, 445, 457, 471].forEach((x) => { s += `<circle cx="${x}" cy="262" r="5.4" fill="${G}"/>`; }); return s + `<circle cx="438" cy="276" r="2.2" fill="${Y}"/><circle cx="464" cy="276" r="2.2" fill="${Y}"/><path d="M434,308 h34 l-3,10 h-28 Z" fill="${B}" stroke="${INK}" stroke-opacity=".25"/><rect x="438" y="318" width="26" height="4" fill="${INK}"/>`; } }
    ] },
    { name: 'Wall clock', bg: WALL, box: [128, 70, 52, 52], opts: [
      { name: 'Pie Chart Clock', d: () => pieSlices(154, 96, 18, [0.45, 0.3, 0.25], [C, Y, TL]) + `<circle cx="154" cy="96" r="18" fill="none" stroke="${INK}" stroke-width="2"/><path d="M154,96 V84 M154,96 L163,100" stroke="${INK}" stroke-width="2.2" stroke-linecap="round"/><circle cx="154" cy="96" r="1.8" fill="${INK}"/>` },
      { name: 'Normal O\'Clock', d: () => `<circle cx="154" cy="96" r="18" fill="${W}" stroke="${B}" stroke-width="2.5"/><path d="${bell(140, 168, 106, 88)}" fill="none" stroke="${B}" stroke-opacity=".5" stroke-width="1.6"/><path d="M154,96 V83 M154,96 L164,92" stroke="${INK}" stroke-width="2.2" stroke-linecap="round"/><circle cx="154" cy="96" r="1.8" fill="${C}"/>` },
      { name: 'n = 12 Clock', d: () => { let s = `<circle cx="154" cy="96" r="18" fill="${INK}"/>`; for (let i = 0; i < 12; i++) { const a = i * Math.PI / 6; s += `<circle cx="${(154 + 14 * Math.sin(a)).toFixed(1)}" cy="${(96 - 14 * Math.cos(a)).toFixed(1)}" r="1.7" fill="${Y}"/>`; } return s + `<path d="M154,96 L147,88 M154,96 H164" stroke="${W}" stroke-width="2" stroke-linecap="round"/>` + t(154, 107, 'n=12', 4.6, Y); } }
    ] },
    { name: 'Bookshelf', bg: WALL, box: [390, 90, 52, 46], opts: [
      { name: 'Greek Letter Library', d: () => [[B, 'μ', 26], [C, 'σ', 30], [TL, 'β', 24], [V, 'ρ', 28]].map(([c, l, h], i) => `<rect x="${399 + i * 9}" y="${128 - h}" width="8" height="${h}" rx="1" fill="${c}"/>` + t(403 + i * 9, 128 - h / 2 + 2.5, l, 6.6, W)).join('') },
      { name: 'The Course Binder', d: () => `<rect x="400" y="98" width="15" height="30" rx="1.5" fill="${W}" stroke="${INK}" stroke-width="1.2"/><rect x="402.5" y="103" width="10" height="13" fill="${Y}"/>${t(407.5, 109, 'STA', 4, INK)}${t(407.5, 114, '145', 4, INK)}<circle cx="407.5" cy="122" r="1.6" fill="none" stroke="${INK}"/><rect x="418" y="104" width="8" height="24" rx="1" fill="${TL}" transform="rotate(12 422 128)"/>` },
      { name: 'Histogram of Books', d: () => [10, 16, 24, 27, 19, 12].map((h, i) => `<rect x="${399 + i * 6.2}" y="${128 - h}" width="5.4" height="${h}" rx=".8" fill="${[C, O, Y, TL, B, V][i]}"/>`).join('') }
    ] },
    { name: 'Desk gadget', bg: WALL, box: [360, 144, 68, 48], opts: [
      { name: 'Laptop Running R', d: () => `<rect x="372" y="158" width="42" height="26" rx="2" fill="${INK}"/><rect x="375" y="161" width="36" height="20" fill="#0E1530"/>${[6, 11, 15, 9, 5].map((h, i) => `<rect x="${388 + i * 4.4}" y="${178 - h}" width="3.4" height="${h}" fill="${Y}"/>`).join('')}<text x="377" y="169" font-size="6" fill="${TL}" font-weight="800">&gt; R</text><path d="M366,184 h54 l-3,3 h-48 Z" fill="#AEB6CC"/>` },
      { name: 'Giant Calculator', d: () => { let s = `<rect x="378" y="150" width="30" height="36" rx="3" fill="${INK}"/><rect x="381" y="153" width="24" height="9" rx="1" fill="#CFF3E6"/><text x="403" y="160" font-size="6" text-anchor="end" fill="${INK}" font-weight="800">0.05</text>`; for (let r = 0; r < 3; r++) for (let c = 0; c < 4; c++) s += `<rect x="${381 + c * 6.2}" y="${165 + r * 6.4}" width="5" height="5" rx="1" fill="${c === 3 ? C : '#DCE1F0'}"/>`; return s; } },
      { name: 'Lucky Dice', d: () => { const die = (x, y, rot, pips) => `<g transform="rotate(${rot} ${x + 9} ${y + 9})"><rect x="${x}" y="${y}" width="18" height="18" rx="3.5" fill="${W}" stroke="${INK}" stroke-width="1.3"/>${pips.map(([a, b]) => `<circle cx="${x + a}" cy="${y + b}" r="1.8" fill="${C}"/>`).join('')}</g>`; return die(374, 167, -8, [[5, 5], [13, 13], [9, 9], [5, 13], [13, 5]]) + die(397, 168, 10, [[5, 5], [13, 13], [9, 9]]); } }
    ] },
    { name: 'String lights', bg: WALL, box: [10, 0, 236, 40], opts: [
      { name: 'Scatter Lights', d: () => { let s = `<path d="${wire}" fill="none" stroke="${INK}" stroke-width="1.2"/>`; for (let x = 24, i = 0; x < 462; x += 23, i++) { const y = wireY(x) + 3, c = [C, Y, TL, B, V][i % 5]; s += `<circle cx="${x}" cy="${y.toFixed(1)}" r="6.5" fill="${c}" opacity=".25"/><circle cx="${x}" cy="${y.toFixed(1)}" r="3.4" fill="${c}"/>`; } return s; } },
      { name: 'Bar Chart Bunting', d: () => { let s = `<path d="${wire}" fill="none" stroke="${INK}" stroke-width="1.2"/>`; const hs = [8, 14, 20, 12, 17, 9, 15, 21, 11, 16]; for (let x = 20, i = 0; x < 462; x += 19, i++) { const y = wireY(x + 5); s += `<rect x="${x}" y="${y.toFixed(1)}" width="10" height="${hs[i % 10]}" fill="${[B, C, Y, TL][i % 4]}"/>`; } return s; } },
      { name: 'Greek Letter Garland', d: () => { let s = `<path d="${wire}" fill="none" stroke="${INK}" stroke-width="1.2"/>`; const ls = ['Σ', 'μ', 'σ', 'β', 'χ', 'α', 'ρ']; for (let x = 30, i = 0; x < 458; x += 34, i++) { const y = wireY(x) + 9, c = [V, C, TL, B, O][i % 5]; s += `<circle cx="${x}" cy="${y.toFixed(1)}" r="8" fill="${c}"/>` + t(x, (y + 3.6).toFixed(1), ls[i % 7], 10, W); } return s; } }
    ] },
    { name: 'Roommate', bg: FLOOR, box: [156, 256, 72, 58], opts: [
      { name: 'Chi the Square Cat', d: () => `<path d="M206,300 q12,2 9,-12" fill="none" stroke="${O}" stroke-width="4" stroke-linecap="round"/><rect x="176" y="276" width="30" height="28" rx="4" fill="${O}" stroke="${INK}" stroke-opacity=".25"/><path d="M178,277 l3,-9 l6,8 Z M204,277 l-3,-9 l-6,8 Z" fill="${O}" stroke="${INK}" stroke-opacity=".25"/><circle cx="185" cy="286" r="1.9" fill="${INK}"/><circle cx="197" cy="286" r="1.9" fill="${INK}"/><path d="M189,291 h4 l-2,2.4 Z" fill="${C}"/><path d="M176,290 h-6 M176,293 h-6 M206,290 h6 M206,293 h6" stroke="${INK}" stroke-width=".8"/>${t(191, 302, 'χ²', 6.4, W)}` },
      { name: 'Stu the t-Rex', d: () => `<path d="M178,300 q-14,2 -18,-8 q10,6 18,-2 Z" fill="${G}"/><ellipse cx="190" cy="292" rx="15" ry="13" fill="${G}" stroke="${INK}" stroke-opacity=".25"/><rect x="194" y="266" width="20" height="16" rx="6" fill="${G}" stroke="${INK}" stroke-opacity=".25"/><rect x="194" y="274" width="12" height="12" fill="${G}"/><circle cx="207" cy="272" r="1.9" fill="${INK}"/><path d="M204,278 h9" stroke="${INK}" stroke-width="1.1"/><path d="M201,288 h7 M201,293 h6" stroke="${G}" stroke-width="3.2" stroke-linecap="round"/><rect x="181" y="302" width="7" height="4" rx="2" fill="${G}"/><rect x="193" y="302" width="7" height="4" rx="2" fill="${G}"/>${t(188, 297, 't', 10, Y)}` },
      { name: 'Outlier the Goldfish', d: () => `<circle cx="190" cy="286" r="17" fill="#CDEBFA" fill-opacity=".8" stroke="${W}" stroke-width="2.4"/><path d="M175,279 q15,-5 30,0" fill="none" stroke="${B}" stroke-opacity=".5" stroke-width="1.4"/><ellipse cx="189" cy="289" rx="7" ry="4.4" fill="${O}"/><path d="M196,289 l6,-4 v8 Z" fill="${O}"/><circle cx="185.5" cy="288" r="1" fill="${INK}"/><circle cx="198" cy="279" r="1.3" fill="${W}"/><rect x="180" y="301" width="20" height="4" rx="2" fill="${INK}" fill-opacity=".5"/><circle cx="217" cy="266" r="2.2" fill="${C}"/>` }
    ] },
    { name: 'Poster over the desk', bg: WALL, box: [320, 52, 74, 84], opts: [
      { name: '"p < .05" Poster', d: () => frame(332, 64, 50, 60, C) + t(357, 92, 'p', 22, W, 'font-style="italic"') + t(357, 112, '< .05', 13, W) },
      { name: 'H₀ vs H₁ Fight Poster', d: () => frame(332, 64, 50, 60, INK) + t(346, 92, 'H₀', 14, Y) + t(368, 92, 'H₁', 14, TL) + t(357, 104, 'VS', 7, W) + `<path d="${star(357, 113, 5)}" fill="${C}"/>` },
      { name: '"Fail to Reject ≠ Accept" Poster', d: () => frame(332, 64, 50, 60, TL) + t(357, 80, 'FAIL TO', 8, W) + t(357, 91, 'REJECT', 8, W) + t(357, 106, '≠', 14, Y) + t(357, 118, 'ACCEPT', 8, W) }
    ] },
    { name: 'Floor seat', bg: FLOOR, box: [248, 232, 76, 68], opts: [
      { name: 'Bell Curve Beanbag', d: () => `<ellipse cx="285" cy="291" rx="30" ry="4" fill="${INK}" fill-opacity=".18"/><path d="${bell(254, 316, 292, 240)} Z" fill="${V}" stroke="${INK}" stroke-opacity=".25"/><path d="${bell(266, 304, 286, 252)}" fill="none" stroke="${W}" stroke-opacity=".6" stroke-width="1.6"/>` },
      { name: 'Dice Ottoman', d: () => `<ellipse cx="287" cy="291" rx="28" ry="4" fill="${INK}" fill-opacity=".18"/><path d="M264,258 l8,-10 h38 l-8,10 Z" fill="#F1F3FA" stroke="${INK}" stroke-width="1.2"/><path d="M302,258 l8,-10 v32 l-8,10 Z" fill="#D5DAEA" stroke="${INK}" stroke-width="1.2"/><rect x="264" y="258" width="38" height="32" rx="2" fill="${W}" stroke="${INK}" stroke-width="1.2"/>${[[272, 266], [294, 266], [283, 274], [272, 282], [294, 282]].map(([x, y]) => `<circle cx="${x}" cy="${y}" r="2.8" fill="${B}"/>`).join('')}` },
      { name: 'Pie Chart Pouf', d: () => `<ellipse cx="285" cy="290" rx="29" ry="5" fill="${INK}" fill-opacity=".18"/><path d="M259,262 v22 a26,8 0 0 0 52,0 v-22 Z" fill="${TL}" stroke="${INK}" stroke-opacity=".25"/><g transform="translate(285 262) scale(1 .31)">${pieSlices(0, 0, 26, [0.35, 0.3, 0.2, 0.15], [Y, C, B, W])}<circle r="26" fill="none" stroke="${INK}" stroke-opacity=".3" stroke-width="2"/></g>` }
    ] },
    { name: 'Mug', bg: WALL, box: [202, 180, 36, 32], opts: [
      { name: 'Mean Coffee Mug (x̄)', d: () => `<path d="M215,190 q-2,-3 0,-5 M220,190 q2,-3 0,-6" fill="none" stroke="${W}" stroke-width="1.2" stroke-linecap="round"/>` + mug(W, 'x', INK) + `<line x1="215.4" y1="196.4" x2="220.6" y2="196.4" stroke="${INK}" stroke-width="1"/>` },
      { name: 'Chai-Square Tea Mug (χ²)', d: () => mug(G, 'χ²', W) + `<path d="M221,193 v-4" stroke="${INK}" stroke-width=".8"/><rect x="219.5" y="186" width="3" height="3.4" fill="${Y}"/>` },
      { name: 'Cocoa-relation Mug (r)', d: () => `<circle cx="215" cy="193" r="1.8" fill="${W}"/><circle cx="219" cy="192.4" r="1.8" fill="${W}"/><circle cx="222" cy="193.2" r="1.5" fill="${W}"/>` + mug(C, 'r', W) }
    ] },
    { name: 'Ceiling mobile', bg: WALL, box: [288, 0, 48, 72], opts: [
      { name: 'Greek Letter Mobile', d: () => `<path d="M312,0 V12 M301,12 H323 M302,12 V26 M312,12 V40 M322,12 V22" stroke="${INK}" stroke-width="1" fill="none"/>` + [[302, 32, 'μ', B], [312, 46, 'σ', C], [322, 28, 'β', TL]].map(([x, y, l, c]) => `<circle cx="${x}" cy="${y}" r="6.5" fill="${c}"/>` + t(x, y + 3.2, l, 9, W)).join('') },
      { name: 'Box-and-Whisker Mobile', d: () => `<path d="M312,0 V12 M301,12 H323 M302,12 V20 M312,12 V30 M322,12 V18" stroke="${INK}" stroke-width="1" fill="none"/>` + [[302, 20, Y], [312, 28, C], [322, 18, TL]].map(([x, y, c]) => `<path d="M${x},${y} v5 M${x},${y + 19} v5 M${x - 3},${y + 24} h6" stroke="${INK}" stroke-width="1.2" fill="none"/><rect x="${x - 4}" y="${y + 5}" width="8" height="14" fill="${c}" stroke="${INK}" stroke-width="1.1"/><line x1="${x - 4}" y1="${y + 11}" x2="${x + 4}" y2="${y + 11}" stroke="${INK}" stroke-width="1.6"/>`).join('') },
      { name: 'Scatter Star Mobile', d: () => `<path d="M312,0 V12 M301,12 H323 M302,12 V27 M312,12 V40 M322,12 V22" stroke="${INK}" stroke-width="1" fill="none"/>` + [[302, 33, 6.5, Y], [312, 47, 7.5, C], [322, 28, 6.5, B]].map(([x, y, r, c]) => `<path d="${star(x, y, r)}" fill="${c}" stroke="${INK}" stroke-opacity=".25"/>`).join('') }
    ] },
    { name: 'Shelf trophy', bg: WALL, box: [432, 92, 40, 44], opts: [
      { name: 'Golden p-value Trophy', d: () => cup(Y, 'p') },
      { name: 'ANOVA Cup', d: () => cup('#B9C3DB', 'F') },
      { name: 'Triple-Star Significance Ribbon', d: () => `<path d="M446,116 l-3,12 l6,-4 l3,4 Z M458,116 l3,12 l-6,-4 l-3,4 Z" fill="${B}"/><circle cx="452" cy="110" r="10" fill="${C}" stroke="${W}" stroke-width="1.6"/>${t(452, 114.5, '***', 9, W)}` }
    ] },
    { name: 'Nightstand treasure', bg: WALL, box: [220, 176, 32, 36], opts: [
      { name: 'Normal Curve Snow Globe', d: () => `<circle cx="237" cy="194" r="9" fill="#D6EEFB" stroke="${W}" stroke-width="1.6"/><path d="${bell(230, 244, 199, 189)}" fill="none" stroke="${B}" stroke-width="1.5"/><circle cx="233" cy="190" r=".9" fill="${W}"/><circle cx="241" cy="192" r=".9" fill="${W}"/><circle cx="237" cy="187" r=".9" fill="${W}"/><path d="M229,201 h16 l1.5,5 h-19 Z" fill="${C}"/>` },
      { name: '95% Confidence Alarm Clock', d: () => `<path d="M230,190 l-2,-3 M244,190 l2,-3" stroke="${INK}" stroke-width="1.6" stroke-linecap="round"/><rect x="227" y="190" width="20" height="16" rx="3" fill="${INK}"/><rect x="229.5" y="193" width="15" height="9" rx="1" fill="#0E1530"/>${t(237, 200.4, '.95', 6.6, Y)}` },
      { name: 'Regression Night-Light', d: () => `<rect x="227" y="186" width="20" height="20" rx="4" fill="${Y}" fill-opacity=".35"/><rect x="229" y="188" width="16" height="16" rx="3" fill="${W}" stroke="${Y}" stroke-width="1.6"/><line x1="231" y1="201" x2="243" y2="191" stroke="${C}" stroke-width="1.6"/><circle cx="233" cy="198" r="1.2" fill="${B}"/><circle cx="237" cy="197.5" r="1.2" fill="${B}"/><circle cx="240" cy="192" r="1.2" fill="${B}"/>` }
    ] },
    { name: 'Neon sign', bg: WALL, box: [182, 22, 116, 40], opts: [
      { name: '"ŷ = α + βx" Neon Sign', d: () => neon('ŷ = α + βx', '#FF8FB3', 11.5) },
      { name: '"Stat Star" Neon Sign', d: () => neon('STAT STAR ★', Y, 10) },
      { name: '"n → ∞" Neon Sign', d: () => neon('n → ∞', '#5EEAD4', 14) }
    ] }
  ];
  const KEYS = ['a', 'b', 'c'];
  /* draw order (slot index = level - 1) */
  const pickOpt = (decor, i) => { const k = decor && decor[i + 1]; const j = KEYS.indexOf(k); return j < 0 ? null : SLOTS[i].opts[j]; };
  const item = (decor, i, fresh) => { const o = pickOpt(decor, i); return o ? `<g class="decor${fresh === i + 1 ? ' fresh' : ''}" data-slot="${i + 1}">${o.d()}</g>` : ''; };

  function render(decor, o = {}) {
    const it = (lvl) => item(decor, lvl - 1, o.fresh);
    const names = SLOTS.map((s, i) => pickOpt(decor, i)).filter(Boolean).map((x) => x.name);
    const label = (o.who ? o.who + '\'s bedroom. ' : 'A bedroom. ') + (names.length ? 'Decorations: ' + names.join(', ') + '.' : 'No decorations yet.');
    const esc = (x) => String(x).replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;').replace(/"/g, '&quot;');
    let s = `<svg class="room" viewBox="0 0 480 340" role="img" aria-label="${esc(label)}">`;
    s += `<rect width="480" height="232" fill="${WALL}"/>` + it(1);
    s += `<rect y="232" width="480" height="108" fill="${FLOOR}"/>`;
    for (let y = 250; y < 340; y += 22) s += `<line x1="0" y1="${y}" x2="480" y2="${y}" stroke="${WOOD2}" stroke-opacity=".22"/>`;
    for (let i = 0; i < 9; i++) s += `<line x1="${30 + i * 55 + (i % 2) * 20}" y1="${232 + (i % 4) * 22}" x2="${30 + i * 55 + (i % 2) * 20}" y2="${254 + (i % 4) * 22}" stroke="${WOOD2}" stroke-opacity=".22"/>`;
    s += `<rect y="224" width="480" height="9" fill="#F6F3EE"/><line x1="0" y1="233" x2="480" y2="233" stroke="${INK}" stroke-opacity=".15"/>`;
    /* window */
    s += `<rect x="194" y="60" width="92" height="92" rx="3" fill="${W}"/><rect x="199" y="65" width="82" height="82" fill="#BFE3F7"/><path d="M199,122 q20,-16 40,-4 t42,-6 v35 h-82 Z" fill="#A9DBA8"/><circle cx="262" cy="84" r="8" fill="#FFE98A"/><path d="M240,65 V147 M199,106 H281" stroke="${W}" stroke-width="4"/><rect x="190" y="150" width="100" height="6" rx="2" fill="${W}" stroke="${INK}" stroke-opacity=".15"/>`;
    s += it(6) + it(12) + it(20) + it(4) + it(9) + it(14);
    /* shelf */
    s += `<rect x="394" y="128" width="74" height="4.5" rx="1" fill="${WOOD}"/><path d="M402,132.5 v7 M460,132.5 v7" stroke="${WOOD2}" stroke-width="2"/>`;
    s += it(10) + it(18) + it(17) + it(2);
    /* bed */
    s += `<rect x="12" y="150" width="12" height="104" rx="3" fill="${WOOD}"/><rect x="194" y="204" width="8" height="50" rx="2" fill="${WOOD}"/><rect x="20" y="216" width="180" height="26" fill="${WOOD}"/><rect x="24" y="242" width="8" height="12" fill="${WOOD2}"/><rect x="186" y="242" width="8" height="12" fill="${WOOD2}"/><rect x="22" y="196" width="176" height="22" rx="5" fill="#FBFAF6" stroke="${INK}" stroke-opacity=".15"/>`;
    s += pickOpt(decor, 2) ? it(3) : duvet('#EEF0F5');
    s += pickOpt(decor, 4) ? it(5) : `<rect x="26" y="177" width="36" height="22" rx="8" fill="${W}" stroke="${INK}" stroke-opacity=".2"/>`;
    /* nightstand */
    s += `<rect x="206" y="206" width="42" height="40" rx="2" fill="${WOOD}"/><rect x="210" y="212" width="34" height="13" rx="1.5" fill="none" stroke="${WOOD2}" stroke-width="1.4"/><circle cx="227" cy="218.5" r="1.8" fill="${Y}"/><rect x="209" y="246" width="5" height="8" fill="${WOOD2}"/><rect x="240" y="246" width="5" height="8" fill="${WOOD2}"/>`;
    s += it(16) + it(19);
    /* desk */
    s += `<rect x="322" y="192" width="7" height="62" fill="${WOOD2}"/><rect x="452" y="192" width="7" height="62" fill="${WOOD2}"/><rect x="398" y="192" width="54" height="20" fill="${WOOD}" stroke="${WOOD2}"/><circle cx="425" cy="202" r="2" fill="${Y}"/><rect x="316" y="186" width="148" height="7" rx="2" fill="${WOOD}"/>`;
    s += it(7) + it(11) + it(15) + it(13) + it(8);
    return s + '</svg>';
  }

  /* thumbnail of one option, cropped to its slot */
  function thumb(level, key) {
    const sl = SLOTS[level - 1], o = sl.opts[KEYS.indexOf(key)], [x, y, w, h] = sl.box;
    let bgx = `<rect x="${x}" y="${y}" width="${w}" height="${h}" fill="${sl.bg}"/>`;
    if (level === 3 || level === 5) bgx += `<rect x="22" y="196" width="176" height="22" rx="5" fill="#FBFAF6"/><rect x="20" y="216" width="180" height="26" fill="${WOOD}"/>` + (level === 5 ? `<rect x="12" y="150" width="12" height="104" rx="3" fill="${WOOD}"/>` : '');
    if (level === 6) bgx += `<rect x="194" y="60" width="92" height="92" rx="3" fill="${W}"/><rect x="199" y="65" width="82" height="82" fill="#BFE3F7"/><path d="M240,65 V147 M199,106 H281" stroke="${W}" stroke-width="4"/>`;
    if (level === 10 || level === 18) bgx += `<rect x="394" y="128" width="74" height="4.5" rx="1" fill="${WOOD}"/>`;
    if (level === 7 || level === 11) bgx += `<rect x="316" y="186" width="148" height="7" rx="2" fill="${WOOD}"/>`;
    if (level === 16 || level === 19) bgx += `<rect x="206" y="206" width="42" height="40" rx="2" fill="${WOOD}"/>`;
    return `<svg class="thumb" viewBox="${x} ${y} ${w} ${h}" aria-hidden="true">${bgx}${o.d()}</svg>`;
  }
  const optName = (level, key) => { const j = KEYS.indexOf(key); return j < 0 ? '' : SLOTS[level - 1].opts[j].name; };
  return { SLOTS, KEYS, render, thumb, optName };
})();
if (typeof module !== 'undefined') module.exports = ROOM;
