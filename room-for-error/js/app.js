/* Room for Error: app shell, play flow, usernames, saving, and the optional class leaderboard.
   Everything a student does is saved in their own browser. Only if config.js names a class leaderboard,
   and only after the student chooses to join it, is anything sent off the device (see js/net.js). */
(() => {
  const PASS = 4, PER = 5, MAX = 20, LS = 'room-for-error-gh-v2';
  const $ = (s, r = document) => r.querySelector(s);
  const esc = (s) => String(s).replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;').replace(/"/g, '&quot;').replace(/'/g, '&#39;');
  /* x̄, ȳ, p̂, ŷ are a letter followed by a combining bar or hat. The reading font has no attachment point for the bar on x
     and no hat at all, so each phone used to place them differently. They are drawn here instead (see .sym in style.css),
     and a screen reader hears "x bar" or "p hat". */
  const sym = (html) => html.replace(/([A-Za-z])([\u0304\u0302])/g, (m, ch, mk) => { const w = mk === '\u0304' ? 'bar' : 'hat'; return `<span class="sym ${w}${ch !== ch.toLowerCase() ? ' uc' : ''}" role="img" aria-label="${ch} ${w}">${ch}</span>`; });
  const escq = (s) => sym(esc(s));
  const app = $('#app'), dlg = $('#dlg');
  const MODS = [[1, 'Module 1: Describing Data'], [11, 'Module 2: Inference'], [15, 'Module 3: Relationships Between Two Variables']];
  const CFG = window.RFE_CONFIG || {}, NET = RFE.enabled;
  const blank = () => ({ name: '', best: {}, stars: {}, fails: {}, decor: {}, reachedAt: 0 });

  const S = {
    me: blank(), anon: false, auth: null, approved: false, classCode: '', serverAt: 0, dirty: false,
    view: 'room', quiz: null, fresh: 0, sync: '', notice: '', codeStale: false,
    board: [], boardState: NET ? 'idle' : 'off', boardAt: 0
  };
  /* Three ways to play: anonymously (S.anon), with a username (S.auth), or not chosen yet. */
  const started = () => S.anon || !!S.auth;

  /* ---------- progress helpers ---------- */
  const levelOf = (best) => { let l = 0; while (l < MAX && (best[l + 1] || 0) >= PASS) l++; return l; };
  const perfectOf = (stars) => Object.keys(stars || {}).length;
  const passed = (l) => (S.me.best[l] || 0) >= PASS;
  /* Ease step for the next attempt: one step per failed attempt, until the level is passed. Replays of a passed level are standard. */
  const easeFor = (l) => (passed(l) ? 0 : Math.min(S.me.fails[l] || 0, QZ.MAX_EASE));
  const EASE_TEXT = ['', 'more of the foundational questions and fewer of the hardest ones', 'mostly foundational questions, with one wrong answer removed from each multiple-choice question', 'the most foundational questions, with one wrong answer removed from each multiple-choice question'];
  const NAME_RULE = 'Use 2 to 16 characters: letters, numbers, spaces, hyphens, or underscores, starting with a letter or number.';
  const cleanName = (s) => String(s == null ? '' : s).replace(/\s+/g, ' ').trim().slice(0, 16);
  const validName = (s) => /^[A-Za-z0-9][A-Za-z0-9 _-]{1,15}$/.test(s);
  const clampInt = (v, lo, hi) => { const n = Math.floor(Number(v)); return n >= lo && n <= hi ? n : lo; };
  /* Nothing read from storage, a backup code, or the leaderboard is trusted: it is rebuilt field by field here. */
  function sanitize(d) {
    const out = blank();
    if (!d || typeof d !== 'object') return out;
    const nm = cleanName(d.name); out.name = validName(nm) ? nm : '';
    for (let l = 1; l <= MAX; l++) {
      const b = d.best && Number(d.best[l]); if (Number.isInteger(b) && b >= 1 && b <= PER) out.best[l] = b;
      if (d.stars && d.stars[l] && out.best[l] === PER) out.stars[l] = 1;
      const f = d.fails && Number(d.fails[l]); if (Number.isInteger(f) && f > 0) out.fails[l] = Math.min(f, 99);
    }
    const lv = levelOf(out.best);
    for (let l = 1; l <= lv; l++) { const k = d.decor && d.decor[l]; if (ROOM.KEYS.includes(k)) out.decor[l] = k; }
    out.reachedAt = Number(d.reachedAt) > 0 ? Math.floor(Number(d.reachedAt)) : 0;
    return out;
  }
  function mergeMax(a, b) { /* keeps the better of two saves of the same player; b supplies the name */
    const m = blank(); m.name = b.name || a.name;
    for (let l = 1; l <= MAX; l++) { const s = Math.max(a.best[l] || 0, b.best[l] || 0); if (s) m.best[l] = s; if (a.stars[l] || b.stars[l]) m.stars[l] = 1; const f = Math.max(a.fails[l] || 0, b.fails[l] || 0); if (f) m.fails[l] = f; const k = a.decor[l] || b.decor[l]; if (k) m.decor[l] = k; }
    m.reachedAt = levelOf(a.best) > levelOf(b.best) ? a.reachedAt : (b.reachedAt || a.reachedAt);
    return sanitize(m);
  }
  const prog = () => ({ best: S.me.best, stars: S.me.stars, fails: S.me.fails, decor: S.me.decor, reachedAt: S.me.reachedAt });
  const signedIn = () => NET && !!S.auth;

  /* ---------- storage on this device ---------- */
  function loadLocal() {
    try {
      const raw = localStorage.getItem(LS); if (!raw) return;
      const d = JSON.parse(raw); S.me = sanitize(d.me);
      if (d.auth && typeof d.auth.id === 'string' && typeof d.auth.code === 'string' && S.me.name) S.auth = { id: d.auth.id.slice(0, 64), code: RFE.normPin(d.auth.code) };
      if (!S.auth) S.me.name = '';
      S.anon = !S.auth && d.anon === true; S.approved = !!S.auth && d.approved === true;
      S.classCode = RFE.normClass(d.classCode); S.serverAt = Number(d.serverAt) || 0; S.dirty = d.dirty === true;
    } catch (e) { /* storage unavailable: play continues in memory */ }
  }
  function saveLocal() { try { localStorage.setItem(LS, JSON.stringify({ v: 2, me: S.me, anon: S.anon, auth: S.auth, approved: S.approved, classCode: S.classCode, serverAt: S.serverAt, dirty: S.dirty })); } catch (e) { /* ignore */ } }
  function save() { S.dirty = true; saveLocal(); push(); paintHeader(); }
  function setSync(msg) { S.sync = msg; const el = $('#sync'); if (el) el.textContent = msg; }

  /* ---------- class leaderboard (only when config.js turns it on and the student has created a username) ---------- */
  const ERR = {
    class_code: 'That class code doesn’t match. Check Canvas for the current code.',
    name_taken: 'Someone already has that username. Try another.',
    name_invalid: NAME_RULE,
    bad_login: 'That username and code don’t match. Check both and try again.',
    pending: 'That username hasn’t been approved yet. You can log in once your professor approves it.',
    locked: 'Too many wrong codes. Wait an hour and try again, or ask your professor for a new code.',
    full: 'The class leaderboard is full. Tell your professor.',
    not_set_up: 'The class leaderboard isn’t set up yet. Tell your professor.',
    network: 'The leaderboard can’t be reached right now. Check your connection and try again.',
    server: 'Something went wrong on the leaderboard. Try again in a minute.'
  };
  const errText = (r) => ERR[r && r.error] || ERR.server;
  function adopt(r) {
    const before = JSON.stringify([S.me, S.approved]);
    const p = sanitize(Object.assign({}, r.progress, { name: r.name })); if (!p.name) p.name = S.me.name;
    S.me = p; if (typeof r.approved === 'boolean') S.approved = r.approved;
    return JSON.stringify([S.me, S.approved]) !== before;
  }
  /* The username was refused or removed, or the code was changed: carry on anonymously with the progress on this device. */
  function lostLogin() { gen++; S.auth = null; S.approved = false; S.me.name = ''; S.anon = true; S.serverAt = 0; S.notice = 'lost'; saveLocal(); setSync(''); }
  let syncing = false, again = false, lastPush = 0, gen = 0; /* gen changes whenever the account on this device changes, so a late answer to an old request is ignored */
  async function push() {
    if (!signedIn() || !S.classCode) return;
    if (syncing) { again = true; return; }
    syncing = true; again = false; lastPush = Date.now(); setSync('Saving…');
    const g = gen, was = S.approved, r = await RFE.call('sync', { classCode: S.classCode, id: S.auth.id, code: S.auth.code, base: S.serverAt, progress: prog() });
    syncing = false;
    if (g !== gen) { if (signedIn()) push(); return; }
    if (r.ok) {
      S.serverAt = Number(r.updatedAt) || S.serverAt; S.codeStale = false;
      const changed = again ? false : adopt(r); if (!again) S.dirty = false;
      saveLocal(); setSync(!was && S.approved ? 'Your username was approved. You are now on the class leaderboard.' : 'Saved online.'); if (changed) soft();
    } else if (r.error === 'bad_login') { lostLogin(); soft(); }
    else if (r.error === 'class_code') { S.codeStale = true; setSync(''); soft(); }
    else setSync('Not saved online yet. Your progress is safe on this device and will be sent on your next save.');
    if (again) push();
  }
  const row = (p) => {
    const nm = cleanName(p && p.name), decor = {};
    for (let l = 1; l <= MAX; l++) { const k = p && p.decor && p.decor[l]; if (ROOM.KEYS.includes(k)) decor[l] = k; }
    return { id: String((p && p.id) || '').slice(0, 64), name: validName(nm) ? nm : '', level: clampInt(p && p.level, 0, MAX), perfect: clampInt(p && p.perfect, 0, MAX), decor, reachedAt: Number(p && p.reachedAt) > 0 ? Number(p.reachedAt) : 0 };
  };
  let boardBusy = false;
  async function loadBoard() {
    if (!NET || boardBusy) return;
    if (!S.classCode) { S.boardState = 'code'; return; }
    boardBusy = true;
    if (S.boardState !== 'live' && S.boardState !== 'stale') { S.boardState = 'loading'; if (S.view === 'board' && !dlg.open) render(); }
    const before = JSON.stringify([S.board, S.boardState]);
    const g = gen, r = await RFE.call('board', { classCode: S.classCode });
    boardBusy = false;
    if (g !== gen) { if (S.classCode) loadBoard(); return; }
    if (r.ok && Array.isArray(r.players)) { S.board = r.players.slice(0, 1000).map(row).filter((p) => p.name && p.id); S.boardState = 'live'; S.boardAt = Date.now(); S.codeStale = false; }
    else if (r.error === 'class_code') { S.boardState = 'code'; S.codeStale = true; }
    else S.boardState = S.boardAt ? 'stale' : 'error';
    if (S.view === 'board' && !dlg.open && JSON.stringify([S.board, S.boardState]) !== before) render();
  }
  const mine = () => ({ id: '__me', name: S.me.name || 'Anonymous', level: levelOf(S.me.best), perfect: perfectOf(S.me.stars), decor: S.me.decor, reachedAt: S.me.reachedAt });
  const ranked = () => {
    const myId = signedIn() ? S.auth.id : '';
    const rows = S.board.filter((p) => p.id !== myId);
    if (started()) rows.push(mine());
    return rows.sort((a, b) => b.level - a.level || b.perfect - a.perfect || (a.reachedAt || Infinity) - (b.reachedAt || Infinity) || a.name.localeCompare(b.name));
  };
  const isMe = (p) => p.id === '__me';

  /* ---------- rendering ---------- */
  const CREDIT = esc(CFG.creditNote || '');
  function paintHeader() {
    $('#meName').textContent = S.me.name || (started() ? 'Anonymous' : 'Start'); $('#meLv').textContent = 'Lv ' + levelOf(S.me.best);
    document.querySelectorAll('.tabs a').forEach((a) => { if (a.dataset.tab === (S.view === 'quiz' ? 'levels' : S.view)) a.setAttribute('aria-current', 'page'); else a.removeAttribute('aria-current'); });
  }
  const banner = () => (RFE.misconfigured ? `<p class="note"><strong>Leaderboard address not recognized.</strong> The address in config.js is not a Google Apps Script web app address, so the leaderboard is off.</p>` : '');
  const modeNote = () => {
    if (!started()) return '';
    if (signedIn()) {
      if (S.codeStale) return `<p class="note"><strong>The class code has changed.</strong> Open your account (top right) and enter the new code from Canvas to keep saving online.</p>`;
      return S.approved ? '' : `<p class="note"><strong>Waiting for approval.</strong> Your professor has not approved your username yet. Until then it is not shown to anyone, and you can’t log in on another device. Your progress is still being saved.</p>`;
    }
    if (NET && S.notice === 'lost') return `<p class="note"><strong>You were logged out.</strong> Your username was not approved or was removed, or your code was changed. Your progress is still on this device, and you are now playing anonymously. You can <button type="button" class="linkbtn" data-act="create">create a new username</button> or ask your professor.</p>`;
    return NET
      ? `<p class="note"><strong>Playing anonymously.</strong> Your progress is saved on this device only. ${CREDIT} <button type="button" class="linkbtn" data-act="create">Create a username</button> to join the class leaderboard.</p>`
      : `<p class="note"><strong>Saved on this device.</strong> Your progress stays in this browser. To move it to another phone or computer, open your account (top right) and copy your backup code.</p>`;
  };
  const choiceBtn = (lvl, k, big) => `<button type="button" class="choice${big ? ' big' : ''}" data-act="decor" data-l="${lvl}" data-k="${k}" aria-pressed="${S.me.decor[lvl] === k}">${ROOM.thumb(lvl, k)}<span>${esc(ROOM.optName(lvl, k))}</span></button>`;

  function viewRoom() {
    const lv = levelOf(S.me.best), next = lv + 1, owed = []; for (let l = 1; l <= lv; l++) if (!S.me.decor[l]) owed.push(l);
    let h = `<section class="card" aria-labelledby="rh"><div class="row-between"><h1 id="rh">${S.me.name ? esc(S.me.name) + '’s room' : 'Your room'}</h1><span class="tag">Level ${lv} of ${MAX}</span></div>`;
    h += `<div class="progress" role="progressbar" aria-valuemin="0" aria-valuemax="${MAX}" aria-valuenow="${lv}" aria-label="Levels completed"><i style="--n:${lv}"></i></div>`;
    h += `<div class="roomwrap">${ROOM.render(S.me.decor, { fresh: S.fresh, who: S.me.name })}</div>`;
    if (owed.length) h += `<p><strong>You have a decoration to pick for Level ${owed[0]}.</strong></p><div class="choices">${ROOM.KEYS.map((k) => choiceBtn(owed[0], k, true)).join('')}</div>`;
    if (lv < MAX) h += `<div class="actions"><button type="button" class="btn primary" data-act="play" data-l="${next}">Play Level ${next}: ${esc(QZ.LEVELS[next - 1].title)}</button></div><p class="muted small">Five questions from workbook section ${esc(QZ.LEVELS[next - 1].mod)}. Get ${PASS} right to level up and choose a new decoration.${S.me.fails[next] ? ` This will be attempt ${S.me.fails[next] + 1}, with an easier set.` : ''}</p>`;
    else h += `<p><strong>All ${MAX} levels complete.</strong> Your room is fully furnished. Replay any level for a perfect score.</p>`;
    h += `<p class="small muted" id="sync" role="status">${esc(S.sync)}</p></section>` + modeNote();
    const have = []; for (let l = 1; l <= lv; l++) if (S.me.decor[l]) have.push(l);
    if (have.length) {
      h += `<section class="card" aria-labelledby="dh"><h2 id="dh">Redecorate</h2><p class="muted small">Swap any decoration you have unlocked.</p><div class="redeco">`;
      have.forEach((l) => { h += `<div class="slot"><div class="row-between"><strong>${esc(ROOM.SLOTS[l - 1].name)}</strong><span class="tag">Level ${l}</span></div><div class="choices">${ROOM.KEYS.map((k) => choiceBtn(l, k)).join('')}</div></div>`; });
      h += '</div></section>';
    }
    return h;
  }

  function viewLevels() {
    const lv = levelOf(S.me.best);
    let h = `<section class="card"><h1>Levels</h1><p class="muted">Each challenge is ${PER} questions on one part of the workbook, drawn fresh every time. Get ${PASS} right to level up and unlock a decoration. If you miss, your next attempt at that level is easier. Get all ${PER} on a standard set for a star.</p></section>`;
    QZ.LEVELS.forEach((L) => {
      const mh = MODS.find((m) => m[0] === L.n); if (mh) h += `<h2 class="modhead">${esc(mh[1])}</h2>`;
      const b = S.me.best[L.n] || 0, done = L.n <= lv, next = L.n === lv + 1, locked = L.n > lv + 1;
      const fl = S.me.fails[L.n] || 0;
      const state = done ? `<span class="state ok">${S.me.stars[L.n] ? '★ Perfect' : 'Passed'} (best ${b}/${PER})</span>` : next ? `<span class="state">Up next${fl ? ': attempt ' + (fl + 1) + ', easier set' : ''}${b ? ' (best so far ' + b + '/' + PER + ')' : ''}</span>` : `<span class="state">Locked. Pass Level ${L.n - 1} first.</span>`;
      h += `<button type="button" class="level ${done ? 'done' : next ? 'next' : 'locked'}" data-act="play" data-l="${L.n}" ${locked ? 'disabled' : ''}><span class="num" aria-hidden="true">${L.n}</span><span class="body"><span class="ttl">Level ${L.n}: ${esc(L.title)}</span><span class="small ${locked ? '' : 'muted'}">${esc(L.skill)}</span><span class="meta"><span class="tag">Workbook ${esc(L.mod)}</span>${state}<span class="muted">Unlocks: ${esc(ROOM.SLOTS[L.n - 1].name.toLowerCase())}</span></span></span></button>`;
    });
    return h;
  }

  function viewBoard() {
    const rows = ranked(), on = signedIn(), shown = on && S.approved;
    let h = `<section class="card" aria-labelledby="bh"><div class="row-between"><h1 id="bh">Leaderboard</h1>${NET && S.classCode ? `<button type="button" class="btn quiet" data-act="refresh">Refresh</button>` : ''}</div>`;
    if (!NET) h += `<p class="muted">This copy of the game runs without a class leaderboard, so only your own progress is shown here.</p>`;
    else {
      h += `<p class="muted">Ranked by level, then by perfect scores, then by who got there first. Tap a classmate to visit their room. A username appears here only after your professor approves it.</p>`;
      if (S.codeStale) h += `<p class="note" role="status"><strong>The class code has changed.</strong> ${on ? 'Open your account (top right) and enter the new code from Canvas.' : 'Use the newest link in Canvas.'}</p>`;
      else if (!S.classCode) h += `<p class="note"><strong>Classmates appear here once the class code is entered.</strong> Create a username or log in to enter it.</p>`;
      else if (S.boardState === 'loading' || S.boardState === 'idle') h += `<p class="muted" role="status">Loading the class leaderboard…</p>`;
      else if (S.boardState === 'error') h += `<p class="note" role="status">The leaderboard can’t be reached right now. Your own progress is safe. Try Refresh in a minute.</p>`;
      else if (S.boardState === 'stale') h += `<p class="note" role="status">Showing the last leaderboard that loaded. Try Refresh in a minute.</p>`;
      if (!on) h += `<div class="actions"><button type="button" class="btn primary" data-act="create">Create a username</button><button type="button" class="btn" data-act="login">Log in</button></div>`;
      else if (!S.approved) h += `<p class="note"><strong>Waiting for approval.</strong> Classmates can’t see you until your professor approves your username.</p>`;
    }
    if (!rows.length) h += `<p>No one is on the board yet. Pass Level 1 to claim the top spot.</p>`;
    const tag = !NET || shown ? ' (you)' : on ? ' (you, waiting for approval, visible only to you)' : ' (you, visible only to you)';
    h += `<ol class="board">` + rows.map((p, i) => `<li><button type="button" class="brow${isMe(p) ? ' you' : ''}" data-act="visit" data-id="${esc(p.id)}"><span class="rank">${i + 1}</span><span class="who"><span class="nm">${esc(p.name)}${isMe(p) ? tag : ''}</span><span class="bar" aria-hidden="true"><i style="width:${p.level * 5}%"></i></span></span><span class="stat"><b>Level ${p.level}</b><span>${p.perfect} perfect</span></span></button></li>`).join('') + '</ol></section>';
    return h + (on ? '' : modeNote());
  }

  const fmtAns = (q) => { const v = Math.abs(q.ans) >= 100 ? Math.round(q.ans * 100) / 100 : Math.round(q.ans * 1000) / 1000; return (q.unit === '$' ? '$' : '') + String(v).replace('-', '−') + (q.unit === '%' ? '%' : ''); };
  function viewQuiz() {
    const z = S.quiz, L = QZ.LEVELS[z.level - 1];
    if (z.done) return viewResult();
    const q = z.qs[z.i], r = z.res[z.i];
    let h = `<section class="card" aria-labelledby="qh"><div class="qhead"><h2 id="qh" tabindex="-1">Level ${L.n}: ${esc(L.title)}</h2><button type="button" class="btn quiet" data-act="quit">Quit</button></div>`;
    h += `<div class="row-between"><span><span class="tag">Question ${z.i + 1} of ${PER}</span>${z.ease ? ` <span class="tag easy">Attempt ${z.attempt}: easier set</span>` : ''}</span><span class="dots" aria-hidden="true">${z.qs.map((_, k) => `<i class="${z.res[k] ? (z.res[k].ok ? 'ok' : 'no') : k === z.i ? 'cur' : ''}"></i>`).join('')}</span></div>`;
    h += `<p class="stem" id="stem">${escq(q.q)}</p>`;
    if (q.table) h += `<div class="scroll" tabindex="0" role="group" aria-label="Table"><table class="data">${q.table.cap ? `<caption>${escq(q.table.cap)}</caption>` : ''}<thead><tr>${q.table.head.map((c) => `<th scope="col">${escq(c)}</th>`).join('')}</tr></thead><tbody>${q.table.rows.map((row) => '<tr>' + row.map((c, ci) => { const ind = typeof c === 'string' && c.startsWith('   '); return ci === 0 ? `<th scope="row" class="${ind ? 'ind' : ''}" style="background:none;font-family:inherit;font-weight:${ind ? 400 : 700}">${escq(String(c).trim())}</th>` : `<td>${escq(c)}</td>`; }).join('') + '</tr>').join('')}</tbody></table></div>`;
    if (q.code) h += `<div class="scroll" tabindex="0" role="group" aria-label="R code or output"><pre class="code">${esc(q.code)}</pre></div><p class="swipe">Swipe sideways to see the whole output.</p>`;
    if (q.svg) h += q.svg;
    if (q.type === 'mc') {
      h += `<fieldset class="opts" ${r ? 'disabled' : ''}><legend class="sr">Answer choices</legend>` + q.opts.map((o, k) => {
        const cls = r ? (k === q.ans ? ' right' : k === r.given ? ' wrong' : '') : '';
        const mark = r ? (k === q.ans ? '<span class="mark">✓ Correct answer</span>' : k === r.given ? '<span class="mark">✗ Your answer</span>' : '') : '';
        return `<button type="button" class="opt${q.mono ? ' mono' : ''}${cls}" data-act="pick" data-k="${k}" aria-pressed="${(r ? r.given : z.sel) === k}"><span class="k" aria-hidden="true">${'ABCDE'[k]}</span><span><span class="t">${q.mono ? esc(o) : escq(o)}</span>${mark}</span></button>`;
      }).join('') + '</fieldset>';
    } else {
      h += `<div class="numrow"><label for="numIn" class="sr">Your answer</label><input id="numIn" type="text" inputmode="${/negative/i.test(q.hint || '') ? 'text' : 'decimal'}" autocomplete="off" autocapitalize="off" spellcheck="false" placeholder="Your answer" value="${r ? esc(r.given) : ''}" ${r ? 'disabled' : ''} aria-describedby="stem numHint"><span class="small muted" id="numHint">${esc(q.hint || 'Type a number')}</span></div>`;
    }
    if (!r) h += `<p class="err" id="qerr" role="alert"></p><div class="actions"><button type="button" class="btn primary" data-act="check">Check answer</button></div>`;
    else {
      h += `<div class="fb ${r.ok ? 'ok' : 'no'}" id="fb" tabindex="-1" role="status"><h3>${r.ok ? 'Correct' : 'Not quite'}</h3>${q.type === 'num' && !r.ok ? `<p><strong>Answer: ${esc(fmtAns(q))}</strong></p>` : ''}<p>${escq(q.why)}</p></div>`;
      h += `<div class="actions"><button type="button" class="btn primary" data-act="next">${z.i + 1 < PER ? 'Next question' : 'See results'}</button></div>`;
    }
    return h + '</section>';
  }
  function viewResult() {
    const z = S.quiz, L = QZ.LEVELS[z.level - 1], score = z.res.filter((r) => r.ok).length, pass = score >= PASS;
    let h = `<section class="card" aria-labelledby="qh"><h2 id="qh" tabindex="-1">${pass ? (z.firstPass ? 'Level up!' : 'Passed again') : 'Not yet'}</h2><div class="row-between"><span class="big-score">${score}/${PER}</span><span class="dots" aria-hidden="true">${z.res.map((r) => `<i class="${r.ok ? 'ok' : 'no'}"></i>`).join('')}</span></div>`;
    if (pass) {
      const starNote = z.ease ? ` You passed on an easier set. A ★ takes ${PER} of ${PER} on a standard set, so replay this level any time to earn it.` : score >= PER ? ' A perfect score on a standard set earns a ★.' : S.me.stars[L.n] ? '' : ` Get all ${PER} on a replay for a ★.`;
      h += `<p>${z.firstPass ? `You are now <strong>Level ${L.n}</strong>.` : 'Nice work.'}${starNote}</p>`;
      if (!S.me.decor[L.n]) h += `<h3>Choose your ${esc(ROOM.SLOTS[L.n - 1].name.toLowerCase())}</h3><div class="choices">${ROOM.KEYS.map((k) => choiceBtn(L.n, k, true)).join('')}</div><p class="muted small">You can swap it later from your room.</p>`;
      else h += `<div class="actions"><button type="button" class="btn primary" data-act="go" data-v="room">Back to my room</button>${L.n < MAX ? `<button type="button" class="btn" data-act="play" data-l="${L.n + 1}">Play Level ${L.n + 1}</button>` : ''}</div>`;
    } else {
      const ne = easeFor(L.n), easier = passed(L.n) ? 'Replays of a level you have already passed stay at standard difficulty.' : ne > z.ease ? `Your next attempt will be easier: ${EASE_TEXT[ne]}.` : 'Your next attempt stays at the easiest setting.';
      h += `<p>You need ${PASS} of ${PER} to pass. <strong>${easier}</strong></p><p>Review <strong>workbook section ${esc(L.mod)}</strong> before you try again.</p><div class="actions"><button type="button" class="btn primary" data-act="play" data-l="${L.n}">Try again</button><button type="button" class="btn" data-act="go" data-v="levels">All levels</button></div>`;
    }
    return h + '</section>';
  }

  /* re-render unless a question is on screen (so a typed answer is never wiped) */
  function soft() { if (S.view === 'quiz' && S.quiz && !S.quiz.done) paintHeader(); else render(); }
  /* which control has keyboard focus, so a re-render can hand focus back to the same control */
  function focusKey() {
    const a = document.activeElement; if (!a || !app.contains(a) || !a.dataset || !a.dataset.act) return null;
    return '[data-act="' + a.dataset.act + '"]' + ['l', 'k', 'id', 'v'].map((k) => (a.dataset[k] !== undefined ? `[data-${k}="${String(a.dataset[k]).replace(/[^A-Za-z0-9_-]/g, '')}"]` : '')).join('');
  }
  function render(focusSel) {
    paintHeader();
    const keep = focusSel ? null : focusKey();
    app.innerHTML = banner() + (S.view === 'quiz' && S.quiz ? viewQuiz() : S.view === 'levels' ? viewLevels() : S.view === 'board' ? viewBoard() : viewRoom());
    document.querySelectorAll('.scroll').forEach((el) => el.classList.toggle('ov', el.scrollWidth > el.clientWidth + 2));
    if (focusSel === '#fb') { const nx = $('[data-act="next"]'); if (nx && nx.scrollIntoView) nx.scrollIntoView({ block: 'nearest' }); }
    if (focusSel) { const el = $(focusSel); if (el) el.focus({ preventScroll: focusSel === '#fb' }); }
    else if (keep) { const el = app.querySelector(keep); if (el && !el.disabled) el.focus({ preventScroll: true }); }
    if (S.view !== 'quiz') S.fresh = 0;
  }
  function go(v, hash = true) {
    S.view = v; if (v !== 'quiz') S.quiz = null;
    if (hash && v !== 'quiz') { try { const hsh = v === 'board' ? '#leaderboard' : '#' + v; if (location.hash !== hsh) location.hash = hsh; } catch (e) { /* hash is a convenience only */ } }
    render(); window.scrollTo(0, 0); const top = $('.top'); if (hash && top && top.scrollIntoView) top.scrollIntoView({ block: 'start' });
    if (v === 'board') loadBoard();
  }

  /* ---------- play ---------- */
  function play(l) {
    if (!started()) { welcomeDialog(); return; }
    if (l < 1 || l > MAX || l > levelOf(S.me.best) + 1) return;
    const ease = easeFor(l);
    S.quiz = { level: l, ease, attempt: (S.me.fails[l] || 0) + 1, qs: QZ.draw(l, PER, ease), i: 0, res: [], sel: null, done: false, firstPass: false };
    S.view = 'quiz'; render('#qh'); window.scrollTo(0, 0);
  }
  function check() {
    const z = S.quiz, q = z.qs[z.i]; let ok, given;
    if (q.type === 'mc') { if (z.sel === null) { $('#qerr').textContent = 'Choose an answer first.'; return; } given = z.sel; ok = given === q.ans; }
    else { given = $('#numIn').value; if (QZ.parseNum(given) === null) { $('#qerr').textContent = 'Type your answer as a number, such as 12, 0.25, or -3.'; $('#numIn').focus(); return; } ok = QZ.checkNum(q, given); }
    z.res[z.i] = { ok, given }; render('#fb');
  }
  function next() {
    const z = S.quiz; z.i++; z.sel = null;
    if (z.i >= PER) {
      z.done = true; const score = z.res.filter((r) => r.ok).length, prev = S.me.best[z.level] || 0;
      let changed = score > prev;
      if (score > prev) S.me.best[z.level] = score;
      if (score >= PASS && prev < PASS) { z.firstPass = true; S.me.reachedAt = Date.now(); }
      if (score >= PER && !z.ease && !S.me.stars[z.level]) { S.me.stars[z.level] = 1; changed = true; }
      if (score < PASS && prev < PASS) { S.me.fails[z.level] = (S.me.fails[z.level] || 0) + 1; changed = true; }
      if (changed) save();
    }
    render('#qh'); window.scrollTo(0, 0);
  }
  function chooseDecor(l, k) {
    if (l > levelOf(S.me.best) || !ROOM.KEYS.includes(k)) return;
    const changed = S.me.decor[l] !== k; S.me.decor[l] = k; S.fresh = l; if (changed) save();
    if (S.view === 'quiz') { S.quiz = null; S.view = 'room'; try { if (location.hash !== '#room') location.hash = '#room'; } catch (e) { /* ignore */ } }
    render(); const rw = $('.roomwrap'); if (rw && rw.scrollIntoView) rw.scrollIntoView({ block: 'nearest' });
  }

  /* ---------- dialogs ---------- */
  function openDlg(kind, html) {
    dlg.dataset.kind = kind; dlg.innerHTML = html;
    if (!dlg.open) { try { dlg.showModal(); } catch (e) { dlg.setAttribute('open', ''); } }
    dlg.scrollTop = 0;
    /* focus moves at once, never on a timer, so it can't jump while someone is already typing in another box */
    const f = dlg.querySelector('input[type=text], select') || $('#dlgTitle'); if (f) { f.focus(); if (f.select && f.value) f.select(); }
  }
  function busy(form, on, msg) {
    form.setAttribute('aria-busy', String(on)); form.querySelectorAll('button, input, select').forEach((el) => { el.disabled = on; });
    const st = form.querySelector('[data-status]'); if (st) st.textContent = on ? msg : '';
  }
  const aboutLink = () => `<p><button type="button" class="linkbtn" data-act="about">About, privacy and accessibility</button></p>`;
  const status = () => `<p class="small muted" role="status" data-status></p>`;
  const classField = (id) => `<div class="field"><label for="${id}">Class code</label><input id="${id}" type="text" maxlength="24" autocomplete="off" autocapitalize="characters" spellcheck="false" aria-describedby="${id}Help"><p class="small muted" id="${id}Help">Your professor posts the class code in Canvas.</p></div>`;
  const publicNote = () => `<div class="note"><ul class="plain"><li><strong>Usernames are public.</strong> Once your professor approves yours, it appears on the class leaderboard and the login page. Don’t include your name or anything else that identifies you.</li><li><strong>Your professor approves every username</strong> before anyone else can see it.</li>${CREDIT ? `<li>${CREDIT}</li>` : ''}</ul></div>`;

  /* first visit: three clear ways to start */
  function welcomeDialog() {
    let h = `<div class="dbody"><h2 id="dlgTitle" tabindex="-1">Welcome to Room for Error</h2><p>Work through 20 levels of STA 145 skills. Each level you pass earns a statistics-themed decoration for your bedroom.</p>`;
    if (NET) h += `${CREDIT ? `<p><strong>${CREDIT}</strong></p>` : ''}<div class="ways"><button type="button" class="way" data-act="anon"><strong>Play anonymously</strong><span>No username. Your progress stays on this device.</span></button><button type="button" class="way" data-act="create"><strong>Create a username</strong><span>Join the class leaderboard. Usernames are public once your professor approves them.</span></button><button type="button" class="way" data-act="login"><strong>Log in</strong><span>You already have a username and a login code.</span></button></div><p><button type="button" class="linkbtn" data-act="restore">I have a backup code from another device</button></p>`;
    else h += `<div class="ways"><button type="button" class="way" data-act="anon"><strong>Play</strong><span>No account needed. Your progress stays on this device.</span></button><button type="button" class="way" data-act="restore"><strong>Restore from a backup code</strong><span>Bring your progress over from another device.</span></button></div>`;
    openDlg('welcome', h + aboutLink() + '</div>');
  }
  function playAnon() { S.anon = true; S.dirty = true; saveLocal(); dlg.close(); render(); }

  function createDialog(keepName) {
    openDlg('create', `<form id="createForm" novalidate><h2 id="dlgTitle" tabindex="-1">Create a username</h2>${publicNote()}<div class="field"><label for="nameIn">Username</label><input id="nameIn" type="text" maxlength="16" autocomplete="username" autocapitalize="off" spellcheck="false" value="${esc(keepName || '')}" aria-describedby="nameInHelp"><p class="small muted" id="nameInHelp">2 to 16 letters, numbers, spaces, hyphens, or underscores. Keep it classroom-friendly.</p></div>${S.classCode ? '' : classField('classIn')}<p class="err" id="createErr" role="alert"></p>${status()}<div class="actions"><button class="btn primary" type="submit">Create username</button><button class="btn" type="button" data-act="back">Back</button></div></form>`);
  }
  async function submitCreate(form) {
    const nm = cleanName($('#nameIn').value), cc = S.classCode || RFE.normClass(($('#classIn') || {}).value), err = $('#createErr');
    err.textContent = '';
    if (!validName(nm)) { err.textContent = NAME_RULE; $('#nameIn').focus(); return; }
    if (!cc) { err.textContent = 'Enter the class code from Canvas.'; $('#classIn').focus(); return; }
    busy(form, true, 'Creating your username…');
    const r = await RFE.call('join', { classCode: cc, name: nm, progress: prog() });
    busy(form, false);
    if (!r.ok && r.error === 'class_code' && S.classCode) { S.classCode = ''; saveLocal(); createDialog(nm); $('#createErr').textContent = errText(r); $('#classIn').focus(); return; }
    if (!r.ok) { err.textContent = errText(r); const f = $(r.error === 'class_code' ? '#classIn' : '#nameIn'); if (f) f.focus(); return; }
    gen++; S.me.name = cleanName(r.name) || nm; S.auth = { id: String(r.id), code: RFE.normPin(r.code) }; S.approved = r.approved === true; S.anon = false; S.classCode = cc; S.serverAt = Number(r.updatedAt) || 0; S.dirty = false; S.notice = ''; S.codeStale = false;
    saveLocal(); render(); codeDialog(); loadBoard();
  }
  function codeDialog() {
    openDlg('code', `<div class="dbody"><h2 id="dlgTitle" tabindex="-1">Write down your login code</h2><p>Your username is <strong>${esc(S.me.name)}</strong>. Your login code is:</p><p class="codebox big" id="rcBox">${esc(S.auth.code)}</p><p><strong>Write this code down now.</strong> You need your username and this code to log in again.</p>${S.approved ? '' : `<p class="note"><strong>You can’t log in again until your professor approves your username.</strong> Until then, keep playing on this device. Your progress is saved, and your username is not shown to anyone yet.</p>`}<p class="small" role="status" id="copyMsg"></p><div class="actions"><button class="btn primary" type="button" data-act="close">I wrote it down</button><button class="btn" type="button" data-act="copy" data-from="rcBox">Copy code</button></div></div>`);
  }

  /* Log in: pick an approved username from the list, then type the code. */
  async function loginDialog(msg) {
    if (!S.classCode) {
      openDlg('login', `<form id="loginClassForm" novalidate><h2 id="dlgTitle" tabindex="-1">Log in</h2>${classField('liClass')}<p class="err" id="loginClassErr" role="alert">${esc(msg || '')}</p><div class="actions"><button class="btn primary" type="submit">Continue</button><button class="btn" type="button" data-act="back">Back</button></div></form>`);
      return;
    }
    openDlg('login', `<div class="dbody"><h2 id="dlgTitle" tabindex="-1">Log in</h2><p class="muted" role="status">Loading usernames…</p><div class="actions"><button class="btn" type="button" data-act="back">Back</button></div></div>`);
    const g = gen, r = await RFE.call('board', { classCode: S.classCode });
    if (g !== gen || !dlg.open || dlg.dataset.kind !== 'login') return;
    if (!r.ok && r.error === 'class_code') { S.classCode = ''; saveLocal(); loginDialog(errText(r)); return; }
    if (!r.ok || !Array.isArray(r.players)) { openDlg('login', `<div class="dbody"><h2 id="dlgTitle" tabindex="-1">Log in</h2><p class="err" role="alert">${esc(errText(r))}</p><div class="actions"><button class="btn primary" type="button" data-act="login">Try again</button><button class="btn" type="button" data-act="back">Back</button></div></div>`); return; }
    const names = r.players.map(row).map((p) => p.name).filter(Boolean).sort((a, b) => a.localeCompare(b));
    const local = levelOf(S.me.best);
    openDlg('login', `<form id="loginForm" novalidate><h2 id="dlgTitle" tabindex="-1">Log in</h2><div class="field"><label for="liName">Username</label><select id="liName" aria-describedby="liNameHelp"><option value="">${names.length ? 'Choose your username' : 'No usernames approved yet'}</option>${names.map((n) => `<option value="${esc(n)}">${esc(n)}</option>`).join('')}</select><p class="small muted" id="liNameHelp">Don’t see your username? Your professor hasn’t approved it yet.</p></div><div class="field"><label for="liCode">Login code</label><input id="liCode" type="text" inputmode="numeric" maxlength="8" autocomplete="off" spellcheck="false" aria-describedby="liCodeHelp"><p class="small muted" id="liCodeHelp">The code you were given when you created your username.</p></div>${local ? `<label class="check" for="liKeep"><input type="checkbox" id="liKeep"><span>This device has anonymous progress (Level ${local}). Add it to my account. Leave this unticked on a shared computer.</span></label>` : ''}<p class="err" id="loginErr" role="alert"></p>${status()}<div class="actions"><button class="btn primary" type="submit">Log in</button><button class="btn" type="button" data-act="back">Back</button></div></form>`);
  }
  async function submitLogin(form) {
    const nm = cleanName($('#liName').value), code = RFE.normPin($('#liCode').value), err = $('#loginErr'), keep = !!($('#liKeep') && $('#liKeep').checked);
    err.textContent = '';
    if (!nm) { err.textContent = 'Choose your username from the list.'; $('#liName').focus(); return; }
    if (!code) { err.textContent = 'Type your login code. It is made of numbers.'; $('#liCode').focus(); return; }
    busy(form, true, 'Logging in…');
    const r = await RFE.call('signin', { classCode: S.classCode, name: nm, code });
    busy(form, false);
    if (!r.ok) { err.textContent = errText(r); $('#liCode').focus(); return; }
    const acct = sanitize(Object.assign({}, r.progress, { name: r.name }));
    gen++; S.me = keep ? mergeMax(S.me, acct) : acct;
    S.auth = { id: String(r.id), code: RFE.normPin(r.code) }; S.approved = true; S.anon = false; S.serverAt = Number(r.updatedAt) || 0; S.dirty = keep; S.notice = ''; S.codeStale = false;
    saveLocal(); dlg.close(); go('room'); if (keep) push(); loadBoard();
  }

  function restoreDialog() {
    openDlg('restore', `<form id="restoreForm" novalidate><h2 id="dlgTitle" tabindex="-1">Restore from a backup code</h2><div class="field"><label for="rsCode">Backup code</label><input id="rsCode" type="text" maxlength="60" autocomplete="off" autocapitalize="characters" spellcheck="false" aria-describedby="rsHelp"><p class="small muted" id="rsHelp">The 40-character code from your account on your other device. Restoring adds that progress to this device. It needs no username.</p></div><p class="err" id="restoreErr" role="alert"></p><div class="actions"><button class="btn primary" type="submit">Restore</button><button class="btn" type="button" data-act="back">Back</button></div></form>`);
  }
  function submitRestore() {
    const err = $('#restoreErr'), p = RFE.backupDecode($('#rsCode').value); err.textContent = '';
    if (!p) { err.textContent = 'That backup code isn’t valid. It should be 40 letters and numbers. Copy it again from your other device.'; $('#rsCode').focus(); return; }
    S.me = mergeMax(S.me, sanitize(p)); if (!started()) S.anon = true;
    dlg.close(); save(); go('room');
  }

  function accountDialog() {
    if (!started()) { welcomeDialog(); return; }
    const on = signedIn();
    let h = `<div class="dbody"><h2 id="dlgTitle" tabindex="-1">Your account</h2>`;
    if (on) {
      h += `<div class="row-between"><p>Username: <strong>${esc(S.me.name)}</strong> <span class="tag">${S.approved ? 'Approved' : 'Waiting for approval'}</span></p><button class="btn quiet" type="button" data-act="rename">Change username</button></div>`;
      if (S.codeStale) h += `<form id="codeForm" novalidate><p class="note"><strong>The class code has changed.</strong> Enter the new code from Canvas to keep saving online.</p>${classField('newClass')}<p class="err" id="codeErr" role="alert"></p>${status()}<div class="actions"><button class="btn" type="submit">Update class code</button></div></form>`;
      if (!S.approved) h += `<p class="note">Your professor has not approved your username yet. Until then it is not shown to anyone, and you can’t log in on another device.</p>`;
      h += `<p class="small muted">Your login code. You need it, with your username, to log in again:</p><p class="codebox" id="rcBox">${esc(S.auth.code)}</p><div class="actions"><button class="btn quiet" type="button" data-act="copy" data-from="rcBox">Copy login code</button></div>`;
    } else {
      h += `<p><strong>You are playing anonymously.</strong> Your progress is saved on this device only.${NET && CREDIT ? ' ' + CREDIT : ''}</p>`;
      if (NET) h += `<div class="actions"><button class="btn" type="button" data-act="create">Create a username</button><button class="btn" type="button" data-act="login">Log in</button></div>`;
    }
    h += `<hr class="sep"><h3>Backup code</h3><p class="small muted">A way to move your progress to another phone or computer with no username. Copy this code, then choose “Restore from a backup code” on the other device. The code changes whenever you make progress.</p><p class="codebox long" id="bkBox">${esc(RFE.backupEncode(S.me))}</p><div class="actions"><button class="btn quiet" type="button" data-act="copy" data-from="bkBox">Copy backup code</button><button class="btn quiet" type="button" data-act="restore">Restore from a backup code</button></div>`;
    h += `<hr class="sep"><h3>Leave or erase</h3><div class="actions">` + (on ? `<button class="btn quiet" type="button" data-act="ask" data-what="signout">Log out on this device</button><button class="btn quiet danger" type="button" data-act="ask" data-what="delete">Delete my account</button>` : `<button class="btn quiet danger" type="button" data-act="ask" data-what="erase">Erase my progress on this device</button>`) + `</div>`;
    h += `<p class="small" role="status" id="copyMsg"></p><div class="actions"><button class="btn primary" type="button" data-act="close">Close</button></div>${aboutLink()}</div>`;
    openDlg('account', h);
  }
  function renameDialog() {
    openDlg('rename', `<form id="renameForm" novalidate><h2 id="dlgTitle" tabindex="-1">Change your username</h2><p class="note"><strong>A new username has to be approved again.</strong> Until your professor approves it, you won’t appear on the leaderboard and you can’t log in on another device. Usernames are public, so don’t include anything that identifies you.</p><div class="field"><label for="nameIn">Username</label><input id="nameIn" type="text" maxlength="16" autocomplete="username" autocapitalize="off" spellcheck="false" value="${esc(S.me.name)}" aria-describedby="nameInHelp"><p class="small muted" id="nameInHelp">2 to 16 letters, numbers, spaces, hyphens, or underscores.</p></div><p class="err" id="renameErr" role="alert"></p>${status()}<div class="actions"><button class="btn primary" type="submit">Save</button><button class="btn" type="button" data-act="account">Cancel</button></div></form>`);
  }
  async function submitRename(form) {
    const nm = cleanName($('#nameIn').value), err = $('#renameErr'); err.textContent = '';
    if (!validName(nm)) { err.textContent = NAME_RULE; $('#nameIn').focus(); return; }
    if (nm === S.me.name || !signedIn()) { accountDialog(); return; }
    busy(form, true, 'Saving…');
    const r = await RFE.call('rename', { classCode: S.classCode, id: S.auth.id, code: S.auth.code, name: nm });
    busy(form, false);
    if (!r.ok && r.error === 'bad_login') { lostLogin(); render(); accountDialog(); return; }
    if (!r.ok) { err.textContent = errText(r); $('#nameIn').focus(); return; }
    gen++; S.me.name = cleanName(r.name) || nm; S.approved = r.approved === true; saveLocal(); render(); accountDialog(); push(); loadBoard();
  }
  async function submitClassCode(form) {
    const cc = RFE.normClass($('#newClass').value), err = $('#codeErr'); err.textContent = '';
    if (!cc) { err.textContent = 'Enter the class code from Canvas.'; return; }
    busy(form, true, 'Checking…');
    const r = await RFE.call('ping', { classCode: cc });
    busy(form, false);
    if (!r.ok) { err.textContent = errText(r); return; }
    S.classCode = cc; S.codeStale = false; saveLocal(); render(); accountDialog(); push(); loadBoard();
  }

  const ASK = {
    signout: () => ({ title: 'Log out on this device?', body: `This removes your username, progress, and login code from this browser. Your progress stays saved online.</p><p>${S.approved ? `To come back you will need your username, <strong>${esc(S.me.name)}</strong>, and your login code, <strong>${esc(S.auth.code)}</strong>.` : `<strong>Your username has not been approved yet, so you will not be able to log back in until it is.</strong> Your username is <strong>${esc(S.me.name)}</strong> and your login code is <strong>${esc(S.auth.code)}</strong>.`}`, yes: 'Log out' }),
    delete: () => ({ title: 'Delete your account?', body: 'This permanently removes your username and progress from the class leaderboard and from online storage. Your progress stays on this device, and you keep playing anonymously.', yes: 'Delete my account' }),
    erase: () => ({ title: 'Erase your progress on this device?', body: 'This removes all progress from this browser. It can’t be undone unless you saved a backup code.', yes: 'Erase' })
  };
  function askDialog(what) {
    const a = ASK[what] && ASK[what](); if (!a) return;
    openDlg('ask', `<form id="askForm" data-what="${what}" novalidate><h2 id="dlgTitle" tabindex="-1">${a.title}</h2><p>${a.body}</p><p class="err" id="askErr" role="alert"></p>${status()}<div class="actions"><button class="btn primary" type="submit">${a.yes}</button><button class="btn" type="button" data-act="account">Cancel</button></div></form>`);
  }
  function wipeDevice() { gen++; again = false; try { localStorage.removeItem(LS); } catch (e) { /* ignore */ } S.me = blank(); S.anon = false; S.auth = null; S.approved = false; S.classCode = ''; S.serverAt = 0; S.dirty = false; S.notice = ''; S.codeStale = false; S.quiz = null; S.board = []; S.boardAt = 0; S.boardState = NET ? 'idle' : 'off'; setSync(''); go('room'); welcomeDialog(); }
  async function submitAsk(form) {
    const what = form.dataset.what, err = $('#askErr'); err.textContent = '';
    if (what === 'erase') { wipeDevice(); return; }
    if (what === 'signout') {
      if (S.dirty || syncing) { busy(form, true, 'Saving your latest progress…'); await push(); for (let i = 0; i < 150 && syncing; i++) await new Promise((r) => setTimeout(r, 100)); busy(form, false); if (S.dirty && signedIn() && form.dataset.warned !== '1') { form.dataset.warned = '1'; err.textContent = 'Your latest progress hasn’t reached the leaderboard yet. Check your connection, or press Log out again to leave anyway.'; return; } }
      wipeDevice(); return;
    }
    busy(form, true, 'Deleting…');
    const r = await RFE.call('delete', { classCode: S.classCode, id: S.auth.id, code: S.auth.code });
    busy(form, false);
    if (!r.ok && r.error !== 'bad_login') { err.textContent = errText(r); return; }
    gen++; S.auth = null; S.approved = false; S.me.name = ''; S.anon = true; S.serverAt = 0; S.dirty = true; S.notice = ''; saveLocal(); render(); accountDialog(); loadBoard();
    const m = $('#copyMsg'); if (m) m.textContent = 'Your account was deleted. You are now playing anonymously.';
  }

  function aboutDialog() {
    const c = (k, d) => esc(CFG[k] || d);
    const ways = NET
      ? `<h3>Three ways to play</h3><ul><li><strong>Anonymously.</strong> No username. Your scores and decorations are saved only in this browser, on this device. Nothing is sent anywhere.</li><li><strong>With a username.</strong> Your username, scores, number of tries, and decorations are also saved in a Google Sheet that your professor controls. Your professor approves every username. Once approved it is public: anyone with the class code can see your username, level, number of perfect scores, and room on the leaderboard, and your username on the login page.</li><li><strong>Logging in.</strong> Your username and login code bring your progress to any device.</li></ul>`
      : `<h3>What is saved</h3><ul><li>Your scores and decorations are saved only in this browser, on this device. Nothing you do in the game is sent anywhere.</li></ul>`;
    const meta = NET
      ? `<h3>What is recorded besides your scores</h3><ul><li>If you create a username, the Sheet also records three times: when you created it, when you reached your current level, and when your progress was last saved. Together they show when you played and how quickly you moved through the levels.</li><li>It also counts how many wrong login codes have been tried against your username.</li><li>The game does not record which questions you saw, which answers you chose, or how long each one took. It keeps only your best score and number of failed tries on each level.</li><li>If you play anonymously, none of this is recorded anywhere but your own browser.</li></ul>`
      : `<h3>What is recorded besides your scores</h3><ul><li>The game does not record which questions you saw, which answers you chose, or how long each one took. It keeps only your best score and number of failed tries on each level, in your own browser.</li></ul>`;
    openDlg('about', `<div class="dbody about"><h2 id="dlgTitle" tabindex="-1">About Room for Error</h2><p>A practice game for ${c('course', 'STA 145')}, made by ${c('instructor', 'your professor')} with the help of Claude AI, an AI model from Anthropic. It is a course tool, not an official website of The College of New Jersey.</p><p><strong>Many errors are present.</strong> The questions were written with AI from the course workbook and have not all been checked. If an answer looks wrong, tell your professor.</p>${CREDIT ? `<p><strong>${CREDIT}</strong></p>` : ''}${ways}${meta}
<h3>What is never collected</h3><ul><li>The game never asks for your real name, email address, student ID, or a password of your own. It has no ads, no analytics, and no tracking cookies.</li><li>Like any website, the service that delivers this page (${c('host', 'the web host')}) receives your device’s internet address when the page loads${NET ? ', and so does Google when the leaderboard is used' : ''}. The game never sees or stores it.</li></ul>
<h3>Your choices</h3><ul>${NET ? '<li>Don’t put your name, or anything else that identifies you, in a username.</li><li>You can delete your account at any time from your account (top right). Your progress stays on your device.</li><li>Your professor can rename or remove a username, and deletes every account after the semester ends.</li><li>The login code is short and easy to guess. It keeps mix-ups out; it is not strong security, so treat this as a game account.</li>' : ''}<li>On a shared or public computer, open your account when you finish and ${NET ? 'log out or ' : ''}erase your progress from that device.</li></ul>
<h3>Accessibility</h3><ul><li>The game works with a keyboard, a screen reader, browser zoom, and dark mode, and every chart has a text description.</li><li>If any part of it does not work for you, ${c('contact', 'contact your professor')} to get the same practice in another format.</li></ul>
<h3>Questions</h3><p>For anything about this game or what it saves, ${c('contact', 'contact your professor')}.</p><div class="actions"><button class="btn primary" type="button" data-act="back">Close</button></div></div>`);
  }

  function visit(id) {
    const p = ranked().find((x) => x.id === id); if (!p) return;
    const names = []; for (let l = 1; l <= MAX; l++) if (p.decor[l]) names.push(ROOM.optName(l, p.decor[l]));
    openDlg('visit', `<div class="dbody"><div class="row-between"><h2 id="dlgTitle" tabindex="-1">${isMe(p) && !S.me.name ? 'Your room' : esc(p.name) + '’s room'}</h2><span class="tag">Level ${p.level} · ${p.perfect} perfect</span></div><div class="roomwrap" style="box-shadow:none">${ROOM.render(p.decor, { who: isMe(p) ? S.me.name : p.name })}</div>${names.length ? `<details><summary>${names.length} decoration${names.length === 1 ? '' : 's'}</summary><ul>${names.map((n) => `<li>${esc(n)}</li>`).join('')}</ul></details>` : '<p class="muted">No decorations yet.</p>'}<div class="actions"><button class="btn primary" type="button" data-act="close">Close</button></div></div>`);
  }
  async function copyFrom(id) {
    const box = $('#' + id), msg = $('#copyMsg'); if (!box) return;
    let ok = false;
    try { await navigator.clipboard.writeText(box.textContent.trim()); ok = true; } catch (e) { try { const r = document.createRange(); r.selectNodeContents(box); const sel = getSelection(); sel.removeAllRanges(); sel.addRange(r); ok = document.execCommand('copy'); } catch (e2) { ok = false; } }
    if (msg) msg.textContent = ok ? 'Copied.' : 'Copying is blocked in this browser. Select the code and copy it by hand.';
  }

  /* ---------- events ---------- */
  document.addEventListener('click', (ev) => {
    const el = ev.target.closest('[data-act]'); if (!el || el.disabled) return; const a = el.dataset.act;
    if (a === 'play') play(Number(el.dataset.l));
    else if (a === 'pick') { if (S.quiz && !S.quiz.res[S.quiz.i]) { S.quiz.sel = Number(el.dataset.k); document.querySelectorAll('.opt').forEach((o) => o.setAttribute('aria-pressed', String(o === el))); const e = $('#qerr'); if (e) e.textContent = ''; } }
    else if (a === 'check') check();
    else if (a === 'next') next();
    else if (a === 'quit') go('levels');
    else if (a === 'go') go(el.dataset.v);
    else if (a === 'decor') chooseDecor(Number(el.dataset.l), el.dataset.k);
    else if (a === 'anon') playAnon();
    else if (a === 'create') { if (NET && !signedIn()) createDialog(); }
    else if (a === 'login') { if (NET && !signedIn()) loginDialog(); }
    else if (a === 'restore') restoreDialog();
    else if (a === 'account') accountDialog();
    else if (a === 'rename') renameDialog();
    else if (a === 'about') aboutDialog();
    else if (a === 'ask') askDialog(el.dataset.what);
    else if (a === 'copy') copyFrom(el.dataset.from);
    else if (a === 'refresh') { loadBoard(); push(); }
    else if (a === 'visit') visit(el.dataset.id);
    else if (a === 'back') { if (!started()) welcomeDialog(); else if (dlg.dataset.kind === 'about') dlg.close(); else accountDialog(); }
    else if (a === 'close') dlg.close();
  });
  document.addEventListener('submit', (ev) => {
    const f = ev.target; ev.preventDefault();
    if (f.id === 'createForm') submitCreate(f); else if (f.id === 'loginForm') submitLogin(f); else if (f.id === 'loginClassForm') { const cc = RFE.normClass($('#liClass').value); if (!cc) { $('#loginClassErr').textContent = 'Enter the class code from Canvas.'; return; } S.classCode = cc; loginDialog(); }
    else if (f.id === 'restoreForm') submitRestore(); else if (f.id === 'renameForm') submitRename(f); else if (f.id === 'codeForm') submitClassCode(f); else if (f.id === 'askForm') submitAsk(f);
  });
  document.addEventListener('keydown', (ev) => { if (ev.key === 'Enter' && ev.target.id === 'numIn') { ev.preventDefault(); check(); } });
  dlg.addEventListener('cancel', (ev) => { if (!started() && dlg.dataset.kind === 'welcome') ev.preventDefault(); });
  dlg.addEventListener('close', () => { if (!started()) { setTimeout(() => { if (!started() && !dlg.open) welcomeDialog(); }, 0); return; } if (S.view === 'board') render(); });
  document.querySelectorAll('.tabs a').forEach((a) => a.addEventListener('click', (ev) => { ev.preventDefault(); go(a.dataset.tab); }));
  window.addEventListener('hashchange', () => { const v = { '#room': 'room', '#levels': 'levels', '#leaderboard': 'board' }[location.hash]; if (v && v !== S.view && S.view !== 'quiz') go(v, false); });
  document.addEventListener('visibilitychange', () => { if (document.visibilityState !== 'visible') return; if (signedIn() && Date.now() - lastPush > 30000 && !(S.view === 'quiz' && S.quiz && !S.quiz.done)) push(); if (S.view === 'board' && !dlg.open) loadBoard(); });

  /* ---------- boot ---------- */
  { const w = $('#footWho'); if (w && CFG.instructor) w.textContent = 'A course practice tool by ' + CFG.instructor + '. Not an official website of The College of New Jersey.'; }
  loadLocal();
  /* A class code can ride in on the link posted in Canvas (…?class=CODE). It is remembered, then removed from the address bar. */
  try {
    const u = new URL(location.href), c = RFE.normClass(u.searchParams.get('class'));
    if (u.searchParams.has('class')) { if (NET && c && c !== S.classCode && !S.auth) { S.classCode = c; saveLocal(); } u.searchParams.delete('class'); history.replaceState(null, '', u.pathname + u.search + u.hash); }
  } catch (e) { /* older browsers: the student types the code instead */ }
  const hv = { '#room': 'room', '#levels': 'levels', '#leaderboard': 'board' }[location.hash]; if (hv) S.view = hv;
  render();
  if (!started()) welcomeDialog();
  if (signedIn()) push();
  if (S.view === 'board') loadBoard();
  setInterval(() => { if (S.view === 'board' && document.visibilityState === 'visible' && !dlg.open) loadBoard(); }, 60000);
})();
