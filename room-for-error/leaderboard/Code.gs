/**
 * Room for Error: class leaderboard for the STA 145 leveling game.
 * Google Apps Script, bound to a Google Sheet. Built with Claude (Anthropic).
 *
 * The next line limits this script's permission to the one spreadsheet it is attached to.
 * @OnlyCurrentDoc
 *
 * What this script stores, in the "players" tab of this sheet, one row per player:
 *   a random row id, the username the student chose, whether you have approved it, their game
 *   progress (best score per level, stars, failed-attempt counts, decoration choices, timestamps),
 *   and a count of wrong login codes tried against that username.
 * What it never stores or asks for: real names, email addresses, student IDs, IP addresses,
 *   passwords, or login codes. A login code is recomputed from the row id each time it is
 *   checked (see codeFor), so no login credential is kept in the sheet.
 *
 * APPROVAL: a new username is not shown on the leaderboard or the login page, and cannot be used
 *   to log in, until you tick its box in column C. Changing a username un-ticks the box.
 */

const SHEET_NAME = 'players';
const HEAD = ['id', 'name', 'approved', 'level', 'perfect', 'best', 'stars', 'fails', 'decor', 'reachedAt', 'updatedAt', 'createdAt', 'codeVersion', 'wrongCodes'];
const COL = {}; HEAD.forEach(function (h, i) { COL[h] = i; });
const MAX_PLAYERS = 600, MAX_LEVEL = 20, PER = 5, PASS = 4, MAX_BODY = 6000;
/* Login codes. Two digits is easy to remember and easy to guess: 1 chance in 100 per try. Raise CODE_DIGITS to 4 for 1 in 10,000.
   After MAX_TRIES wrong codes, that username is locked for LOCK_SECONDS. */
const CODE_DIGITS = 2, MAX_TRIES = 3, LOCK_SECONDS = 3600;
const NAME_RE = /^[A-Za-z0-9][A-Za-z0-9 _-]{1,15}$/;
const B32 = '0123456789ABCDEFGHJKMNPQRSTVWXYZ';

/* ---------------- menu (instructor tools) ---------------- */

function onOpen() {
  SpreadsheetApp.getUi().createMenu('Room for Error')
    .addItem('1. Set up this sheet', 'setup')
    .addItem('Show usernames waiting for approval', 'showWaiting')
    .addItem('Show the class code', 'showClassCode')
    .addItem('Make a new class code', 'newClassCode')
    .addItem('Give a player a new login code', 'resetReturnCode')
    .addItem('Delete all players (end of semester)', 'deleteAllPlayers')
    .addToUi();
}

function setup() {
  const ss = SpreadsheetApp.getActiveSpreadsheet();
  let sh = ss.getSheetByName(SHEET_NAME);
  if (!sh) sh = ss.insertSheet(SHEET_NAME);
  if (sh.getLastRow() === 0) { sh.appendRow(HEAD); sh.setFrozenRows(1); }
  sh.getRange('B:B').setNumberFormat('@'); /* usernames stay plain text, so "007" or "3-4" is never turned into a number or a date */
  const props = PropertiesService.getScriptProperties();
  if (!props.getProperty('SECRET')) props.setProperty('SECRET', Utilities.getUuid() + Utilities.getUuid());
  if (!props.getProperty('CLASS_CODE')) props.setProperty('CLASS_CODE', randomClassCode());
  SpreadsheetApp.getUi().alert('Set up is complete.\n\nClass code: ' + props.getProperty('CLASS_CODE') + '\n\nGive this code to your students inside Canvas. New usernames wait in the players tab until you tick their box in column C.\n\nNext: Deploy > New deployment > Web app.');
}

function showWaiting() {
  const sh = sheet(), names = (sh ? readRows(sh) : []).filter(function (r) { return !isTrue(r[COL.approved]); }).map(function (r) { return String(r[COL.name]); });
  SpreadsheetApp.getUi().alert(names.length ? names.length + ' waiting for approval:\n\n' + names.join('\n') + '\n\nTick the box in column C to approve a username. Delete the row to refuse it.' : 'No usernames are waiting for approval.');
}

function showClassCode() {
  SpreadsheetApp.getUi().alert('Class code: ' + (PropertiesService.getScriptProperties().getProperty('CLASS_CODE') || '(run "Set up this sheet" first)'));
}

function newClassCode() {
  const code = randomClassCode();
  PropertiesService.getScriptProperties().setProperty('CLASS_CODE', code);
  SpreadsheetApp.getUi().alert('New class code: ' + code + '\n\nThe old code no longer works. Students who already joined will be asked for the new one.');
}

function deleteAllPlayers() {
  const ui = SpreadsheetApp.getUi(), sh = sheet();
  if (!sh || lastRow(sh) < 2) { ui.alert('There are no players to delete.'); return; }
  const n = lastRow(sh) - 1;
  if (ui.alert('Delete all ' + n + ' players?', 'This permanently removes every username and all progress from this sheet. Students keep the progress saved in their own browsers.', ui.ButtonSet.OK_CANCEL) !== ui.Button.OK) return;
  sh.getRange(2, 1, n, HEAD.length).clearContent().clearDataValidations(); /* emptied, not deleted: Sheets refuses to delete every row below the header */
  ui.alert('All players were deleted. Make a new class code before you reuse this sheet with another class.');
}

function resetReturnCode() {
  const ui = SpreadsheetApp.getUi();
  const ans = ui.prompt('Give a player a new login code', 'Type the player\'s username exactly as it appears in the name column:', ui.ButtonSet.OK_CANCEL);
  if (ans.getSelectedButton() !== ui.Button.OK) return;
  const want = String(ans.getResponseText()).trim().toLowerCase();
  const sh = sheet(), rows = readRows(sh);
  for (let i = 0; i < rows.length; i++) {
    if (String(rows[i][COL.name]).toLowerCase() === want) {
      const cur = Number(rows[i][COL.codeVersion]) || 1, old = codeFor(rows[i][COL.id], cur);
      let v = cur + 1; while (codeFor(rows[i][COL.id], v) === old && v < cur + 60) v++; /* short codes can repeat by chance, so keep going until it changes */
      sh.getRange(i + 2, COL.codeVersion + 1).setValue(v);
      ui.alert('New login code for ' + rows[i][COL.name] + ': ' + codeFor(rows[i][COL.id], v) + '\n\nGive it to the student. Their old code no longer works.');
      return;
    }
  }
  ui.alert('No player has that username.');
}

/* ---------------- web app ---------------- */

function doGet() { return out({ ok: true, service: 'room-for-error', version: 1 }); }

function doPost(e) {
  let req = null;
  try {
    const raw = e && e.postData && e.postData.contents;
    if (!raw || raw.length > MAX_BODY) return out({ ok: false, error: 'bad_request' });
    req = JSON.parse(raw);
  } catch (err) { return out({ ok: false, error: 'bad_request' }); }
  try { return out(handle(req)); } catch (err) { return out({ ok: false, error: 'server' }); }
}

function out(obj) { return ContentService.createTextOutput(JSON.stringify(obj)).setMimeType(ContentService.MimeType.JSON); }

function handle(req) {
  if (!req || typeof req !== 'object') return { ok: false, error: 'bad_request' };
  const props = PropertiesService.getScriptProperties();
  const secret = props.getProperty('SECRET'), classCode = props.getProperty('CLASS_CODE');
  if (!secret || !classCode) return { ok: false, error: 'not_set_up' };
  if (normClass(req.classCode) !== classCode) return { ok: false, error: 'class_code' };
  switch (req.action) {
    case 'ping': return { ok: true };
    case 'board': return board();
    case 'join': return locked(function () { return join(req); });
    case 'signin': return locked(function () { return signin(req); });
    case 'sync': return locked(function () { return sync(req); });
    case 'rename': return locked(function () { return rename(req); });
    case 'delete': return locked(function () { return remove(req); });
    default: return { ok: false, error: 'bad_request' };
  }
}

function locked(fn) {
  const lock = LockService.getScriptLock();
  lock.waitLock(20000);
  try { return fn(); } finally { lock.releaseLock(); }
}

/* ---------------- actions ---------------- */

function board() {
  const rows = readRows(sheet()), players = [];
  rows.forEach(function (r) {
    if (!isTrue(r[COL.approved])) return; /* only approved usernames are ever sent to a browser */
    if (!NAME_RE.test(String(r[COL.name]))) return;
    players.push({ id: String(r[COL.id]), name: String(r[COL.name]), level: Number(r[COL.level]) || 0, perfect: Number(r[COL.perfect]) || 0, decor: cleanDecor(parse(r[COL.decor]), MAX_LEVEL), reachedAt: Number(r[COL.reachedAt]) || 0 });
  });
  return { ok: true, players: players, at: Date.now() };
}

function join(req) {
  const name = cleanName(req.name);
  if (!NAME_RE.test(name)) return { ok: false, error: 'name_invalid' };
  const sh = sheet(), rows = readRows(sh);
  if (rows.length >= MAX_PLAYERS) return { ok: false, error: 'full' };
  if (findByName(rows, name) >= 0) return { ok: false, error: 'name_taken' };
  const id = Utilities.getUuid(), now = Date.now(), p = cleanProgress(req.progress);
  const r = lastRow(sh) + 1; /* the first empty row, found from column A (see lastRow) */
  if (r > sh.getMaxRows()) sh.insertRowsAfter(sh.getMaxRows(), 50);
  sh.getRange(r, 1, 1, HEAD.length).setValues([[id, '', false, p.level, p.perfect, JSON.stringify(p.best), JSON.stringify(p.stars), JSON.stringify(p.fails), JSON.stringify(p.decor), p.reachedAt, now, now, 1, 0]]);
  setName(sh, r, name);
  sh.getRange(r, COL.approved + 1).insertCheckboxes(); /* the approval box for this username, unticked */
  return { ok: true, id: id, name: name, code: codeFor(id, 1), approved: false, progress: p, updatedAt: now };
}

function signin(req) {
  const name = cleanName(req.name), key = 'fail:' + name.toLowerCase(), cache = CacheService.getScriptCache();
  const fails = Number(cache.get(key)) || 0;
  if (fails >= MAX_TRIES) return { ok: false, error: 'locked' };
  const sh = sheet(), rows = readRows(sh), i = findByName(rows, name);
  if (i < 0 || normCode(req.code) !== codeFor(rows[i][COL.id], rows[i][COL.codeVersion])) {
    cache.put(key, String(fails + 1), LOCK_SECONDS);
    if (i >= 0) sh.getRange(i + 2, COL.wrongCodes + 1).setValue((Number(rows[i][COL.wrongCodes]) || 0) + 1); /* a running count you can see in the sheet */
    return { ok: false, error: 'bad_login' };
  }
  if (!isTrue(rows[i][COL.approved])) return { ok: false, error: 'pending' };
  cache.remove(key);
  return { ok: true, id: String(rows[i][COL.id]), name: String(rows[i][COL.name]), code: codeFor(rows[i][COL.id], rows[i][COL.codeVersion]), approved: true, progress: rowProgress(rows[i]), updatedAt: Number(rows[i][COL.updatedAt]) || 0 };
}

/* Merge what the device has with what the sheet has, store the result, and send it back.
   Scores only ever go up, so they merge by taking the higher one. Decorations can be swapped, so one side has to win:
   a device that has already seen the sheet's latest save (req.base) wins, otherwise the sheet does. */
function sync(req) {
  const sh = sheet(), rows = readRows(sh), i = findAuthed(rows, req);
  if (i < 0) return { ok: false, error: 'bad_login' };
  const old = rowProgress(rows[i]), inc = cleanProgress(req.progress), was = Number(rows[i][COL.updatedAt]) || 0, current = (Number(req.base) || 0) >= was;
  const incDecor = cleanDecor(req.progress && req.progress.decor, MAX_LEVEL); /* checked against the merged scores below, not the device's alone */
  const m = { best: {}, stars: {}, fails: {}, decor: {} };
  for (let l = 1; l <= MAX_LEVEL; l++) {
    const b = Math.max(old.best[l] || 0, inc.best[l] || 0); if (b) m.best[l] = b;
    if (old.stars[l] || inc.stars[l]) m.stars[l] = 1;
    const f = Math.max(old.fails[l] || 0, inc.fails[l] || 0); if (f) m.fails[l] = f;
    const d = current ? (incDecor[l] || old.decor[l]) : (old.decor[l] || incDecor[l]); if (d) m.decor[l] = d;
  }
  m.reachedAt = levelOf(inc.best) > levelOf(old.best) ? inc.reachedAt : (old.reachedAt || inc.reachedAt);
  const p = cleanProgress(m);
  const next = [p.level, p.perfect, JSON.stringify(p.best), JSON.stringify(p.stars), JSON.stringify(p.fails), JSON.stringify(p.decor), p.reachedAt];
  const prev = [old.level, old.perfect, JSON.stringify(old.best), JSON.stringify(old.stars), JSON.stringify(old.fails), JSON.stringify(old.decor), old.reachedAt];
  let at = was;
  if (JSON.stringify(next) !== JSON.stringify(prev)) { at = Math.max(Date.now(), was + 1); sh.getRange(i + 2, COL.level + 1, 1, 8).setValues([next.concat([at])]); }
  return { ok: true, name: String(rows[i][COL.name]), approved: isTrue(rows[i][COL.approved]), progress: p, updatedAt: at };
}

function rename(req) {
  const name = cleanName(req.name);
  if (!NAME_RE.test(name)) return { ok: false, error: 'name_invalid' };
  const sh = sheet(), rows = readRows(sh), i = findAuthed(rows, req);
  if (i < 0) return { ok: false, error: 'bad_login' };
  const j = findByName(rows, name);
  if (j >= 0 && j !== i) return { ok: false, error: 'name_taken' };
  if (String(rows[i][COL.name]) === name) return { ok: true, name: name, approved: isTrue(rows[i][COL.approved]) };
  setName(sh, i + 2, name);
  sh.getRange(i + 2, COL.approved + 1).setValue(false); /* a changed username has to be approved again */
  return { ok: true, name: name, approved: false };
}

function remove(req) {
  const sh = sheet(), rows = readRows(sh), i = findAuthed(rows, req);
  if (i < 0) return { ok: false, error: 'bad_login' };
  if (sh.getMaxRows() - sh.getFrozenRows() <= 1) sh.getRange(i + 2, 1, 1, HEAD.length).clearContent().clearDataValidations(); else sh.deleteRow(i + 2);
  return { ok: true };
}

/* ---------------- helpers ---------------- */

function setName(sh, row, name) { sh.getRange(row, COL.name + 1).setNumberFormat('@').setValue(name); }
function isTrue(v) { return v === true || String(v).toUpperCase() === 'TRUE'; }
function sheet() { return SpreadsheetApp.getActiveSpreadsheet().getSheetByName(SHEET_NAME); }
/* The last row that holds a player, judged by column A. Sheets counts an unticked checkbox as content, so getLastRow() alone
   can point far below the real data when a column is full of checkboxes. */
function lastRow(sh) {
  const n = sh.getLastRow(); if (n < 2) return n;
  const ids = sh.getRange(1, 1, n, 1).getValues();
  let r = n; while (r > 1 && String(ids[r - 1][0]) === '') r--;
  return r;
}
function readRows(sh) { const n = lastRow(sh); return n < 2 ? [] : sh.getRange(2, 1, n - 1, HEAD.length).getValues(); }
function findByName(rows, name) { const want = String(name).toLowerCase(); for (let i = 0; i < rows.length; i++) if (String(rows[i][COL.name]).toLowerCase() === want) return i; return -1; }
function findAuthed(rows, req) {
  const id = String(req.id || '');
  for (let i = 0; i < rows.length; i++) if (String(rows[i][COL.id]) === id) return normCode(req.code) === codeFor(id, rows[i][COL.codeVersion]) ? i : -1;
  return -1;
}
function cleanName(s) { return String(s == null ? '' : s).replace(/\s+/g, ' ').trim().slice(0, 16); }
function normClass(s) { return String(s == null ? '' : s).toUpperCase().replace(/[^A-Z0-9]/g, ''); }
function normCode(s) { return String(s == null ? '' : s).replace(/[^0-9]/g, ''); }
function parse(s) { try { const o = JSON.parse(String(s)); return o && typeof o === 'object' ? o : {}; } catch (e) { return {}; } }
function levelOf(best) { let l = 0; while (l < MAX_LEVEL && (best[l + 1] || 0) >= PASS) l++; return l; }
function cleanDecor(d, level) { const o = {}; for (let l = 1; l <= level; l++) if (d && (d[l] === 'a' || d[l] === 'b' || d[l] === 'c')) o[l] = d[l]; return o; }

/* Keep only well-formed progress, and recompute level and star count from it. */
function cleanProgress(p) {
  p = p && typeof p === 'object' ? p : {};
  const o = { best: {}, stars: {}, fails: {}, decor: {}, reachedAt: 0 };
  for (let l = 1; l <= MAX_LEVEL; l++) {
    const b = Number(p.best && p.best[l]); if (b === Math.floor(b) && b >= 1 && b <= PER) o.best[l] = b;
    if (p.stars && p.stars[l] && o.best[l] === PER) o.stars[l] = 1;
    const f = Number(p.fails && p.fails[l]); if (f === Math.floor(f) && f >= 1) o.fails[l] = Math.min(f, 99);
  }
  o.level = levelOf(o.best);
  o.decor = cleanDecor(p.decor, o.level);
  o.perfect = Object.keys(o.stars).length;
  const r = Number(p.reachedAt); o.reachedAt = r > 0 && r < Date.now() + 86400000 ? Math.floor(r) : 0;
  return o;
}
function rowProgress(r) { return cleanProgress({ best: parse(r[COL.best]), stars: parse(r[COL.stars]), fails: parse(r[COL.fails]), decor: parse(r[COL.decor]), reachedAt: r[COL.reachedAt] }); }

/* A login code is worked out from the row id and this script's private key (HMAC-SHA-256). Nothing about it is stored in the sheet. */
function codeFor(id, version) {
  const secret = PropertiesService.getScriptProperties().getProperty('SECRET');
  const sig = Utilities.computeHmacSha256Signature(String(id) + ':' + (Number(version) || 1), secret);
  let n = 0;
  for (let i = 0; i < 4; i++) n = n * 256 + ((sig[i] + 256) % 256);
  return ('00000000' + String(n % Math.pow(10, CODE_DIGITS))).slice(-CODE_DIGITS);
}

function randomClassCode() {
  const hex = Utilities.getUuid().replace(/-/g, '').toUpperCase();
  let s = '';
  for (let i = 0; i < 6; i++) s += B32.charAt(parseInt(hex.substr(i * 2, 2), 16) % 32);
  return 'STA' + s;
}
