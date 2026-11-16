/* Room for Error: the optional class leaderboard connection, and backup codes that need no server at all. */
const RFE = (() => {
  const B32 = '0123456789ABCDEFGHJKMNPQRSTVWXYZ';
  const W = typeof window !== 'undefined' ? window : {};
  const cfg = W.RFE_CONFIG || {};
  /* The only address this page will ever send anything to is a Google Apps Script web app. Anything else in config.js is ignored. */
  const URL_OK = /^https:\/\/script\.google\.com\/macros\/s\/[A-Za-z0-9_-]{20,}\/exec$/;
  const raw = String(cfg.leaderboardUrl || '').trim();
  /* A stand-in leaderboard that lives inside the page. Nothing in this folder defines one; it exists so the game can be tried out and tested without a Google Sheet. */
  const preview = typeof W.RFE_PREVIEW_BACKEND === 'function';
  const url = URL_OK.test(raw) ? raw : '';
  const enabled = preview || !!url;
  const misconfigured = !!raw && !url && !preview;

  const normClass = (s) => String(s == null ? '' : s).toUpperCase().replace(/[^A-Z0-9]/g, '').slice(0, 24);
  const normCode = (s) => String(s == null ? '' : s).toUpperCase().replace(/O/g, '0').replace(/[IL]/g, '1').replace(/[^0-9A-Z]/g, '');
  const group = (s) => String(s).replace(/(.{4})(?=.)/g, '$1-');
  const normPin = (s) => String(s == null ? '' : s).replace(/[^0-9]/g, '').slice(0, 8); /* login codes are digits only */

  /* One request = one small JSON message. No cookies are sent (credentials: 'omit') and no referrer. */
  async function call(action, body) {
    const msg = JSON.stringify(Object.assign({ action }, body));
    if (preview) { await new Promise((r) => setTimeout(r, 120)); try { return JSON.parse(W.RFE_PREVIEW_BACKEND(msg)); } catch (e) { return { ok: false, error: 'server' }; } }
    if (!url) return { ok: false, error: 'off' };
    const ctrl = new AbortController(), timer = setTimeout(() => ctrl.abort(), 25000);
    try {
      const res = await fetch(url, { method: 'POST', body: msg, headers: { 'Content-Type': 'text/plain;charset=utf-8' }, redirect: 'follow', credentials: 'omit', referrerPolicy: 'no-referrer', cache: 'no-store', signal: ctrl.signal });
      if (!res.ok) return { ok: false, error: 'network' };
      const data = await res.json();
      return data && typeof data === 'object' ? data : { ok: false, error: 'server' };
    } catch (e) { return { ok: false, error: 'network' }; } finally { clearTimeout(timer); }
  }

  /* ---------- backup codes ----------
     A backup code is the player's progress itself, packed into 40 letters and numbers: one byte per level
     (best score, star, decoration, failed tries), four bytes for when the top level was reached, one check byte.
     It holds no username and nothing personal, and it is never sent anywhere. */
  const EPOCH_MIN = Date.UTC(2026, 0, 1) / 60000;
  function crc(bytes) { let c = 0x5a; for (const b of bytes) { c ^= b; for (let i = 0; i < 8; i++) c = c & 0x80 ? ((c << 1) ^ 0x07) & 0xff : (c << 1) & 0xff; } return c; }
  function toB32(bytes) { let acc = 0, n = 0, out = ''; for (const b of bytes) { acc = (acc << 8) | b; n += 8; while (n >= 5) { out += B32[(acc >>> (n - 5)) & 31]; n -= 5; } acc &= (1 << n) - 1; } if (n) out += B32[(acc << (5 - n)) & 31]; return out; }
  function fromB32(s) { let acc = 0, n = 0; const out = []; for (const ch of s) { const v = B32.indexOf(ch); if (v < 0) return null; acc = (acc << 5) | v; n += 5; if (n >= 8) { out.push((acc >>> (n - 8)) & 255); n -= 8; } acc &= (1 << n) - 1; } return out; }
  function backupEncode(me) {
    const bytes = [];
    for (let l = 1; l <= 20; l++) { const b = Math.min(me.best[l] || 0, 5), st = me.stars[l] ? 1 : 0, d = Math.max(0, ' abc'.indexOf(me.decor[l] || ' ')), f = Math.min(me.fails[l] || 0, 3); bytes.push((b << 5) | (st << 4) | (d << 2) | f); }
    const m = me.reachedAt ? Math.min(0xffffffff, Math.max(1, Math.round(me.reachedAt / 60000 - EPOCH_MIN))) : 0;
    bytes.push((m >>> 24) & 255, (m >>> 16) & 255, (m >>> 8) & 255, m & 255);
    bytes.push(crc(bytes));
    return group(toB32(bytes));
  }
  function backupDecode(str) {
    const s = normCode(str); if (s.length !== 40) return null;
    const bytes = fromB32(s); if (!bytes || bytes.length !== 25 || crc(bytes.slice(0, 24)) !== bytes[24]) return null;
    const out = { best: {}, stars: {}, fails: {}, decor: {}, reachedAt: 0 };
    for (let l = 1; l <= 20; l++) { const v = bytes[l - 1], b = v >> 5, d = (v >> 2) & 3, f = v & 3; if (b > 5) return null; if (b) out.best[l] = b; if ((v >> 4) & 1) out.stars[l] = 1; if (d) out.decor[l] = ' abc'[d]; if (f) out.fails[l] = f; }
    const m = ((bytes[20] << 24) >>> 0) + (bytes[21] << 16) + (bytes[22] << 8) + bytes[23];
    if (m) out.reachedAt = (m + EPOCH_MIN) * 60000;
    return out;
  }

  return { enabled, preview, misconfigured, call, normClass, normCode, normPin, group, backupEncode, backupDecode };
})();
if (typeof module !== 'undefined') module.exports = RFE;
