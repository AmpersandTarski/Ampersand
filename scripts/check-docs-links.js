#!/usr/bin/env node
/*
 * Documentation link check (docs-hygiene).
 *
 * Mirrors the part of the Docusaurus build that fails on broken links
 * (`onBrokenLinks: 'throw'`), but only for links this repository can verify
 * on its own. It is deliberately CONSERVATIVE: it would rather miss a broken
 * link than raise a false positive, so a green run never blocks a good PR.
 *
 * Checked (and only this):
 *   - a link with a .md/.mdx extension whose target file does not exist;
 *   - an extension-less link whose target is neither a sibling .md/.mdx file
 *     nor a directory. This includes links ending in README/index, because a
 *     README/index file becomes the FOLDER route (e.g. /modeling/), never
 *     /modeling/README.
 *
 * Deliberately skipped (cannot be proven broken here, so never flagged):
 *   - links that resolve OUTSIDE docs/ — cross-repo links such as
 *     ../../prototype/... or ../../../rap/... (those docs are not in this
 *     checkout; the site build validates them);
 *   - external links (http(s):, mailto:, tel:, //host, pathname:// ...),
 *     absolute paths (/...), and pure anchors (#...);
 *   - links inside fenced or inline code (example code is not a real link);
 *   - non-page targets with another extension (.png, .pdf, .zip, ...);
 *   - targets under a '_'-prefixed (excluded) path.
 *
 * Case-sensitive, matching the Linux build runner — so a link whose casing
 * differs from the file (fine on macOS) is caught here too.
 *
 * No dependencies, no network. Exit code 1 on any broken link.
 */
const fs = require('fs');
const path = require('path');

const ROOT = path.join(__dirname, '..');
const DOCS = fs.realpathSync(path.join(ROOT, 'docs'));

function walk(dir, out = []) {
  for (const e of fs.readdirSync(dir, { withFileTypes: true })) {
    if (e.name.startsWith('_')) continue; // Docusaurus excludes '_' paths
    const p = path.join(dir, e.name);
    if (e.isDirectory()) walk(p, out);
    else if (/\.mdx?$/.test(e.name)) out.push(p);
  }
  return out;
}

// Remove fenced and inline code so example code is never scanned for links.
function stripCode(s) {
  return s
    .replace(/```[\s\S]*?```/g, '')
    .replace(/~~~[\s\S]*?~~~/g, '')
    .replace(/`[^`\n]*`/g, '');
}

const LINK = /(?<!!)\[[^\]]*\]\(\s*(<[^>]+>|[^)\s]+)/g;
const EXTERNAL = /^([a-z][a-z0-9+.-]*:|\/\/|\/|#)/i; // scheme:, //host, /abs, #anchor

const insideDocs = p => p === DOCS || p.startsWith(DOCS + path.sep);
const excluded = p => path.relative(DOCS, p).split(path.sep).some(s => s.startsWith('_'));

const broken = [];
for (const file of walk(DOCS)) {
  const text = stripCode(fs.readFileSync(file, 'utf8'));
  let m;
  while ((m = LINK.exec(text))) {
    let tgt = m[1].trim();
    if (tgt.startsWith('<') && tgt.endsWith('>')) tgt = tgt.slice(1, -1).trim();
    if (!tgt || EXTERNAL.test(tgt)) continue;

    let rel;
    try { rel = decodeURI(tgt.split('#')[0].split('?')[0]).trim(); }
    catch { continue; } // malformed escape -> don't guess
    if (!rel) continue;

    const base = path.normalize(path.join(path.dirname(file), rel.replace(/\/+$/, '')));
    if (!insideDocs(base) || excluded(base)) continue; // out-of-tree / excluded -> skip

    const ext = (path.basename(rel).match(/\.([a-z0-9]+)$/i) || [])[1];
    if (ext) {
      if (/^mdx?$/i.test(ext) && !fs.existsSync(base)) broken.push([file, tgt, 'missing page']);
      continue; // other extensions are assets/downloads -> not our concern
    }

    const last = path.basename(base).toLowerCase();
    if (last === 'readme' || last === 'index') {
      broken.push([file, tgt, 'README/index has no own route — link the folder (e.g. ../modeling/) or add .md']);
    } else {
      const isDoc = fs.existsSync(base + '.md') || fs.existsSync(base + '.mdx');
      const isDir = fs.existsSync(base) && fs.statSync(base).isDirectory();
      if (!isDoc && !isDir) broken.push([file, tgt, 'no such page or folder']);
    }
  }
}

const rp = f => path.relative(ROOT, f);
if (broken.length) {
  console.error(`✖ Documentation link check: ${broken.length} broken intra-repo link(s)\n`);
  for (const [f, t, why] of broken) console.error(`  ${rp(f)}\n      -> ${t}   [${why}]`);
  console.error('\nThese links resolve inside docs/ but point at something Docusaurus will not serve.');
  process.exit(1);
}
console.log('✔ Documentation link check: no broken intra-repo links.');
