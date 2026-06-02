#!/usr/bin/env node
/*
 * Documentation hygiene check (F0.5).
 *
 * Hard failures:
 *   1. duplicate sidebar ids  — a doc referenced more than once (cf. F0.2)
 *   2. broken sidebar references — an id with no matching .md file
 *   3. internal scratch notes published — a developer note sitting in the
 *      published tree instead of under a '_'-prefixed path (cf. F0.1)
 *
 * Warning only (does not fail the build):
 *   - published pages that are not in any sidebar (orphans)
 *
 * Doc-id resolution mirrors Docusaurus: a leading "NN-" is stripped from
 * every path segment, and a frontmatter `id:`
 * overrides the file name. Docusaurus keeps README/index in the id.
 */
const fs = require('fs');
const path = require('path');

const DOCS = path.join(__dirname, '..', 'docs');
const PREFIX = 'ampersand/';
const SCRATCH = /(^|\/)(analysis|diagnosis|report|sandbox-prompt|prompt-compiler|fc5)[-/]|-(fix|checklist|analysis)\.mdx?$|(^|\/)(scratch|wip|draft)-/i;

const stripNum = seg => seg.replace(/^\d+-/, '');
function frontmatterId(file) {
  const txt = fs.readFileSync(file, 'utf8');
  const m = txt.match(/^---\n([\s\S]*?)\n---/);
  if (!m) return null;
  const id = m[1].match(/^\s*id:\s*(.+?)\s*$/m);
  return id ? id[1].replace(/['"]/g, '') : null;
}
function docId(rel) {                       // rel like "docker/1-compiler.md"
  const noext = rel.replace(/\.mdx?$/, '');
  let segs = noext.split('/').map(stripNum);
  const fid = frontmatterId(path.join(DOCS, rel));
  if (fid) segs[segs.length - 1] = fid;
  return PREFIX + segs.join('/');
}

// referenced ids
const sidebars = require(path.join(DOCS, 'sidebar.js'));
const refs = [];
(function walk(n){ if(n==null)return;
  if(typeof n==='string'){refs.push(n);return;}
  if(Array.isArray(n)){n.forEach(walk);return;}
  if(typeof n==='object'){ if(typeof n.id==='string')refs.push(n.id); if(n.items)walk(n.items);} })(Object.values(sidebars));
const refCount={}; refs.forEach(id=>refCount[id]=(refCount[id]||0)+1);

// published files
function excluded(rel){ return rel.split('/').some(s=>s.startsWith('_')); }
const files=[];
(function collect(dir){ for(const e of fs.readdirSync(dir,{withFileTypes:true})){
  const full=path.join(dir,e.name), rel=path.relative(DOCS,full);
  if(excluded(rel))continue;
  if(e.isDirectory())collect(full); else if(/\.mdx?$/.test(e.name))files.push(rel);
}})(DOCS);

const idOf={}; files.forEach(rel=>{ idOf[docId(rel)]=rel; });

const duplicates=Object.entries(refCount).filter(([,n])=>n>1).map(([id])=>id).sort();
const broken=refs.filter(id=>id.startsWith(PREFIX)&&!(id in idOf)).sort();
const scratch=files.filter(rel=>SCRATCH.test(rel)).sort();
const orphans=Object.keys(idOf).filter(id=>!(id in refCount)).sort();

let bad=false;
function fail(t,items,hint){ if(!items.length)return; bad=true;
  console.error(`\n✗ ${t} (${items.length}):`); items.forEach(i=>console.error('    '+i)); if(hint)console.error('  → '+hint); }
fail('Duplicate sidebar ids',duplicates,'give each menu item a distinct page.');
fail('Broken sidebar references',broken,'these ids have no matching .md file.');
fail('Internal scratch notes in the published tree',scratch,"move them under a '_'-prefixed path (e.g. docs/_notes/).");

if(orphans.length){ console.log(`\n⚠ ${orphans.length} published page(s) not in any sidebar (warning only):`); orphans.forEach(i=>console.log('    '+i)); }

if(bad){ console.error('\nDocumentation hygiene check FAILED.\n'); process.exit(1); }
console.log(`\nDocumentation hygiene OK — ${files.length} pages, no duplicates, no broken refs, no stray notes.`);
