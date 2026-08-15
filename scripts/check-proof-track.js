#!/usr/bin/env node
/*
 * Proof-track consistency check.
 *
 * The register docs/proofs/README.md discloses every proof session under
 * proofs/ as a claim (PRF-n) and groups claims into trails (TRAIL-n).
 * This script keeps register and repository in step. Hard failures:
 *
 *   1. duplicate claim or trail identifiers in the register
 *   2. a claim row whose status is outside the fixed vocabulary
 *      (machine-checked | paper proof | stated | in progress)
 *   3. a tracked proof session (a proofs/<name>/ directory holding an
 *      Isabelle ROOT or a Lean lakefile) that the register never mentions
 *   4. a claim row referencing a trail that has no row, or a trail row
 *      referencing a claim that has no row
 *   5. a claim row without a matching detail heading `### PRF-n {#prf-n}`,
 *      or a detail heading without a table row
 *
 * Only tracked files count (git ls-files), so an uncommitted experiment
 * next to the sessions does not trip the check.
 */
const fs = require('fs');
const path = require('path');
const { execSync } = require('child_process');

const ROOT = path.join(__dirname, '..');
const REGISTER = path.join(ROOT, 'docs', 'proofs', 'README.md');
const STATUSES = ['machine-checked', 'paper proof', 'stated', 'in progress'];

const errors = [];
const text = fs.readFileSync(REGISTER, 'utf8');
const lines = text.split('\n');

// --- parse the register -----------------------------------------------------
const claimRows = new Map();   // PRF-n  -> { status, trails: [TRAIL-n] }
const trailRows = new Map();   // TRAIL-n -> { claims: [PRF-n] }
const detailIds = [];          // PRF-n from `### PRF-n {#prf-n}` headings

for (const line of lines) {
  const h = line.match(/^### (PRF-\d+) \{#(prf-\d+)\}/);
  if (h) {
    detailIds.push(h[1]);
    if (h[2] !== h[1].toLowerCase())
      errors.push(`heading '${h[1]}' has mismatching anchor '{#${h[2]}}'`);
    continue;
  }
  if (!line.startsWith('|')) continue;
  const cells = line.split('|').map(c => c.trim()).slice(1, -1);
  if (cells.length === 0) continue;
  const claim = cells[0].match(/^\[?(PRF-\d+)\]?/);
  if (claim && cells.length >= 5) {
    if (claimRows.has(claim[1])) errors.push(`duplicate claim id ${claim[1]}`);
    claimRows.set(claim[1], {
      status: cells[2],
      trails: cells[4].match(/TRAIL-\d+/g) || [],
    });
    continue;
  }
  const trail = cells[0].match(/^(TRAIL-\d+)$/);
  if (trail && cells.length >= 5) {
    if (trailRows.has(trail[1])) errors.push(`duplicate trail id ${trail[1]}`);
    trailRows.set(trail[1], { claims: cells[4].match(/PRF-\d+/g) || [] });
  }
}

if (claimRows.size === 0) errors.push('no claim rows found in the register');
if (trailRows.size === 0) errors.push('no trail rows found in the register');

// --- 2. status vocabulary ---------------------------------------------------
for (const [id, row] of claimRows)
  if (!STATUSES.includes(row.status))
    errors.push(`claim ${id} has status '${row.status}', expected one of: ${STATUSES.join(' | ')}`);

// --- 3. every tracked proof session is in the register ----------------------
const tracked = execSync('git ls-files proofs', { cwd: ROOT, encoding: 'utf8' })
  .split('\n').filter(Boolean);
const sessions = new Set();
for (const f of tracked) {
  const m = f.match(/^proofs\/([^/]+)\/(ROOT|lakefile\.(lean|toml))$/);
  if (m) sessions.add(m[1]);
}
for (const s of sessions)
  if (!text.includes(`proofs/${s}`))
    errors.push(`proof session 'proofs/${s}/' has no entry in the register`);

// --- 4. cross-references between the two tables -----------------------------
for (const [id, row] of claimRows)
  for (const t of row.trails)
    if (!trailRows.has(t)) errors.push(`claim ${id} references ${t}, which has no trail row`);
for (const [id, row] of trailRows)
  for (const c of row.claims)
    if (!claimRows.has(c)) errors.push(`trail ${id} references ${c}, which has no claim row`);

// --- 5. table rows and detail headings match --------------------------------
for (const id of claimRows.keys())
  if (!detailIds.includes(id)) errors.push(`claim ${id} has a table row but no '### ${id} {#${id.toLowerCase()}}' detail heading`);
for (const id of detailIds)
  if (!claimRows.has(id)) errors.push(`detail heading for ${id} has no row in the claims table`);

// --- report -----------------------------------------------------------------
if (errors.length) {
  console.error('Proof-track check failed:');
  for (const e of errors) console.error('  - ' + e);
  process.exit(1);
}
console.log(`Proof-track check passed: ${claimRows.size} claims, ${trailRows.size} trails, ${sessions.size} proof sessions.`);
