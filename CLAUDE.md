# Ampersand — Project Instructions

Whenever you learn something new about this Ampersand working directory that will
help with future work here, update these instructions. Keep this file concise:
short, accurate conventions that are easy to miss.

## Tech stack & architecture

- **Language:** Haskell (GHC 9.6.6)
- **Build tool:** Stack (LTS 22.39) with Cabal
- **Extensions:** `NoImplicitPrelude`, `OverloadedStrings`
- **Key libraries:** `parsec`, `megaparsec`, `aeson`, `yaml`, `pandoc`, `xlsx`
- **Ecosystem:** Ampersand compiles ADL (Ampersand Definition Language) into a
  PHP/Angular framework backed by a MariaDB 10.4 database.

## Build & test workflow

**Always run `stack build` after changing code and before testing with
`stack exec ampersand`.** This is a hard requirement — without a build you test
stale code.

Standard cycle:

1. Change code
2. `stack build` (or `stack build --fast`)
3. `stack exec ampersand -- check <file.adl>`
4. Iterate

## Common commands

- **Fast build:** `stack build --fast`
- **Run the regression suite:** `stack test`
  (or `nice -n 10 time stack test 2>&1 | tee test.log`)
- **Check a specific ADL file:** `stack exec ampersand -- check <path/to/file.adl>`
- **CLI help:** `stack exec ampersand -- --help`
- **Lint:** `hlint src/`
- **Generate docs:** `stack haddock`

## Test infrastructure

- The regression test set is in `testing/`.

## Debugging & development

- **Type holes:** Put `_` in the code to have GHC infer the expected type.
- **Debug.Trace:** Use for runtime debugging output.
- **HLS:** Use the Haskell Language Server in VS Code for real-time error checking.

## Release notes are enforced on pull requests

Every pull request that changes a file **outside `docs/`** must add an entry to
`ReleaseNotes.md`, or the CI check **"Check release notes / changelog"**
(the `dangoslen/changelog-enforcer` action) fails. Add a bullet under an
`## Unreleased` section at the top of `ReleaseNotes.md`.

Pull requests that touch **only** `docs/**` are exempt — that workflow sets
`paths-ignore: docs/**` — so pure documentation changes need no ReleaseNotes entry.

## Branching & merging

- The **`documentation`** branch is merged **directly** (fast-forward / direct
  merge) — **no pull request**. Other branches (notably `main`) go through a PR.
- `main` requires no review (since 30 October 2026) and the repository has
  auto-merge on. After `gh pr create`, run `gh pr merge <nr> --auto --merge`;
  GitHub merges when the six required checks are green and every review
  conversation is resolved. `--admin` is never used.
- The required build jobs (`ci2.yml`, `codeQuality.yml`) sit behind the job
  `changes`, which runs them only when a build input changed since the
  merge-base with `main`; a skipped job counts as passed. A PR whose head
  commit predates that job (before PR #1752) and changed only text has no
  checks on its head and needs a new push, such as a merge of `main`.
- A job chooses its runner with `runs-on: ${{ vars.UBUNTU_RUNNER || 'ubuntu-latest' }}`
  (or `vars.WINDOWS_RUNNER`), and the name of a required check holds no runner
  version ("Build and test on Linux", "Build and test on Windows").
  A new runner version is then a change of the repository variable, and the
  protection rule of `main` keeps naming checks that run.
  Stef asked for this on 21 October 2026, after a required check named after
  `ubuntu-22.04` blocked the pipeline while nothing ran on that version any more:
  "Kan je zoiets niet met een variabele regelen, zodat het altijd en overal
  correct blijft draaien?" (PR #1740).
- The branch **`incremental-evaluation`** does not go to `main` for now.
  A branch for its work starts from `incremental-evaluation`, not from `main`,
  and is merged into it and pushed without a pull request.
  A request to merge or release something that sits on that branch concerns
  only the part outside the incremental evaluation.
  Stef corrected this three times: on 6 October 2026 PR #1703 had brought it
  into `main` "per ongeluk ... terwijl incremental-evaluation daar nog niet klaar
  voor is" (reverted with #1713); the same day "op incremental-evaluation doen we
  niet aan PR's. Daar mergen en pushen we gewoon. Weet je nog?"; and on
  8 October "ik bedoel alleen de EXCEPT wijziging, NIET de incrementele evaluatie!".
- A push to `main` or `documentation` that changes `docs/**` automatically
  rebuilds the site: `.github/workflows/triggerDocsUpdate.yml` dispatches the
  `DeployToPages` workflow in the `AmpersandTarski.github.io` repo. No manual
  deploy and no PR are needed to publish docs.
- The site build runs Docusaurus with `onBrokenLinks: 'throw'` on case-sensitive
  Linux, so a link whose casing differs from the target file (fine on macOS)
  breaks the build. Keep doc filenames and links lowercase.

## Documentation

- The documentation site (Docusaurus) is assembled from the `docs/` folders of
  several repositories. Ampersand's docs are published from the **`documentation`**
  branch, not `main`.
- Internal or work-in-progress notes must not be published. Put them under a
  `_`-prefixed path (e.g. `docs/_notes/`); Docusaurus excludes `_`-prefixed files by
  default. Long-lived developer notes live in `memorybank/`.
- `scripts/check-docs-sidebar.js` (run in CI) fails on duplicate sidebar ids, broken
  sidebar references, and scratch notes in the published tree. Run it before pushing
  documentation changes.
- `scripts/check-docs-links.js` (run in CI) fails on broken **intra-repo** doc links —
  including links to a `README`/`index` without `.md` (those map to the folder route,
  not `/README`) and casing mismatches. It deliberately skips cross-repo links
  (`../../prototype/...`, `../../../rap/...`) and external links to avoid false
  positives, so a green run never blocks a good PR. Link cross-repo and README targets
  with the `.md` extension so Docusaurus resolves them to the right route.

## Proof track

Machine-checked proofs are part of the deliverable, not an afterthought.

- `proofs/` holds the proof sessions (Isabelle/HOL today; **prefer Lean 4 with
  mathlib for new formalisations**). `docs/proofs/README.md` is the published
  register: claims `PRF-n` and trails `TRAIL-n`, with statuses
  `machine-checked` / `paper proof` / `stated` / `in progress`.
- Identifiers are permanent — never renumber, never reuse; check the git
  history before assigning a number.
- A new or changed proof session gets its register row **in the same change**.
  `scripts/check-proof-track.js` (CI workflow "Documentation hygiene") enforces
  the register's consistency; run it before pushing.
- Ordinary documentation pages refer to a claim only via the fixed one-liner
  `*Proof track: [PRF-n — <claim>](../proofs/README.md#prf-n).*` — no formal
  notation or proof sketches outside `docs/proofs/`.
- Claim `machine-checked` only after observing the build yourself
  (`isabelle build -D proofs/<session>`, or `lake build` for Lean). One
  `isabelle build` at a time — concurrent builds corrupt the shared heap
  database.
- **Planning discipline:** a task that changes semantics-bearing code — the
  rewrite laws in `NormalForms.hs`, the delta calculus, the SQL generation of
  terms, the typology/affected-conjunct logic — includes a proof obligation:
  state the claim in the register (status `stated`) as part of the task, and
  plan the proof. The claim precedes the proof; the proof precedes the
  confidence.

## Gotcha

- The `.gitignore` pattern `*GitHub*` accidentally matches `.github/`, so adding a
  **new** workflow file requires `git add -f`.
- The `.gitignore` pattern `*.py` ignores every Python script, also a measurement
  script under `memorybank/` that a note refers to; add such a script with `git add -f`.
- `ampersand validate` and `ampersand incremental-bench --sql` write their PHP
  helper to a **fixed** temp path (`$TMPDIR/tmpPhpQueryOfAmpersand.php`, see
  `executePHPStr` in `src/Ampersand/Prototype/PHP.hs`) and use a temp database
  named after the context. Two concurrent runs (e.g. an interactive
  `ampersand validate` next to a running `stack test`) clobber each other's
  script and fail spuriously with exit 60 or a "Parse error on php result"
  fatal (exit 2). Serialize them: never run either while the regression suite
  is running; `scripts/test-local.sh` serializes suite runs between terminals
  with the lock directory `/tmp/ampersand-regression-db.lock`, and a manual
  run should take that lock too (`mkdir` it, run, `rmdir` it). An interrupted
  harness leaves its temp database behind; drop it before the next run.
