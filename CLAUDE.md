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

- The original test set is in `testing/`.
- An improved test set with populations is in `testing_with_populations/`.
- To run the new set, temporarily rename the directories so
  `testing_with_populations` takes the name `testing`, then run `stack test`.
- ~14 known files trigger pre-existing compiler bugs (see
  `COMPILATION_STATUS_REPORT.md`).

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

## Proof layer (proofs/)

- `proofs/spike/` holds the machine-checked proof layer for rule rewrites (see
  `HANDOFF_bewijsassistent_skill.md` and `proofs/spike/SPIKE_REPORT.md`).
- Toolchain: Isabelle 2025-2 (CLI `/Applications/Isabelle2025-2.app/bin/isabelle`,
  headless via `isabelle build`) + Prover9/Mace4 (Homebrew). After the cask install,
  strip the quarantine attribute or every binary dies with SIGKILL (`install.sh`
  does this).
- Prover9/Mace4 print "THEOREM PROVED"/"MODEL" on **stderr**, not stdout.

## Gotcha

- The `.gitignore` pattern `*GitHub*` accidentally matches `.github/`, so adding a
  **new** workflow file requires `git add -f`.
