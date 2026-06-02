# Working in this repository (notes for Claude)

Short, accurate conventions that are easy to miss. Keep this file concise.

## Release notes are enforced on pull requests

Every pull request that changes a file **outside `docs/`** must add an entry to
`ReleaseNotes.md`, or the CI check **"Check release notes / changelog"**
(the `dangoslen/changelog-enforcer` action) fails. Add a bullet under an
`## Unreleased` section at the top of `ReleaseNotes.md`.

Pull requests that touch **only** `docs/**` are exempt — that workflow sets
`paths-ignore: docs/**` — so pure documentation changes need no ReleaseNotes entry.

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

## Gotcha

- The `.gitignore` pattern `*GitHub*` accidentally matches `.github/`, so adding a
  **new** workflow file requires `git add -f`.
