# Working in this repository (notes for Claude)

Short, accurate conventions that are easy to miss. Keep this file concise.

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

## Gotcha

- The `.gitignore` pattern `*GitHub*` accidentally matches `.github/`, so adding a
  **new** workflow file requires `git add -f`.
