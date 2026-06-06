# Internal notes (not published)

This folder holds internal working notes, diagnoses, refactor plans and other
developer scratch material that belongs with the code but must **not** appear on
the public documentation site.

The leading underscore in the folder name (`_notes`) makes Docusaurus skip it:
its default `exclude` pattern (`**/_*/**`) treats any path segment that starts
with `_` as a partial and never turns it into a published route. Files placed
here are therefore versioned alongside the code, but invisible to site visitors
and to the search index.

Conventions:

- Put any note that is not meant for readers of the site in `docs/_notes/`
  (or give the file itself a leading underscore, e.g. `_my-note.md`).
- Do not reference these files from any `sidebar.js`.
- Published documentation lives in the regular `docs/` tree and is reachable
  from a sidebar.
