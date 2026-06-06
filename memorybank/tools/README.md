# Script toolkit

A growing, shared set of helper scripts that contributors (including Claude and Cline) accumulate
while working on Ampersand. The aim: keep the genuinely **reusable** ones, document
what each does, and throw away run-just-once scratch.

**Conventions**
- One row per script, with its path **relative to the repo root**.
- *Reusable?* — ✅ generic (works on any checkout), 🟡 workflow-specific (useful but
  coupled to a task/brittle hard-coded paths — parameterise before reuse), ❌ one-shot
  (delete after use; should never land here).
- When you write a new helper that you might run twice, add it here. When a script was
  clearly run once, delete it instead of keeping it.

## Index

| Script (relative path) | Purpose | Reusable? |
|------------------------|---------|:---------:|
| `extract_imports.sh` | Extract Haskell module imports from `src/**/*.hs` and render a Graphviz diagram (`.dot` + `.png`/`.svg`) of the compiler's module-import structure. | ✅ generic |
| `generate_instance_spreadsheet.py` | Parse Haskell `instance` declarations (via `grep`) and build an `.xlsx` matrix of class × type with VS Code hyperlinks and a second sheet of constraint conditions. Takes a directory argument. | ✅ generic |
| `dump_xlsx.py` | Dump every sheet of an `.xlsx` workbook to TSV files (quoting-safe). Currently the input/output paths are hard-coded — parameterise for general use. | ✅ generic (after path tidy) |
| `testing_with_populations/Travis/testcases/TypeChecker/run-all-tests.sh` | Type-checker test runner: runs `ampersand check` on every `shouldPass/*.adl` (expects success) and `ampersand export` on `shouldFail/*.adl` (expects exit code 10), printing a pass/fail summary. | ✅ generic |
| `compare_diagnose.py` | Compare two TSV dumps of Ampersand's *diagnose* output (an independent computation vs. the generated spreadsheet) and print a per-pattern / per-rule / per-interface diff checklist. Coupled to the diagnose column schema and `/tmp` dump dirs. | 🟡 workflow-specific |
| `verify_fc5.sh` | End-to-end diagnose-verification loop for one project (FC5 `main.adl`): `stack build` → `ampersand documentation` → `dump_xlsx.py` → `ghci` independent diagnose → `compare_diagnose.py`. Hard-codes the project path and a `.stack-work` binary hash — a *template* for diagnose verification rather than a drop-in tool. | 🟡 workflow-specific |
| `docs/trial.py` | Experiment: build a Graphviz link-graph of the `.md` documentation by scraping `(...)` links across `docs/` trees. Rough; overlaps `scripts/check-docs-links.js` (which *checks* links rather than *visualising* them). | 🟡 experimental |
