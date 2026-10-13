# A specialisation of a marked concept that no term names

These four models belong to the issue "A specialisation of a MULTITABLE concept gets no table when no term names it".
In each of them the concept `B` is a direct specialisation of a concept marked `REPRESENT ... TYPE MULTITABLE`,
and no rule, interface or univalent relation names `B`.

What must hold for each model: `concepts.json` gives `B` a `conceptTable` whose first column is `B`,
and `database.sql` creates a table that holds that column.
Compiler v5.11.0 gives `B` the concept table `{"name": "A", "cols": ["A"]}` instead, in all four.

There is no `testinfo.yaml` here yet.
The regression runner compares exit codes, `ampersand proto` and `ampersand validate` both end with 0 on these models,
and a term that names `B` makes the table appear, so the defect is visible in the generated layout only.
The issue asks how the suite should assert on that layout.
