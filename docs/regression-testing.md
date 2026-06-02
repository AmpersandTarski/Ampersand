# Regression testing in Ampersand

This document explains how the Ampersand regression test suite works, how to run
it locally, and how to "park" a test case that is not yet supported.

## What it does

The regression suite compiles a large collection of `.adl` (and `.archimate`)
scripts and checks that the Ampersand compiler produces the **expected exit
code** for each one. Some scripts must compile successfully; others must fail in
a specific way. A test passes when the actual exit code matches the expected
exit code.

The suite is the test stanza `ampersand-test` (see `package.yaml`), whose entry
point is `app/Test/Main.hs` → `mainTest` in `src/MainApps.hs`. `mainTest` simply
runs the compiler subcommand:

```
ampersand test testing
```

The logic lives in `src/Ampersand/Test/Regression.hs`.

## How a run is structured

1. **Walk the tree.** Starting at the `testing/` directory, the runner walks all
   subdirectories recursively. Hidden entries (names starting with `.`) are
   skipped.
2. **Find instructions.** In each directory it looks for a file named
   `testinfo.yaml`. A directory **without** `testinfo.yaml` is traversed but no
   tests run there — its scripts are effectively skipped.
3. **Collect candidates.** When `testinfo.yaml` is present, the candidate scripts
   are all files in that directory with extension `.adl` or `.archimate`.
   (Other files such as `.ifc` or `.xlsx` are not tested directly; they are
   pulled in through `INCLUDE` statements or by the `.adl` itself.)
4. **Run each instruction on each candidate.** Every command in `testinfo.yaml`
   is run against every candidate script, in the directory of the script.
5. **Compare exit codes.** The test passes when the actual exit code equals the
   expected `exitcode`. On failure the runner logs the command, the expected vs.
   actual exit code, and captured `stdout`/`stderr`.
6. **Summarize.** At the end it reports the number of successes and failures. If
   **any** test fails, the process exits with a non-zero status (`SomeTestsFailed`),
   which makes `stack test` fail.

## The `testinfo.yaml` format

```yaml
testCmds:
  - command: "ampersand check"      # the command, without the script argument
    exitcode: 0                     # 0 = must succeed; non-zero = must fail with this code
  - command: "ampersand validate --verbose"
    exitcode: 0
```

- `command` is the compiler invocation **without** the script filename — the
  runner appends each candidate file as the last argument.
- `exitcode: 0` means the script must compile successfully (a "should succeed"
  case). A non-zero value means the script must fail with exactly that exit code
  (a "should fail" case).
- A `testinfo.yaml` that cannot be parsed counts as one failure.

## Running it locally

Use the helper script, which prepares the prerequisites and then runs the suite:

```
bash stacktest.sh
```

`stacktest.sh` checks that PHP (with the `mysqli` extension) is available, starts
the Ampersand MariaDB container (or uses a database already listening on port
3306), applies the GRANT the PHP scripts need, verifies the PHP→DB connection,
sets `MYSQL_HOST`/`MYSQL_USER`/`MYSQL_PASSWORD`, and finally runs `stack test`.

> A database and PHP are required because some test instructions generate and
> validate a prototype against MariaDB.

To run the suite directly (after `stack build`):

```
stack test
# or, equivalently, on a chosen directory:
stack exec ampersand -- test testing
```

## Parking a test case (mark as "still unsupported")

Sometimes a script triggers a known, pre-existing limitation that should not
block the build. To keep the regression at zero failures, move such a case to:

```
testing/StillUnsupported/
```

That directory has **no** `testinfo.yaml`, so its scripts are walked but never
executed — they are parked. The naming convention there is `Issue<N>.adl`.

When you park a script, move its companion files too (e.g. the matching `.xlsx`,
any `INCLUDE`d `.adl`/`.ifc` files), so the parked case stays self-contained.

> Only park cases that fail due to a pre-existing or unrelated limitation. Do
> **not** park a case that fails because of the change you are working on — that
> would mask a real regression. Trace the failure first.

## Where to look

| Concern | Location |
| --- | --- |
| Test entry point | `app/Test/Main.hs`, `mainTest` in `src/MainApps.hs` |
| Walk / run / summarize logic | `src/Ampersand/Test/Regression.hs` |
| Local setup + run script | `stacktest.sh` |
| Test scripts | `testing/` (recursively) |
| Parking lot | `testing/StillUnsupported/` |
