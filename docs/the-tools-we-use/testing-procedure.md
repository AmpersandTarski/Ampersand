# Ampersand Testing Procedure

This guide provides reference commands for testing Ampersand during development.

For full testing documentation, see the [Testing Infrastructure Guide](testing-infrastructure.md).

---

## Regression Test Suite

Run before committing or creating a PR:

```bash
# Build and run all regression tests (recommended)
stack test --flag ampersand:buildAll

# Alternative: Build first, then test
stack build --flag ampersand:buildAll
stack test
```

This runs the test suite in `testing/Travis/testcases/` that must pass for all commits.

---

## Manual Testing (Individual Files)

For validation during development:

### Step 1: Build the Compiler

```bash
stack build
```

### Step 2: Test Your ADL File

```bash
# Check syntax and types
stack exec ampersand check <filename>

# Examples:
stack exec ampersand check test_diamond.adl
stack exec ampersand check testing/Travis/testcases/Check/testTypes.adl
```

### Other Common Test Commands

```bash
# Validate and check consistency
stack exec ampersand validate <filename>

# Export to analyze structure
stack exec ampersand export <filename>

# Generate prototype
stack exec ampersand proto <filename>
```

---

## Notes

- Compile with `stack build` before manual testing
- Use `stack exec ampersand check` (NOT `stack exec ampersand -- check`)
- The `check` command performs syntax and type checking
- Manual testing provides development feedback
- Regression tests (`stack test`) are required before committing

## Mistakes to Avoid

- ❌ `stack exec ampersand -- check <filename>` (incorrect double-dash syntax)
- ❌ `ampersand check <filename>` (may not use the correct build)
- ❌ Committing without running `stack test`

---

## When to Use Each Method

| Scenario | Command | Purpose |
|----------|---------|---------|
| Quick syntax check during coding | `stack exec ampersand check file.adl` | Fast feedback loop |
| Before committing changes | `stack test` | Ensure no regressions |
| Testing specific functionality | Manual commands | Targeted validation |
| Debugging test failures | Navigate to test dir + manual run | Isolate issues |

---

For more details, including test directory structure, adding new tests, CI/CD integration, and debugging procedures, see the [Testing Infrastructure Guide](testing-infrastructure.md).
