# Ampersand Type Checker Test Guidelines

## Test File Structure

Each type checker test file should:

1. **Be minimal** - Test only ONE specific type checking scenario
2. **Have clear PURPOSE** - Explain WHY the test should pass or fail
3. **Use simple concept names** - A, B, C for unrelated concepts; use ISA when testing hierarchies
4. **Include concise comments** - Only non-obvious information, avoid redundancy
5. **Follow naming convention** - OperatorName.adl or OperatorNameTypeMismatch.adl

## Standard Test File Template

### For shouldPass tests:

```adl
CONTEXT TestName IN ENGLISH

PURPOSE CONTEXT TestName
{+ Brief explanation of why this test should pass. +}

RELATION r[A*B]
RELATION s[B*C]
RELATION result[A*C]

RULE testRule: result = r;s

ENDCONTEXT
```

### For shouldFail tests:

```adl
CONTEXT TestNameFail IN ENGLISH

PURPOSE CONTEXT TestNameFail
{+ Brief explanation of why this test should fail. +}

RELATION r[A*B]
RELATION s[C*D]
RELATION result[A*C]

RULE testRule: result = r;s  -- Will fail: meet(B,C) doesn't exist

ENDCONTEXT
```

## Test Organization

- **shouldPass/** - Tests that should type-check successfully
- **shouldFail/** - Tests that should fail with type errors

## Key Principles

1. **No redundant comments** - Don't explain what's obvious from the code
2. **Focus on type constraints** - The PURPOSE should explain the type checking logic
3. **One test per file** - Each file tests exactly one scenario
4. **Minimal relations** - Only declare what's needed for the test
5. **Clear failure reasons** - For shouldFail tests, explain which type constraint is violated

## Examples of Good Comments

✅ GOOD: `{+ This test must succeed because meet(A,B) = A exists (A ISA B). +}`
✅ GOOD: `{+ Left residual requires target(r) ⊑ target(s), which fails here. +}`
❌ BAD: `{+ This test tests composition of two relations +}` (too obvious)
❌ BAD: `{+ result = r;s should work +}` (doesn't explain WHY)

## Test File Naming Conventions

- Basic operator tests: `Operator.adl` (e.g., `Composition.adl`)
- Type mismatch tests: `OperatorTypeMismatch.adl`
- ISA hierarchy tests: `OperatorISA_Scenario.adl`
- Special scenarios: Descriptive name that indicates what's being tested
