# Ampersand Testing Procedure

## CRITICAL: Testing Steps for Ampersand Compiler

**NEVER FORGET THIS PROCEDURE:**

1. **Compile**: `stack build`
2. **Type check**: `stack exec ampersand check <filename>`

## Usage Examples

```bash
# Step 1: Always compile first
stack build

# Step 2: Then type check your ADL file
stack exec ampersand check test_diamond.adl
stack exec ampersand check testing/Travis/testcases/Check/testTypes.adl
```

## Important Notes

- Always compile with `stack build` before testing
- Use `stack exec ampersand check` (not `stack exec ampersand -- check`)
- The `check` command performs both syntax and type checking
- This is the definitive testing procedure for the Ampersand compiler

## DO NOT USE

- ❌ `stack exec ampersand -- check <filename>` (incorrect syntax)
- ❌ `ampersand check <filename>` (may not use correct build)
- ❌ Any other testing commands

## REMEMBER

This procedure must be followed every time when testing Ampersand compiler changes.

**Date learned:** August 24, 2025  
**Context:** Working on type checker diamond pattern fix
