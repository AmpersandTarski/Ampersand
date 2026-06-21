# Ampersand Build and Test Workflow

## Critical Workflow Rule

**ALWAYS run `stack build` after making code changes and BEFORE testing with `stack exec ampersand`**

This is non-negotiable. The compiled executable must reflect your code changes before any testing.

## Standard Development Workflow

1. **Make code changes** - Edit Haskell source files
2. **Build** - Run `stack build` to compile changes
3. **Test** - Run `stack exec ampersand -- check <file.adl>` or other test commands
4. **Iterate** - If changes needed, go back to step 1

## Why This Matters

- `stack exec ampersand` runs the compiled executable from `.stack-work/`
- Without building first, you're testing old code
- This leads to confusion when fixes appear not to work
- Always verify build succeeded before testing

## Common Mistake

❌ **Wrong:**
```bash
# Edit src/Ampersand/ADL1/P2A_Converters.hs
stack exec ampersand -- check testing/Travis/testcases/Misc/ARM20-Test7.adl
```

✅ **Correct:**
```bash
# Edit src/Ampersand/ADL1/P2A_Converters.hs
stack build
stack exec ampersand -- check testing/Travis/testcases/Misc/ARM20-Test7.adl
```

## Testing Commands

After building, common test commands include:

- `stack exec ampersand -- check <file.adl>` - Type-check a single file
- `stack test` - Run full test suite
- `stack exec ampersand -- <other commands>` - Other ampersand operations

## Memory Aid

Think of it as: **Edit → Build → Test** - never skip the Build step!
