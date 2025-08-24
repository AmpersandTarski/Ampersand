# Critical Code Modification Protocol

## MANDATORY STEPS FOR ALL CODE CHANGES

When modifying source code in any project (especially compiler/system code):

### 1. Make the Code Changes
- Implement the required modifications
- Follow existing code patterns and conventions
- In Haskell, preferrably use meaning preserving transformations

### 2. **ALWAYS RECOMPILE TO VERIFY** 
- **This step is MANDATORY and must NEVER be skipped**
- Run the build command appropriate for the project
- Check for compilation errors, warnings, or issues
- Verify the build completes successfully
- Test basic functionality if possible

### 3. Document and Complete
- Only after successful compilation, document the changes
- Provide completion summary

## Why Recompilation is Critical
- Syntax errors can break the entire build
- Type errors may not be caught until compile-time
- Dependencies and imports need verification
- The change might work locally but break the overall system
- Compiler projects are especially sensitive to code changes

## Project-Specific Build Commands
- **Ampersand (Haskell)**: `stack build` or `cabal build`
- **General Haskell**: `ghc` or appropriate build tool
- **Other projects**: Use project's standard build command

## Upon commit
- Before committing, enhance the release notes with a one-line summary of the change, focusing on user value. Commit these release notes too. 

## NO EXCEPTIONS
This protocol applies to ALL code modifications, no matter how small or seemingly trivial.
