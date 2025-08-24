# Ampersand Term Analysis Methodology

## Key Principles for Working with Ampersand Terms

**CRITICAL**: Ampersand terms are mathematical objects that require mathematical precision in analysis and application.

### Methodology Steps

1. **Always Consult Documentation First**
   - I will look up the definitions in the official Ampersand documentation if I do not remember them exactly.
   - I will never assume or guess the meaning of operators or expressions
   - I will use web_fetch to access https://ampersandtarski.gitbook.io/documentation/ when needed

2. **Apply Definitions Precisely**
   - Work through expressions step-by-step using exact mathematical definitions
   - Show concrete examples with specific atoms/values when possible
   - Determine the exact type signature of expressions

3. **Verify Results Where Possible**
   - Check answers against the documentation definitions
   - Use concrete examples to validate abstract reasoning
   - Cross-reference with compiler output when available

### Example of Correct Analysis

When analyzing `I[CountryCode]#("English"[Taal])`:

1. **Documentation Check**: 
   - `I[C]` = identity relation = {(c,c) | c∈C}
   - `#` = cartesian product operator  
   - `("English"[Taal])` = singleton set {"English"}

2. **Precise Application**:
   - `I[CountryCode]` with NL, UK, VS = {(NL,NL), (UK,UK), (VS,VS)}
   - Cartesian product with {"English"} = {(NL,"English"), (UK,"English"), (VS,"English")}
   - Type: [CountryCode * Taal]

3. **Verification**: 
   - Matches mathematical definition of cartesian product
   - Type signature is consistent
   - Concrete example validates abstract reasoning

### Key Reminder

**Mathematical precision is essential when working with Ampersand terms - always document definitions, apply them carefully, and verify results. Think, don't guess! **
