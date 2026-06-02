# Ampersand Operators Reference

## Boolean/Set Operators

### Intersection (∩)
**Operator:** `∩` or `/\`
**Syntax:** `r ∩ s` or `r /\ s`
**Meaning:** Pairs that are in BOTH relations r and s
**Logic:** `a (r∩s) b ⇔ a r b ∧ a s b`

### Union (∪)  
**Operator:** `∪` or `\/`
**Syntax:** `r ∪ s` or `r \/ s`
**Meaning:** Pairs that are in EITHER relation r OR s (or both)
**Logic:** `a (r∪s) b ⇔ a r b ∨ a s b`

### Difference (-)
**Operator:** `-` (binary)
**Syntax:** `r - s`  
**Meaning:** Pairs that are in r but NOT in s
**Logic:** `a (r-s) b ⇔ a r b ∧ ¬(a s b)`

### Complement (-)
**Operator:** `-` (unary)
**Syntax:** `-r`
**Meaning:** All possible pairs in the type domain that are NOT in r
**Logic:** `-r = V[A×B] - r` (where r has type [A×B])

## Relational Operators

### Composition (;)
**Operator:** `;`
**Syntax:** `r ; s`
**Meaning:** "r followed by s" - if a relates to b via r, and b relates to c via s, then a relates to c
**Logic:** `a (r;s) c ⇔ ∃b∈B • a r b ∧ b s c`
**Type:** If r[A×B] and s[B×C], then r;s[A×C]

### Converse (⌣)
**Operator:** `⌣` or `~`
**Syntax:** `r⌣` or `r~`
**Meaning:** "Flip" or reverse the relation - swap source and target
**Logic:** `a (r⌣) b ⇔ b r a`
**Type:** If r[A×B], then r⌣[B×A]

## Special Operators

### Cartesian Product (#)
**Operator:** `#`
**Syntax:** `r # s`
**Meaning:** All combinations of elements from source of r with elements from target of s
**Example:** `preferredLang#"English"` = all pairs (country, "English") where country has some preferredLang

### V (Full relation)
**Syntax:** `V[A×B]` or `V`
**Meaning:** Contains ALL possible pairs between concept A and B
**Usage:** Used in complement definition: `-r = V - r`

### I (Identity)
**Syntax:** `I[A]` or `I`
**Meaning:** Identity relation - pairs (a,a) for all elements a in concept A
**Usage:** `I[Country]` = all pairs (country, country)

## Typing Rules

### Basic Rules:
- `r[A×B] ∩ s[A×B] : [A×B]` (same types required)
- `r[A×B] ∪ s[A×B] : [A×B]` (same types required)  
- `r[A×B] - s[A×B] : [A×B]` (same types required)
- `-r[A×B] : [A×B]` (complement in same type)

### Composition Rules:
- `r[A×B] ; s[B×C] : [A×C]` (target of r must match source of s)
- `r[A×B]⌣ : [B×A]` (types are swapped)

## Precedence Rules (Binding Power)

**CRITICAL:** Unary operators bind BEFORE binary operators!

| Family | Binary Operators | Binding Power | Unary Operators | Binding Power |
|--------|------------------|---------------|-----------------|---------------|
| rules | `===`, `⊆` (`|-`) | 1 (weakest) | - | - |
| boolean | `∪` (`\/`), `∩` (`/\`), `−` (`-`) | 2 | `−` (`-`) | prefix |
| residual | `\`, `/`, `♢` (`<>`) | 3 | - | - |
| relational | `;`, `×` (`#`), `†` (`!`) | 4 | `⌣` (`~`) | postfix |

**Key Rules:**
1. **Unary prefix/postfix operators bind first** (strongest)
2. **Relational operators** (`;`, `#`, `!`) bind power 4
3. **Residual operators** (`\`, `/`, `<>`) bind power 3  
4. **Boolean operators** (`\/`, `/\`, `-`) bind power 2
5. **Rule operators** (`|-`, `===`) bind power 1 (weakest)

**Example:** `-preferredLang#"English"` is parsed as `(-preferredLang)#"English"`, NOT `-(preferredLang#"English")`

## Common Patterns

### Total Relation (TOT)
```adl
RULE totalityRule: I[A] |- r;V
-- Every element in A must have at least one r-relation
```

### Functional Relation (UNI)  
```adl
RULE functionalRule: r;r⌣ |- I
-- Every source element relates to at most one target
```

### Default Value Pattern
```adl
-- Set default when no value exists
RULE setDefault: I[A] - r;V |- defaultR
-- Elements without r get defaultR
```

### Example: Make `r` total with a default value ⭐
**Algebraic Rule:** `r \/ (-r#"default" - r#V)` is ALWAYS UNIVALENT and TOTAL for any univalent relation r and any atom "default"

**Proof without knowing population:**
1. For any source element s, exactly one of these is true:
   - (s, "default") ∈ r → s appears in first part of union
   - (s, "default") ∉ r → s appears in (-r) → (s, "default") ∈ (-r#"default") → s appears in second part of union
2. Therefore every source element appears in at least one part of the union
3. QED: The expression is total

**Practical applications:**
```adl
-- Always total: official languages OR English as fallback
officialLang \/ (-officialLang#"English")

-- Always total: preferred settings OR system default
userPrefs \/ (-userPrefs#"systemDefault")

-- Always total: custom values OR standard fallback  
customConfig \/ (-customConfig#"standard")
```

**Why this works:** The pattern creates a "safety net" - any element missing from the primary relation automatically gets the default value via the complement mechanism.

### Complement Usage
```adl
-- All countries except those with English as preferred language
-preferredLang#"English"
-- Equivalent to: V[Country×Language] - preferredLang#"English"
```

## Debugging Tips

1. **Check types carefully** - many errors come from type mismatches
2. **Use parentheses** to make precedence explicit: `(r ∪ s) ; t` vs `r ∪ (s ; t)`
3. **Complement is type-dependent** - `-r` depends on the declared type of r
4. **Cartesian product creates full combinations** - `r#s` can be very large

## Examples from FC4 Context

```adl
-- Countries that have any preferred language OR all country-language 
-- combinations except those where country prefers English
preferredLang \/ -preferredLang#"English"

-- This means: for most country-language pairs, there should be a defaultLang
RULE InsDefaultLang: (preferredLang \/ -preferredLang#"English") |- defaultLang
```

## Quick Reference Links
- [Ampersand Boolean Operators](https://ampersandtarski.gitbook.io/documentation/the-language-ampersand/terms/semantics-in-logic/boolean-operators)
- [Ampersand Relational Operators](https://ampersandtarski.gitbook.io/documentation/the-language-ampersand/terms/semantics-in-logic/relational-operators)
