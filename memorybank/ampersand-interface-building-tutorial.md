# Building Interfaces in Ampersand: A Tutorial for Beginners

**Date:** August 11, 2025  
**Example:** LandeneisOverzicht interface from project/Plagen.adl  

## Introduction: What Are Interfaces?

Interfaces in Ampersand define how users interact with your data. They control what data users can see, how data is organized on screen, what users can edit, create, or delete, and how data flows from one concept to another.

## Lesson 1: The Basics - Main Menu Interfaces

### Rule 1: Creating a Main Menu Interface

Every interface that should appear in the application's main menu starts with this pattern:

```ampersand
INTERFACE InterfaceName : "_SESSION"[SESSION] CRuD BOX<TABS>
```


`SESSION` is Ampersand's built-in concept representing a user session. `"_SESSION"` is the atom (instance) of SESSION that exists for every user. Interfaces starting with this pattern automatically appear in the main menu.

### Exercise 1: Create Your First Main Menu Interface

```ampersand
INTERFACE CountryRequirementsOverview : "_SESSION"[SESSION] CRuD BOX<TABS>
  [ "Start here" : I cRud
  ]
```

This creates a main menu item called "CountryRequirementsOverview" with one tab.

## Lesson 2: Understanding BOX-atoms and BOX-terms

### The BOX System Explained

Ampersand interfaces work with a recursive box system. A **BOX-atom** represents the "current focus" - what concept you're looking at right now. A **BOX-term** is a relation expression that takes you from current BOX-atom to new BOX-atoms. The **new BOX-atoms** become the new "current focus" for the next level.

### Example: From SESSION to Country Codes

```ampersand
INTERFACE Example : "_SESSION"[SESSION] CRuD BOX<TABS>
  [ "Countries" : V[SESSION*LandEis];landeisLand cRud BOX<TABLE>
  ]
```

**Step by step:**
The **start BOX-atom** is `_SESSION` (type: SESSION). The **BOX-term** `V[SESSION*LandEis];landeisLand` transforms this into **result BOX-atoms** representing multiple country codes (type: LandCode).

### Breaking Down the BOX-term

`V[SESSION*LandEis];landeisLand` works as follows. `V[SESSION*LandEis]` gets all LandEis (country requirements) from SESSION. The `;` acts as a composition operator meaning "then". `landeisLand` takes each LandEis and gets its country code. The final result is all unique country codes that have requirements.

## Lesson 3: Building a Complete Interface

### Step-by-Step: LandeneisOverzicht Interface

Let's build a practical interface for viewing country requirements:

```ampersand
INTERFACE LandeneisOverzicht : "_SESSION"[SESSION] CRuD BOX<TABS>
  [ "📋 Per Country" : V[SESSION*LandEis];landeisLand cRud BOX<TABLE>
      [ "Country Code"     : I                      cRud<LandCode>
      , "Country Name"     : naam                   cRud  
      , "Requirements"     : landeisLand~           cRud BOX<TABLE>
          [ "PO Combination" : landeisPO          cRud
          , "Product"        : landeisPO;productNaam cRud
          , "Requirement Type" : landeisType      cRud
          ]
      ]
  ]
```

### Understanding Each Level

**Level 1: Countries** starts with SESSION as the BOX-atom. The BOX-term `V[SESSION*LandEis];landeisLand` produces one row per country code.

**Level 2: Country Information** uses LandCode as the BOX-atom for each country. Fields show country properties and related requirements.

**Level 3: Requirements Details** uses LandEis (country requirements) as the BOX-atom. The BOX-term `landeisLand~` (inverse: from country back to its requirements) produces all requirements for that specific country.

## Lesson 4: Field Types and CRUD Rights

### Field Types

```ampersand
"Country Code" : I                    cRud<LandCode>  -- Shows the atom itself
"Country Name" : naam                 cRud            -- Shows a property
"Requirements" : landeisLand~         cRud BOX<TABLE> -- Shows related data in table
```

### CRUD Rights Explained

**C**reate means the user can create new instances. **R**ead means the user can view the data. **u**pdate means the user can modify existing data. **D**elete means the user can remove instances.

Examples include `CRuD` where the user can create, read, update, delete. `cRud` means the user can only read (most common for display). `CRud` means the user can create, read, delete but not update.

## Lesson 5: Common Interface Patterns

### Pattern 1: Master-Detail View

```ampersand
"Master Records" : V[SESSION*MainConcept] cRud BOX<TABLE>
  [ "ID"      : I cRud<MainConcept>
  , "Name"    : conceptName cRud
  , "Details" : relatedConcept~ cRud BOX<TABLE>
      [ "Detail 1" : detailProperty1 cRud
      , "Detail 2" : detailProperty2 cRud
      ]
  ]
```

### Pattern 2: Form View for Editing

```ampersand
"Edit Record" : V[SESSION*Concept] CRuD BOX<FORM>
  [ "Field 1" : property1 CRuD
  , "Field 2" : property2 CRuD
  , "Related" : relation  CRuD
  ]
```

## Lesson 6: Type Safety in Interface Design

### Understanding Relation Types

Every relation has a signature: `relationName[Domain*Codomain]`

**Example:**
```ampersand
RELATION landeisLand[LandEis*LandCode]
RELATION naam[LandCode*Name]
```

### Type Checking Your BOX-terms

When composing relations, types must match:
```ampersand
landeisLand[LandEis*LandCode] ; naam[LandCode*Name] = [LandEis*Name]
```

This is valid because `landeisLand` ends with LandCode, `naam` starts with LandCode, so the result goes from LandEis to Name.

## Lesson 7: Common Mistakes and How to Fix Them

### Mistake 1: Inconsistent Nesting

❌ **Wrong:**
```ampersand
"Countries" : V[SESSION*LandEis];landeisLand cRud BOX<TABLE>
  [ "Requirements" : landeisLand~ cRud BOX<TABLE>
      [ "More Requirements" : someRelation cRud BOX<TABLE>  -- Too deep!
```

✅ **Better:**
```ampersand
"Countries" : V[SESSION*LandEis];landeisLand cRud BOX<TABLE>
  [ "Code" : I cRud<LandCode>
  , "Requirements" : landeisLand~ cRud BOX<TABLE>
      [ "Type" : landeisType cRud
      , "Product" : landeisPO;productNaam cRud
      ]
```

### Mistake 2: Wrong BOX Type

❌ **Wrong:** Using TABLE for single record editing
```ampersand
"Edit Country" : specificCountry cRud BOX<TABLE>  -- Should be FORM
```

✅ **Better:**
```ampersand
"Edit Country" : specificCountry CRuD BOX<FORM>
```

## Lesson 8: Exercise - Build Your Own Interface

### Challenge: Create a Product Overview Interface

Using these concepts: `Product` represents products that can be imported, `productNaam[Product*ProductName]` contains product names, `LandEis` represents country requirements, and `landeisPO[LandEis*Product]` shows which products are affected by requirements.

**Your task:**
Create an interface that shows all products in a table. For each product, show its name. For each product, show which country requirements affect it.

### Solution Template:

```ampersand
INTERFACE ProductOverview : "_SESSION"[SESSION] CRuD BOX<TABS>
  [ "📦 Products" : -- Your BOX-term here -- cRud BOX<TABLE>
      [ "Product ID"   : -- Show product identifier --
      , "Product Name" : -- Show product name --
      , "Requirements" : -- Show related requirements -- cRud BOX<TABLE>
          [ "Country"     : -- Show which country --
          , "Requirement" : -- Show requirement type --
          ]
      ]
  ]
```

## Next Steps

Once you master these basics, learn about VIEWs to customize how data appears. Study RULEs to add business logic. Explore advanced interface features like API endpoints. Practice with increasingly complex data relationships.

Remember: Start simple, build incrementally, and always test your interfaces with real data!
