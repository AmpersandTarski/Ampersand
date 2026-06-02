---
name: Bug report
about: Create a report to help us improve
title: "Misleading Error Message for Cartesian Product Expression in VIOLATION Statement"
labels: status: needs triage
assignees: ''
---

# What happened

The Ampersand compiler produces a misleading error message when evaluating the cartesian product expression `I#("Engels"[Taal])` in a VIOLATION statement. Instead of showing the expected single target value "Engels", the error message displays all Taal values from the population.

**Actual compiler output (proof of issue):**
```
/Users/sjo00577/surfdrive/NVWA/FC/FC4/test_cartesian_bug.adl:11:1 error:
  There are 3 violations of RULE TestCartesianProduct:
    {EX} InsPair;voorkeursTaal;LandCode;"NL";Taal;{"Engels", "Duits", "Nederlands", "Spaans", "Frans"}
    {EX} InsPair;voorkeursTaal;LandCode;"VS";Taal;{"Engels", "Duits", "Nederlands", "Spaans", "Frans"}
    {EX} InsPair;voorkeursTaal;LandCode;"UK";Taal;{"Engels", "Duits", "Nederlands", "Spaans", "Frans"}
```

Notice that the TGT field shows all five Taal values, when it should only show "Engels".

# What I expected

According to Ampersand documentation, `I[LandCode]#("Engels"[Taal])` should create pairs where each LandCode is combined with the single Taal value "Engels". The mathematical definition is:
- Identity relation: `I[LandCode]` = {(NL,NL), (UK,UK), (VS,VS)}
- Singleton set: `("Engels"[Taal])` = {"Engels"}  
- Cartesian product: `I[LandCode]#("Engels"[Taal])` = {(NL,"Engels"), (UK,"Engels"), (VS,"Engels")}
- Type: `[LandCode * Taal]`

**Expected compiler output:**
```
{EX} InsPair;voorkeursTaal;LandCode;"NL";Taal;"Engels"
{EX} InsPair;voorkeursTaal;LandCode;"VS";Taal;"Engels"  
{EX} InsPair;voorkeursTaal;LandCode;"UK";Taal;"Engels"
```

# Version of ampersand that was used

Ampersand-v5.3.7 [HEAD:c53a2a9cd]

# Steps to reproduce

**Minimal reproducible example:**

1. Create file `test_cartesian_bug.adl`:

```Ampersand
CONTEXT TestCartesianBug IN ENGLISH

CONCEPT LandCode "A country code"
CONCEPT Taal "A language"

RELATION voorkeursTaal[LandCode*Taal] [UNI]

POPULATION LandCode CONTAINS [ "NL", "UK", "VS" ]
POPULATION Taal CONTAINS [ "Engels", "Nederlands", "Frans", "Duits", "Spaans" ]

RULE TestCartesianProduct : I[LandCode] |- voorkeursTaal;voorkeursTaal~
MEANING "Every country should have a preference language."
VIOLATION (TXT "{EX} InsPair;voorkeursTaal;LandCode;", SRC I, TXT ";Taal;", TGT I#("Engels"[Taal]))

ENDCONTEXT
```

2. Run command:
```bash
ampersand check test_cartesian_bug.adl --verbose
```

3. Observe that the error message shows all Taal values instead of just "Engels"
