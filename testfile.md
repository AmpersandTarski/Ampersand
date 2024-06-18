-   [1 Introduction](#sec:Intro)
-   [2 Shared Language](#sec:SharedLang)
    -   [2.1 PatternName](#sec:SharedLangtheme-4868607006306872369)
    -   [2.2 Remaining](#sec:SharedLangtheme--8624456145932377398)
-   [3 Diagnosis](#sec:Diagnosis)
-   [4 Conceptual Analysis](#sec:ConceptualAnalysis)
    -   [4.1 PatternName](#sec:SharedLangtheme-4868607006306872369)
        -   4.1.1 Relation
        -   4.1.2 Rules
    -   [4.2 Remaining](#sec:SharedLangtheme--8624456145932377398)
        -   4.2.1 Relation
        -   4.2.2 Rules
-   [5 Data structure](#sec:DataAnalysis)
    -   5.1 Rules
        -   5.1.1 Process rules
        -   5.1.2 Invariants
    -   5.2 Logical data model
    -   5.3 Technical datamodel
        -   5.3.1 Table: Concept0Ctx
        -   5.3.2 Table: Concept0Ptn
        -   5.3.3 Table: Concept1Ctx
        -   5.3.4 Table: Concept1Ptn
        -   5.3.5 Table: Concept2Ctx
        -   5.3.6 Table: Concept2Ptn
        -   5.3.7 Table: Concept3Ctx
        -   5.3.8 Table: Concept3Ptn
        -   5.3.9 Table: Concept4Ctx
        -   5.3.10 Table: Concept4Ptn
        -   5.3.11 Table: Concept5Ctx
        -   5.3.12 Table: ONE
        -   5.3.13 Table: relatie1Ctx
        -   5.3.14 Table: relatie1Ptn
        -   5.3.15 Table: relatie2Ctx
        -   5.3.16 Table: relatie2Ptn
        -   5.3.17 Table: relatie3Ctx
        -   5.3.18 Table: relatie3Ptn
        -   5.3.19 Table: relatie4Ctx
        -   5.3.20 Table: relatie4Ptn
        -   5.3.21 Table: relatie5Ctx
        -   5.3.22 Table: relatie5Ptn
        -   5.3.23 Table: relatie6Ctx
        -   5.3.24 Table: relatie6Ptn
        -   5.3.25 Table: relatie7Ctx1
        -   5.3.26 Table: relatie7Ctx2
        -   5.3.27 Table: relatie7Ptn
    -   5.4 Logical data model

# Introduction

> this is the purpose of the CONTEXT ContextName, gedefineerd in de
> CONTEXT

# Shared Language

This chapter describes functional requirements for ‘ContextName’ in
natural language. It contains definitions and agreements. The purpose of
this chapter is to create shared understanding among stakeholders. All
definitions and agreements have been numbered for the sake of
traceability.

## PatternName

> this is the purpose of the PATTERN PatternName, gedefineerd in de
> PATTERN

> Definition Concept0Ptn:  
> this is the definition of CONCEPT Concept0Ptn in de PATTERN

> this is the purpose of the CONCEPT ConceptPtn, gedefineerd in de
> PATTERN

> Definition Concept1Ptn:  
> this is the definition of CONCEPT Concept1Ptn in de PATTERN

> Definition Concept2Ptn:  
> this is the definition of CONCEPT Concept2Ptn in de PATTERN

> Definition Concept3Ptn:  
> this is the definition of CONCEPT Concept3Ptn in de PATTERN

> Definition Concept4Ptn:  
> this is the definition of CONCEPT Concept4Ptn in de PATTERN

> this is the purpose of the RELATION relatie1Ptn, gedefineerd in de
> PATTERN

Relation 0: relatie1Ptn\[Concept0Ptn\*Concept0Ptn\]  
this is the meaning of the RELATION relatie1Ptn, gedefineerd in de
PATTERN

&nbsp;

Relation 1: relatie2Ptn\[Concept0Ptn\*Concept0Ptn\]  
this is the meaning of the RELATION relatie2Ptn, gedefineerd in de
PATTERN

&nbsp;

Relation 2: relatie3Ptn\[Concept0Ptn\*Concept0Ptn\]  
this is the meaning of the RELATION relatie3Ptn, gedefineerd in de
PATTERN

&nbsp;

Relation 3: relatie4Ptn\[Concept1Ptn\*Concept2Ptn\]  
this is the meaning of the RELATION relatie4Ptn, gedefineerd in de
PATTERN

This relation is total.

Phrases that can be made are for instance:

-   John* corresponds to *Business IT* in relation *relatie4Ptn.
-   Peter* corresponds to *Management* in relation *relatie4Ptn.
-   Susan* corresponds to *Business IT* in relation *relatie4Ptn.

Relation 4: relatie5Ptn\[Concept3Ptn\*Concept2Ptn\]  
this is the meaning of the RELATION relatie5Ptn, gedefineerd in de
PATTERN

Phrases that can be made are for instance:

-   Finance* corresponds to *Management* in relation *relatie5Ptn.
-   IT-Governance* corresponds to *Management* in relation *relatie5Ptn.
-   IT-Governance* corresponds to *Business IT* in relation *relatie5Ptn.

Relation 5: relatie6Ptn\[Concept1Ptn\*Concept3Ptn\]  
this is the meaning of the RELATION relatie6Ptn, gedefineerd in de
PATTERN

&nbsp;

Relation 6: relatie7Ptn\[Concept4Ptn\*Concept4Ptn\]  
this is the meaning of the RELATION relatie7Ptn, gedefineerd in de
PATTERN

> this is the purpose of the RULE Rule1Ptn, gedefineerd in de PATTERN

Agreement 0: "Rule1Ptn".  
The rule  is undocumented.

&nbsp;

Agreement 1: "Rule2Ptn".  
this is the meaning of the RULE Rule2Ptn, gedefineerd in de PATTERN

## Remaining

This paragraph shows remaining artifacts that have not been described in previous paragraphs.

> this is the purpose of the CONCEPT Concept0Ctx, gedefineerd in de
> CONTEXT

> Definition Concept0Ctx:  
> this is the definition of CONCEPT Concept0Ctx in de CONTEXT

> Definition Concept1Ctx:  
> this is the definition of CONCEPT Concept1Ctx in de CONTEXT

> Definition Concept2Ctx:  
> this is the definition of CONCEPT Concept2Ctx in de CONTEXT

> Definition Concept3Ctx:  
> this is the definition of CONCEPT Concept3Ctx in de CONTEXT

> Definition Concept4Ctx:  
> this is the definition of CONCEPT Concept4Ctx in de CONTEXT

> Definition Concept5Ctx:  
> this is the definition of CONCEPT Concept5Ctx in de CONTEXT

> this is the purpose of the RELATION relatie1Ctx, gedefineerd in de
> CONTEXT

Relation 15: relatie1Ctx\[Concept0Ctx\*Concept0Ctx\]  
this is the meaning of the RELATION relatie1Ctx, gedefineerd in de
CONTEXT

&nbsp;

Relation 16: relatie2Ctx\[Concept0Ctx\*Concept0Ctx\]  
this is the meaning of the RELATION relatie2Ctx, gedefineerd in de
CONTEXT

&nbsp;

Relation 17: relatie3Ctx\[Concept0Ctx\*Concept0Ctx\]  
this is the meaning of the RELATION relatie3Ctx, gedefineerd in de
CONTEXT

&nbsp;

Relation 18: relatie4Ctx\[Concept1Ctx\*Concept2Ctx\]  
this is the meaning of the RELATION relatie4Ctx, gedefineerd in de
CONTEXT

This relation is total.

Phrases that can be made are for instance:

-   John* corresponds to *Business IT* in relation *relatie4Ctx.
-   Peter* corresponds to *Management* in relation *relatie4Ctx.
-   Susan* corresponds to *Business IT* in relation *relatie4Ctx.

Relation 19: relatie5Ctx\[Concept3Ctx\*Concept2Ctx\]  
this is the meaning of the RELATION relatie5Ctx, gedefineerd in de
CONTEXT

Phrases that can be made are for instance:

-   Finance* corresponds to *Management* in relation *relatie5Ctx.
-   IT-Governance* corresponds to *Management* in relation *relatie5Ctx.
-   IT-Governance* corresponds to *Business IT* in relation *relatie5Ctx.

Relation 20: relatie6Ctx\[Concept1Ctx\*Concept3Ctx\]  
this is the meaning of the RELATION relatie6Ctx, gedefineerd in de
CONTEXT

&nbsp;

Relation 21: relatie7Ctx\[Concept4Ctx\*Concept4Ctx\]  
this is the meaning of the RELATION relatie7Ctx, gedefineerd in de
CONTEXT

&nbsp;

Relation 22: relatie7Ctx\[Concept5Ctx\*Concept5Ctx\]  

> this is the purpose of the RULE RuleCtx, gedefineerd in de CONTEXT

Agreement 4: "Rule1Ctx".  
The rule  is undocumented.

&nbsp;

Agreement 5: "Rule2Ctx".  
this is the meaning of the RULE Rule2Ctx, gedefineerd in de CONTEXT

# Diagnosis

This chapter provides an analysis of the Ampersand script of *‘ContextName’*. This analysis is intended for the author(s) of this script. It can be used to complete the script or to improve possible flaws.

*ContextName* assigns rules to roles. The following table shows the rules that are being maintained by a given role.

[TABLE]

Concepts Concept0Ptn, Concept1Ctx, Concept2Ctx, Concept2Ptn,
Concept3Ctx, Concept3Ptn, Concept4Ctx, Concept4Ptn, and
Concept5Ctx remain without a purpose.

The relation *r**e**l**a**t**i**e*7*C**t**x*\[*C**o**n**c**e**p**t*5*C**t**x* \* *C**o**n**c**e**p**t*5*C**t**x*\] lacks both a purpose as well as a meaning.

The purpose of 12 relations is not specified. Three examples of relations without a purpose are: *r**e**l**a**t**i**e*2*C**t**x*\[*C**o**n**c**e**p**t*0*C**t**x* \* *C**o**n**c**e**p**t*0*C**t**x*\],
*r**e**l**a**t**i**e*2*P**t**n*\[*C**o**n**c**e**p**t*0*P**t**n* \* *C**o**n**c**e**p**t*0*P**t**n*\],
and
*r**e**l**a**t**i**e*3*C**t**x*\[*C**o**n**c**e**p**t*0*C**t**x* \* *C**o**n**c**e**p**t*0*C**t**x*\]

Concepts Concept4Ctx, Concept5Ctx, and
Concept4Ptn are defined, but not used.

Relations *r**e**l**a**t**i**e*3*C**t**x*,
*r**e**l**a**t**i**e*3*P**t**n*, *r**e**l**a**t**i**e*7*C**t**x*,
*r**e**l**a**t**i**e*7*C**t**x*, and
*r**e**l**a**t**i**e*7*P**t**n* are not used in any rule. 

**¿fig:Concept diagram of relations in PatternName?**
 shows a conceptual diagram with all relations.

![Concept diagram of relations in
PatternName](./images/RelationsInPatternPatternName.dot "fig:Concept diagram of relations in PatternName")

The following rules are defined without documenting their purpose:

-   *Rule2Ctx* (/workspaces/Ampersand/testfile.adl:38:1)

    *r**e**l**a**t**i**e*6*C**t**x* ⊢ *r**e**l**a**t**i**e*4*C**t**x*; *r**e**l**a**t**i**e*5*C**t**x*^(⌣)

    For all c, c': If c relatie6Ctx c' then (There exists c'': c
    relatie4Ctx c'' and c' relatie5Ctx c'')

-   *Rule2Ptn* (/workspaces/Ampersand/testfile.adl:113:1)

    *r**e**l**a**t**i**e*6*P**t**n* ⊢ *r**e**l**a**t**i**e*4*P**t**n*; *r**e**l**a**t**i**e*5*P**t**n*^(⌣)

    For all c, c': If c relatie6Ptn c' then (There exists c'': c
    relatie4Ptn c'' and c' relatie5Ptn c'')

The following rules are not accompanied by a meaning written in natural language:

-   *Rule1Ctx* (/workspaces/Ampersand/testfile.adl:36:1)

    *r**e**l**a**t**i**e*1*C**t**x* ⊢ *r**e**l**a**t**i**e*2*C**t**x*

    For all c, c': If c relatie1Ctx c' then c relatie2Ctx c'

-   *Rule1Ptn* (/workspaces/Ampersand/testfile.adl:111:1)

    *r**e**l**a**t**i**e*1*P**t**n* ⊢ *r**e**l**a**t**i**e*2*P**t**n*

    For all c, c': If c relatie1Ptn c' then c relatie2Ptn c'

The table below shows for each theme (i.e. pattern) the number of relations and rules, followed by the number and percentage that have a reference. Relations declared in multiple themes are counted multiple times.

[TABLE]

The table below shows the signal rules per role.

[TABLE]

The population in this script does not specify any work in progress. 

The population in this script violates no rule. 

&nbsp;

# Conceptual Analysis

> this is the purpose of the CONTEXT ContextName, gedefineerd in de
> CONTEXT

## PatternName

> this is the purpose of the PATTERN PatternName, gedefineerd in de
> PATTERN

> Definition Concept0Ptn:  
> this is the definition of CONCEPT Concept0Ptn in de PATTERN

> this is the purpose of the CONCEPT ConceptPtn, gedefineerd in de
> PATTERN

> Definition Concept1Ptn:  
> this is the definition of CONCEPT Concept1Ptn in de PATTERN

> Definition Concept2Ptn:  
> this is the definition of CONCEPT Concept2Ptn in de PATTERN

> Definition Concept3Ptn:  
> this is the definition of CONCEPT Concept3Ptn in de PATTERN

> Definition Concept4Ptn:  
> this is the definition of CONCEPT Concept4Ptn in de PATTERN

**¿fig:Concept diagram of the rules in PatternName?** Conceptual diagram
of ‘PatternName’.

![Concept diagram of the rules in
PatternName](./images/CDPatternPatternName.dot "fig:Concept diagram of the rules in PatternName")

### Relation

[TABLE]

### Rules

This section itemizes the rules with a reference to the shared language
of stakeholders for the sake of traceability.

  
The undocumented agreement \[ @agr:SharedLangrule-6351609249149493099 \]
has been made:

this is the meaning of the RULE Rule2Ptn, gedefineerd in de PATTERN

Using relations **¿eq:ConceptualAnalysisrelation-2207915830139284388?**
(relatie4Ptn),  **¿eq:ConceptualAnalysisrelation--7040742365795626749?**
(relatie5Ptn),  **¿eq:ConceptualAnalysisrelation--3113214035548729453?**
(relatie6Ptn), this is formalized as

**Rule
***r**e**l**a**t**i**e*6*P**t**n* ⊢ *r**e**l**a**t**i**e*4*P**t**n*; *r**e**l**a**t**i**e*5*P**t**n*^(⌣)

**¿fig:Concept diagram of rule Rule2Ptn?** shows a conceptual diagram of
this rule.

![Concept diagram of rule
Rule2Ptn](./images/CDRuleRule2Ptn.dot "fig:Concept diagram of rule Rule2Ptn")

## Remaining

> this is the purpose of the CONCEPT Concept0Ctx, gedefineerd in de
> CONTEXT

> Definition Concept0Ctx:  
> this is the definition of CONCEPT Concept0Ctx in de CONTEXT

> Definition Concept1Ctx:  
> this is the definition of CONCEPT Concept1Ctx in de CONTEXT

> Definition Concept2Ctx:  
> this is the definition of CONCEPT Concept2Ctx in de CONTEXT

> Definition Concept3Ctx:  
> this is the definition of CONCEPT Concept3Ctx in de CONTEXT

> Definition Concept4Ctx:  
> this is the definition of CONCEPT Concept4Ctx in de CONTEXT

> Definition Concept5Ctx:  
> this is the definition of CONCEPT Concept5Ctx in de CONTEXT

### Relation

[TABLE]

### Rules

This section itemizes the rules with a reference to the shared language
of stakeholders for the sake of traceability.

  
The undocumented agreement \[ @agr:SharedLangrule-6367651123801874590 \]
has been made:

this is the meaning of the RULE Rule2Ctx, gedefineerd in de CONTEXT

Using relations **¿eq:ConceptualAnalysisrelation-759614536307421747?**
(relatie4Ctx),  **¿eq:ConceptualAnalysisrelation--6924562008301087824?**
(relatie5Ctx),  **¿eq:ConceptualAnalysisrelation-4792836765589561620?**
(relatie6Ctx), this is formalized as

**Rule
***r**e**l**a**t**i**e*6*C**t**x* ⊢ *r**e**l**a**t**i**e*4*C**t**x*; *r**e**l**a**t**i**e*5*C**t**x*^(⌣)

**¿fig:Concept diagram of rule Rule2Ctx?** shows a conceptual diagram of
this rule.

![Concept diagram of rule
Rule2Ctx](./images/CDRuleRule2Ctx.dot "fig:Concept diagram of rule Rule2Ctx")

# Data structure

This chapter contains the result of the data analysis. It is structured
as follows:

We start with the classification model, followed by a list of all
relations, that are the foundation of the rest of the analysis. Finally,
the logical and technical data model are discussed.

## Rules

In this section an overview of all rules by printing the term of each
rule. The process rules are given first, followed by the invariants.

### Process rules

Process rules are rules that are signalled.

**Process rule: *Compute relatie1Ctx\[Concept0Ctx\*Concept0Ctx\] using
DelPair***

*r**e**l**a**t**i**e*1*C**t**x* ⊢ *r**e**l**a**t**i**e*2*C**t**x* ∩ *r**e**l**a**t**i**e*3*C**t**x*

**Process rule: *Compute relatie1Ptn\[Concept0Ptn\*Concept0Ptn\] using
DelPair***

*r**e**l**a**t**i**e*1*P**t**n* ⊢ *r**e**l**a**t**i**e*2*P**t**n* ∩ *r**e**l**a**t**i**e*3*P**t**n*

**Process rule: *Rule1Ctx***

> this is the purpose of the RULE RuleCtx, gedefineerd in de CONTEXT

*r**e**l**a**t**i**e*1*C**t**x* ⊢ *r**e**l**a**t**i**e*2*C**t**x*

**Process rule: *Rule1Ptn***

> this is the purpose of the RULE Rule1Ptn, gedefineerd in de PATTERN

*r**e**l**a**t**i**e*1*P**t**n* ⊢ *r**e**l**a**t**i**e*2*P**t**n*

### Invariants

Invariants are rules that are enforced by the database. It is guaranteed
that violations cannot occur in the database.

**Invariant: *Rule2Ctx***

this is the meaning of the RULE Rule2Ctx, gedefineerd in de CONTEXT

*r**e**l**a**t**i**e*6*C**t**x* ⊢ *r**e**l**a**t**i**e*4*C**t**x*; *r**e**l**a**t**i**e*5*C**t**x*^(⌣)

Violations of this rule will result in an error message for the user:

-   &lt;violation message should be printed here&gt;

**Invariant: *Rule2Ptn***

this is the meaning of the RULE Rule2Ptn, gedefineerd in de PATTERN

*r**e**l**a**t**i**e*6*P**t**n* ⊢ *r**e**l**a**t**i**e*4*P**t**n*; *r**e**l**a**t**i**e*5*P**t**n*^(⌣)

Violations of this rule will result in an error message for the user:

-   &lt;violation message should be printed here&gt;

**Invariant: *TOT relatie4Ctx\[Concept1Ctx\*Concept2Ctx\]***

relatie4Ctx\[Concept1Ctx\*Concept2Ctx\] is total

*I*_(\[*C**o**n**c**e**p**t*1*C**t**x*\]) ⊢ *r**e**l**a**t**i**e*4*C**t**x*; *r**e**l**a**t**i**e*4*C**t**x*^(⌣)

**Invariant: *TOT relatie4Ptn\[Concept1Ptn\*Concept2Ptn\]***

relatie4Ptn\[Concept1Ptn\*Concept2Ptn\] is total

*I*_(\[*C**o**n**c**e**p**t*1*P**t**n*\]) ⊢ *r**e**l**a**t**i**e*4*P**t**n*; *r**e**l**a**t**i**e*4*P**t**n*^(⌣)

## Logical data model

The functional requirements have been translated into a data model. This
model is shown by **¿fig:Logical data model of ContextName?** .

![Logical data model of
ContextName](./images/LogicalDataModel.dot "fig:Logical data model of ContextName")

There are no entity types.

[TABLE]

[TABLE]

## Technical datamodel

The functional requirements have been translated into a technical data
model. This model is shown by **¿fig:Technical data model of
ContextName?** .

![Technical data model of
ContextName](./images/TechnicalDataModel.dot "fig:Technical data model of ContextName")

The technical datamodel consists of the following 27 tables:

### Table: Concept0Ctx

This table has the following 1 attributes:

-   **Concept0Ctx**  
    This attribute is the primary key.  
    `OBJECT`, Mandatory, Unique.

### Table: Concept0Ptn

This table has the following 1 attributes:

-   **Concept0Ptn**  
    This attribute is the primary key.  
    `OBJECT`, Mandatory, Unique.

### Table: Concept1Ctx

This table has the following 1 attributes:

-   **Concept1Ctx**  
    This attribute is the primary key.  
    `OBJECT`, Mandatory, Unique.

### Table: Concept1Ptn

This table has the following 1 attributes:

-   **Concept1Ptn**  
    This attribute is the primary key.  
    `OBJECT`, Mandatory, Unique.

### Table: Concept2Ctx

This table has the following 1 attributes:

-   **Concept2Ctx**  
    This attribute is the primary key.  
    `OBJECT`, Mandatory, Unique.

### Table: Concept2Ptn

This table has the following 1 attributes:

-   **Concept2Ptn**  
    This attribute is the primary key.  
    `OBJECT`, Mandatory, Unique.

### Table: Concept3Ctx

This table has the following 1 attributes:

-   **Concept3Ctx**  
    This attribute is the primary key.  
    `OBJECT`, Mandatory, Unique.

### Table: Concept3Ptn

This table has the following 1 attributes:

-   **Concept3Ptn**  
    This attribute is the primary key.  
    `OBJECT`, Mandatory, Unique.

### Table: Concept4Ctx

This table has the following 1 attributes:

-   **Concept4Ctx**  
    This attribute implements the identityrelation of
    *C**o**n**c**e**p**t*4*C**t**x*.  
    `ALPHANUMERIC`, Mandatory, Unique.

### Table: Concept4Ptn

This table has the following 1 attributes:

-   **Concept4Ptn**  
    This attribute implements the identityrelation of
    *C**o**n**c**e**p**t*4*P**t**n*.  
    `ALPHANUMERIC`, Mandatory, Unique.

### Table: Concept5Ctx

This table has the following 1 attributes:

-   **Concept5Ctx**  
    This attribute is the primary key.  
    `OBJECT`, Mandatory, Unique.

### Table: ONE

This table has the following 1 attributes:

-   **ONE**  
    This attribute is the primary key.  
    `OBJECT`, Mandatory, Unique.

### Table: relatie1Ctx

This is a link-table, implementing the relation
*C**o**n**c**e**p**t*0*C**t**x* → *r**e**l**a**t**i**e*1*C**t**x**C**o**n**c**e**p**t*0*C**t**x*.
It contains the following columns:

-   **SrcConcept0Ctx**  
    This attribute is a foreign key to Concept0Ctx  
    `OBJECT`, Mandatory.

-   **TgtConcept0Ctx**  
    This attribute is a foreign key to Concept0Ctx  
    `OBJECT`, Mandatory.

### Table: relatie1Ptn

This is a link-table, implementing the relation
*C**o**n**c**e**p**t*0*P**t**n* → *r**e**l**a**t**i**e*1*P**t**n**C**o**n**c**e**p**t*0*P**t**n*.
It contains the following columns:

-   **SrcConcept0Ptn**  
    This attribute is a foreign key to Concept0Ptn  
    `OBJECT`, Mandatory.

-   **TgtConcept0Ptn**  
    This attribute is a foreign key to Concept0Ptn  
    `OBJECT`, Mandatory.

### Table: relatie2Ctx

This is a link-table, implementing the relation
*C**o**n**c**e**p**t*0*C**t**x* → *r**e**l**a**t**i**e*2*C**t**x**C**o**n**c**e**p**t*0*C**t**x*.
It contains the following columns:

-   **SrcConcept0Ctx**  
    This attribute is a foreign key to Concept0Ctx  
    `OBJECT`, Mandatory.

-   **TgtConcept0Ctx**  
    This attribute is a foreign key to Concept0Ctx  
    `OBJECT`, Mandatory.

### Table: relatie2Ptn

This is a link-table, implementing the relation
*C**o**n**c**e**p**t*0*P**t**n* → *r**e**l**a**t**i**e*2*P**t**n**C**o**n**c**e**p**t*0*P**t**n*.
It contains the following columns:

-   **SrcConcept0Ptn**  
    This attribute is a foreign key to Concept0Ptn  
    `OBJECT`, Mandatory.

-   **TgtConcept0Ptn**  
    This attribute is a foreign key to Concept0Ptn  
    `OBJECT`, Mandatory.

### Table: relatie3Ctx

This is a link-table, implementing the relation
*C**o**n**c**e**p**t*0*C**t**x* → *r**e**l**a**t**i**e*3*C**t**x**C**o**n**c**e**p**t*0*C**t**x*.
It contains the following columns:

-   **SrcConcept0Ctx**  
    This attribute is a foreign key to Concept0Ctx  
    `OBJECT`, Mandatory.

-   **TgtConcept0Ctx**  
    This attribute is a foreign key to Concept0Ctx  
    `OBJECT`, Mandatory.

### Table: relatie3Ptn

This is a link-table, implementing the relation
*C**o**n**c**e**p**t*0*P**t**n* → *r**e**l**a**t**i**e*3*P**t**n**C**o**n**c**e**p**t*0*P**t**n*.
It contains the following columns:

-   **SrcConcept0Ptn**  
    This attribute is a foreign key to Concept0Ptn  
    `OBJECT`, Mandatory.

-   **TgtConcept0Ptn**  
    This attribute is a foreign key to Concept0Ptn  
    `OBJECT`, Mandatory.

### Table: relatie4Ctx

This is a link-table, implementing the relation
*C**o**n**c**e**p**t*1*C**t**x* → *r**e**l**a**t**i**e*4*C**t**x**C**o**n**c**e**p**t*2*C**t**x*.
It contains the following columns:

-   **Concept1Ctx**  
    This attribute is a foreign key to Concept1Ctx  
    `OBJECT`, Mandatory.

-   **Concept2Ctx**  
    This attribute is a foreign key to Concept2Ctx  
    `OBJECT`, Mandatory.

### Table: relatie4Ptn

This is a link-table, implementing the relation
*C**o**n**c**e**p**t*1*P**t**n* → *r**e**l**a**t**i**e*4*P**t**n**C**o**n**c**e**p**t*2*P**t**n*.
It contains the following columns:

-   **Concept1Ptn**  
    This attribute is a foreign key to Concept1Ptn  
    `OBJECT`, Mandatory.

-   **Concept2Ptn**  
    This attribute is a foreign key to Concept2Ptn  
    `OBJECT`, Mandatory.

### Table: relatie5Ctx

This is a link-table, implementing the relation
*C**o**n**c**e**p**t*3*C**t**x* → *r**e**l**a**t**i**e*5*C**t**x**C**o**n**c**e**p**t*2*C**t**x*.
It contains the following columns:

-   **Concept3Ctx**  
    This attribute is a foreign key to Concept3Ctx  
    `OBJECT`, Mandatory.

-   **Concept2Ctx**  
    This attribute is a foreign key to Concept2Ctx  
    `OBJECT`, Mandatory.

### Table: relatie5Ptn

This is a link-table, implementing the relation
*C**o**n**c**e**p**t*3*P**t**n* → *r**e**l**a**t**i**e*5*P**t**n**C**o**n**c**e**p**t*2*P**t**n*.
It contains the following columns:

-   **Concept3Ptn**  
    This attribute is a foreign key to Concept3Ptn  
    `OBJECT`, Mandatory.

-   **Concept2Ptn**  
    This attribute is a foreign key to Concept2Ptn  
    `OBJECT`, Mandatory.

### Table: relatie6Ctx

This is a link-table, implementing the relation
*C**o**n**c**e**p**t*1*C**t**x* → *r**e**l**a**t**i**e*6*C**t**x**C**o**n**c**e**p**t*3*C**t**x*.
It contains the following columns:

-   **Concept1Ctx**  
    This attribute is a foreign key to Concept1Ctx  
    `OBJECT`, Mandatory.

-   **Concept3Ctx**  
    This attribute is a foreign key to Concept3Ctx  
    `OBJECT`, Mandatory.

### Table: relatie6Ptn

This is a link-table, implementing the relation
*C**o**n**c**e**p**t*1*P**t**n* → *r**e**l**a**t**i**e*6*P**t**n**C**o**n**c**e**p**t*3*P**t**n*.
It contains the following columns:

-   **Concept1Ptn**  
    This attribute is a foreign key to Concept1Ptn  
    `OBJECT`, Mandatory.

-   **Concept3Ptn**  
    This attribute is a foreign key to Concept3Ptn  
    `OBJECT`, Mandatory.

### Table: relatie7Ctx1

This is a link-table, implementing the relation
*C**o**n**c**e**p**t*5*C**t**x* → *r**e**l**a**t**i**e*7*C**t**x**C**o**n**c**e**p**t*5*C**t**x*.
It contains the following columns:

-   **SrcConcept5Ctx**  
    This attribute is a foreign key to Concept5Ctx  
    `OBJECT`, Mandatory.

-   **TgtConcept5Ctx**  
    This attribute is a foreign key to Concept5Ctx  
    `OBJECT`, Mandatory.

### Table: relatie7Ctx2

This is a link-table, implementing the relation
*C**o**n**c**e**p**t*4*C**t**x* → *r**e**l**a**t**i**e*7*C**t**x**C**o**n**c**e**p**t*4*C**t**x*.
It contains the following columns:

-   **SrcConcept4Ctx**  
    This attribute is a foreign key to Concept4Ctx  
    `ALPHANUMERIC`, Mandatory.

-   **TgtConcept4Ctx**  
    This attribute is a foreign key to Concept4Ctx  
    `ALPHANUMERIC`, Mandatory.

### Table: relatie7Ptn

This is a link-table, implementing the relation
*C**o**n**c**e**p**t*4*P**t**n* → *r**e**l**a**t**i**e*7*P**t**n**C**o**n**c**e**p**t*4*P**t**n*.
It contains the following columns:

-   **SrcConcept4Ptn**  
    This attribute is a foreign key to Concept4Ptn  
    `ALPHANUMERIC`, Mandatory.

-   **TgtConcept4Ptn**  
    This attribute is a foreign key to Concept4Ptn  
    `ALPHANUMERIC`, Mandatory.

## Logical data model

[TABLE]
