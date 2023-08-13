---
title: Syntax of Ampersand
id: syntax
---

# Syntax and semantics of Ampersand

This page defines the syntax and semantics of the statements in the Ampersand language. [Terms](./the-language-ampersand/terms.md) and [services](./the-language-ampersand/services.md) are defined in separate pages.

## The CONCEPT statement

### Purpose:

A concept statement defines a concept in natural language. A concept is a name for similar things. For example: `Peter`, `John`, and `Barack` are things you might want to call `Person`, whereas `45-NP-88` and `KD-686-D` could be instances of the concept `LicensePlate`.

### Syntax:

```text
CONCEPT <Uppercase identifier> <String> <String>?
```

This statement may occur anywhere within a context, either inside or outside a pattern.

### Semantics

This statement means that there exists a concept called `<Uppercase identifier>` in the current context.

- `<Uppercase identifier>` specifies the name of the concept.
- `String` contains a definition of the concept. This definition is used by the documentation generator, which expects it to be a grammatically correct and complete sentence.
- `String?` is an \(optional\) reference to the source of the definition. It is meant for traceability.

### Examples

```text
CONCEPT Person "A person is a human creature." "Ventroli1997"
```

```text
CONCEPT Organization "An organization is a collection of persons that work together to achieve specific objectives."
```

```text
CONCEPT Criterion "A criterion is a standard on which a judgment or decision may be based." "Merriam-Webster"
```

### Miscellaneous

- The name of a concept starts with an uppercase.
- A concept should be used for immutable concepts. E.g. use a concept `Person` to express that a person will always be a person and will not change in, let us say, a table. However, don't use `Employee`, because termination of an employee's contract causes a person to be an employee no longer. So employees are not immutable. To be an employee is a dynamic property, so model it as a relation.
- The description will be printed in the functional specification, so please check that your definition is a complete sentence.
- Concepts need not be defined. If you use a concept without a definition, Ampersand defines it for you \(regardless of whether you defined it or not\).

## The CONTEXT statement

### Purpose

The data contained in a business system represents a view of \(a very small part of\) the real world. Ideally, this view must be consistent, meaning that there may be no contradictions within that view. Since different business systems have different ways of viewing the real world, and/or look at different parts of the real world, we need to be able to distinguish between such views. We use the term 'Context' to refer to an individual view. Thus, a Context is defined in terms of concepts, relations and rules, and it consists of atoms and links to populate them.

### Semantics

Any Ampersand model has one context.  
The model is true within its context and there is no knowledge in a model about other contexts.

### Syntax

The model is specified between the keywords CONTEXT and ENDCONTEXT. A context has a name.

```text
CONTEXT MyModel
INCLUDE*

<all kind of elements in the model>

ENDCONTEXT
```

Other models included with the INCLUDE statement become part of the context they are included in.

#### Optional parts

##### Language definition

To tell Ampersand what language your context is in, you can append a language directive to your context. Currently English and Dutch are supported. To do so, directly following the name of your context, you can specify

```text
IN <language>
```

Where `<language>` can be `ENGLISH` or `DUTCH`.

##### Markup format

Directly following the optional language definition, you can optionally specify the format of your texts \(see PURPOSE statement\). Ampersand understands some different markup styles. The default style is REST \(Restructured Text\)

```text
<markupStyle>
```

where can be one of

`REST`,

`HTML`,

`LATEX`,

`MARKDOWN`.

\(For details on these formats, see [pandoc.org](http://pandoc.org/)\).

## The CLASSIFY statement

### Purpose

A _**classify statement**_ is also called a _**specialization**_. It specifies that atoms of one concept are atoms of another concept as well. You can use it to buils classifications like [Linnaeus](https://www.britannica.com/science/taxonomy/The-Linnaean-system) did.

### Syntax and meaning

```
CLASSIFY <upper case identifier> ISA <upper case identifier>
```

In a specialization, e.g. `CLASSIFY Sedan ISA Car`, we call the first concept (`Sedan`) the specific concept and the second (`Car`) the generic concept. The meaning of a specialization is that every atom from the specific concept is an atom from the generic concept as well. So every (atom that is a) Sedan is a Car as well.

So in general: `CLASSIFY` $$A$$ `ISA` $$B$$ means: $$\forall a: a\in A\Rightarrow a\in B$$.

### Examples

```
CLASSIFY Monkey ISA Mammal
```

```
CLASSIFY Sedan ISA Car
```

To save some writing, you may specify

```
CLASSIFY Monkey, Cow, Human ISA Mammal
```

`This means exactly the same as`

```
CLASSIFY Monkey ISA Mammal
CLASSIFY Cow ISA Mammal
CLASSIFY Human ISA Mammal
```

### Best practice

A specialization is a static relationship. If you want to say that a student is a person, please consider whether you want this to be static. If a person can enroll to become a student, or graduate or drop out to become non-student again, the dynamics of that cannot be captured in a specialization. Use a relationship instead to model the state of being a student. \
E.g. `RELATION student[Person*Enrollment]`

By adding and removing pairs to that relation, it continuously reflects which persons are a student.

## The ENFORCE statement {#the-enforce-statement}

### Purpose

The purpose of this statement is to automatically modify the population of a relation based on rules.

### Syntax

Since ampersand 4.4.0 the syntax of this statement is:

```
ENFORCE <RelationRef> <type>?
        <operator>
        <Term>
```

The `<operator>` can be one of **`:=`,** `:<` or `>:` .

This statement may occur anywhere within a context, either inside or outside a pattern.

### Semantics

This statement means the population of the relation will automatically be kept respectively equal ( **`:=`**), a subset (`:<`) or a superset (`>:`) of the population of the given term.

### Examples

```
ENFORCE r := s;t
{- Ampersand will keep the population of the relation r equal to the population
   of the term s;t . It will do so by changing the contents of r
   without affecting the contents of s;t .
   The effect can be observed in the prototype.
-}
```

```
ENFORCE canDrive :< hasCar /\ hasDriverLicence
{- Ampersand will keep the population of the relation canDrive smaller than
   the population of the term hasCar /\ hasDriverLicence .
   It will do so by deleting pairs from the contents of canDrive
   without affecting the contents of hasCar /\ hasDriverLicence .
   So, whenever a person can drive, that person needs to have a car and a driver licence.
   However, if that person has both these assets, it is still possible that he/she
   cannot drive.
-}
```

### Miscellaneous

- Both the sources and the targets of the relation and the term must match. An error message is given otherwise.
- The relation must be specified in order to use it here, as is the case with any relation used in a term.

## The IDENT statement

### Purpose:

This statement is a rule, which defines an identity on a concept. It is syntactic sugar for specifying a set of relations that identify atoms in a specific concept. For example, if relations `pi` and `rho` determine an atom of concept `T` uniquely, you can write:

```text
IDENT "T uniqueness" :  T (pi, rho)
```

As the IDENT statement defines a rule, it can be in the same places as any other RULE.

### Syntax

```text
`IDENT` (<label> `:`)? <Concept> `(` <term>* `)`
```

where:

- `<label>` is the name of the rule. It can be a single word or a string \(enclosed by double brackets\). It is followed by a colon \(`:`\) to distinguish the label from the concept that follows.
- `<Concept>` is the name of the Concept for atoms of which the rule specifies an identity
- Between brackets are terms whose source concept must be `<Concept>`. This is enforced by the type system.

### Informal Semantics

```text
IDENT "Rule Name" : C (e1, e2, ...)
```

translates into the following rule:

```text
  RULE "Rule Name":  {e1}<>{e1}~ /\ {e2}<>{e2}~ /\ ... |- I[C]
```

Note that

- in case every`e`is both univalent and total, `e<>e~` equals `e;e~`, and the rule is equivalent to:

```text
   RULE "Rule Name":  {e1};{e1}~ /\ {e2};{e2}~ /\ ... |- I[C]
```

- in case every `e` is univalent but not total, you should use the `IDENT` statement \(or the rule that it implements\), because that also works when an `e` is not populated.

## The INCLUDE statement

### Purpose

To facilitate reusing code, Ampersand allows its user to divide code over different files.

### Description

The `INCLUDE`-statement includes the code of another Ampersand-script or the data of a .xlsx-file into the context.

### Examples

```text
INCLUDE "foo.adl"
INCLUDE "subdirectory/foo.adl"
INCLUDE "bar.xlsx"
```

### Syntax and meaning

```text
INCLUDE <filename>
```

This statement specifies files that need to be included before compiling. The filename is given in double quotes, including a path that is relative to the position of the main adl-file. The main adl-file is the file that is called with the command Ampersand.

Possible files to include are:

- other adl-files
- xlsx-files to include population

All code in the included adl-files will become part of the context of the main adl-file.

Make sure to include the adl-files before including xlsx-files.

Included files may contain `INCLUDE`statements themselves. The files mentioned there are treated as though they were included in the main file. So their code is also part of the same context. Nested adl-files can have their own xlsx-files included.

For formatting your excel-file see the text on [the Excel Importer](../the-excel-importer.md).

## The MEANING sub-statement

A meaning is optional and is characterized by the reserved word `MEANING`. It specifies the meaning of a concept, a relation, or a rule in natural language. The meaning is used to generate documentation and is printed in the functional specification. A `<meaning>` can be any text, starting with `{+` and ending with `+}` e.g.
MEANING can be used with [CONCEPT](#the-concept-statement), [RELATION](#the-relation-statement), and [RULE](#the-rule-statement)-statements, to define the meaning of your concepts, relations, and rules.

```text
MEANING
{+ This is an example that is
   spread over multiple lines.
+}
```

The optional `<language>` is specified as

- `IN ENGLISH` or
- `IN DUTCH`.

Example :

```text
MEANING IN DUTCH {+ Dit is een voorbeeld in een (1) regel.+}
```

This is a way to override the default language \(which is English\).

Sometimes you need formatting in the meaning, such as dotted lists, italics, or mathematical symbols. For this purpose you have a choice in which syntax you specify the meaning. The optional `<markup>` is one of :

- `REST` \(Restructured text. This is the default\)
- `HTML`
- `LATEX`
- `MARKDOWN`

Example :

```text
MEANING LATEX {+This is a {\em mathematical} formula $\frac{3}{x+7}$.+}
```

Ampersand uses Pandoc to offer a choice for your markup. See [pandoc.org](http://pandoc.org/) for details.

## Patterns

### Purpose

Patterns are meant to isolate discussions and make solutions reusable, as known from [design patterns](http://en.wikipedia.org/wiki/Design_pattern).

### Description

A pattern is a set of rules that describes a theme or a general reusable solution to a commonly occurring problem.

For instance, if specific concerns about security arise, you might want to discuss this with stakeholders in security. With them you can discuss which rules in particular constitute your solution. Divide your problem in smaller pieces and discuss each piece with just the right stakeholders. This allows you to go deeper by talking to the right people. It saves time as well by freeing others from having to participate. An even larger benefit arises if you reuse patterns that have been discussed and scrutinized before. The best thing comes once your stakeholders agree. By that time, your pattern represents their agreement formally in Ampersand, so you can use it in the larger context of the information system.

### Example

```
PATTERN Security

RELATION required[Subject*Destination]
MEANING "A subject that you must have passed to qualify for the school trip to a destination"

RELATION pass[Subject*Student]
MEANING "The subjects that have been passed by specific students"

RELATION attends[Student*Destination]

PURPOSE RULE guardPrerequisites
{+ This rule prevents students from registering for a trip
without having passed the required courses. +}
RULE guardPrerequisites : attends;required |- pass

ENDPATTERN
```

### Syntax

Every pattern has the following form:

```
PATTERN <pattern name>
    <pattern element>*
ENDPATTERN
```

A pattern consists of any number of pattern elements in an arbitrary order. The following pattern elements are allowed:

|                    |                                                                                                            |
| ------------------ | ---------------------------------------------------------------------------------------------------------- |
| `<rule>`           | a statement that declares a [rule](#the-rule-statement)                                                        |
| `<classify>`       | a statement that specifies generalization/specialization of [concepts](#CLASSIFY-statement)                |
| `<relation>`       | a declaration of a relation, stating the existence of a [relation](#the-relation-statement) within the context |
| `<conceptDef>`     | a description of a [concept](#the-concept-statement), to document its meaning                              |
| `<representation>` | a statement that defines the atomic type of a [concept](#the-concept-statement)                            |
| `<roleRule>`       | a statement that makes a role responsible for satisfying a rule                                            |
| `<ident>`          | a rule that defines an [identity](#the-ident-statement) on a concept                                           |
| `<viewDef>`        | a statement for presenting facts in a readable sentence. It is part of a rule, to show violations.         |
| `<purpose>`        | a statement to describe the [purpose](#the-purpose-statement) of a pattern or a pattern element                |
| `<population>`     | a statement that sums up the initial [population](#the-population-statement) of a relation                     |

### Good practice

A model can have as many patterns as you want.
It has no effect on how the code is processed.

The service definition must be outside a pattern

A pattern contains rules in an arbitrary order.
The context in which these rules are valid must contain the definition for each of the relations that are used in those rules.
It is good practice to declare all relations in the pattern itself.
That practice makes the pattern self-contained and therefore more suitable for reuse.

Ampersand advocates **one theme in one pattern**. Stakeholders confine their discussion to one theme, and deliver the result in one pattern.

### Restrictions

In the current implementation of Ampersand, patterns are defined within a context. (This will change in a future version.) If you want to reuse patterns, you have to cut-and-paste them from one context to another. In the future, there will be a better mechanism for reusing patterns in different contexts.

## The POPULATION statement

### Purpose

To store data in a database corresponds to populating the relations in a context. Atoms are the data and pairs of atoms are inserted and deleted during the lifetime of a relation.

### Description

All pairs in a relation are called the population of that relation. All atoms in a concept constitute the population of that concept. The population of all relations and concepts in a context make the population of that context.

There are two ways to populate a concept with atoms:

- A `POPULATION` statement defines the initial population of a concept or a relation.
- An `INCLUDE` statement defines the initial population from an xlsx-file \(i.e. an Excel speadsheet\)

[Using spreadsheets](#population-in-spreadsheets) to define an initial population allows you to work with larger populations. Often you can use an existing spreadsheet and adapt it to become acceptable as Ampersand input.

### Syntax

You can define atoms separately and you can define the pairs in a relation. Both methods result in added population for each concept.

```text
POPULATION Tree CONTAINS
    [ "Oak"
    , "Birch"
    , "Willow"
    ]
```

```text
POPULATION personBank[Person*Bank] CONTAINS
    [ ("John", "ING")
    , ("Jane", "TRIODOS")
    ]
```

The list of pairs is a comma-separated list between square brackets. Pairs are comma-separated pairs between round brackets. Each atom is enclosed in double quotes.

### Population in spreadsheets

In this section we will make an Ampersand script that is based on an existing spreadsheet. This technique is useful for quickly adding population to an information system. Ampersand has a facility that allows you to import existing .xlsx files with minimal changes.

#### Theory: tables vs. binary relations

We can consider Ampersand as a finite system of relations. Every relation is a set of \(ordered\) pairs and each pair contains two atoms. However, in the real world we also store information in wider tables, as we do in spreadsheets and relational databases. Here is the trick. If we have two pairs that share the same left atom, e.g. \(1, Abraham\) and \(1, Lincoln\), we can put them in the same row. Using the same trick, we can interpret a row in a spreadsheet as a number of pairs.

##### Example

Let us look at an example:

|     | firstname | lastname   | birth             |
| :-- | :-------- | :--------- | :---------------- |
| 1   | Abraham   | Lincoln    | February 12, 1809 |
| 2   | Barack    | Obama      | August 4, 1961    |
| 3   | Calvin    | Coolidge   | July 4, 1872      |
| 4   | Dwight    | Eisenhower | October 14, 1890  |

Since Ampersand works with relations, it must represent this table as relations. Three relations can do the job in the following manner:

```text
POPULATION firstname[President*Name] CONTAINS
  [ ("1", "Abraham")
  , ("2", "Barack")
  , ("3", "Calvin")
  , ("4", "Dwight")
  ]

POPULATION lastname[President*Surname] CONTAINS
  [ ("1", "Lincoln")
  , ("2", "Obama")
  , ("3", "Coolidge")
  , ("4", "Eisenhower")
  ]

POPULATION birth[President*Date] CONTAINS
  [ ("1", "February 12, 1809")
  , ("2", "August 4, 1961")
  , ("3", "July 4, 1872")
  , ("4", "October 14, 1890")
  ]
```

Notice that the column names in the table correspond with the relation names in Ampersand. In the table we call them "attributes". So it makes sense to say that a relation in Ampersand can correspond with an attribute in a table.

#### Practice: how to prepare a spreadsheet

In theory, the population of the Hawaii-script might just as well be given in a spreadsheet. This works in practice too. It looks like this:

| \[Subject\]     | pass      | required    |
| :-------------- | :-------- | :---------- |
| Subject         | Student   | Destination |
| Surfing         | Brown     | Hawaii      |
| Surfing         | Conway    |             |
| Latin           | Brown     | Rome        |
| World Religions | Applegate |             |
| World Religions | Brown     | Rome        |

Please copy this in a spreadsheet of your own. The element in the first column with square brackets tells Ampersand that a new table starts. The first row contains relation names. The second row contains concept names. The rows that follow contain pairs. Ampersand reconstructs those pairs as in the example above.

#### Reusing existing data

In practical applications, you might want to reuse data from existing spreadsheets. People tend to have lots of "informal administration" in spreadsheets, which gives you access to authentic population. Surely you need that data organized in rows, but fortunately that is reasonably common. In such cases, you just add two lines above each table to inform Ampersand about the relations that are populated. In other cases, you have some work organizing the spreadsheet for importing it.

#### Uploading your spreadsheet

You will find the Excel import function in the menu bar on the top right of your screen:

![](../assets/screenshot-import.png)

This is what your upload screen looks like:

![](../assets/screenshot-upload-excel.png)

You can upload one or more .xlsx-files by dropping them in the drop zone or by selecting them. You have to upload the population with the green

_Upload_

button. At that time, all population from the .xlsx-file is added to the context and checked for inconsistencies. As a result, you may get errors when uploading. Only error-free spreadsheets will be uploaded successfully. As long as an error remains, the population in your context will not change.

#### Assignment

Make a population of your own for the Hawaii-script and put it in a .xlsx spreadsheet. As described above. Make sure to delete the population statements from your Hawaii source code, to make sure that you get to see the population from your .xlsx-file. Generate a prototype from your Hawaii-application, upload your population in Excel and play around with the results.

#### What have you learned?

After finishing your assignment, you have learned:

- to upload population to your Ampersand application in the form of a spreadsheet in .xlsx-format;
- to understand how a `POPULATION`-statement relates to the contents of a spreadsheet;
- that the contents of the spreadsheet is added to the population of your context, provided this does not lead to any conflict.

### Purpose

Patterns are meant to isolate discussions and make solutions reusable, as known from [design patterns](http://en.wikipedia.org/wiki/Design_pattern).

### Description

A pattern is a set of [rules](#the-rule-statement) that describes a theme or a general reusable solution to a commonly occurring problem.

For instance, if specific concerns about security arise, you might want to discuss this with stakeholders in security. With them you can discuss which rules in particular constitute your solution. Divide your problem in smaller pieces and discuss each piece with just the right stakeholders. This allows you to go deeper by talking to the right people. It saves time as well by freeing others from having to participate. An even larger benefit arises if you reuse patterns that have been discussed and scrutinized before. The best thing comes once your stakeholders agree. By that time, your pattern represents their agreement formally in Ampersand, so you can use it in the larger context of the information system.

### Example

```text
PATTERN Security

RELATION required[Subject*Destination]
MEANING "A subject that you must have passed to qualify for the school trip to a destination"

RELATION pass[Subject*Student]
MEANING "The subjects that have been passed by specific students"

RELATION attends[Student*Destination]

PURPOSE RULE guardPrerequisites
{+ This rule prevents students from registering for a trip
without having passed the required courses. +}
RULE guardPrerequisites : attends;required |- pass

ENDPATTERN
```

## The PURPOSE statement

### Semantics

Most things in your model are in it for a reason. To document these, you should use the PURPOSE statement.

### Syntax

`PURPOSE` `<type of thing>` `<name>` `<language>?` `<markup>?`

`{+` `<anything>` `+}`

Where `<type of thing>` and `<name>` are the type and name of the thing that is refered to. This could be one of: `CONCEPT`, `RELATION`, `RULE`, `IDENT`, `VIEW`, `PATTERN`, `INTERFACE`, `CONTEXT`

The optional and can be used to override the settings for language and markup. If omitted, these are inherited from the pattern of context where the PURPOSE statement is specified in.

### Examples

```text
PURPOSE CONCEPT Person {+The concept Person keeps all personal data together.+}
```

```text
PURPOSE RELATION accountOwner
{+ The system shall register all accounts to an owner,
   so accounts with the same owner are linked in this way.
+}
```

When defining the purpose of a relation, make sure that Ampersand can identify the relation unambiguously. If you have multiple relations `accountOwner`, add the signature to disambiguate it. For instance:

```text
PURPOSE RELATION accountOwner[Account*Owner]
{+ The system shall register all accounts to an owner,
   so accounts with the same owner are linked in this way.
+}
```

### Markup

For the purpose of documentation, you may state the language in which you write a purpose. You may also state in which markup language you use. Examples:

```text
PURPOSE CONCEPT Person IN ENGLISH {+ The concept PERSON keeps all personal data together, which we need to comply with the GDPR.  +}
```

If you specify the language, Ampersand can restrict the documentation for the language you choose. Currently, you can only choose `DUTCH` or `ENGLISH`. The default language is English.

```text
PURPOSE RELATION accountOwner LATEX
{+ The system {\em shall} register all accounts to an owner, so accounts with the same owner are linked in this way.
+}
```

By specifying a markup language, Ampersand interprets the text as specified. If you do not specify the markup language, your text is interpreted as REStructured Text \(`REST`\). The available markup languages are `LATEX`, `MARKDOWN`, `HTML`, and `REST`.

```text
PURPOSE RULE "Check Digit Character"
IN ENGLISH MARKDOWN
{+ This rule enforces the use of a check digit character
   as described in [ISO 7064](en.wikipedia.org/wiki/ISO/IEC_7064).
   This is applicatble to IBAN bank account numbers.
+}
```

## The RELATION statement

### Purpose

A _**relation statement**_ says that a relation exists. It introduces (defines, declares) the relation in the context that uses the relation statement.

A _**population statement**_ specifies which pairs (of atoms) are in a relation.

### Description

A relation is a set that contains pairs of atoms. Over time, pairs can be inserted into or deleted from a relation, for example by a user typing data into an Ampersand application. So the content of a relation is changing over time.

When discussing relations, an arbitrary relation is referred to as $$r$$, $$s$$, or $$t$$. To say that a pair $$(a,b)$$ belongs to a relation $$r$$, we write $$a\ r\ b$$ or alternatively $$(a,b)\in r$$.

### Examples

```
RELATION soldBy[Order*Person]
```

```
RELATION contract[Order*ContractID] [UNI,TOT]
PRAGMA "Order " " has contract " " as its legal basis."
MEANING
{+ Every Order has a unique ContractID which specifies the legal basis
   for that particular order.
+}
```

In this example:

- `contract` is the _**name**_ of the relation,
- `Order` is the _**source concept**_ of the relation,
- `ContractID` is the _**target concept**_ of this relation, and
- `UNI` and `TOT` are _**constraints**_ of this relation.

### Syntax and meaning

Each relation used in Ampersand has to be declared. This means that the developer tells the system that this particular relation exists. A relation declaration can have one of the following formats:

```
RELATION <lower case identifier>
         '[' <upper case identifier> '*' <upper case identifier> ']'
         <properties>? <pragma>? <meaning>?
```

In the _**declaration**_ `RELATION owner[Person*Building]`, `owner` is the _**name**_ and `[Person*Building]` is the _**type**_ of the relation. Relation names start with a lower case character, to avoid confusion with concept names. The _**signature**_ of this relation is `owner[Person*Building]`. The signature identifies the relation within its context. The left hand concept, `Person`, is called the _**source**_ of the relation and the right concept, `Building`, is called the _**target**_.

All three formats define a relation by its name, its source concept and its target concept. By convention, the name of a relation is a single word that starts with a lower case letter. The source and target concepts start with an upper case letter. This convention avoids confusion between concepts and relations.

A relation statement means that there exists a relation in the current context with the specified name, source concept and target concept.

A relation statement may occur anywhere inside a context, both inside and outside a pattern.

The optional `<properties>` and `<pragma>`-parts are discussed in the sequel. The `<meaning>`-part is discussed [here](#the-meaning-substatement\).

The name, source concept and target concept together identify a relation uniquely within its context. As a consequence, the name of a relation does not have to be unique. E.g. `name[Book*Name]` can be specified in the same context as `name[Person*Name]`. Because they have different source concepts, these are different relations.

### Properties

The `<properties>`-part is meant for writing multiplicity constraints in a comma separated list between square brackets '\[' and ']'. E.g. `[UNI,TOT]` . The following properties can be specified on any relation `r[A*B]`

| &   | property   | semantics                                                                                                                                                                      |
| --- | ---------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| UNI | univalent  | For any `a` in `A` there can be not more than one `b` in `B` in the population of `r`. This implies that every `a` occurs not more than once (is unique) in the source of `r`. |
| INJ | injective  | For any `b` in `B` there can be not more than one `a` in `A` in the population of `r`. So, every `b` occurs not more than once in the target of `r`.                           |
| SUR | surjective | For any `b` in `B` there must be at least one `a` in `A` in the population of `r`.                                                                                             |
| TOT | total      | For any `a` in `A` there must be at least one `b` in `B` in the population of `r`.                                                                                             |

There are additional relations that can be specified on endo relations. An endo relation is a relation where the source and target concepts are equal. `r[A*A]`.

| &    | property      | semantics                                                                 |
| ---- | ------------- | ------------------------------------------------------------------------- |
| SYM  | symmetric     | For each (`a`,`b`) in `r`, (`b`,`a`) is in `r`.                           |
| ASY  | antisymmetric | If (`a`,`b`) and (`b`,`a`) are both in `r`, then `a` = `b`                |
| TRN  | transitive    | If (`a`,`b`) and (`b`,`c`) are both in `r`, then (`a`,`c`) is in `r`.     |
| RFX  | reflexive     | For each `a` in `A`, the pair (`a`,`a`) is in the population of `r`       |
| IRF  | irreflexive   | For each `a` in `A`, the pair (`a`,`a`) is _not_ in the population of `r` |
| PROP | -             | shortcut for the combination of symmetric and antisymmetric.              |

Let's assume that we want to express that any person can live in one city only. So under this constraint "_Joe Smith lives in New York_" and "_Joe Smith lives in Denver_" cannot both be true at the same time.

In relation algebra, we say that the relation is univalent, which means that every atom in the source concept can only be paired with a single atom in the target concept. This is modeled as

```
RELATION lives[Person*City][UNI]
MEANING "A person can live in one city only."
```

### PRAGMA

A pragma is optional and is characterized by the reserved word `PRAGMA`. The `PRAGMA` is followed by two or three strings. It is used to construct sentences in natural language, using pairs from the actual population of a relation. A pragma specifies how we speak (in natural language) about any pair in the relation. Ampersand also uses pragmas to generate examples in the functional specification. Example of a pragma with three strings:

```
PRAGMA "Student " " flies the flag of " " in top."
```

To use this pragma on the pair `(John,Amsterdam)` results in the sentence `"Student John flies the flag of Amsterdam in top."`. The two atoms are fitted in between the three strings. A pragma with two strings is identical to a pragma in which the third string is empty.

(The `PRAGMA` keyword will become obsolete in a future version of Ampersand. It will be replaced by the `VIEW`-statement which offers more flexibility in composing sentences.)

Example:

```
RELATION accepted[Provider * Order] [INJ] PRAGMA "Provider " " has accepted order "
```

The `PRAGMA` tells us that it makes sense to utter the phrase `"Provider Mario's Pizza's has accepted order 12345."`

### MEANING

For a full discussion of meaning, we refer to [`this page`](#the-meaning-substatement\).

### Miscellaneous

-

## The RULE statement

### Purpose

The purpose of a rule is to constrain data. Refer to the chapter about rules in the tutorial for examples and a practice oriented explanation.

A rule statement defines something that should be true. It does not define the enforcement.

### Syntax of rules

A `<rule>` has the following syntax:

```text
RULE <label>? <term> <meaning>* <message>* <violation>?
```

### Syntax of labels

A `<label>` is optional. It can be a single word or a string \(enclosed by double brackets\) followed by a colon \(`:`\).

#### Term

A term can be any of:

- Term BinaryOperator Term
- UnaryOpPre Term
- Term UnaryOpPost
- a \(reference to a\) relation \(including an optional signature, when required to disambiguate\):
  - A relation by name
  - `I` \(the Identity relation\)
  - `V` \(carthesian product\) Note that this can also be used to denote the empty relation, by using the unary negation operator: '-v'
  - A singleton term \(the value of an atom\)
- a term enclosed in brackets.

##### Operators

The following operators are available to build expressions:

- Binary operators
  - equivalence: `=`
  - composition: `;`
  - inclusion: `|-`
  - intersection: `/\`
  - union: `\/`
  - difference: `-`
  - left residual: `/`
  - right residual: `\`
  - diamond: `<>`
  - relative addition: `!`
  - cartesian product: `#`
- Unary operator \(pre-operator\)
  - complement: `-`
- Unary operators \(post-operator\)
  - conversion \(flip\): `~`
  - Reflexive, transitive closure: `*` \(Kleene star\) --currently not implemented
  - transitive closure: `+` \(Kleene plus\) --currently not implemented

#### MEANING\*

The meaning of a rule can be written in natural language in the Meaning part of the RULE statement.  
It is a good habit to specify the meaning! The meaning will be printed in the functional specification.  
The meaning is optional.

##### Syntax

```text
MEANING Language? Markup? <text>
```

The `<text>` part is where the the meaning is written down. We support both:

- a simple string, enclosed by double quotes
- any text, starting with `{+` and ending with `-}`

The optional language is specified as

- `IN ENGLISH` or
- `IN DUTCH`.

The optional Markup is one of :

- `REST` \(Restructured text\)
- `HTML`
- `LATEX`
- `MARKDOWN`

If you need specific markup, there are several options to do so. The default markup is used, but you can override that here. We rely on [Pandoc](http://pandoc.org/) to read the markup.

#### MESSAGE\*

Messages may be defined to give feedback whenever the rule is violated. The message is a predefined string. Every message for a rule should be for another Language.

```text
MESSAGE Markup
```

#### VIOLATION?

A violation message can be constructed so that it gives specific information about the violating atoms:

```text
VIOLATION (Segment1,Segment2,... )
```

Every segment must be of one of the following forms:

- `TXT` String
- `SRC` Term
- `TGT` Term

A rule is violated by a pair of atoms \(source, target\). The source atom is the root of the violation message. In the message the target atoms are printed. With the Identity relation the root atom itself can be printed. You can use a term to print other atoms. Below two examples reporting a violation of the rule that each project must have a project leader. The first prints the project's ID, the second the project's name using the relation projectName:

`VIOLATION ( TXT "Project ", SRC I, TXT " does not have a projectleader")`

`VIOLATION ( TXT "Project ", SRC projectName, TXT " does not have a projectleader")`

### ROLE MAINTAINS

By default rules are invariant rules.  
By preceding the rule statement with a role specification for this rule, the rule becomes a process rule.

## Language support

### Purpose

To generate documentation, Ampersand is language aware.

### Description

Ampersand assigns a language to every text written as documentation, whether it is a `MEANING`, `PURPOSE` or other text except comment.

Ampersand does not recognize any language, so you must tell which language is meant. To tell Ampersand what language you use, you can append a language directive to a context, a meaning, and to a purpose statement. Currently English and Dutch are supported.

### Syntax

A language directive has the following syntax

```text
IN <language>
```

Where `<language>` can be `ENGLISH` or `DUTCH`.

### Semantics by example

The first example is a context declaration in which the language `ENGLISH` is specified.

```text
CONTEXT Foo IN ENGLISH
...
ENDCONTEXT
```

This means that all natural language elements within this context are written in `ENGLISH`, unless specified otherwise.

The second example is a `MEANING`, which can be used in a `RULE` statement and in a `RELATION` statement. This example uses a `MEANING` in `ENGLISH`:

```text
RELATION ptpic[Pattern*Image] [UNI]
MEANING IN ENGLISH "Relation ptpic relates a pattern to the image of its conceptual diagram."
```

The language directive `IN ENGLISH` means that the meaning of the relation `ptpic[Pattern*Image]` is written in `ENGLISH`.

The third example is a `PURPOSE` statement in which the language `DUTCH` is specified.

```text
PURPOSE CONCEPT Person IN DUTCH
{+ Een persoon is een natuurlijke persoon of een rechtspersoon +}
```

This means that the contents of this purpose statement is written in `DUTCH`.

### Additional information

Ampersand assumes that whatever is written is written in the language denoted in the language directive. It doesn't check whether that language is actually used, because it cannot recognize languages.

If a `CONTEXT` has no language directive, `IN ENGLISH` is used by default. If a `CONTEXT` has a language directive, that language will be the default language of all natural language items within that context.

If a `PURPOSE` statement or a `MEANING` has no language directive, Ampersand assumes this to be the language of its context. So, the user needs to specify a language only if it is an exception to the default.

Documentation generated by the Ampersand-compiler is written in a single language, which is specified when the compiler is called.

Documentation generated by RAP4 is written in `DUTCH`. Natural language items written in any other language are ignored. This is [not a mistake](https://github.com/AmpersandTarski/Ampersand/issues/702), but a feature. RAP4 only "speaks Dutch" and ignores anything else.
