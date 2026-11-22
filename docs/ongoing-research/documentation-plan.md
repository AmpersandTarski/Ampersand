---
sidebar_label: "Documentation plan"
description: "Unpublished plan: what complete documentation of an Ampersand context contains,
for which reader, and from which element of the model each part comes (issue #1760)."
---

# A plan for the documentation of an Ampersand context

> **Status: plan under review (stage 1 of issue #1760).**
> The owner of the project reviews this plan before the generator is built.
> Code references are to `origin/main` at 5901e4df1 (22 November 2026).

## Summary and decisions for the owner

Everything a reader needs to know about an Ampersand context that is fixed at compile time is in the hands of the compiler.
This plan states what complete documentation of a context contains,
so that the generator has a definition of done.
It distinguishes four readers,
names per element of the model what the modeller writes and what the compiler derives,
and lists fifteen requirements, DP-1 to DP-15, for the generator and the diagnosis.
A sixteenth, DP-16, gives each of them a test in `testing/`.

Two choices in this plan belong to the owner.

The first choice is the default markup of a text such as a `PURPOSE`.
The default is reStructuredText.
A modeller who indents a continuation line,
as in relation `r` of the reproduction in [Markup](#markup),
gets a block quote with a definition list in it, without a warning.
Option A keeps the default and lets the diagnosis warn
when a text written in the default markup contains a block quote or a definition list.
Option B makes Markdown the default;
a context or a text that relies on reStructuredText then needs the word `REST`.
Option A changes no existing script.
Option B matches what modellers write elsewhere (the documentation site, GitHub, the CommonMark of OpenAPI),
but it changes the rendering of existing texts without saying so.
A count on 22 November 2026 gives the size of that change.
Of the 370 free texts (`{+ … +}`) in the 460 scripts under `testing/`, 251 use the default markup.
Of those, 13 have an indented continuation line,
and 115 contain a character that the two markups read differently
(`*`, `_`, a backquote, `::`, or a line that starts with `-` or `#`).
The count is an upper bound, because a character such as `_` in a name renders the same in both.
All 250 texts of the Artefactenkaart are in contexts that declare `MARKDOWN`, so option B changes nothing there.
This plan recommends A for the generator of stage 2,
and leaves B to a release that announces a breaking change.

The second choice is what describes a field of an interface.
A field has no `PURPOSE` of its own, because it is part of its interface.
This plan proposes that a field takes the `MEANING` and the `PURPOSE` of a relation
when its term, after normalisation, is that relation, its flip, or either of them composed with an identity.
These are the terms for which the OpenAPI description already emits `x-ampersand-relation` (`getExpressionRelation`).
Every other field, such as one with the term `r;s` or `I`, gets its term and its CRUD rights, and no description.
The diagnosis lists those fields as information,
and does not count them as missing documentation, because the modeller has no place to write their text ([DP-7](#requirements)).
Option A is this proposal.
Option B gives a field a text of its own.
That changes the language, and the rule of the syntax reference that a part of a thing needs no purpose.
A third way, in which the compiler composes a description from the meanings in a composed term,
is the composition of intent that requirement 2 of the issue rules out.
This plan recommends A.

## Why a plan

The current generator grew piece by piece.
`ampersand documentation` produces a functional design with five chapters (`src/Ampersand/Output/ToPandoc/`),
and the diagnosis also comes as a workbook (`src/Ampersand/Output/Diagnosis2Xlsx.hs`, issue #1625).
Each part was built when someone needed it,
and no yardstick says when the documentation of a context is complete.
Issue #1760 measured the result on the Artefactenkaart,
a context of 70 concepts, 281 relations, 71 rules and 137 interfaces.
Interfaces, `ENFORCE` statements and roles are absent from its documentation,
and the OpenAPI description and the generics contain almost none of the text the modeller wrote.
A plan with sources gives the generator a list of what it must contain that a reviewer can check,
and gives the diagnosis a list of what it must measure.

## What published guidance says

This section names, per source, what the plan takes from it.
Quotations are from the pages as read on 22 November 2026.

Diátaxis (Daniele Procida, [diataxis.fr](https://diataxis.fr/start-here/))
distinguishes four kinds of documentation: tutorials, how-to guides, reference and explanation.
It defines reference as "technical descriptions of the machinery and how to operate it",
says that "the structure of the documentation should mirror the structure of the product",
and that reference "should be austere" and "neutral".
It also says that "some reference material (such as API documentation) can be generated automatically by the software it describes",
and warns that "too many software developers think that auto-generated reference material is all the documentation required."
The plan takes three things from Diátaxis.
What the compiler generates is reference, and its structure mirrors the model (DP-5).
A `PURPOSE` is explanation: it "provides context and background" and answers why an element exists,
so the generator places it next to the reference entry of that element.
Tutorials and how-to guides are not generated;
[What stays hand-written](#what-stays-hand-written) says where they live.

Write the Docs ([documentation principles](https://www.writethedocs.org/guide/writing/docs-principles/))
lists, among others, the principles Current, Nearby, Unique, Complete and Discoverable.
Under Current it says: "Consider incorrect documentation to be worse than missing documentation."
Under Complete: "Within each publication, cover concepts in full, or not at all",
with the image that "a map that displays fifty out of one hundred fire hydrants in a neighborhood is worse than a map that displays none."
The plan takes from this that every output that documents a kind of element documents every element of that kind (DP-1 to DP-4),
and that the diagnosis names what is missing instead of the output hiding it (DP-13).
Generation from the model makes the documentation current by construction.

The Google documentation guide ([best practices](https://google.github.io/styleguide/docguide/best_practices))
says "Change your documentation in the same CL as the code change",
"A small set of fresh and accurate docs is better than a large assembly", and "Dead docs are bad."
The model is the code here: a `PURPOSE` changes in the same commit as the relation it explains.
The plan takes from this that the compiler composes no text that could go stale independently of the model (DP-11).

The chapter on documentation in *Software Engineering at Google* ([chapter 10](https://abseil.io/resources/swe-book/html/ch10.html))
says: "Always try to identify a primary audience and write to that audience".
It distinguishes audiences by experience level, domain knowledge and purpose.
It says "it is important to know the different types, and to not mix types",
and that documentation "should, as much as possible, be treated as code."
The page was read in part, so the plan relies only on these sentences.
The plan takes from this that each output has one primary reader ([The readers](#the-readers)).

arc42 ([docs.arc42.org](https://docs.arc42.org/home/)) is a template of twelve sections for architecture documentation;
section 5 is the building block view, section 8 the crosscutting concepts, and section 12 the glossary.
An Ampersand context is a specification, not an architecture,
so the plan does not adopt the template.
It takes the observation that a glossary of terms is a section of its own:
the concepts with their definitions are that glossary.

ISO/IEC/IEEE 26514:2022, *Design and development of information for users*,
sets requirements for establishing what information users need, how it is presented,
and how it is prepared and made available ([iso.org](https://www.iso.org/standard/77451.html)).
The plan was written from the abstract of the standard, not from its text.
It takes the order of work the abstract describes: first the reader and the need, then the form.

The OpenAPI documentation
([Providing documentation and examples](https://learn.openapis.org/specification/docs))
says that "almost every object in the OpenAPI Specification accepts a `description` field".
It adds that "`summary` is used for list view, and should be kept short",
and that "`description` fields allow rich text formatting by using CommonMark 0.27."
The plan takes from this that the OpenAPI description carries the modeller's text in `description` fields,
rendered as CommonMark, and that a `summary` stays a short derived sentence (DP-8).

## The readers

Each output of the compiler has one primary reader.
The table names the question that reader brings,
the kind of documentation in the sense of Diátaxis, and the output that answers it.

| Reader | The question the reader brings | Kind | Output |
| --- | --- | --- | --- |
| Domain expert, who checks the rules | Does this model say what we mean? What does each term mean, and which agreements does the system enforce? | Reference with explanation | Functional design: introduction, shared language, conceptual analysis |
| Developer, who maintains the model | How is the model built, what keeps which relation up to date, who maintains which rule, and what is undocumented? | Reference | Functional design: data analysis, enforcements, roles, interfaces; the diagnosis |
| Client of the API | Which resources exist, what does each field mean, and what may I create, read, update or delete? | Reference | `openapi.json` |
| End user of a prototype | What is this screen for, and what does this field mean? | Reference, shown in place | The generics (`concepts.json`, `relations.json`, `interfaces.json`, `roles.json`, `rules.json`), shown by the framework |

The compiler's responsibility for the end user ends at the generics.
How the prototype shows a meaning or a purpose is a matter for the PrototypeFramework (issue #1754).

## Principles

The model is the single source.
Documentation that follows from the model is generated;
documentation that does not stays hand-written.

The modeller writes intent, and the compiler derives structure.
Intent is the text of a `PURPOSE`, a `MEANING`, a concept definition and a `MESSAGE`.
Structure is everything that follows from the script: signatures, properties, representations,
classifications, which interface shows which relation, which role maintains which rule,
CRUD rights, how an `ENFORCE` statement is stored.

The compiler composes no intent the modeller did not write.
A derived sentence states a fact about the script, such as "This relation is univalent",
and never a reason.

Each text has one place in the FSpec, and every output takes it from there, so the document,
the OpenAPI description and the generics never disagree.

Each publication covers a kind of element in full or not at all,
and the diagnosis names what the script lacks.

## What the modeller writes and what the compiler derives

The rows of this table come from the abstract syntax:
the ten kinds of `PURPOSE` (`ExplObj` in `src/Ampersand/Core/AbstractSyntaxTree.hs`),
the three places where a modeller writes a `MEANING` (concept, relation, rule),
and the other statements that a reader meets.

| Element | The modeller writes | The compiler derives | Documented today |
| --- | --- | --- | --- |
| Context | `PURPOSE CONTEXT`, `META "title"`, `META "authors"` | Language, markup, the patterns and the counts | Introduction of the functional design |
| Pattern | `PURPOSE PATTERN` | Its concepts, relations, rules and enforcements | A section per pattern |
| Concept | `CONCEPT` definition, `MEANING`, `PURPOSE CONCEPT` | `REPRESENT` type, generalisations and specialisations (`CLASSIFY`), the interfaces in which it occurs | Functional design and diagnosis; not in OpenAPI or generics |
| Relation | `MEANING`, `PURPOSE RELATION`, `PRAGMA` | Signature, properties, the rules and interfaces that use it, its table | Functional design and diagnosis; not in OpenAPI or generics |
| Rule | `MEANING`, `PURPOSE RULE`, `MESSAGE`, `VIOLATION` | The formal term, invariant or process rule, the roles that maintain it | Functional design, diagnosis; meaning and message in `rules.json` |
| Identity (`IDENT`) | `PURPOSE IDENT` | The concept and the terms that identify it | Conceptual analysis, as a rule |
| View (`VIEW`) | `PURPOSE VIEW` | The concept and the segments | Absent |
| `ENFORCE` statement | `PURPOSE ENFORCE` (only for a named statement) | The relation, the operator, the term, stored relation or derived view, the rules the ExecEngine runs | Absent |
| Role | `PURPOSE ROLE` | The rules it maintains (`MAINTAINS`), the interfaces available to it, service or not | Absent; `roles.json` lists the maintained rules |
| Interface | `PURPOSE INTERFACE` | Roles, `API`, `TRANSACTIONAL`, root concept, the tree of fields | A table of concept against interface; OpenAPI without the purpose |
| Field of an interface | Its label | Its term, its relation if it has one, CRUD rights, view, sub-interface | OpenAPI and `interfaces.json` without a description |

A field of an interface has no `PURPOSE` of its own, because it is part of the interface;
the [syntax reference](../reference-material/syntax-of-ampersand.md#the-purpose-statement) explains why.
Its description therefore comes from the relation it shows, by the rule in DP-7.

Three observations in the code follow from this table.
The function `purposesOf` (`src/Ampersand/FSpec/Motivations.hs`) finds the purposes of an element of a kind
that has an instance of the class `Motivated`.
`ViewDef` and `AEnforce` have no such instance,
so no output can find a `PURPOSE VIEW` or a `PURPOSE ENFORCE`, although the parser accepts both.
`Interface` has a field `ifcPurpose :: Text` next to `PURPOSE INTERFACE`.
The parser fills that field with an empty text,
and for an interface that Ampersand generates, `src/Ampersand/FSpec/ToFSpec/ADL2FSpec.hs:512`
fills it with "Interface … has been generated by Ampersand."
No output reads the field.
`PURPOSE INTERFACE` is the one source (DP-10); the field goes,
and that a generated interface was generated is a derived fact (DP-11).
The generics are less empty than the issue reports:
`rules.json` already carries the meaning and the message of each rule,
and `interfaces.json` carries for each field its relation and whether it is flipped.

## Outputs

### The functional design

The functional design keeps its chapters and gains three sections for the developer:
one for the `ENFORCE` statements, one for the roles and one for the interfaces.
Its structure mirrors the model: a section per pattern,
and for the elements outside patterns a section per kind of element.
Every text the modeller wrote appears at the element it belongs to.

### The OpenAPI description

`info.description` carries the `PURPOSE CONTEXT`.
Each operation of an interface carries the `PURPOSE INTERFACE` as its `description`;
its `summary` stays the short derived sentence it is today.
The schema of a field carries the description that DP-7 gives it,
and the schema of a concept carries the definition of the concept.
Every text is rendered as CommonMark from the parsed text in the FSpec,
whatever markup the modeller used.

### The generics

`concepts.json` gets the definition and the purpose of each concept,
`relations.json` the meaning and the purpose of each relation,
`interfaces.json` the purpose of each interface and the description of each field,
`roles.json` the purpose of each role,
and `rules.json` the purpose of each rule next to the meaning and message it already has.
The texts are rendered the same way as in the OpenAPI description.

### The diagnosis

The diagnosis measures this plan.
It already counts, per pattern, the concepts with a definition and a purpose,
the relations with a purpose and a meaning,
and the rules with a purpose and a meaning (`src/Ampersand/Output/ToPandoc/ChapterDiagnosis.hs`).
It extends to every element in the table above, and lists per element what is missing.

## Requirements

Each requirement has a permanent identifier.
Identifiers are never renumbered and never reused.
Each requirement gets a test in `testing/` (stage 3).
A requirement on a text that the modeller can write also gets a measure in the diagnosis (DP-13).

| Id | Requirement |
| --- | --- |
| DP-1 | Every text the modeller wrote (each of the ten kinds of `PURPOSE`, each `MEANING`, each concept definition, each `MESSAGE`) appears in the functional design at the element it belongs to. |
| DP-2 | The functional design has a section on the `ENFORCE` statements: per statement its name or position, the statement as written, the relation it maintains, how it is stored, and its `PURPOSE`. |
| DP-3 | The functional design has a section on the roles: per role its `PURPOSE`, the rules it maintains and the interfaces available to it. |
| DP-4 | The functional design has a section on the interfaces: per interface its `PURPOSE`, roles, `API` and `TRANSACTIONAL` flags, root concept, and the tree of fields with label, term, relation, CRUD rights and the description of DP-7. |
| DP-5 | The structure of the functional design mirrors the model: a section per pattern, and for elements outside patterns a section per kind of element, with a heading in the output language. |
| DP-6 | Every cross-reference in the functional design resolves, in every output format. |
| DP-7 | A field whose term normalises to one relation, its flip, or either of them composed with an identity has the `MEANING` and `PURPOSE` of that relation as its description, with the direction stated when the relation is flipped. Any other field has no description; the diagnosis lists it as information and not as missing documentation. |
| DP-8 | `openapi.json` carries the `PURPOSE CONTEXT` in `info.description`, the `PURPOSE INTERFACE` in the description of each operation of that interface, the description of DP-7 in the schema of each field, and the concept definition in the schema of each concept, all as CommonMark. |
| DP-9 | The generics carry the texts of the section [The generics](#the-generics). |
| DP-10 | Each text has one place in the FSpec, and every output takes it from there; for the same element, the functional design, `openapi.json` and the generics show the same text. |
| DP-11 | The compiler composes no intent: every sentence it writes itself states a fact that follows from the script. |
| DP-12 | A text of more than one line renders as the modeller meant it, by the markup choice in [Markup](#markup), in both markups. |
| DP-13 | The diagnosis reports, per element of the table above, which text that the modeller can write is missing, and the `ENFORCE` statements that cannot have a purpose because they have no name. It lists the fields without a description (DP-7) separately, as information. |
| DP-14 | The diagnosis reports an element whose text exists only in a language other than the output language, because such a text is filtered out today without a warning. |
| DP-15 | A project can let its build fail on missing documentation in the sense of DP-13, without parsing a document; the information about fields never fails a build. The mechanism is chosen in stage 3. |
| DP-16 | Each requirement DP-1 to DP-15 has a test in `testing/` that fails when the requirement is not met. |

## How the plan covers the gaps of issue #1760

| Gap reported in the issue | Requirement |
| --- | --- |
| A multi-line `PURPOSE` becomes a block quote with a definition list | DP-12, with the owner's choice on the default |
| A context without patterns puts all its relations under one section "Other" | DP-5 |
| The section for relations outside patterns has the English heading "Relation" where the Dutch reads "Overige relaties" (`ChapterConceptualAnalysis.hs:236`, found while writing this plan) | DP-5 |
| 25 references `eq:ConceptualAnalysisrelation-…` stay undefined in the markdown output | DP-6 |
| `ENFORCE` statements and roles are absent, with their purposes | DP-2, DP-3 |
| Interfaces and APIs are absent except for a table of concept against interface | DP-4 |
| The OpenAPI description has no text from the model | DP-8 |
| The generics have no meaning or purpose | DP-9; `rules.json` already has meaning and message |
| Coverage is not measured for every element | DP-13, DP-14, DP-15 |

## Markup

A text in Ampersand is parsed into a Pandoc document
when the script is read (`Markup` in `src/Ampersand/Basics/PandocExtended.hs`),
so every output can render it from one parsed form.
The markup only decides how the modeller's characters are read.
The reproduction of the issue shows the difference:

```
CONTEXT Proef IN DUTCH
RELATION r[A*B]
MEANING "Een proefrelatie."
PURPOSE RELATION r
{+ Eerste regel van de uitleg,
   met een ingesprongen vervolgregel. +}
RELATION s[A*B]
MEANING "Nog een proefrelatie."
PURPOSE RELATION s
{+ Eerste regel van de uitleg,
met een vervolgregel zonder inspringing. +}
ENDCONTEXT
```

In reStructuredText, the default,
the purpose of `r` becomes a block quote with a definition list in it,
and the purpose of `s` a block quote followed by a paragraph.
With `MARKDOWN` on the context or on the purpose, both become one paragraph:
"Eerste regel van de uitleg, met een ingesprongen vervolgregel."
The owner chooses between options A and B of the [summary](#summary-and-decisions-for-the-owner);
the test of DP-12 contains this script in both markups.

## What stays hand-written

Tutorials, how-to guides and the explanation of Ampersand itself live on the documentation site,
which is assembled from the `docs/` folders of the Ampersand repositories.
They describe the language and the tools, not a particular context,
so no compiler can generate them.
A modeller links to them from a `PURPOSE`,
which is Markdown or reStructuredText and can contain a link.
The compiler adds no links of its own.

## References

- Daniele Procida, *Diátaxis*, [Start here](https://diataxis.fr/start-here/) and [Reference](https://diataxis.fr/reference/).
- Write the Docs, [*Documentation principles*](https://www.writethedocs.org/guide/writing/docs-principles/).
- Google, [*Documentation best practices*](https://google.github.io/styleguide/docguide/best_practices).
- Titus Winters, Tom Manshreck and Hyrum Wright (eds.), *Software Engineering at Google*,
  [chapter 10, *Documentation*](https://abseil.io/resources/swe-book/html/ch10.html).
- arc42, [*The arc42 template*](https://docs.arc42.org/home/).
- ISO/IEC/IEEE 26514:2022, [*Systems and software engineering — Design and development of information for users*](https://www.iso.org/standard/77451.html).
- OpenAPI Initiative, [*Providing documentation and examples*](https://learn.openapis.org/specification/docs).
