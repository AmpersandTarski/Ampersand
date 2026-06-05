---
title: For scientists
---

# Ampersand as a research programme

Behind Ampersand sits a claim worth examining: that a relation-algebraic theory of information systems can be precise enough for a compiler to generate the very software it describes — with defined semantics, a compiler that embodies them, and two decades of peer-reviewed work to reason from. If you look for ideas solid enough to stand on and extend, read on: below, you will find the theoretical foundation, the publications and the concrete results, all out in the open — enough to judge for yourself whether this is ground worth building on.

Ampersand is a research programme into the automatic generation of information systems from formal specifications. It originated at the Open University of the Netherlands, with research contributions over the years from partners including TNO and Ordina. The quest is to develop better information systems, faster, by keeping specifications as close to *the business* as possible and generating software that is actually deployed in production.

## The theoretical foundation

Ampersand rests on relation algebra: business rules are formal constraints whose violations can be computed and signalled in real time.

- [A theory of information systems](../conceptual/theory.md) — the model of information systems that Ampersand generates from.
- [Design considerations](../reference-material/design-considerations.md) — the principles behind the language.
- [Why declarative?](../conceptual/why-declarative.md) — why Ampersand specifies *what* must hold rather than *how*.
- [Truth](../reference-material/truth.md) — how Ampersand treats data as facts and preserves their truth.
- [The Business Rules Manifesto and Ampersand](../why-ampersand/business-rules-in-ampersand.md) — each article of the 2003 manifesto, and how Ampersand realises it.

## Publications and results

- [Which research papers have been produced?](../research#Publications) — the full list of publications by the Ampersand team, from 2000 to today.
- [Which research results have been produced?](../research#Results) — RAP, graduations, the *Rule Based Design* course and book, and systems built in production.

## Background and direction

- [What is the purpose of Ampersand?](./1-interested-visitor.md#whyAmpersand)
- [How is the Ampersand project being run?](./1-interested-visitor.md#Governance)
- [What are the plans for the future?](../future-plans.md)

Want to discuss research, or contribute to it? [Open an issue on GitHub](https://github.com/AmpersandTarski/Ampersand/issues), or read more in the [research overview](../research).

## Questions & answers

Ampersand is a research programme as much as a tool: a relation-algebraic theory of information systems with a working compiler that turns formal constraints into running software. If you are weighing whether to build on, extend, or cite this work, the answers below point to the foundations, the semantics, the published results, and the open directions.

### The theoretical foundation

**What is the theoretical foundation underlying Ampersand?**
You get a clean relation-algebraic basis to reason about: business rules are formal constraints whose violations can be computed and signalled in real time. This makes the gap between specification and implementation a calculable one. [Learn more →](https://ampersandtarski.github.io/ampersand/landingpage/scientist)

**How does the theory define a dataset, and why is it based on triples?**
The triple-based formulation gives you a data model independent of any database technology: a dataset is a set of well-typed triples plus an instance relation. That independence is what lets the theory carry across implementations. [Learn more →](https://ampersandtarski.github.io/ampersand/conceptual/theory)

**What is the formal definition of an information system in Ampersand's theory?**
The definition is precise enough to build on: an information system is a tuple of a dataset, a schema, a set of roles, and a maintenance relation linking each role to the rules it keeps satisfied. [Learn more →](https://ampersandtarski.github.io/ampersand/conceptual/theory)

**How do Ampersand's notions of rules, data, and terms map onto software engineering and model theory?**
A correspondence table situates the theory within fields you already know: rule–script–program–theory, data–population–state–model, and term–condition–term line up across the disciplines, so you can relate Ampersand's claims to established results. [Learn more →](https://ampersandtarski.github.io/ampersand/conceptual/theory)

**What design principles underlie the Ampersand language?**
The rationale is explicit and worth scrutinising: Ampersand favours constraints over obligations, reliable relation-algebraic semantics, automated design, and working systems over comprehensive documentation, all in service of incremental development. [Learn more →](https://ampersandtarski.github.io/ampersand/reference-material/design-considerations)

### Semantics & the language

**What is a term in Ampersand, and what is its meaning?**
Terms are the object of study: a term combines relations with operators and denotes a set of pairs, i.e. a newly created relation. Everything else in the language is built from this denotational core. [Learn more →](https://ampersandtarski.github.io/ampersand/reference-material/terms)

**What does composition (r;s) mean in relation algebra here?**
The semantics is the standard one, stated precisely: r;s relates a to c whenever some b satisfies a r b and b s c, yielding a relation of type [A*C]. [Learn more →](https://ampersandtarski.github.io/ampersand/reference-material/terms)

**What does it mean for a statement to be true in Ampersand?**
Truth has an exact, context-relative meaning you can reason with: a pair in a relation is a fact, i.e. a true statement, and truth holds only within a given context. [Learn more →](https://ampersandtarski.github.io/ampersand/reference-material/truth)

**What key features set Ampersand apart from other languages?**
For comparison against your own work, the distinguishing combination is: declarative, reactive, statically typed, formal (relation algebra), and constraint-programming-driven for incremental development. [Learn more →](https://ampersandtarski.github.io/ampersand/landingpage/interested-visitor)

### Research, publications & results

**Where can I find the research papers and results produced by the Ampersand team?**
A single research overview lists every publication from 2000 onward, alongside results such as RAP, graduation projects, the Rule-Based Design course and book, and production systems, so you can locate citable work quickly. [Learn more →](https://ampersandtarski.github.io/ampersand/landingpage/scientist)

**Where are the academic publications about Ampersand and relation algebra?**
The Research page collects the papers with links, including the 2018 JLAMP paper and several RAMiCS proceedings, giving you peer-reviewed anchors for the theory. [Learn more →](https://ampersandtarski.github.io/ampersand/research)

**What practical results has the Ampersand research project produced?**
The evidence of real-world impact is concrete: RAP was built as a tool, TNO put ten systems into production, and hundreds of students completed the Rule Based Design course. [Learn more →](https://ampersandtarski.github.io/ampersand/research)

**Is there a book or course on rule-based design with Ampersand?**
Yes, and it is openly available to teach or reuse: the Open Universiteit publishes the open book "Rule Based Design", used in its course of the same name. [Learn more →](https://ampersandtarski.github.io/ampersand/research)

### Extending Ampersand & where it's heading

**What problem does automating ExecEngine rules solve in the compiler?**
This is a research target you can engage with directly: the aim is to derive the violation-repair code of an automated rule automatically, rather than writing ExecEngine code by hand. [Learn more →](https://ampersandtarski.github.io/ampersand/conceptual/automated-rules)

**How could Kleene star and plus operators be added to Ampersand?**
A worked extension path exists if you want to push the algebra further: precompile each Kleene term into a new relation plus ENFORCE rules that maintain the transitive closure. [Learn more →](https://ampersandtarski.github.io/ampersand/future-plans)

**What makes legal modeling with Ampersand time consuming?**
An honest open problem for interdisciplinary research: legal jargon and the scarcity of modelers fluent in both law and technology make the trial-and-error process slow to converge. [Learn more →](https://ampersandtarski.github.io/ampersand/modeling/legal-modeling)

**How does an Ampersand application handle security concerns such as access control, encryption, and injection?**
The documentation is candid about scope, which matters if you assess deployments: access control comes via SIAM/ROLE or the platform, there is no built-in encryption or injection protection, and remaining OWASP-style risks are discussed per topic. [Learn more →](https://ampersandtarski.github.io/ampersand/reusing-available-modules)

**Where should I start reading the documentation as a researcher?**
The introduction routes you by role, so as a scientist you are pointed straight to the theory, publications, and results rather than the user-facing material. [Learn more →](https://ampersandtarski.github.io/ampersand/intro)

