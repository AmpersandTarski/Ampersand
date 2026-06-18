---
title: Just curious?
---

# Why Ampersand?

Some ideas are worth slowing down for. Here is one: If you know the rules of your business, you have what you need to generate a working information system. It is a large claim with a clean idea underneath, and half the pleasure is seeing how far that idea reaches. If that is the kind of thing that draws you in, read on: this page paints a plain-language picture of what Ampersand is, what sets it apart, and what it is not — enough to decide, in a few minutes, whether it deserves more of your time.

Ampersand automates the development of software for information systems.
Ampersand is many things:
* It is a migration instrument that lets you clean up your data, get rid of spreadsheet administrations, and simplify your business accordingly.
* It is a way of formalizing domain models to standardize language and produce software at the same time.
* It is a scientific quest into generative software, making formal theories useful for practitioners.
* It is a development environment in which production information systems have been built.
* It is a prototyping environment, that lets you experiment with real users of a future application during its design.
* It is an [open source project](https://github.com/AmpersandTarski/), driven by enthousiasts without budget.
* It is a [programming/specification language](https://cs.ou.nl/oursi/OUrsi052-Joosten-programming-with-relation-algebras.pdf) in which you can specify your own information systems.

In the Ampersand project,
we believe in correct data, to help information intensive organizations to prevent mistakes and save them the effort of fixing those mistakes.
We believe in correct software, to prevent teething troubles of new information systems and save on debugging and rework.
We believe in generating software, to save time by eliminating laborious and error-prone tasks from software development projects.
We believe in incremental development, to create predictable results in a predictable time.


## Key capabilities
Ampersand can help you with information system development. It helps you to:
* Generate your information systems, to save work on programming.
* Develop incrementally, also when your data model changes, to support agile software development and to remain flexible while functional requirements *e changing.
* Make a domain analysis of your business problems to create a shared understanding. 
* Automate making your database schema, to kick-start your application(s).
* Experiment with rules to [_**simplify**_ business processes](https://player.ou.nl/wowzaportlets/#!production/P2w4YD0) until they represent your intention, to enhance compliance by design.
* Generate documentation of your information system, to make the result more maintainable.
* Ensure that every system invariant remains satisfied during its lifetime, to automate business policies. This comes without the burden of a formal proof.

## Benefits
Ampersand yields benefits in the process of developing your information systems. It lets you:
* Free your users from unneccessary constraints, which workflow models can sometimes impose.
* Be baffled by the precision with which you can formalize rules, even legal rules.
* Reduce your programming effort by using Ampersand's software generator. Bring complex changes, even when changing your data model, to production faster.
* Use Ampersand to specify _**your own**_ [_business rules_](http://www.businessrulesgroup.org/brmanifesto/BRManifesto.pdf) to design information systems that comply. It is a lot easier to prove compliance with rules that Ampersand keeps satisfied. This is compliance by design.
* Gain mathematical _**certainty of compliance**_. Ampersand uses _relation algebra_ to align the IT system to the business, by exploiting its natural language interpretation alongside its technical interpretation as working software. Your claim that business stakeholders understand (solely in natural language) what the computer does (in software) can't be made more convincingly.
* True _**low-code**_ platforms, such as Ampersand, give you full functionality with little code. Do the [tutorial](../tutorial-rap4) and experience a full non-trivial example of an information system specified in 61 lines of code only.
* _**Reduce risk**_ by developing in _small increments_. Add constraints, user interfaces, relations, and other design elements one at a time. Generate a prototype at any intermediate stage, to try out your system long before it is finished. 
* _**Reduce risk**_ by dividing the work into small subsystems. To isolate subsystems is easy, due to [_conceptual independence_](http://dl.acm.org/citation.cfm?id=2946158.2946405). Ampersand lets you combine subsystems into larger systems, automating the burden of combining them.  Reuse _design patterns_ to assemble systems, rather than re-invent from scratch.
* _**Deploy quickly**_ by building, configuring, and taking it to production automatically.

## Key features
The key features of Ampersand characterize it 
### Declarative
Ampersand is a [declarative](../conceptual/why-declarative.md) language, which means that you can add, update, or delete any rule in your code without affecting other rules.
This is enables incremental development.
It also means that rules are free of side effects.
That means that the result of evaluating a rule only depends on the context and not on the number of times you evaluate that rule.

This feature characterizes Ampersand as different from *procedural* languages.

### Reactive
Ampersand is a [reactive](../reactive-programming.md) language.
It does not follow pre-specified threads of action, but it reacts to events from outside and inside the system.

Reactive programming sets Ampersand apart from multi-threaded programming.

### Statically typed
All relations in a context together form a conceptual model.
The Ampersand compiler uses this model for type checking the rules you specify in an Ampersand script.
The word ''static'' refers to type checking by the compiler.
This prevents many runtime errors, presenting them at compile time as type errors.
Static typing is known to save modelers much effort and to enhance the quality of the generated code.
For a novice, static type checking can yield some frustration, which will quickly dissolve as your experience grows.

Static type checking is the opposite of dynamic type checking, which is typically done at runtime.

### Constraint programming
Each rule in Ampersand is a constraint on a dataset.
The information system generated by Ampersand keeps that constraint satisfied.
If compliance with a rule means that it is satisfied all the time, it is fair to say that an Ampersand application delivers compliance with all the rules in its script.
Constraint programming is typically used in optimization problems, planning and scheduling (See Rossi et.al., 2006).

Ampersand uses constraints instead of instructions to define an information system.
This is related to the business rules approach, although Ampersand uses constraints for more than decision making only.
It defines the workflow of a user as any sequence of events that keeps all constraints satisfied,
saving the user from having to specify workflows.
A difference with many other constraint-based languages is the use of relation algebra, where other constraint-based systems use first order predicate logic.

F. Rossi, P. van Beek, and T. Walsh, eds., Handbook of Constraint Programming, Elsevier Science, 2006.

### Incremental development
To change the database schema of an existing information system typically involves a lot of work and bears a high probability of introducing new errors.
So, database developers consider it good practise to design the schema carefully up front and avoid schema changes during the lifetime of the system.
This limits incremental development to changes that do not affect the database schema.
Ampersand stretches this boundary by means of its generator.
Mistakes that are caused by a changed database schema are caught by Ampersand's type checking mechanism, pre-empting runtime problems caused by a changing schema.
Bulk input (via spreadsheets) is independent of the database schema, which helps to migrate data.
Such features make incremental development easier, especially when the database schema changes.

### Formal
Ampersand is a formal language, based on Relation Algebra (also known as the Calculus of Relations).
It uses a formal language to ensure that business rules can be defined precisely and that a compiler can transform it to software.
Relation Algebra has been chosen because it is easy to write software to reason with terms in relation algebra.
(That is because relation algebra does not use variables and quantifiers and because its algebraic properties are well-established.)
Also, Relation Algebra is closely related to Relation*al* Algebra, which facilitates generating correct code in for example SQL.
For users of Ampersand, getting to work with relations, concepts and interfaces is easy.
However, for specifying your own rules you must learn to use the operators of relation algebra.

Scientific foundations of Ampersand have been published in the International Conference on Relation Algebraic Methods in Computer Science ([RAMiCS](http://www.ramics-conference.org/)) from 2011 onwards.
## Some examples of information systems built in Ampersand

* Medications, a demonstrator built by TNO in Ampersand to showcase attestation on the internet. This example is undocumented.
* [SETU standards](https://setu.t4smm.nl/), a site to disclose standards for electronic messaging in the sector of flexible labour. This example is undocumented.
* [RAP4](https://rap.cs.ou.nl), a tool for students to learn how to work with Ampersand. This project is documented in [https://github.com/AmpersandTarski/RAP](https://github.com/AmpersandTarski/RAP).

### Why rule-based?

Ampersand lets you encode your own [business rules](https://www.businessrulesgroup.org/brmanifesto/BRManifesto.pdf) as constraints on a data set.
You specify your business as constraints and Ampersand generates information systems that keep these constraints satisfied.

Ampersand adheres closely to the concept of business rule as defined by the Business Rules Group.
This [essay](../why-ampersand/business-rules-in-ampersand.md) explains the correspondence between the Business Rules Manifesto and Ampersand.


### How does it work?

Ampersand is a way of designing information systems for enterprises, supported by a method, a tool, and a course.
These are the things you do (click on the hyperlinks to watch video clips):

1. Communicate with the business solely [in their own _**language**_](https://player.ou.nl/wowzaportlets/#!production/xqW5z2v), which is a natural language.
2. Define a domain language (in Ampersand) to consolidate _**agreement of terms**_ among stakeholders, using it solely for technical purposes.
3. [_**Formalize**_ the agreements](https://player.ou.nl/wowzaportlets/#!production/BDAXK2L) into rules that are relevant for the information system using Ampersand-script. 
4. Generate a [working _**prototype**_](https://player.ou.nl/wowzaportlets/#!production/7qozHDH) of your information system for verification purposes.
5. [Use this prototype](https://player.ou.nl/wowzaportlets/#!production/7qozHDH) to _**walk through**_ user stories, test user acceptance, elicit requirements, or otherwise gain more assurance that your design is what your audience wants.
6. Use the documented ontology that Ampersand generates to _**validate**_ the agreed rules.

### Foundations
The Open University of the Netherlands has a [course in Rule Based Design](https://www.ou.nl/-/IM0403_Rule-Based-Design), in which students use Ampersand for this purpose.

### Licenses

Ampersand is freely available as an [open source project in GitHub](https://github.com/AmpersandTarski).

## How does Ampersand compare to other methods and tools?
## How is the Ampersand project being run?{#Governance}
The governance of Ampersand is founded on the following principles:
* Ampersand is a research project, driven by the academic virtues of curiosity, spontaneous collaboration and mutual assistance.
* The repositories are visible to the entire world, but contributions require membership of an Ampersand team, to prevent silly accidents.
* Maintainability is by far the most highly valued property of anything in the Ampersand project.
* There is no money involved.
* There are no deadlines.
* We continually draw on best practices from others to improve our productivity.

### Future directions
We lay out the [future directions](../future-plans.md) in terms of research questions.
Such topics are usually discussed on Ampersand days or physical meetings of the core team.
[Stef Joosten](https://github.com/stefjoosten) has a final say in this, to ensure progress.
The following topics are on our wish-list
* Migration in production, to facilitate changing the schema (i.e. the data model and rules) of an Ampersand application while it is running.
* Arithmetic, to enable users to do computations in Ampersand.
* Dynamic deployment of partial applications within a running system, to facilitate the maintenance of large information systems in a flexible way.

### Decision making
The core team, consisting of [Stef Joosten](https://github.com/stefjoosten), [Han Joosten](https://github.com/hanjoosten), and [Michiel Stornebrink](https://github.com/Michiel-s),
makes all decisions.
All [Ampersand repositories](https://github.com/AmpersandTarski) are managed by the core team.

New contributors and temporary contributors are individuals with a GitHub identity, so that we can make them a member of the project.

### Management
The [Ampersand repository](https://github.com/AmpersandTarski/Ampersand) and [Prototype repository](https://github.com/AmpersandTarski/Prototype)
both have a "main" branch, which contains all releases as tags.
Contributors make pull requests to merge their branch into the main branch.
This merge also requires a code review by one of the core team members.
Automatic testing (by GitHub actions) ensures a minimal amount of hygiene.
We merge only compilable code that passes the regression test.
## Questions & answers

New to Ampersand and trying to decide whether it's worth a closer look? Here are the questions newcomers ask most, with short answers to help you judge in a few minutes whether Ampersand fits you.

### What Ampersand is, and what it's for

**What is Ampersand, and what can it do for me?**
Ampersand turns your business rules into a working information system, so you spend your time describing what should be true rather than coding how to enforce it. It's a low-code, rule-based environment for modelling, prototyping and generating software. [Learn more →](./1-interested-visitor.md)

**How do I get started building an information system with Ampersand?**
You can start in your browser with the web-based RAP4 tool to write, compile and run scripts with nothing to install. The tutorial walks you end to end through a complete Enrollment example. [Learn more →](../tutorial-rap4.md)

**Where should I start reading, given my role or interest?**
The introduction points you straight to the right starting place, whether you're simply curious, a student, a professional, a scientist or a software engineer, so you skip the parts that don't apply to you. [Learn more →](../intro.md)

**Is there a video showing Ampersand in action?**
Yes. A recorded session shows you how to build an Atlas step by step, so you can see the tool at work before trying it yourself. [Learn more →](../videos.md)

**Can other software talk to a generated prototype?**
Besides the user interface and the database, the compiler writes `generics/openapi.json`: an OpenAPI 3.0 description of the prototype's REST API. Standard tools read that file to display the API, generate client code in various languages, or send requests to it, so a prototype can be reached programmatically as well as through its screens. [Learn more →](../the-tools-we-use/openapi-generation.md)

### What makes it different

**What sets Ampersand apart from other languages?**
You get correctness built in: Ampersand is declarative, reactive and statically typed, grounded in the formal mathematics of relation algebra, and it uses constraint programming so you can grow your system incrementally. [Learn more →](./1-interested-visitor.md)

**Why is Ampersand declarative rather than imperative?**
Because you describe an information system by stating its constraints, in any order you like, much as you would in YAML, SQL or Haskell, instead of scripting every step. That lets you focus on what must hold true and leave the sequencing to the tool. [Learn more →](../conceptual/why-declarative.md)

**What's the difference between a declarative and a procedural specification?**
A declarative spec defines something by the constraints it must satisfy; a procedural one prescribes the ordered steps to produce it. Stating the "what" keeps your specification shorter, clearer and easier to reason about. [Learn more →](../conceptual/why-declarative.md)

**What does it mean that Ampersand is "reactive"?**
It treats your system as data that changes over time plus rules that must always stay satisfied, and it reacts automatically whenever an event would break one of those rules. The result is data you can trust to stay consistent. [Learn more →](../reactive-programming.md)

**How does Ampersand relate to the Business Rules Manifesto?**
If you care about business rules as first-class citizens, you'll recognise Ampersand's philosophy: it walks through each article of the manifesto and shows how it realises it, treating rules as declarative constraints kept separate from process. [Learn more →](../why-ampersand/business-rules-in-ampersand.md)

### Is this for me? — expectations, ownership and licensing

**Is Ampersand a silver bullet that solves all my organisation's problems out of the box?**
No, and it's honest about that: Ampersand offers correct data and fast development to those willing to invest the time to learn it, not an effortless fix. If you're ready to put in that effort, the payoff is real. [Learn more →](../intro.md)

**Who owns Ampersand, and what can it cost me to use it?**
No one owns it, and it's free: the repositories are licensed under GPLv3 while authors keep their own copyright, so you can use it without paying or asking permission. [Learn more →](../ownership/README.md)

**Under which licence is the documentation published?**
The documentation is released under a Creative Commons Attribution-ShareAlike 4.0 International licence unless stated otherwise, so you're free to reuse and adapt it as long as you give credit and share alike. [Learn more →](../intro.md)

**How is the project run, and who makes the decisions?**
Ampersand is a volunteer-driven, non-commercial research project, with a small core team (Stef, Han and Michiel Joosten/Stornebrink) making the decisions and valuing maintainability above all. That tells you what to expect of its pace and priorities. [Learn more →](./1-interested-visitor.md)

### Track record, and how it's built

**What practical results has the Ampersand research produced?**
It has delivered real-world impact: the RAP tool was built with it, TNO put ten systems into production, and hundreds of students have completed the Rule Based Design course. [Learn more →](../research.md)

**What principles guide the team building Ampersand?**
The team produces free open-source software, automates its own production, writes for maintainability, and diagnoses issues openly on GitHub before fixing them, so you can see how the project is cared for. [Learn more →](../the-tools-we-use/README.md)

**Which tools does the Ampersand project rely on?**
Under the hood it draws on Haskell, Stack, Docker, Git, Graphviz, MariaDB, Node.js, Pandoc, VS Code and more, each with a specific role, giving you a clear picture of the toolchain you'd be working with. [Learn more →](../the-tools-we-use/tools-used-in-the-ampersand-project.md)


