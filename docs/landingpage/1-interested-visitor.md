---
title: Just curious?
---

# What is Ampersand all about?
Thank you for being curious. 

Ampersand automates the development of software for information systems.
Ampersand is many things:
* It is a [programming language](https://cs.ou.nl/oursi/OUrsi052-Joosten-programming-with-relation-algebras.pdf) in which you can define your * information systems.
* It is a way of formalizing domain models to standardize language and produce software at the same time.
* It is a scientific quest into generative software, making formal theories useful for practitioners.
* It is a [development environment](https://rap.cs.ou.nl) in which you can make your prototypes.
* It is an [open source project](https://github.com/AmpersandTarski/), driven by enthousiasts without budget.

In the Ampersand project,
we believe in correct data, to help information intensive organizations to prevent mistakes and save them the effort of fixing those mistakes.
We believe in correct software, to prevent teething troubles of new information systems and save on debugging and rework.
We believe in generating software, to save time by eliminating laborious and error-prone tasks from software development projects.
We believe in incremental development, to create predictable results in a predictable time.


## Why Ampersand?

## What are the key capabilities of Ampersand?
1. Generate your information systems incrementally, to save work on programming.
A generator is very practical when your data model changes, which lets you stay flexible when functional requirements change.
2. Make a domain analysis of your business problems to create a shared understanding. Then use the resulting data model to kick-start your application(s).
5. Experiment with rules to [_**simplify**_ business processes](https://player.ou.nl/wowzaportlets/#!production/P2w4YD0) until they represent your intention, to enhance compliance by design.

### Benefits

2. Free your users from unneccessary constraints, which workflow models can sometimes impose.
3. Be baffled by the precision with which you can formalize legal rules.
6. Reduce your programming effort by using Ampersand's software generator. Bring complex changes, even when changing your data model, to production faster.
7. Use Ampersand to specify _**your own**_ [_business rules_](http://www.businessrulesgroup.org/brmanifesto/BRManifesto.pdf) to design information systems that comply. It is a lot easier to prove compliance with rules that Ampersand keeps satisfied. This is compliance by design.
9. Gain mathematical _**certainty of compliance**_. Ampersand uses _relation algebra_ to align the IT system to the business, by exploiting its natural language interpretation alongside its technical interpretation as working software. Your claim that business stakeholders understand (solely in natural language) what the computer does (in software) can't be made more convincingly.
10. True _**low-code**_ platforms, such as Ampersand, give you full functionality with little code. Do the [tutorial](../tutorial-rap4) and experience a full non-trivial example of an information system specified in 61 lines of code only.
11. _**Reduce risk**_ by developing in _small increments_. Add constraints, user interfaces, relations, and other design elements one at a time. Generate a prototype at any intermediate stage, to try out your system long before it is finished. 
12. _**Reduce risk**_ by dividing the work into small subsystems. To isolate subsystems is easy, due to [_conceptual independence_](http://dl.acm.org/citation.cfm?id=2946158.2946405). Ampersand lets you combine subsystems into larger systems, automating the burden of combining them.  Reuse _design patterns_ to assemble systems, rather than re-invent from scratch.
13. _**Deploy quickly**_ by building, configuring, and taking it to production automatically.

### Some examples of information systems built in Ampersand

* Medications, a demonstrator built by TNO in Ampersand to showcase attestation on the internet. This example is undocumented.
* [SETU standards](https://setu.t4smm.nl/), a site to disclose standards for electronic messaging in the sector of flexible labour. This example is undocumented.
* [RAP4](https://rap.cs.ou.nl), a tool for students to learn how to work with Ampersand. This project is documented in [https://github.com/AmpersandTarski/RAP](https://github.com/AmpersandTarski/RAP).

### Why rule-based?

Ampersand lets you encode your own [business rules]((http://www.businessrulesgroup.org/brmanifesto/BRManifesto.pdf)) as constraints on a data set.
You specify your business as constraints and Ampersand generates information systems that keep these constraints satisfied.

Ampersand adheres closely to the concept of business rule as defined by the Business Rules Group. This [essay](../why-ampersand/business-rules-in-ampersand.md) explains the correspondence between the Business Rules Manifesto and Ampersand.


### How does it work?

Ampersand is a way of designing information systems for enterprises, supported by a method, a tool, and a course. These are the things you do (click on the hyperlinks to watch video clips):

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

## What are the key features of Ampersand?
### Declarative
Ampersand is a [declarative](../conceptual/why-declarative.md) language, which means that you can add, update, or delete any rule in your code without affecting other rules. This is enables incremental development. It also means that rules are free of side effects. That means that the result of evaluating a rule only depends on the context and not on the number of times you evaluate that rule.

### Reactive
Ampersand is a [reactive] language.
It does not follow a pre-specified procedure, but it reacts to events from outside and inside the system.
If you want to enforce a procedure, you must specify the right constraints.

### Statically typed
All relations in a context together form a conceptual model.
The Ampersand compiler uses this model for type checking the rules you specify in an Ampersand script.
The word ``static'' refers to type checking by the compiler (as opposed to dynamic typing, which is done at run time).
This prevents many run time errors, presenting them at compile time as type errors.
Static typing is known to save modelers much effort and to enhance the quality of the generated code.
For a novice, static type checking can yield some frustration, which will quickly dissolve as your experience grows.

### Constraints
Each rule in Ampersand is a constraint on a dataset.
The information system generated by Ampersand keeps that constraint satisfied.
If compliance with a rule means that it is satisfied all the time, it is fair to say that an Ampersand application delivers compliance with all the rules in its script.

This differs from decision rules and computation rules, which can be ``called'' to get a result.
Decision rules are a specific form of constraint, so Ampersand can deal with that naturally.
Computation rules have not been implemented yet.

### Formal
Ampersand is a formal language, based on Relation Algebra (also known as the Calculus of Relations).
It uses a formal language to ensure that business rules can be defined precisely and that a compiler can transform it to software.
Relation Algebra has been chosen because it is easy to write software to reason with terms in relation algebra.
(That is because relation algebra does not use variables and quantifiers and because its algebraic properties are well-established.)
Also, Relation Algebra is closely related to Relational Algebra, so generating correct code is easy.
For users of Ampersand, getting to work with relations, concepts and interfaces is easy.
However, for specifying your own rules you must learn to use the operators of relation algebra.

Scientific foundations of Ampersand have been published in the International Conference on Relation Algebraic Methods in Computer Science ([RAMiCS](http://www.ramics-conference.org/)) from 2011 onwards.

## Who is behind Ampersand?
## How does Ampersand compare to other methods and tools?
## What is the history of Ampersand?
## How is the Ampersand project being run?
## What are the plans for the future?
## What do you need to use Ampersand?
