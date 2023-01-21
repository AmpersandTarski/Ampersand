# Theory & background
You need very little theory to use Ampersand; just some persistence and curiosity to get used to thinking in relations and gain a beginner's proficiency in relation algebra.
However, there is a wealth of theory you might want to explore.

## Relation Algebra
Ampersand uses relation algebraic terms to specify rules because
* [Relation algebra](https://en.wikipedia.org/wiki/Relation_algebra) is a point-free formalism, so terms are easy to manipulate algorithmically.
  This yields maintainable code in the compiler and a smaller chance that programming errors remain undetected.
* Relation algebra is related to [knowledge representation and reasoning](https://en.wikipedia.org/wiki/Knowledge_representation_and_reasoning), which makes Ampersand models easy to grasp for knowledge engineers.
* Relation algebra can be translated to [SQL](https://en.wikipedia.org/wiki/SQL), which makes it possible to generate [information systems](https://en.wikipedia.org/wiki/Database).
* Relation algebra fits in a type system in which the concepts of users serve as types. This makes typing quite understandable for users.
* Relation algebra is compatible with specialization, so specialization of concepts is integrated in Ampersand.
* Relation algebra is well studied, well documented, and well understood, which eliminates the need to ``reinvent the wheel''.

The major drawback of relation algebra is that it is largely unknown in the communities of software engineers, business analysts, business rule specialists, and IT-architects.
So these people must make a genuine investment of their time to learn how to use Ampersand.

## Information Systems
Ampersand is used to generate information systems.
However, there is little formal theory about information systems to latch on to.

## Business Rules
Ampersand rules are a formalization of business rules.
Business rules have gained a lot of traction due to the Business Rules Manifesto.
Ampersand is actually (very close to the Business Rules Manifesto)[Ampersand/why-ampersand/BRManifestoAndAmpersand].
However, the fact that each rule in Ampersand is a constraint (hence declarative) makes it different from decision rules,
which dominate the mainstream understanding of business rules.

## Reactive Programming
Ampersand can be interpreted as a [reactive programming language](Ampersand/reactive-programming).
