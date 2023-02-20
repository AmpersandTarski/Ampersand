# Theory

This chapter documents some of the theory behind Ampersand.

## Overview
An information system (IS) consists of the following components:
1. A persistent store, which is structured. Whether it is distributed or not, in the cloud, relational or not, is irrelevant. In Ampersand, you can define the structure of the persistent store in terms of relations and rules.
2. Data, living in the persistent store. This data can change over time. All data together at one moment in time is called the "state" of the information system at that moment. In Ampersand, the data exists  as pairs of atoms that kept in a relation.
3. Interfaces, which match the structure of the persistent store.

The following table compares the language used in the world of information systems with related worlds. It is compared with Ampersand, because we use Ampersand to design information systems. It is compared with the world of software, because Ampersand generates software. It is compared with the world of model theory [ref required], because the formal theory of Ampersand can be understood formally in model theory. 

| information system | Ampersand | software | model theory |
| -- | -- | -- | -- |
| rules | script | program | theory |
| data | population | state | model |
| formal statement | term | condition | term |
| generator | generator | compiler |
| store | database | database |
| service | service | service |

The following theoretical topics are relevant for Ampersand.

1. Relation Algebra [Maddux 2006]
Ampersand uses heterogeneous relation algebras with specialization as  a language to specify information systems. This is existing theory. It is relevant for people who specify information systems. It allows them to formalize business rules. Thus, they can ensure on design time that an information system complies with business rules.

2. Rewrite systems
The Ampersand compiler manipulates relation algebra terms. For this purpose, it uses rewrite systems. This is existing theory. It is relevant for people who make the Ampersand-compiler generate efficient code.

3. Type systems
The Ampersand compiler ensures that every type-correct specification can be built [1]. For this purpose it has a type system, which signals type-errors in Ampersand-scripts. The Ampersand-compiler generates code only for scripts without type errors.

4. Generating computations
It is possible to generate code to keep constraints satisfied. The theory for this is under development. It is relevant for automating tasks in information systems.

5. Compiler theory
Ampersand is a compiler. Its syntax is parsed by the Parsec-module of Haskell. A hand-written generator embodies the semantics. The theory is existing. This subject is relevant for people who wish to change the language of Ampersand.

Each of the following sections treats on of the topics mentioned above (work to be done).

[1] Michels, G., Joosten, S., van der Woude, J., Joosten, S.: Ampersand: ApplyingRelation Algebra in Practice. In: de Swart, H. (ed.) RAMICS 2011. LNCS, vol. 6663,pp. 280–293. Springer, Heidelberg (2011)