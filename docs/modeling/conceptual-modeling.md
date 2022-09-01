# Domain Driven Design

## Why bother?

Domain modeling is used for various purposes:

1. To explore unknown domains. The act of making domain models is an efficient way to familiarize yourself with a new domain and to unravel anfractuous jargon.
2. To define the boundaries of individual services. A domain model should make these boundaries clear, to develop components as independently as possible.
3. To avoid misunderstandings over terminology in a project with multiple stakeholders. A terminology list does not always suffice in practice. A domain model should consolidate consensus and make misunderstandings explicit and debatable before they do harm.
4. Learning. A learner must familiarize herself with new words and specific phrases. Making a domain model improves both the speed and depth of learning.

The Ampersand way of domain modeling is classic in the sense that a conceptual model is made of a domain of your choice. It differs from other approaches in specific ways:

1. Your model can be interpreted by a computer to create specific useful artifacts, such as a data model, a prototype implementation, or documentation;
2. Your model has an interpretation in natural language, which you can use to calibrate and uniform the language of the business.&#x20;

## Steps

Here is a summary of the things you do.

1.  Start with an informal analysis of the business, defining

    1. stakeholders, such as: sales rep, student, DBA, inspector, etc.
    2. relevant areas of expertise, such as insurance, security, legal, management, etc.
    3. business functions, such as invoicing, applying for a job, computing rates, etc.

    In essence, this step yields three lists. You obtain them by studying documentation and talking to people as you scope your work.
2. Formalize concepts and relations to reconstruct the language of the business. Validate that language with your stakeholders, creating consensus over terms and phrases. Your goal is to define the smallest agreed language in which agreements can be expressed.
3.  Use this language and adapt it when needed:

    1. to generate visual representations of your domain model (letting your computer do the drawing work);
    2. to resolve language-based misunderstandings in your team;
    3. to generate a database, either for prototyping or production purposes;
    4. to define, verify and validate services that constitute your application;
    5. to audit designs;
    6. to generate documentation (letting your computer do a lot of writing);
    7. other purposes, that arise incidentally.

    In essence, this step yields a conceptual model of your domain. In some cases, however, the act of modeling is more important than having the model.
4. draw boundaries to define bounded contexts. Identify reusable patterns. Identify entities, attributes, aggregates and services for each bounded context. Assign developers, stakeholders, product-owners to develop each bounded context further.

## Informal analysis

## Formalize concepts and relations

## Use and maintain the model

## Drawing boundaries
