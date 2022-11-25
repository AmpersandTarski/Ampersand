---
description: >-
  To document our long-term vision, this page summarizes the plans we have with
  Ampersand.
---

# Future plans

## Summary

This chapter discusses future plans. Click on the hyperlinks for details

* In this [video](https://youtu.be/SAzFxZ7Cz9I) discusses the plans in 7min30 \(in Dutch\)


## Current State

To understand future developments, you may want to know where we stand now.


One purpose of Ampersand is to generate a web-application. Currently, the generator and the generated application follow this architecture:

![Current state of the Ampersand project](<./.gitbook/assets/state-of-ampersand-project-2019.png>)

The architecture is explained in more detail in [this chapter](./architecture-of-an-ampersand-application/).


## NoSQL storage

Instead of storing data to an SQL database only, we want to other types of databases to work with Ampersand as well. Especially NoSQL databases \(e.g. triple stores, graph databases, persistent event streaming\) seem very suited for Ampersand.
### Purpose

Data storage is a diverse landscape of technologies. Triple stores, graph databases, persistent event streaming are but a few technologies that have become commonplace. We want Ampersand to link up with such technologies to serve a wider spectrum of problems.

Ampersand is prepared for this by the plug-mechanism.

![Towards multiple types of persistence](<./.gitbook/assets/towards-multiple-types-of-persistence.png>)



## API documentation
We also want to generate documentation of the (generated) API to make it more useful in real life.
### Purpose

To take an API in production, it needs to be documented. For this purpose we want to generate Swagger files.

![Generating a Swagger file](<./.gitbook/assets/Untitled Diagram (8).png>)



## OWL and RDFS input
To work closer in sync with the semantic web we want to accept OWL and RDFS input. Currently Ampersand accepts Ampersand-syntax, Archimate-xml, and Excel as input.
### Purpose

Ampersand and semantic web technologies have much in common. To explore this topic and to benefit from available ontologies expressed in RDF and OWL, we want to make an OWL/RDFS parser so we can interpret ontologies in Ampersand.

![An extra parser, specifically for OWL/RDFS](./.gitbook/assets/untitled-diagram-7%20%281%29.png)







## Refactor the front-end

To generate reactive web-applications instead of a classical object-oriented web-application is why we want to refactor the Ampersand front-end.

The current front-end is based on the Angular-JS framework. To enter the world of reactive programming, we want to refactor the front-end.

![Refactoring the front-end (pink items)](<./.gitbook/assets/Untitled Diagram (6).png>)
