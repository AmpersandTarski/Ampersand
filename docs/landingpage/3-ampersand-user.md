---
title: For users of Ampersand
---

# Building an information system

If you want to build an information system with Ampersand, you have come to the right place. You write a specification in terms of *concepts*, *relations* and *business rules*, and Ampersand generates a working web-based prototype from it. This page is the journey from a first idea to a deployed, maintainable application.

If you have never used Ampersand before, start with the [student learning path](./student) and come back here once you have run your first prototype.

## Your journey, stage by stage

### Stage 1 — Get the idea *(about 1–2 hours)*

Do the [tutorial](../tutorial-rap4). It walks you through a small system end to end, so you understand what a specification looks like and what the generated prototype does. Everything below assumes you have done this once.

### Stage 2 — Design your model

This is where most of your work happens. Capture your domain as concepts, relations and rules, and let the rules express the business logic.

- Write correct code with the [Syntax of Ampersand](../reference-material/syntax-of-ampersand) as your companion — keep it open while you work.
- Learn the craft of modelling from the [best practices for Ampersand modellers](../guides/best-practices) and the [Modelling](../modeling/README) background.
- Reuse what exists: there are [modules available for re-use](../reusing-available-modules) that can save you a lot of time.

### Stage 3 — Install Ampersand

To turn your model into a running prototype you need the compiler. Follow [Installing Ampersand](../guides/installing-ampersand). You can use the web-based RAP environment (no installation) or install the compiler locally — the guide covers both.

### Stage 4 — Generate and run your prototype

Compile your script into a working application. If you work from the command line, see [the command-line tool](../the-command-line-tool) for the compiler functions you need to generate a prototype and its database.

### Stage 5 — Deploy it

When your prototype does what you want, put it online. [Deploying your prototype](../guides/deploying-your-prototype) takes you through running, deploying and redeploying your application, and what to do when you get stuck.

### Stage 6 — Understand, maintain and grow

To keep your application healthy over time, understand [the architecture of an Ampersand application](../reference-material/architecture-of-an-ampersand-application) — how the generated system fits together, so you can build, deploy and maintain it with confidence.

## Where to get help

- A question about the language? Look it up in the [Syntax of Ampersand](../reference-material/syntax-of-ampersand) or the [frequently asked questions](../guides/frequently-asked-questions).
- Something not working, or a gap in these docs? [Open an issue on GitHub](https://github.com/AmpersandTarski/Ampersand/issues) with your own account.

Ready to build? Start at [Stage 2 — Design your model](#stage-2--design-your-model), or revisit [the tutorial](../tutorial-rap4) first.
