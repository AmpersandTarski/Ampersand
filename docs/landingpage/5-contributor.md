---
title: For contributors
---

# Contributing to Ampersand

People who care make worthwhile software, and there is genuinely satisfying work here for someone who does — a compiler, a framework, and the documentation around them, all in the open. Below you will find how we organise the project, where your kind of work lives, and exactly what a first contribution involves — branch, change, release note, review — so you can see clearly what you would be stepping into, and decide, on your own terms, whether to take a seat.

Many people have contributed to the Ampersand project, each in their own way. We're proud of the results, and amazed daily at the software that tiny specifications can produce.

If you want to be part of it, [open an issue on GitHub](https://github.com/AmpersandTarski/Ampersand/issues) with your own GitHub account, or contact [Stef Joosten](https://github.com/stefjoosten) on GitHub to discuss your contribution.

## 🚀 Getting started

New here? Work through the [onboarding guide](../guides/onboarding.md): request access to the GitHub organisation, clone the repositories you need, and learn how the project is organised.

## 📝 Documentation & communication

Day-to-day communication runs through GitHub: [issues](https://github.com/AmpersandTarski/Ampersand/issues) for questions and bugs, pull requests for changes, and [discussions](https://github.com/AmpersandTarski/Ampersand/discussions) for broader topics.

### How is the documentation organised, and why?

- **[The architecture of the Ampersand documentation](../guides/documentation-architecture.md)** — the documentation system, its design choices, and how we grow it.

### How do I document my changes to the codebase?

- **[Documenting Prototype Framework Changes](../../prototype/guides/documenting-prototype-changes.md)** — where files go, naming conventions, `sidebar.js`, and the test/deploy workflow.

## 🏗 The project structure

The Ampersand ecosystem consists of four repositories:

1. **[Ampersand](https://github.com/AmpersandTarski/Ampersand)** — the core compiler and language implementation.
2. **[Prototype](https://github.com/AmpersandTarski/prototype)** — the interface framework for Ampersand applications.
3. **[RAP](https://github.com/AmpersandTarski/RAP)** — Repository for Ampersand Projects (used by the Open University).
4. **[AmpersandTarski.github.io](https://github.com/AmpersandTarski/AmpersandTarski.github.io)** — the documentation website builder.

## 🔧 Development workflow

- Understand what Ampersand produces: **[Architecture of an Ampersand application](../reference-material/architecture-of-an-ampersand-application.md)**.
- When you work on documentation, edit the `docs/` folder of the **right repository** and work in the **`documentation` branch** — this keeps documentation changes from triggering the heavy build pipelines. Update the relevant `sidebar.js` when you add a page.

## 📦 Submitting your contribution

1. Create a branch for your change (or work from a fork if you are not a member of the organisation).
2. Open a pull request. Documentation changes target the **`documentation`** branch; code changes follow the normal review process.
3. **Add a release note for code changes.** Any pull request that touches files **outside `docs/`** must add an entry to `ReleaseNotes.md`, or the *Check release notes* CI check fails. Pure documentation changes (only `docs/**`) are exempt.
4. **Let CI run.** Documentation changes are checked by the documentation-hygiene workflow (no duplicate sidebar ids, no broken references, no internal notes published). Changes to the build kernel (`src`, `app`, the package/stack files, and so on) run the build, quality and formatting workflows. Make sure the checks pass.
5. A maintainer reviews and merges your pull request.

## 💡 Where you can help

- **Core development**: compiler improvements, prototype-framework enhancements, RAP development.
- **Documentation**: guides, tutorials and reference material.
- **Quality**: tests, code review, performance, and bug fixes.

## Guidelines

- Follow the established coding standards and best practices.
- Document your changes (see above).
- Test thoroughly before submitting.
- Keep the focus on user needs and maintainability.

Ready to contribute? Start with the [onboarding guide](../guides/onboarding.md), and when you're ready to document your work, follow the [documentation guide](../../prototype/guides/documenting-prototype-changes.md).

Welcome aboard! 🎉
