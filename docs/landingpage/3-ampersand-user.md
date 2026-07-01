---
title: For users of Ampersand
---

# Building an information system

Imagine your information model gives you a running system right away. You only specify the rules of your business and the user interfaces. By generating a prototype from it, prove to the world that your specification is sound. Simply because it works. "Show, don't tell" was never closer. No longer will you make designs for archives only. That is what this page invites you to build on. The journey below sets out that path in stages — from a first model to a deployed, maintainable application — and names what each stage asks of you, so you can weigh, before you begin, whether this way of building befits you.

If you have never used Ampersand before, start with the [student learning path](./2-student.md) and come back here once you have run your first prototype.

## Your journey, stage by stage

### Stage 1 — Get the idea *(about 1–2 hours)*

Do the [tutorial](../tutorial-rap4). It walks you through a small system end to end, so you understand what a specification looks like and what the generated prototype does. Everything below assumes you have done this once.

### Stage 2 — Design your model

This is where most of your work happens. Capture your domain as concepts, relations and rules, and let the rules express the business logic.

- Write correct code with the [Syntax of Ampersand](../reference-material/syntax-of-ampersand) as your companion — keep it open while you work.
- Learn the craft of modelling from the [best practices for Ampersand modellers](../guides/best-practices) and the [Modelling](../modeling/README.md) background.
- Reuse what exists: there are [modules available for re-use](../reusing-available-modules) that can save you a lot of time.

### Stage 3 — Install Ampersand

To turn your model into a running prototype you need the compiler. Follow [Installing Ampersand](../guides/installing-ampersand). You can use the web-based RAP environment (no installation) or install the compiler locally — the guide covers both.

### Stage 4 — Generate and run your prototype

Compile your script into a working application. If you work from the command line, see [the command-line tool](../the-command-line-tool) for the compiler functions you need to generate a prototype and its database.

### Stage 5 — Deploy it

When your prototype does what you want, put it online. [Deploying your prototype](../guides/deploying-your-prototype) takes you through running, deploying and redeploying your application, and what to do when you get stuck.

### Stage 6 — Understand, maintain and grow

To keep your application healthy over time, understand [the architecture of an Ampersand application](../reference-material/architecture-of-an-ampersand-application) — how the generated system fits together, so you can build, deploy and maintain it with confidence.

## Questions & answers

Whether you are sketching your first model or operating a live system, these answers cut to what matters for a builder: how to design clean models, generate and run prototypes, deploy them, get real data in, shape the user interface, and keep it all running. Pick the section that matches where you are on your build journey.

### Designing your model — concepts, relations, rules, syntax

**Where do I start, and what does building a system with Ampersand actually involve?**
Follow a clear route: get the idea from the tutorial, design your model, install Ampersand, generate and run a prototype, deploy it, then understand and maintain the architecture. Knowing the stages up front saves you from blind alleys. [Learn more →](./3-ampersand-user.md)

**Why bother building a conceptual model at all?**
A model pays off when you want to study a domain, design an application, spot architecture flaws early, write a functional specification, or cleanse data — and it is especially valuable when a computer must interpret it to generate artifacts, or when you want to standardise the language of the business. [Learn more →](../modeling/conceptual-modeling.md)

**What steps does Ampersand suggest for specifying a system?**
Work in this order: agree on the domain language, agree on the rules, validate those rules, then define the interfaces. A conceptual model is simply concepts, relations, language structure, and the constraints that govern the domain. [Learn more →](../modeling/README.md)

**What is an atom, and why must it identify exactly one real-world object?**
An atom is the core data unit: it refers to one individual object within a context so every statement you write about it stays unambiguous. Get this right and your whole model stays honest. [Learn more →](../reference-material/atoms.md)

**Which atomic types can I give a concept, and how do they map to SQL?**
Pick from ALPHANUMERIC, INTEGER, DATE, BOOLEAN, FLOAT and more, each mapping to an SQL type, and assign one with a REPRESENT statement. Choosing the right type controls both storage and the input control you get in the UI. [Learn more →](../reference-material/atoms.md)

**Why can't I use the complement or the full relation V on some concepts?**
The complement, V, and interface targets need closed types; open types like HUGEALPHANUMERIC or BINARY do not support those operators. If you hit this restriction, switch the concept to a closed type. [Learn more →](../reference-material/atoms.md)

**How do I declare a relation with its type and multiplicity?**
Write `RELATION name[Source*Target]` with optional properties like `[UNI,TOT]`, an optional PRAGMA, and a MEANING. The properties are where you encode business constraints directly into the data model. [Learn more →](../reference-material/syntax-of-ampersand.md)

**How do I make a relation hold at most one value, like one date of birth per person?**
Add the `[UNI]` property (univalence) to the relation declaration, and `[INJ]` when you also need injectivity. Ampersand then enforces that uniqueness for you everywhere it matters. [Learn more →](../modeling/data-modeling.md)

**How do I turn a spreadsheet table into relations?**
Treat each row as an instance of the source concept and each column as a relation, naming each relation after its column. This gives you a fast, mechanical path from existing data to a working model. [Learn more →](../modeling/data-modeling.md)

**How do I state that one concept specialises another?**
Use `CLASSIFY Specific ISA Generic`, which says every atom of the specific concept is also an atom of the generic one. This lets you reuse relations and rules across a hierarchy. [Learn more →](../reference-material/syntax-of-ampersand.md)

**How do I write a rule with a violation message?**
A RULE takes an optional label, a term, optional meanings and messages, and an optional VIOLATION built from TXT, SRC, and TGT segments. Good violation messages turn a constraint into a clear instruction for your end users. [Learn more →](../reference-material/syntax-of-ampersand.md)

**What is the difference between an invariant, an enforcement rule, and a process rule?**
An invariant always forbids or auto-resolves violations, an enforcement rule auto-resolves violations as they occur, and a process rule stays satisfied only once a user resolves the violation. Choosing the kind decides who does the work: the system or the user. [Learn more →](../reference-material/dictionary.md)

**What does the ENFORCE statement do?**
ENFORCE automatically keeps a relation's population equal (`:=`), a subset (`:<`), or a superset (`>:`) of a given term, so derived data maintains itself. Use it to compute relations instead of asking users to keep them in sync. [Learn more →](../reference-material/syntax-of-ampersand.md)

**How do I write an automated rule that fixes the data itself?**
Give the rule a `ROLE ExecEngine MAINTAINS` clause and a VIOLATION with EX/InsPair/DelAtom actions that repair the data, as in the Sequence example. The ExecEngine then restores the rule automatically whenever it is broken. [Learn more →](../conceptual/automated-rules.md)

**What are my options for enforcing a rule once I have specified it?**
A rule can be an invariant (blocks the transaction), a process rule (signals a user), an automated rule (restored by an action), or derivable from other rules. Separating the rule from how it is enforced lets you change enforcement without rewriting the logic. [Learn more →](../conceptual/automated-rules.md)

**What can I put inside a context, and why does the context matter?**
A context is a set of true statements in one language that must stay free of contradictions, and it may contain metas, patterns, rules, classify and role rules, relations, concept definitions, representations, views, interfaces, purposes, populations, and includes. Everything you build lives inside one. [Learn more →](../reference-material/context.md)

**How do I set the language and markup for a context?**
After the context name, add `IN ENGLISH` or `IN DUTCH` plus a markup style (REST, HTML, LATEX, or MARKDOWN) as the defaults for purposes and meanings. Set this once and your generated documentation comes out in the right language and format. [Learn more →](../reference-material/context.md)

**How do I write comments and name concepts and relations?**
Single-line comments start with `--` and multiline comments use `{- -}`; concepts start with a capital letter and relation names with a lowercase letter. Following the convention keeps your scripts readable and avoids parser surprises. [Learn more →](../reference-material/how-to-read-syntax-statements.md)

**How do I type relational operators like composition, converse, and union?**
Map them to keyboard symbols: `;` compose, `~` converse, `\/` union, `/\` intersect, and `\` and `/` for the residuals. Knowing the symbols lets you write expressive terms without hunting for special characters. [Learn more →](../reference-material/terms.md)

**What are the operator precedence rules for terms?**
Logic operators bind weakest, then binary boolean, then binary relational, with unary complement and converse binding strongest; operators of the same category need explicit brackets. When in doubt, add brackets and the meaning becomes unambiguous. [Learn more →](../reference-material/how-to-read-syntax-statements.md)

**What do the `?`, `+`, and `*` symbols mean in the syntax descriptions?**
They mean zero-or-one, one-or-more, and zero-or-more occurrences in Ampersand's EBNF-like notation. Reading this notation lets you decode any grammar rule in the reference. [Learn more →](../reference-material/how-to-read-syntax-statements.md)

**How do I conditionally include or exclude parts of a script?**
The preprocessor uses `--#IF` / `--#IFNOT` / `--#ELSE` / `--#ENDIF` directives driven by variables set in INCLUDE statements. This lets you keep one script that builds different variants. [Learn more →](../reference-material/the-preprocessor.md)

**How do I split a specification across several files?**
Use an INCLUDE statement to pull all definitions of another `.adl` file into your context. This keeps large models modular and lets teams work on separate files. [Learn more →](../examples.md)

**Where is the complete reference for the language, and what do the core terms mean?**
The reference-material section describes the full Ampersand language as a reference (not a course), and the dictionary defines each notion — atom, concept, relation, rule, pattern — with a meaning, example, and purpose. Keep both open while you model. [Learn more →](../reference-material/README.md)

### Generating & running your prototype — CLI, Docker, installation

**Do I have to install the compiler locally to build a prototype?**
No. You can use the web-based RAP environment without installing anything, or install the compiler locally if you prefer. Either way you can be generating prototypes in minutes. [Learn more →](./3-ampersand-user.md)

**How do I install Ampersand on my own computer?**
Run it through Docker on any platform, grab a prebuilt executable on Windows or Linux, or build it from source with Stack. Pick whichever fits your environment with the least friction. [Learn more →](../guides/installing-ampersand.md)

**Is there editor support for writing scripts?**
Yes — install the "Ampersand language support" VS Code extension (`ampersandtarski.language-ampersand`) and choose the Ampersand colouring theme. It adds modelling support with settings like the main script name and project folder, and needs `ampersand` on your PATH. [Learn more →](../the-tools-we-use/ampersand-language-support.md)

**Which generate commands does the VS Code extension provide?**
It can show the version, run the daemon, and generate functional specs, an Atlas JSON, and a prototype — all without leaving your editor. This keeps your edit-generate loop tight. [Learn more →](../the-tools-we-use/ampersand-language-support.md)

**How do I run the compiler from the command line with Docker?**
Run `docker run -it -v "$(pwd)":/scripts ampersandtarski/ampersand COMMAND [OPTIONS] FILE`, with Docker installed and internet access. This gives you the full compiler without a local install. [Learn more →](../the-command-line-tool.md)

**Can I run the compiler without installing Ampersand at all?**
Yes — pull the `ampersandtarski/ampersand` Docker image and run it with `docker run`, mounting your project directory. Nothing lands on your machine except the image. [Learn more →](../docker/1-compiler.md)

**Which Docker tag should I pick for a given version?**
Use `:latest` for the development build, `:stable` for the latest stable release, `:[branch]` for a branch, or `:v3.17` for a specific release. Pin a version tag in production so builds stay reproducible. [Learn more →](../docker/1-compiler.md)

**What do I do if downloading the image from GitHub packages fails?**
Generate a GitHub token with `repo` and `read:packages` rights and run `docker login` against `docker.pkg.github.com`. That clears most authentication errors when pulling images. [Learn more →](../docker/1-compiler.md)

**How do I generate a functional design document from my model?**
Run `ampersand documentation MyModel.adl` to produce a `.docx` with Intro, Shared Language, Diagnosis, Conceptual Analysis, and Data Structure chapters, all controllable via options. It is a ready-made specification you can hand to stakeholders. [Learn more →](../the-command-line-tool.md)

**The compiler failed and returned a number — what does it mean?**
Each exit code has a meaning: 0 success, 10 invalid script, 20 inconsistent population, 50 sanity-check violations, 70 wrong arguments, and more. Reading the code tells you whether to fix the script, the data, or the command. [Learn more →](../the-command-line-tool.md)

### Deploying & operating your application

**How do I deploy and run my prototype with Docker?**
Put a Dockerfile, a docker-compose.yml, and your `.adl` file in a working directory, run `docker-compose up -d`, then browse to localhost. You go from script to running web app with one command. [Learn more →](../guides/deploying-your-prototype.md)

**How do I deploy my own web application on my laptop?**
Clone the project-template, swap in your `.adl` script, set passwords in `.env`, then run `docker compose build`, create the proxy network, and `docker compose up -d`. The template handles the wiring so you focus on your model. [Learn more →](../docker/2-modelling-environment.md)

**Which services does `docker-compose up` start locally?**
An Apache webserver on port 80, a phpMyAdmin dashboard on port 8080, and a MariaDB database that stays unexposed on the host. You get a full local stack, including a way to inspect the database. [Learn more →](../docker/README.md)

**Which image provides the prototype's database?**
Use the `prototype-db` image at `docker.pkg.github.com/ampersandtarski/prototype-db/prototype-db`. It is preconfigured for the prototype framework so you do not set up a database by hand. [Learn more →](../docker/5-prototype-database.md)

**How do I redeploy after editing my script?**
Run `docker-compose up -d --build` to rebuild and restart the container. Your changes go live in a single step. [Learn more →](../guides/deploying-your-prototype.md)

**My deployed app looks empty or shows a database error — what did I miss?**
Press the red install button in the application to create and populate the database. This one step is the usual fix for a blank-looking first deploy. [Learn more →](../docker/2-modelling-environment.md)

**Why does Docker say "network proxy not found" or fail to bind a port?**
Create the proxy network once with `docker network create proxy`, and make sure ports 80/443 are free of any other web server. Both are quick, one-time fixes. [Learn more →](../docker/2-modelling-environment.md)

**My deployed prototype is not reachable in the browser — what should I check?**
Confirm the firewall allows port 80, verify the port mapping in `docker-compose.yml`, and use a recent browser like Firefox or Chrome. These three checks resolve most connectivity issues. [Learn more →](../guides/deploying-your-prototype.md)

**How do I install and run the RAP environment with Docker on Windows?**
Install Docker Desktop, download the RAP repo as a ZIP, run `docker-compose up -d` (creating the proxy network if needed), and open localhost. You get the full modelling environment on Windows without a manual build. [Learn more →](../docker/README.md)

**Which container images does Ampersand use, and what is each for?**
It uses the `haskell` and `debian` base images, the ampersand compiler and devcontainer images, ampersand-rap, mariadb, phpMyAdmin, and the prototype-framework image. Knowing the roster helps you reason about what runs where. [Learn more →](../docker/README.md)

**What does the architecture of a generated application look like?**
Ampersand generates a monolithic web app with a stateful database service and a stateless service of interfaces, fronted by an API and an MVC-style front-end. Understanding this shape helps you scale and operate it. [Learn more →](../reference-material/architecture-of-an-ampersand-application.md)

**How is an application typically deployed on Docker?**
It runs as a stateless service with a separate database, a proxy for https/http, and reused open-source components, all defined statically in docker-compose. The stateless service plus separate database is what makes deployment and upgrades predictable. [Learn more →](../reference-material/architecture-of-an-ampersand-application.md)

**How do I inspect the database underneath my application?**
Deploy phpMyAdmin alongside your application to browse and query its database directly. It is the fastest way to confirm what your model actually stored. [Learn more →](../guides/frequently-asked-questions.md)

**How do I monitor an application in production?**
Use Grafana and Prometheus on a Kubernetes cluster, copying the setup from the RAP repository's manifest files. You get production-grade dashboards without building monitoring from scratch. [Learn more →](../guides/frequently-asked-questions.md)

### Getting data in & reusing work — Excel import, modules

**How do I import spreadsheet data into my prototype?**
Use an INCLUDE statement for a compile-time import, or the importer on the running prototype's home page for a run-time import. Either way you load real data instead of typing it in by hand. [Learn more →](../the-excel-importer.md)

**How do I bulk-load lots of data at runtime?**
Use the built-in Excel Import Extension from the menu bar to import data from Excel files into a running application. It is the standard route for loading volume data after deployment. [Learn more →](../guides/frequently-asked-questions.md)

**How must I lay out a spreadsheet so the importer matches columns to relations?**
Start each block with a bracketed cell in column A, put relation names in a first header row and concept names in a second, and add delimiters for multi-value cells. Get the layout right and the importer reconstructs all your pairs automatically. [Learn more →](../the-excel-importer.md)

**How can I load an initial population from a spreadsheet via INCLUDE?**
Use INCLUDE on an `.xlsx` file whose tables start with a `[Relation]` marker, a relation-name row, and a concept-name row, and Ampersand rebuilds the pairs at compile time. This bakes your seed data straight into the model. [Learn more →](../reference-material/syntax-of-ampersand.md)

**How does the importer convert cell types to Ampersand datatypes?**
It maps text, number, boolean, and date cells to the concept's REPRESENT type, with specific rules and errors for mismatches (binary types, for example, cannot be imported). Knowing the mapping saves you from surprise import failures. [Learn more →](../the-excel-importer.md)

**Which reusable modules can I drop into my model?**
Available modules include Messaging, Sets, Sequences, and SIAM (Session, Identity and Access Management). Reusing them saves you from re-modelling common machinery. [Learn more →](../reusing-available-modules.md)

**How do I add login, accounts, and role-based access control without building it myself?**
Include the reusable SIAM module, which provides sessions, identity, login, passwords, and role-based interface access control out of the box. It is a major shortcut for any real deployment. [Learn more →](../reusing-available-modules.md)

**Where do I find modelling best practices and reusable work?**
The best-practices guide and the Modelling background teach the craft, and ready-made modules are available for reuse. Lean on both to model well and avoid reinventing the wheel. [Learn more →](./3-ampersand-user.md)

### Customising the user interface — interfaces, BOX/VIEW templates, components

**How do I define an interface, and what is its syntax?**
An INTERFACE has a name, optional roles, an interface term, optional CRUD and view, and a nested box of interface items that specify the fields shown. Each field shows the data selected by its term for the current atom. [Learn more →](../reference-material/interfaces.md)

**What does a worked client interface look like?**
It has a header (name, FOR-roles, interface term) followed by a BOX of labelled box items, each with a term that selects the data to display. Studying one worked example is the quickest way to grasp the structure. [Learn more →](../examples.md)

**How do I create a nested, hierarchical interface?**
Follow a field term with a BOX containing subinterfaces; each atom in the field term's codomain gets its own box, mirroring the page hierarchy. This is how you build master-detail and drill-down screens. [Learn more →](../tutorial/interfaces.md)

**Which built-in layout options do I have for interfaces?**
Use `BOX <TABLE>` for columns, `BOX <FORM>` for rows, and `BOX <TABS>` for tabs, and mix them freely without changing the semantics. You restyle the page without touching the underlying logic. [Learn more →](../reference-material/interfaces.md)

**What BOX components are available for laying out an interface?**
FORM, TABLE, TABS, RAW, PROPBUTTON, and FILTEREDDROPDOWN, which nest to build structured layouts. Combining them covers most interface patterns you will need. [Learn more →](https://ampersandtarski.github.io/prototype/reference-material/frontend-components)

**How do the CRUD annotations on a field control what users can do?**
CRUD grants Create, Read, Update, and Delete rights per term: a capital letter allows the action and a lowercase letter forbids it (so `cRud` is read-only). This is how you scope each user's permissions field by field. [Learn more →](../reference-material/interfaces.md)

**How do CRUD flags decide which input control gets rendered?**
Each uppercase letter enables an operation, and its combination with UNI/TOT determines whether you get a single field, a list, or a read-only display. So your annotations shape both rights and the rendered widget. [Learn more →](https://ampersandtarski.github.io/prototype/reference-material/data_flow_analysis)

**How do I control which input control Ampersand renders for a value?**
Declare a REPRESENT with an atomic type like ALPHANUMERIC, DATE, BOOLEAN, or INTEGER (the default is ALPHANUMERIC). The type drives the picker, checkbox, or text field your users see. [Learn more →](https://ampersandtarski.github.io/prototype/reference-material/frontend-components)

**How can I make interface parts appear only under certain conditions, like login vs logout?**
Use box terms that are non-empty only under the desired condition, combined with a hiding template so empty subboxes are not shown. This gives you conditional UI driven entirely by your model. [Learn more →](../examples.md)

**How do I add a one-click button that toggles a boolean property?**
Use `BOX<PROPBUTTON>` with the prescribed `label` and `property` fields on a `[PROP,UNI]` relation whose CRUD allows Update. It is the simplest way to give users a tidy toggle. [Learn more →](https://ampersandtarski.github.io/prototype/reference-material/built-in-box-templates)

**Which fields must a PROPBUTTON box use, and what actions does it support?**
It needs `label` for the button text and `property` for the boolean relation to modify, matched exactly; the action is toggle (switch true/false, the default), set (always true), or clear (always false). Knowing this lets you wire up the right behaviour first time. [Learn more →](https://ampersandtarski.github.io/prototype/reference-material/built-in-box-templates)

**Why does my PROPBUTTON give a type-mismatch compilation error?**
The property relation is missing the `[UNI]` constraint — declare it as `[PROP,UNI]` and the error clears. This is the most common PROPBUTTON pitfall. [Learn more →](https://ampersandtarski.github.io/prototype/reference-material/built-in-box-templates)

**What are the limitations of PROPBUTTON?**
It handles only one boolean property per button, offers no visual customisation or conditional visibility, and uses default PrimeNG styling. Knowing the limits tells you when to reach for a custom template instead. [Learn more →](https://ampersandtarski.github.io/prototype/reference-material/propbutton-template)

**Can I make a custom dropdown without writing a new Angular component?**
Yes — FILTEREDDROPDOWN reuses `app-atomic-object` and takes a `[selectOptions]` property to supply the filtered data. You get a tailored dropdown with no new component code. [Learn more →](https://ampersandtarski.github.io/prototype/guides/creating-custom-box-templates)

**When should I create a custom BOX template instead of reusing one?**
Create one only when the existing FORM/TABLE/TABS/RAW templates cannot give you an interaction pattern that you intend to reuse. For one-offs, prefer the built-ins. [Learn more →](https://ampersandtarski.github.io/prototype/reference-material/box-template-architecture)

**What do I need to build to create a new BOX template?**
Three pieces: a Mustache HTML template, an Angular component (ts/html/scss), and a registration entry in `shared.module.ts`. With those in place your template behaves like a built-in one. [Learn more →](https://ampersandtarski.github.io/prototype/guides/box-template-development-guide)

**What naming and location does a custom BOX template need?**
Name it `Box-{TEMPLATENAME}.html` and place it in `frontend/src/app/generated/.templates/` for all projects, or in a project's `templates` folder for project-specific use. The location decides whether it is global or local. [Learn more →](https://ampersandtarski.github.io/prototype/guides/creating-custom-box-templates)

**Where do the UI HTML templates live, and how do I override them for one project?**
Built-in templates are in `frontend/src/app/generated/.templates/`; drop your own copies in that project's `templates/` folder to override the built-ins for that project only. This lets you customise one app without affecting others. [Learn more →](https://ampersandtarski.github.io/prototype/FAQ)

**Why does my interface show internal ids instead of readable names, and how do I prevent it?**
Give each user-visible concept a default VIEW, e.g. `VIEW Person : Person(name)`; without one the prototype falls back to the atom's internal identifier (an "ugly id"). Defining default views keeps those codes out of your production screens. [Learn more →](../reference-material/syntax-of-ampersand#the-view-statement)

**What is a VIEW template, and how does it differ from a BOX template?**
A VIEW controls how a single atom renders by mapping relations to named slots, while a BOX controls how a container lays out multiple fields. Reach for a VIEW when you want to change how one thing looks, not the layout around it. [Learn more →](https://ampersandtarski.github.io/prototype/guides/creating-custom-view-templates)

**How do I show a short label with a longer tooltip on hover?**
Define a VIEW with text and popover slots using an HTML TEMPLATE like `TextWithPopover.html` that binds the `title` attribute. It is a small touch that makes dense screens much clearer. [Learn more →](https://ampersandtarski.github.io/prototype/guides/creating-custom-view-templates)

**Where does the backend put VIEW data, and how does the template read it?**
The backend collects the evaluated VIEW relations in a JSON object named `_view_`, and the template reads them via `viewData['_view_']['slotname']`. Knowing the path lets you bind any slot you define. [Learn more →](https://ampersandtarski.github.io/prototype/guides/creating-custom-view-templates)

### Back-end services & extending the framework

**Which back-end services does a generated application expose?**
It offers endpoints for login testing, session cleanup, the installer, the rule/exec engine, population import/export, metamodel export, and reporting on relations, conjuncts, and interfaces. These give you operational control without custom code. [Learn more →](https://ampersandtarski.github.io/prototype/guides/back-end-services)

**Which endpoint exports my whole population?**
The `/export/all` endpoint exports all population. It is the one-call way to take a full snapshot of your data. [Learn more →](https://ampersandtarski.github.io/prototype/guides/back-end-services)

**How do I run custom code when atoms or links are added or deleted?**
Connect a listener to the framework's Symfony event dispatcher for events like `AtomEvent ADDED` or `TransactionEvent COMMITTED`. This is the supported hook for adding side effects to your application. [Learn more →](https://ampersandtarski.github.io/prototype/reference-material/prototype-framework)

**How does the framework handle files and uploads?**
It uses the Flysystem PHP abstraction with a default Local adapter on the AmpersandApp object, swappable for adapters like SFTP. So you can move file storage to remote backends without rewriting your code. [Learn more →](https://ampersandtarski.github.io/prototype/reference-material/prototype-framework)

**How does the framework bundle a compiler, and how do I use a custom one?**
The Dockerfile copies a compiler from the `ampersandtarski/ampersand` image, and you can inject your own with `ADD` or `COPY` before the compiler runs. This lets you test against a custom or pre-release compiler. [Learn more →](https://ampersandtarski.github.io/prototype/reference-material/prototype-framework)

### Configuring your application

**What can I configure in `localSettings.php`?**
It holds switches and parameters such as debug-mode, logging, time zone, production mode, database credentials, login, importer roles, and ExecEngine parameters. It is the central place to tune one deployment. [Learn more →](../reference-material/configuring-your-application.md)

**How do I switch from development to production settings?**
Set environment variables like `AMPERSAND_DEBUG_MODE` and `AMPERSAND_PRODUCTION_MODE`; these always override the config files. That makes per-environment configuration clean and overridable at deploy time. [Learn more →](https://ampersandtarski.github.io/prototype/guides/configuring-environments)

**What do `global.debugMode` and `global.productionEnv` do?**
`debugMode` controls how much error detail is returned, and `productionEnv` blocks database reinstallation to prevent accidental data loss. Turn the latter on in production to protect your live data. [Learn more →](https://ampersandtarski.github.io/prototype/guides/configuring-environments)

**How do I restrict who can use the installer, exec engine, and population exporter?**
Set `rbac.adminRoles` in `config/project.yaml` to a list of role names; `null` means everyone, `[]` means nobody. This locks down the powerful admin functions to the roles you trust. [Learn more →](https://ampersandtarski.github.io/prototype/guides/configuring-environments)

### Best practices & troubleshooting

**What are the best practices for maintainable models?**
Use PURPOSE statements generously, separate requirements from specifications, work in pairs, iterate and test in tiny steps, and keep rules minimal. These habits keep a growing model understandable and changeable. [Learn more →](../guides/best-practices.md)

**Is it safe to change my relations and rules often as the model grows?**
Yes — change them freely as your insights develop; the type checker walks you through the consequences for rules and interfaces. That safety net is what makes rapid, iterative modelling practical. [Learn more →](../guides/best-practices.md)

**What is the difference between requirements and specifications?**
Requirements state in prose what users want; Ampersand specifications define unambiguously what to build. Keeping the two distinct prevents wishful prose from leaking into your formal model. [Learn more →](../guides/best-practices.md)

**How does an application handle security concerns like access control, encryption, and injection?**
Access control comes from SIAM/ROLE or the platform; there is no built-in encryption or injection protection, and the page works through the other OWASP-style risks topic by topic. Read it before you deploy anything sensitive so you know what you must add yourself. [Learn more →](../reusing-available-modules.md)

**What should I do when I hit a problem the documentation does not cover?**
Check the common problems, search existing issues, then file a new issue against the documentation or against Ampersand itself. That is the fastest route to a fix or an answer from the maintainers. [Learn more →](../troubleshooting.md)

**Is Ampersand a silver bullet that solves everything out of the box?**
No. It promises correct data and fast development to those willing to learn it — it is a powerful tool, not an effortless fix. Set that expectation and the payoff is real. [Learn more →](../intro.md)

### Background — ownership, roadmap, and where to start

**Where should I start reading based on my role?**
The introduction routes you to the right starting point depending on whether you are curious, a student, a professional, a scientist, or a software engineer. Start there and skip straight to what is relevant for you. [Learn more →](../intro.md)

**Who owns Ampersand, and under what licence can I use it?**
No one owns the project; the repositories are GPLv3-licensed and authors keep their own copyright, so anyone can use it for free. That makes it safe to adopt without licensing fees. [Learn more →](../ownership/README.md)

**What is on the roadmap?**
Planned work includes low-code Ampersand, schema-changing data migration, NoSQL storage, OpenAPI documentation, OWL/RDFS input, GraphQL, and Kleene operators. It is worth knowing what is coming before you commit to a workaround today. [Learn more →](../future-plans.md)

**Why can't Ampersand yet upgrade a production system without losing data?**
It lacks schema-changing data migration today, so upgrades that must preserve production data have to be migrated manually for now. Plan your production upgrades with that limitation in mind. [Learn more →](../future-plans.md)

**Is there a recording showing how to build an Atlas separately?**
Yes — this page embeds a meeting recording that demonstrates building an Atlas separately. Watch it to see the workflow end to end. [Learn more →](../videos.md)


## Where to get help

- A question about the language? Look it up in the [Syntax of Ampersand](../reference-material/syntax-of-ampersand) or the [frequently asked questions](../guides/frequently-asked-questions).
- Something not working, or a gap in these docs? [Open an issue on GitHub](https://github.com/AmpersandTarski/Ampersand/issues) with your own account.

Ready to build? Start at [Stage 2 — Design your model](#stage-2--design-your-model), or revisit [the tutorial](../tutorial-rap4) first.
