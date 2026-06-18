---
title: For contributors
---

# Contributing to Ampersand

If you want to help develop Ampersand further, there is genuinely satisfying work here — a compiler, a framework, and the documentation around them, all in the open source space. Below you will find how we organise the project, where your kind of work lives, and exactly what a first contribution involves — branch, change, release note, review — so you can see clearly what you would be stepping into, and decide, on your own terms, whether to take a seat.

Many people have contributed to the Ampersand project, each in their own way. We're proud of the results, and amazed daily at the software that tiny specifications can produce.

If you want to be part of it, get your own GitHub account and contact [Stef Joosten](https://github.com/stefjoosten) on GitHub to discuss your contribution.

## 🚀 Getting started

New here? Work through the [onboarding guide](../guides/onboarding.md): request access to the GitHub organisation, clone the repositories you need, and learn how the project is organised.

## 📝 Documentation & communication

Day-to-day communication runs through GitHub: [issues](https://github.com/AmpersandTarski/Ampersand/issues) for questions and bugs, pull requests for changes, and [discussions](https://github.com/AmpersandTarski/Ampersand/discussions) for broader topics.

### How is the documentation organised, and why?

- **[The architecture of the Ampersand documentation](../guides/documentation-architecture.md)** — the documentation system, its design choices, and how we grow it.

### How is the documentation organised, and why?
- **[The architecture of the Ampersand documentation](../guides/documentation-architecture.md)** - The documentation system, its design choices, and how we grow it
  - One site assembled from the Ampersand, Prototype, and RAP repositories
  - Audience landing pages and the Diátaxis structure
  - The matrix model for extending and maintaining the docs

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
- **Research & design directions**: pick up an open design thread documented under [unpublished research](../research#unpublished-research) — for example [making oscillation risk visible](../ongoing-research/making-oscillation-risk-visible.md), whose Stage 2 is the next step.

## Guidelines

- Follow the established coding standards and best practices.
- Document your changes (see above).
- Test thoroughly before submitting.
- Keep the focus on user needs and maintainability.

## Questions & answers

Welcome, contributor. Whether you are hacking on the Haskell compiler, building Angular components for the Prototype framework, deploying RAP, or improving these very docs, this chapter answers the practical questions you hit while getting something done. Find your theme, grab the command or convention you need, and follow the link when you want the full story.

### Getting started & project structure

**Where should I start as a new contributor?**
The fastest path in: open a GitHub issue or contact Stef Joosten, request membership of the AmpersandTarski organization, then clone only the repositories you need. [Learn more →](../guides/onboarding.md)

**Which repositories make up the Ampersand ecosystem, and what does each do?**
Knowing where code lives saves you hunting: **Ampersand** is the compiler, **Prototype** gives the visual interface and BOX/VIEW templates, **RAP** stores and builds Ampersand scripts, and **AmpersandTarski.github.io** builds the documentation site. The wider Git org also holds ampersand-models, Ampersand-Language-Support and Publications. [Learn more →](./5-contributor.md)

**How do I clone the source with Git?**
Get yourself coding in minutes: create a GitHub account, ask for team membership, set up an SSH key, then run `git clone git@github.com:AmpersandTarski/Ampersand.git`. [Learn more →](../the-tools-we-use/git.md)

**Which tools does the project use, and what role does each play?**
Before you set up your machine, this is your shopping list: Haskell, Stack, Docker, Git, Graphviz, MariaDB, Node.js, Pandoc, VS Code and more, each with its specific job spelled out. [Learn more →](../the-tools-we-use/tools-used-in-the-ampersand-project.md)

**Are there videos to walk me through setting up the environment?**
If you learn faster by watching, the Quick Start Guide videos cover setup and build, folder structure, and local setup; have Git and Docker installed first. [Learn more →](../video-tutorials.md)

**Where should I start reading these docs for my role?**
Don't waste time wandering: the introduction routes you to the right starting point depending on whether you are curious, a student, a professional, a scientist, or a software engineer. [Learn more →](../intro.md)

**What working principles does the team follow?**
Aligning with the team's habits makes reviews smoother: produce free open-source software, automate production, write for maintainability, and diagnose issues on GitHub before fixing. [Learn more →](../the-tools-we-use/README.md)

**What editor support is available for writing Ampersand models?**
Get syntax help while you model: the `ampersandtarski.language-ampersand` VS Code extension adds modelling support with settings like the main script name and project folder, and needs `ampersand` on your PATH. [Learn more →](../the-tools-we-use/ampersand-language-support.md)

### Building the compiler — Haskell, Stack, Docker, VS Code

**How do I build the compiler from source?**
This is your core dev loop: install Stack, clone the repo, run `stack build` (it installs the pinned GHC), then `stack test` to run the regression suite. With package.yaml in your working directory you can also `stack install`, adding `--flag ampersand:buildAll` to include the preprocessor. [Learn more →](../guides/installing-ampersand.md)

**Which build tools build the compiler and related artefacts?**
Pick the right tool for the job: Stack builds the compiler binary on MacOS/Windows, while Docker builds images for the compiler, applications and RAP. [Learn more →](../the-tools-we-use/building/README.md)

**How do I set up a Haskell dev environment in VS Code?**
Skip the manual toolchain setup: use the provided `.devcontainer` with the Remote Development extension and Docker, then "Reopen in Container" for a ready-made Haskell environment. [Learn more →](../the-tools-we-use/group-1/development-using-vs-code.md)

**How must I format Haskell code before contributing?**
Avoid a red CI check on your PR: all Haskell code must be formatted with ormolu, enforced by a GitHub action — enable format-on-save or run `stack exec ormolu inplace`. [Learn more →](../the-tools-we-use/group-1/development-using-vs-code.md)

**How do I build a Docker image with the compiler?**
To reproduce a clean build in a container, clone the repo and run `docker build -t myampersand ~/Ampersand/`; it is a 2-stage build and needs at least 5G of Docker memory. [Learn more →](../the-tools-we-use/building/building-an-ampersand-compiler-as-docker-image.md)

**How do I reproduce a build manually?**
When CI does something you want to repeat by hand, from the repo root run `docker build .` for the image, or `stack install` for the compiler. [Learn more →](../the-tools-we-use/building/automated-builds.md)

**Which container images does the project use, and what is each for?**
Handy when you are debugging the image stack: Ampersand uses haskell and debian base images, the ampersand compiler and devcontainer images, ampersand-rap, mariadb, phpMyAdmin, and the prototype-framework image. [Learn more →](../docker/README.md)

**What do the compiler exit codes mean?**
When a build script fails, the exit code tells you why: e.g. 0 success, 10 invalid script, 20 inconsistent population, 50 sanity-check violations, and 70 wrong arguments. [Learn more →](../the-command-line-tool.md)

### The contribution workflow — Git, releasing, CI/CD, documentation

**What does submitting a contribution involve?**
Here is the whole loop in one breath: create a branch, open a PR (docs target the `documentation` branch), add a ReleaseNotes entry for code changes, let CI pass, and a maintainer reviews and merges. [Learn more →](./5-contributor.md)

**What branching strategy does Ampersand use?**
Branch with confidence: experiment in feature branches off main, record your intentions in ReleaseNotes.md, and merge to the release-only main branch via pull request. [Learn more →](../the-tools-we-use/releasing-ampersand-and-workflow-details.md)

**How do I release a new Ampersand version?**
When it is your turn to cut a release: update the version in package.yaml and ReleaseNotes.md on development, push to trigger tests, verify the build, then open a pull request from development to master. [Learn more →](../the-tools-we-use/releasing-ampersand-and-workflow-details.md)

**Why and how is releasing automated?**
So you spend your time on code, not ceremony: releases run on GitHub Actions with git-flow branching, producing frequent, reliable, reproducible builds. [Learn more →](../the-tools-we-use/automation-of-releasing-ci-cd/README.md)

**How can I list the packages and versions published to the repo?**
To check what shipped, send a GraphQL POST to `api.github.com/graphql` with a packages query, using a personal access token and the `packages-preview` Accept header. [Learn more →](../the-tools-we-use/automation-of-releasing-ci-cd/github-packages.md)

**How can I contribute to the documentation?**
Improving the docs is one of the easiest first contributions; this page introduces how to help. [Learn more →](../the-tools-we-use/Documentation.md)

**How is the documentation site assembled?**
Knowing this prevents you from editing the wrong copy: it is one Docusaurus site built from the docs/ folders of the Ampersand, Prototype and RAP repos, organised by audience landing pages plus a Diataxis structure. [Learn more →](../guides/documentation-architecture.md)

**Where should I put a new documentation page?**
Place it in the docs/ folder of the repository it describes, and in the Diataxis area (guides, reference, theory, or introduction) that matches its kind — so readers and the sidebar find it where they expect. [Learn more →](../guides/documentation-architecture.md)

**Which audiences do the docs target?**
Write for the right reader: the docs serve five audiences — the interested visitor, the student, the user, the scientist, and the contributor. [Learn more →](../guides/documentation-architecture.md)

**How do my doc changes reach the live website?**
Publishing is push-button: three repositories feed one Docusaurus site, and pushing docs/ changes to main triggers the automated build. [Learn more →](https://ampersandtarski.github.io/prototype)

**What sidebar id must a new Prototype doc page use?**
Get the id right so the build doesn't break: it is the path relative to docs/ without `.md` and with `prototype/` prepended, e.g. `prototype/guides/my-new-guide`. [Learn more →](https://ampersandtarski.github.io/prototype/guides/documenting-prototype-changes)

**What steps publish a new page from the Prototype repo?**
Follow this checklist to go live: write the page, register it in docs/sidebar.js, test locally, push to main, and verify it appears on the site. [Learn more →](https://ampersandtarski.github.io/prototype/guides/documenting-prototype-changes)

**Where do the Prototype Framework docs live and who are they for?**
If you are documenting framework internals, your pages go in guides/ and reference-material/ under docs/, aimed at framework developers and advanced users, and published to the site from the main branch. [Learn more →](https://ampersandtarski.github.io/prototype)

**Under which licence are the docs published?**
Reuse them freely with attribution: the documentation is licensed under Creative Commons Attribution-ShareAlike 4.0 International unless otherwise specified. [Learn more →](../the-tools-we-use/README.md)

### The Prototype framework — templates, components & generation

**How do ADL interfaces become rendered HTML in the browser?**
This is the mental model behind every UI bug you'll chase: the compiler substitutes template placeholders into Angular components, the PHP backend supplies resource data at runtime, and Angular binds it into the DOM. [Learn more →](https://ampersandtarski.github.io/prototype/reference-material/data_flow_analysis)

**How do BOX templates connect Ampersand scripts to the rendered UI?**
They are HTML templates whose variables are substituted at compile time to generate Angular components, mapped via selectors like `app-atomic-object` — so editing one template changes a whole class of screens. [Learn more →](https://ampersandtarski.github.io/prototype/reference-material/box-template-architecture)

**What three components do I build to create a new BOX template?**
Budget for exactly three artefacts: a Mustache HTML template, an Angular component (ts/html/scss), and registration in `shared.module.ts`. [Learn more →](https://ampersandtarski.github.io/prototype/guides/box-template-development-guide)

**What naming convention and location does a custom BOX template need?**
Name it `Box-{TEMPLATENAME}.html` and place it in `frontend/src/app/generated/.templates/` for all projects, or in the project's templates folder for project-specific use. [Learn more →](https://ampersandtarski.github.io/prototype/guides/creating-custom-box-templates)

**What are the two compilation stages for a custom BOX template?**
Knowing both stages tells you which one to look at when output is wrong: Ampersand compilation generates TypeScript components and substitutes template variables, then Angular compilation builds the browser JavaScript. [Learn more →](https://ampersandtarski.github.io/prototype/guides/creating-custom-box-templates)

**When should I create a custom BOX template instead of reusing one?**
Save yourself the work unless you need to: build a custom one only when the existing FORM/TABLE/TABS/RAW templates cannot give the reusable interaction pattern you want. [Learn more →](https://ampersandtarski.github.io/prototype/reference-material/box-template-architecture)

**Where do the HTML templates for prototype UIs live?**
Find them fast: built-in templates are in `frontend/src/app/generated/.templates/`, and project-level overrides go in the project's `templates/` folder. [Learn more →](https://ampersandtarski.github.io/prototype/FAQ)

**Why does my BOX template show literal `$name$` instead of a value?**
That means the variable wasn't substituted — check the `$variableName$` syntax and confirm the variable exists in the compilation context. [Learn more →](https://ampersandtarski.github.io/prototype/guides/box-template-development-guide)

**Where does the backend put evaluated VIEW relations, and how does the template read them?**
To wire up a custom VIEW: the backend collects them in a JSON object named `_view_`, and the template reads them via `viewData['_view_']['slotname']`. [Learn more →](https://ampersandtarski.github.io/prototype/guides/creating-custom-view-templates)

**How do CRUD flags decide which input control is rendered?**
This explains why a field renders the way it does: each uppercase letter enables an operation, and the combination with UNI/TOT determines whether you get a single field, a list, or a read-only display. [Learn more →](https://ampersandtarski.github.io/prototype/reference-material/data_flow_analysis)

**How does a BOX component send a property change to the backend?**
When you need to persist an edit, the component calls `interfaceComponent.patch()` with a replace operation, then updates local data once the response is committed. [Learn more →](https://ampersandtarski.github.io/prototype/guides/box-template-development-guide)

**How does AtomicObjectComponent differ from AtomicAlphanumericComponent?**
Pick the right base for your field: the object component uses a PrimeNG dropdown over existing DB objects with a sub-interface link and a selection signal, while alphanumeric uses a free-text input reading data directly. [Learn more →](https://ampersandtarski.github.io/prototype/reference-material/frontend-component-internals)

**What is the Angular component inheritance hierarchy behind BOX templates?**
Reuse instead of reinvent: components extend `BaseAtomicComponent` or `BaseBoxComponent`, which provide CRUD rights, UNI/TOT constraint handling, and shared data binding. [Learn more →](https://ampersandtarski.github.io/prototype/reference-material/box-template-architecture)

**What shared logic do the base components provide?**
Lean on them so you write less: `BaseAtomicComponent` and `BaseBoxComponent` give you CRUD helper methods, UNI/TOT multiplicity inputs, item management, and the patch-based update lifecycle. [Learn more →](https://ampersandtarski.github.io/prototype/reference-material/frontend-component-internals)

**When does the framework hide the remove or delete button?**
So your UI stays consistent automatically: the remove button hides when `isTot` would be violated, and the delete button hides similarly based on the D permission and the TOT constraint. [Learn more →](https://ampersandtarski.github.io/prototype/reference-material/frontend-component-internals)

**Why do generated property names look like `_49__46__32_`?**
No, nothing is broken: special characters in field names are ASCII-encoded as `_XX_` so the names stay valid identifiers in TypeScript, HTML, and URLs. [Learn more →](https://ampersandtarski.github.io/prototype/reference-material/data_flow_analysis)

**How do I get the framework running locally to develop frontend components?**
Quickest path to a live UI: install Docker, run `docker compose up -d --build`, then `./generate.sh` to compile a project and open localhost. [Learn more →](https://ampersandtarski.github.io/prototype/guides/frontender-quick-start)

**How do I add and run my own test project?**
To iterate on real data, create it under `test/projects` with an `.adl` entry file in `model/`, then run `./generate.sh <project-name> <entry-file>`. [Learn more →](https://ampersandtarski.github.io/prototype/guides/frontender-quick-start)

**How do I test an Angular component in isolation?**
For tight, focused feedback: create Storybook stories and Cypress tests, run `npm run storybook`, then run the Cypress tests against Storybook. [Learn more →](https://ampersandtarski.github.io/prototype/guides/frontender-quick-start)

**Which back-end services does a generated application expose?**
Know your endpoints before you call them: login testing, session cleanup, installer, rule/exec engine, population import/export, metamodel export, and reporting on relations, conjuncts and interfaces. [Learn more →](https://ampersandtarski.github.io/prototype/guides/back-end-services)

**How can I run custom code when atoms or links are added or deleted?**
Hook into the data lifecycle: connect a listener to the framework's Symfony event dispatcher for events like `AtomEvent ADDED` or `TransactionEvent COMMITTED`. [Learn more →](https://ampersandtarski.github.io/prototype/reference-material/prototype-framework)

**How does the framework handle files and uploads?**
Swap storage without touching app code: it uses the Flysystem PHP abstraction with a default Local adapter on the `AmpersandApp` object, swappable for adapters like SFTP. [Learn more →](https://ampersandtarski.github.io/prototype/reference-material/prototype-framework)

**How does the framework handle errors and exceptions?**
This is what to expect when debugging: it throws exceptions using HTTP status codes — 4xx are user errors shown in full, 5xx are server errors hidden unless `debugMode` is on, and all are logged. [Learn more →](../the-tools-we-use/prototype-framework.md)

**What hookpoints can I use to extend a generated application?**
Plug in cleanly: hookpoints use camelCased names prefixed with pre/post (e.g. `postDatabaseUpdate`) and are listed alongside the extensions that use them. [Learn more →](../reference-material/architecture-of-an-ampersand-application.md)

**How can an Ampersand application authenticate users with OAuth?**
Add named-user access without rolling your own auth: an application can grant access via the OAuth standard, for example using GitHub as the access provider. [Learn more →](../the-tools-we-use/authentication-and-access-management-with-oauth.md)

### Releasing the Prototype framework

**How do I build and run the framework locally before releasing?**
Smoke-test first: run `docker compose up -d --build` to start the containers, then `./generate.sh` to compile and serve a test project on localhost. [Learn more →](https://ampersandtarski.github.io/prototype/guides/updating-and-releasing)

**How is a new framework version released?**
When you cut the release, a maintainer manually creates a tagged GitHub Release, which triggers `release.yml` to build and publish artefacts to Docker Hub. [Learn more →](https://ampersandtarski.github.io/prototype/guides/updating-and-releasing)

**What compiler compatibility must I check before releasing?**
Avoid a version mismatch in production: verify the constraint in `backend/generics/compiler-version.txt` still matches the compiler version bundled in the Dockerfile. [Learn more →](https://ampersandtarski.github.io/prototype/guides/updating-and-releasing)

**How does the framework bundle a compiler, and how do I inject a custom one?**
To test an unreleased compiler, note the Dockerfile copies one from the `ampersandtarski/ampersand` image — inject your own via `ADD` or `COPY` before running the compiler. [Learn more →](https://ampersandtarski.github.io/prototype/reference-material/prototype-framework)

### Deploying & operating RAP — Docker, Kubernetes, Azure, Windows

**What do I need to cover to deploy RAP?**
Start with the map: deploying RAP means preparing Azure (AKS), preparing a Windows environment, the deployment architecture for docker-compose and Kubernetes, and Gatling testing. [Learn more →](https://ampersandtarski.github.io/rap)

**What software must I install before deploying RAP?**
Set up your machine once: you need VS Code (for its built-in terminal) and Docker Desktop, plus the Azure CLI for AKS deployments. [Learn more →](https://ampersandtarski.github.io/rap/deployment-guide)

**How are the RAP, Prototype-framework, and Ampersand images related?**
Understanding the stack explains why RAP can compile scripts itself: the RAP image builds on the Prototype-framework image, which includes the Ampersand compiler image. [Learn more →](https://ampersandtarski.github.io/rap/rap-deployment)

**Which containers and volumes make up a Docker deployment of RAP?**
Know what you are running: a Traefik reverse proxy plus RAP, Enroll, phpMyAdmin and MariaDB containers across two networks, with `rap4-data`, `db-data` and `letsencrypt` volumes for persistence. [Learn more →](https://ampersandtarski.github.io/rap/rap-deployment)

**How do I install and run RAP4 on Docker from scratch?**
Step by step: clone the RAP repo, copy `.example.env` to `.env`, run `docker compose build`, create the `proxy` and `rap_db` networks, then `docker compose up -d` and reinstall the database. [Learn more →](https://ampersandtarski.github.io/rap/docker-deployment-guide)

**How do I build the RAP container images?**
To roll your own images, start Docker Desktop, cd into the RAP4 or RAP4USER folder, and run `docker build` with a tag like `ampersandtarski/ampersand-rap:2025-03-03`. [Learn more →](https://ampersandtarski.github.io/rap/deployment-guide)

**How do I redeploy a new version of RAP4 after a push?**
Ship the update on the server: cd into RAP, `git pull`, `docker compose build`, then `docker compose -f docker-compose.yml -f docker-compose.prod.yml up -d`. [Learn more →](https://ampersandtarski.github.io/rap/docker-deployment-guide)

**How do I deploy RAP to a local Kubernetes cluster on Docker Desktop?**
Test the k8s path on your laptop: enable Kubernetes in Docker Desktop, then `kubectl apply -k` the general kustomization (ingress and cert-manager) and the `overlays/local/dev` kustomization. [Learn more →](https://ampersandtarski.github.io/rap/deployment-guide)

**How do I install the RAP database on Kubernetes and stop it resetting?**
Avoid wiping data on restart: set `AMPERSAND_PRODUCTION_MODE` and `DISABLE_DB_INSTALL` to false in `administration-configmap` to install, then set both to true and restart the RAP pod. [Learn more →](https://ampersandtarski.github.io/rap/rap-deployment)

**How does RAP create and run a student prototype on Kubernetes?**
Useful when debugging student deployments: RAP's service account applies a student-manifest template (deployment, service, ingress) with kubectl, substituting the student name, namespace and base64-encoded script. [Learn more →](https://ampersandtarski.github.io/rap/rap-deployment)

**How are leftover student prototype pods cleaned up?**
The cluster tidies itself: a CronJob runs hourly with a `bitnami/kubectl` image to delete any student-prototype deployment, service and ingress older than one hour. [Learn more →](https://ampersandtarski.github.io/rap/rap-deployment)

**What do I install and configure on Windows to deploy RAP?**
Get a Windows box deployment-ready: install VS Code, Docker Desktop (with WSL enabled) and the Azure CLI, then enable Kubernetes in Docker Desktop and verify kubectl works. [Learn more →](https://ampersandtarski.github.io/rap/preparing-windows)

**How do I create an Azure Kubernetes Service cluster for RAP?**
Stand up the cluster: after logging in and setting the subscription, create a resource group with `az group create` and the cluster with `az aks create` using two Standard_B2s nodes. [Learn more →](https://ampersandtarski.github.io/rap/preparing-azure)

**How do I point kubectl at an Azure cluster and switch between clusters?**
Connect and context-switch safely: run `az aks get-credentials` with the resource group and cluster name, then use `kubectl config get-contexts` and `kubectl config use-context`. [Learn more →](https://ampersandtarski.github.io/rap/preparing-windows)

**What DNS records must I configure for the RAP cluster?**
For per-student subdomains to resolve, point an A record (e.g. `rap.cs.ou.nl`) to the AKS static public IP and add wildcard CNAME records pointing to it. [Learn more →](https://ampersandtarski.github.io/rap/preparing-azure)

**What does running RAP on Azure cost, and what permissions are needed?**
Plan budget and access up front: the described resources cost around €80/month; the creator needs global administrator (or resource-group creation) rights, while developers need Reader and AKS RBAC Cluster Admin roles. [Learn more →](https://ampersandtarski.github.io/rap/preparing-azure)

**How do I let GitHub Actions deploy to AKS without stored credentials?**
Use OIDC so no secrets leak: create an Azure AD app and service principal, assign it the contributor role, add federated credentials for the repo, and store `AZURE_CLIENT_ID`, `AZURE_TENANT_ID` and `AZURE_SUBSCRIPTION_ID` as GitHub secrets. [Learn more →](https://ampersandtarski.github.io/rap/preparing-azure)

**What steps does the RAP CI/CD pipeline perform?**
Know what happens on a merge: a Build step checks out main and pushes a tagged Docker image to DockerHub, and a Deploy step connects to Azure via OIDC and applies the new deployment. [Learn more →](https://ampersandtarski.github.io/rap/pipeline)

**How does the pipeline authenticate to Azure?**
Securely and credential-free: the Deploy step connects to the Azure account using OpenID Connect (OIDC) before applying the new deployment. [Learn more →](https://ampersandtarski.github.io/rap/pipeline)

**Why do student prototypes fail with a `/var/run/docker.sock` permission error?**
Quick fix: RAP needs read/write access to the host docker socket — enter the rap4 container and `chmod 666 /var/run/docker.sock` so RAP can spin up student containers. [Learn more →](https://ampersandtarski.github.io/rap/docker-deployment-guide)

**Why does kubectl say "the connection to the server was refused"?**
The Kubernetes server in Docker Desktop probably failed to start; reset the cluster in Docker Desktop settings and check `$HOME/.kube/config` for stale contexts. [Learn more →](https://ampersandtarski.github.io/rap/deployment-guide)

**How do I reset the RAP application in Azure production?**
When users hit timeouts or slowness, change the `DISABLE_DB_INSTALL` variable in `administration-configmap-production` via k9s, then restart the rap-production pod by deleting it. [Learn more →](https://ampersandtarski.github.io/rap/deployment-guide)

**What is the difference between a Secret and a ConfigMap?**
Store config in the right place: a Secret holds sensitive data like passwords or API keys in encrypted form, while a ConfigMap holds non-sensitive configuration as key-value pairs or files. [Learn more →](https://ampersandtarski.github.io/rap/kubernetes-terminology)

**What is the difference between a Deployment, ReplicaSet, and StatefulSet?**
Choose the right workload type: a Deployment manages identical scalable Pods, a ReplicaSet keeps a set number of Pod replicas running, and a StatefulSet manages Pods with stable network identities for stateful apps. [Learn more →](https://ampersandtarski.github.io/rap/kubernetes-terminology)

**Where do I learn to set up Kubernetes on Azure for RAP?**
Go straight to the "Prepare Azure Kubernetes Service (AKS)" topic, which walks through standing up the cluster on Azure. [Learn more →](https://ampersandtarski.github.io/rap)

### Testing & regression testing

**How do I run the regression test suite locally?**
Catch breakage before CI does: run `bash stacktest.sh`, which sets up PHP and MariaDB and then runs `stack test` (equivalently `ampersand test testing`). You can also run `stack test --flag ampersand:buildAll`. [Learn more →](../regression-testing.md)

**How does the regression runner decide what to test and what passes?**
So you know what "green" means: it walks `testing/` for directories with a `testinfo.yaml`, runs each command against every `.adl`/`.archimate` script, and passes when the actual exit code matches the expected `exitcode`. [Learn more →](../regression-testing.md)

**How do I add a new regression test for a bug fix?**
Lock in your fix: create a directory under `testing/Travis/testcases/`, add your `.adl` files and a `testinfo.yaml` with the expected behaviour, and make sure it passes before opening a PR. [Learn more →](../the-tools-we-use/testing-infrastructure.md)

**How can I park a test case that fails due to a pre-existing limitation?**
Don't let a known-broken case block the build: move it (and its companion files) to `testing/StillUnsupported/`, which has no `testinfo.yaml`, so its scripts are walked but never executed. [Learn more →](../regression-testing.md)

**What is the difference between Travis tests and Sentinel tests?**
Know which tests gate your PR: Travis tests are regression tests that must pass, while Sentinel tests document known issues expected to fail and don't block development. [Learn more →](../the-tools-we-use/testing-infrastructure.md)

**How is RAP tested, and what does it test?**
RAP uses the Gatling framework to simulate user behaviour, exercising flows like login, registration, and creating and compiling a script. [Learn more →](https://ampersandtarski.github.io/rap/tests)

**How are RAP's Gatling tests structured?**
To extend them, follow the liatrio folder layout (common, requests, scenarios, simulations); a simulation sets up an HTTP protocol and injects users into a scenario of HTTP requests. [Learn more →](https://ampersandtarski.github.io/rap/tests)

### Architecture, design & governance

**What does the architecture of a generated application look like?**
The big picture before you extend it: Ampersand generates a monolithic web application with a stateful database service and a stateless service of interfaces, fronted by an API and an MVC-style front-end. [Learn more →](../reference-material/architecture-of-an-ampersand-application.md)

**Where does the compiler describe that API as OpenAPI?**
`Ampersand.Output.ToJSON.OpenAPI` builds `generics/openapi.json` from the FSpec, alongside the other `generics/*.json` generators, and `doGenBackend` writes it unless you pass `--no-openapi`. A separate page explains the mapping it applies, from interfaces and CRUD rights to paths and schemas, and how to extend it. [Learn more →](../the-tools-we-use/openapi-generation.md)

**What design principles underlie the Ampersand language?**
These principles guide what a "good" contribution looks like: Ampersand favours constraints over obligations, reliable relation-algebraic semantics, automated design, and working systems over comprehensive documentation, supporting incremental development. [Learn more →](../reference-material/design-considerations.md)

**What problem does ExecEngine rule automation solve in the compiler?**
It is the heart of a major compiler feature: deriving the violation-repair code of an automated rule automatically, instead of writing ExecEngine code by hand. [Learn more →](../conceptual/automated-rules.md)

**How do I write an automated rule the ExecEngine will restore?**
The pattern you'll reuse: give the rule a `ROLE ExecEngine MAINTAINS` clause and a `VIOLATION` with EX/InsPair/DelAtom actions that fix the data. [Learn more →](../conceptual/automated-rules.md)

**How does the compiler detect when automated rules might oscillate?**
A design-time analysis you can extend: the compiler builds a signed, refined rule-level triggering graph and warns when ExecEngine rules form a cycle through a delete or merge that may never reach a fixpoint ("Maximum reruns exceeded"). The reasoning, the design decisions and the planned Stage 2 are written up as unpublished research. [Learn more →](../ongoing-research/making-oscillation-risk-visible.md)

**How can I conditionally include or exclude parts of a script?**
Toggle code fragments with the preprocessor's `--#IF` / `--#IFNOT` / `--#ELSE` / `--#ENDIF` directives and variables set in INCLUDE statements. [Learn more →](../reference-material/the-preprocessor.md)

**What is on the roadmap, and what's planned next?**
See where you can make an impact: planned work includes low-code Ampersand, schema-changing data migration, NoSQL storage, OpenAPI documentation, OWL/RDFS input, GraphQL, and Kleene operators. [Learn more →](../future-plans.md)

**How could Kleene star and plus operators be added to the language?**
A concrete extension idea: precompile each Kleene term into a new relation plus ENFORCE rules that maintain the transitive closure. [Learn more →](../future-plans.md)

**How is the project governed and who decides?**
Set expectations on how change happens: it is a volunteer-driven, non-commercial research project where a core team of Stef, Han and Michiel Joosten/Stornebrink makes the decisions, valuing maintainability above all. [Learn more →](./1-interested-visitor.md)

**What do contributors agree to regarding rights and compensation?**
Know the deal before you commit: contributors subscribe to the ownership philosophy, waive financial rights on their contributions, and receive no compensation from the project. [Learn more →](../ownership/README.md)

### Legacy & older setups

These pages describe older recipes (RAP4-era or dated drafts). They may be out of date — treat them as historical reference rather than current practice.

**How were the Ampersand and RAP4 Docker images baked and published to Docker Hub?**
The older recipe: run `build.sh` to build the Ampersand base image, build the RAP image on top, then `docker push` to Docker Hub. [Learn more →](../the-tools-we-use/making-docker-images.md)

**How do I set up a local Kubernetes cluster with Minikube?**
This dated (2017, under-construction) draft installs kubectl, docker, minikube and virtualbox, runs `minikube start`, then builds, pushes and deploys images. [Learn more →](../the-tools-we-use/deploying-with-kubernetes.md)

**How do I configure my laptop so localhost behaves like a top-level domain?**
For older local testing setups, ensure localhost and its subdomains route to 127.0.0.1, using a local DNS server (e.g. dnsmasq) or by editing the hosts file. [Learn more →](../the-tools-we-use/building/testing-with-docker-on-your-own-laptop.md)

**How do I set up and extend the SpecFlow tests for RAP4?**
Using the older SpecFlow harness: open VS2022 with Docker Desktop, install the listed NuGet packages, run RAP in Docker, then load `SpecFlowRAP.sln` and run the tests. [Learn more →](../the-tools-we-use/building/testing-with-docker-on-your-own-laptop.md)


Ready to contribute? Start with the [onboarding guide](../guides/onboarding.md), and when you're ready to document your work, follow the [documentation guide](../../prototype/guides/documenting-prototype-changes.md).

Welcome aboard! 🎉
