# Ampersand Product Context & Resource Map

## Why this project exists

### Core Problem: From Specification to Executable Application
This project delivers a **prototype framework for Ampersand** that bridges the gap between formal specification and working software. It enables:

- **Ampersand ADL scripts** to be automatically converted to **fully working web applications**
- **Prototypes to run on any machine** (that supports Docker)  
- **Quick validation** of business requirements through stakeholder feedback
- **Model-driven development** where code is automatically generated from rules

### Academic and Practical Motivation
- **Education:** Open Universiteit Nederland course "Rule Based Design"
- **Research:** Scientific research into formal methodologies (collaboration TNO/Sopra Steria)
- **Industry:** Proof-of-concept development and requirements engineering
- **Methodology:** Rule-based design of information systems

## What problem does it solve?

### 1. The Execution Problem
**Problem:** An Ampersand script specifies an information system, but you can't execute a specification.
**Solution:** The prototype framework receives input from the Ampersand compiler and turns it into a working application.

### 2. The Validation Problem  
**Problem:** Stakeholders find it difficult to give feedback on abstract specifications.
**Solution:** Working prototypes that users can directly interact with.

### 3. The Deployment Problem
**Problem:** Complex setup required for testing Ampersand models.
**Solution:** Docker-based deployment - one command for complete working application.

### 4. The Development Cycle Problem
**Problem:** Long feedback loops between specification and working software.
**Solution:** Automatic code generation - from ADL script to web app in minutes.

### 5. The Educational Problem
**Problem:** Students have difficulty learning from abstract rule specifications.
**Solution:** Immediately visible results of their Ampersand models.

## How it works

### Technical Architecture

#### 1. Docker-based Deployment
- **Framework as Docker image** - standardized runtime environment
- **FROM basis** in project Dockerfiles for automatic deployments  
- **Portable** - runs on any machine with Docker support

#### 2. Code Generation Pipeline
```
ADL Script → Ampersand Compiler → Generated Code → Framework → Web Application
```

**Input:** Ampersand ADL (Ampersand Definition Language) files
**Processing:** Framework processes generated PHP backend + Angular frontend
**Output:** Fully working web application with database

#### 3. Multi-tier Architecture

**Frontend (Angular + TypeScript)**
- Automatically generated user interfaces from relations
- Responsive design with SCSS styling
- Component architecture:
  - Atomic components (reusable UI elements)
  - Box components (forms, tables, data views)
  - Layout system (menu, topbar, sidebar)
  - Admin interface for system management

**Backend (PHP + MySQL)**
- RESTful API endpoints (`/backend/public/api/v1/`)
- Execution Engine for business rule enforcement
- Automatic database schema generation
- Bootstrap framework for initialization

**Database Layer**
- MySQL/MariaDB with auto-generated schemas
- Relational structure derived from Ampersand concepts
- Automatic constraints and business rules

### Framework Engineering

#### Adaptability
**Repository goal:** Software engineers can adapt the framework themselves
- Modify code-generation templates
- Extend UI components
- Performance optimizations
- Add framework features

#### Extensibility
- **Template system** for custom UI generation
- **Plugin architecture** for custom business logic
- **API endpoints** for external integrations
- **Configuration management** via YAML files

## Core User Questions & Primary Resources

### "What is Ampersand and why should I use it?"

**Primary Resources:**
- `docs/intro.md` - **Essential starting point** with persona-based guidance (students, professionals, researchers, etc.)
- `README.md` - Quick project overview with build status and links
- Main docs: https://ampersandtarski.github.io/ - **Comprehensive documentation site** with full-text search
- `docs/research.md` - Academic foundation and formal methods background

**Why Access These:**
- `intro.md` helps users quickly identify if Ampersand fits their needs and directs them to appropriate learning paths
- The main docs site provides searchable, consolidated documentation across repositories
- Research background helps users understand the theoretical foundation (relation algebra)

### "How do I install/setup Ampersand?"

**Primary Resources:**
- `docs/guides/installing-ampersand.md` - **Installation instructions**
- `.devcontainer/devcontainer.json` - **Pre-configured development environment**
- `Dockerfile` - Container-based deployment
- `docs/guides/onboarding.md` - Getting started workflow
- `docs/docker/` - Docker-specific guidance

### "How do I learn Ampersand syntax and language?"

**Primary Resources:**
- `docs/reference-material/syntax-of-ampersand.md` - **THE definitive syntax reference**
- `docs/tutorial-rap4.md` - **Main tutorial designed for students**
- `docs/reference-material/terms.md` - **Term expressions and operators**
- `docs/reference-material/interfaces.md` - **Interface definitions and layouts**
- `docs/video-tutorials.md` & `docs/videos.md` - Video learning resources

### "How do I use Ampersand command-line tools?"

**Primary Resources:**
- `commands.md` - **Complete CLI reference** with version 4.0+ changes
- `docs/the-command-line-tool.md` - Usage examples and patterns
- `ReleaseNotes.md` - **Recent changes and breaking changes**

### "How do I work with Excel files and data import?"

**Primary Resources:**
- `docs/the-excel-importer.md` - **Excel integration guide**
- `docs/reference-material/syntax-of-ampersand.md` (POPULATION section) - **Excel population syntax**
- Example .xlsx files in test directories for format reference

### "How do I create user interfaces and prototypes?"

**Primary Resources:**
- `docs/reference-material/interfaces.md` - **Complete interface reference**
- `docs/tutorial/interfaces.md` - **Interface tutorial**
- `AmpersandData/PrototypeContext/` - **Interface examples and patterns**
- `docs/reference-material/architecture-of-an-ampersand-application.md` - **Application architecture**

## Value Proposition

### For Students
- **Immediate feedback** on their Ampersand models
- **Visual learning process** - from abstract rules to concrete application
- **Hands-on experience** with model-driven development

### For Scientists  
- **Research platform** for formal methodologies
- **Experimental environment** for new Ampersand features
- **Publication support** with working demonstrations

### For Industry
- **Rapid prototyping** for business requirement validation
- **Stakeholder engagement** via working demos
- **Risk reduction** through early validation of business logic
- **Time-to-market** acceleration through automatic code generation

### For Developers
- **No manual UI coding** - automatically generated
- **Consistent architecture** - standardized patterns
- **Deployment automation** - Docker-based packaging
- **Framework extensibility** - adaptable for specific needs

## Product Position in Ampersand Ecosystem

### Upstream Dependencies
- **Ampersand Compiler** (Haskell) - generates code for this framework
- **ADL Language** - input specification format
- **Business Rules** - encoded in relational algebra

### Downstream Consumers  
- **Project Templates** - use this framework as basis
- **Student Projects** - Open Universiteit course projects
- **Research Prototypes** - academic research applications
- **Industry POCs** - commercial proof-of-concepts

### Parallel Tools
- **RAP (Rule-based Analysis & Prototyping)** - alternative web-based interface
- **VS Code Extension** - development environment support
- **Documentation Site** - user documentation and tutorials

## Current Status & Roadmap

### Active Development (August 2025)
- **38 open issues** for framework improvements
- **3 "good first issue"** labels for new contributors
- **MIT license** for open-source adoption
- **Small but active community**

### Technical Improvements
- Continuous integration/deployment optimizations
- Framework performance and stability
- Expanded documentation via GitHub Pages
- Improved VS Code development experience

This product context shows how the Ampersand Prototype Framework plays a crucial role in realizing the Ampersand methodology - from academic theory to practical, working software.
