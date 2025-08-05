# Ampersand Product Context & Resource Map

This document provides a comprehensive mapping of common user questions to specific Ampersand resources, enabling efficient navigation of the Ampersand ecosystem for any user inquiry.

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

**Why Access These:**
- DevContainer provides fastest setup for development (works with VS Code/Codespaces)
- Docker approach for production/testing environments
- Installation guide covers native installation methods

### "How do I learn Ampersand syntax and language?"

**Primary Resources:**
- `docs/reference-material/syntax-of-ampersand.md` - **THE definitive syntax reference** (comprehensive)
- `docs/tutorial-rap4.md` - **Main tutorial designed for students**
- `docs/reference-material/terms.md` - **Term expressions and operators**
- `docs/reference-material/interfaces.md` - **Interface definitions and layouts**
- `docs/video-tutorials.md` & `docs/videos.md` - Video learning resources
- `docs/reference-material/how-to-read-syntax-statements.md` - Meta-learning guide

**Why Access These:**
- Syntax reference is exhaustive - covers all language constructs with examples
- Tutorial provides structured learning path with practical exercises
- Terms and interfaces docs essential for understanding core concepts
- Video resources for visual learners

### "How do I use Ampersand command-line tools?"

**Primary Resources:**
- `commands.md` - **Complete CLI reference** with version 4.0+ changes (ESSENTIAL)
- `docs/the-command-line-tool.md` - Usage examples and patterns
- `ReleaseNotes.md` - **Recent changes and breaking changes**

**Why Access These:**
- `commands.md` documents the major CLI restructuring from v4.0+ (commands vs options)
- Shows which old options map to new commands (backwards compatibility info)
- Release notes essential for understanding version-specific behavior

### "What are best practices and how do I avoid common pitfalls?"

**Primary Resources:**
- `docs/guides/best-practices.md` - **Recommended approaches**
- `docs/guides/frequently-asked-questions.md` - **Common issues and solutions**
- `docs/troubleshooting.md` - **Error resolution guide**
- `docs/reference-material/design-considerations.md` - Architecture guidance

**Why Access These:**
- FAQ addresses most common user stumbling blocks
- Best practices prevent common design mistakes
- Troubleshooting guide has specific error scenarios and solutions

### "How do I work with Excel files and data import?"

**Primary Resources:**
- `docs/the-excel-importer.md` - **Excel integration guide**
- `docs/reference-material/syntax-of-ampersand.md` (POPULATION section) - **Excel population syntax**
- Example .xlsx files in test directories for format reference

**Why Access These:**
- Excel integration is a major Ampersand feature for data import/export
- Syntax guide shows exact format requirements for Excel files
- Examples provide working templates

### "How do I create user interfaces and prototypes?"

**Primary Resources:**
- `docs/reference-material/interfaces.md` - **Complete interface reference**
- `docs/tutorial/interfaces.md` - **Interface tutorial**
- `AmpersandData/PrototypeContext/` - **Interface examples and patterns**
- `docs/reference-material/architecture-of-an-ampersand-application.md` - **Application architecture**

**Why Access These:**
- Interface reference covers all template types (FORM, TABLE, TABS, etc.)
- PrototypeContext shows real working interface examples
- Architecture guide explains how frontend/backend interact

### "How does Ampersand work internally / Architecture questions?"

**Primary Resources:**
- `docs/reference-material/architecture-of-an-ampersand-application.md` - **System architecture**
- `src/` directory structure - **Haskell compiler source code**
- `ArchitectureAndDesign/` - **Design documents and diagrams**
- `docs/reference-material/design-considerations.md` - **Design philosophy**

**Why Access These:**
- Architecture doc explains generated application structure
- Source code for deep understanding of compiler behavior
- Design docs provide historical context and decision rationale

## Advanced Topics & Specialized Resources

### Code Generation & Compilation

**Resources:**
- `src/Ampersand/FSpec/` - **Functional specification generation**
- `src/Ampersand/Core/` - **Core compiler functionality**
- `outputTemplates/` - **Document generation templates**
- `docs/reference-material/configuring-your-application.md` - **Runtime configuration**

### Formal Methods & Theory

**Resources:**
- `docs/research.md` - **Academic background**
- `AmpersandData/FormalAmpersand/` - **Metamodel definitions**
- `docs/reference-material/truth.md` - **Logical foundations**
- Testing examples in `testing/Travis/testcases/` - **Formal verification examples**

### Example Models & Learning

**Resources:**
- `testing/Travis/testcases/` - **Comprehensive test cases** (hundreds of examples)
- `AmpersandData/` - **Working model examples**
- `docs/Examples.md` - **Curated example list**
- `try.adl` - **Quick experiment file**

**Key Subdirectories:**
- `testing/Travis/testcases/Misc/` - General examples
- `testing/Travis/testcases/FuncSpec/` - Function specification tests
- `AmpersandData/PrototypeContext/` - Interface patterns
- `AmpersandData/Semantics/` - Semantic model examples

### Development & Contribution

**Resources:**
- `package.yaml` & `stack.yaml` - **Haskell build configuration**
- `testing/` - **Test framework and examples**
- `.github/workflows/` - **CI/CD pipeline**
- `docs/the-tools-we-use/` - **Development toolchain**

## Version & Release Information

### Current Status & Changes
- **Current Version Pattern:** v5.x.x (see `ReleaseNotes.md`)
- **Major Change:** v4.0.0 introduced command-based CLI (breaking change)
- **Latest Features:** Turtle file parsing, namespace support, transitive closures

### Migration & Compatibility
- `commands.md` - **v4.0+ CLI migration guide**
- `ReleaseNotes.md` - **Breaking changes by version**
- Version-specific behavior documented in release notes

## Quick Reference Patterns

### For Beginners:
1. Start with `docs/intro.md` to identify learning path
2. Use `docs/tutorial-rap4.md` for structured learning
3. Reference `docs/reference-material/syntax-of-ampersand.md` for syntax questions

### For Developers:
1. Setup: `.devcontainer/` or `docs/guides/installing-ampersand.md`
2. CLI: `commands.md` for all command-line operations
3. Interfaces: `docs/reference-material/interfaces.md` + examples in `AmpersandData/`

### For Troubleshooting:
1. `docs/troubleshooting.md` for common issues
2. `docs/guides/frequently-asked-questions.md` for FAQ
3. `ReleaseNotes.md` for version-specific problems

### For Advanced Users:
1. `docs/reference-material/architecture-of-an-ampersand-application.md` for architecture
2. `src/` directory for compiler internals
3. `testing/Travis/testcases/` for comprehensive examples

## Resource Access Priorities

### **Tier 1 (Essential):**
- `docs/reference-material/syntax-of-ampersand.md` - Complete language reference
- `commands.md` - CLI reference with v4.0+ changes
- `docs/intro.md` - User persona guidance
- `ReleaseNotes.md` - Version-specific information

### **Tier 2 (Common Needs):**
- `docs/reference-material/interfaces.md` - UI development
- `docs/tutorial-rap4.md` - Structured learning
- `docs/troubleshooting.md` - Problem solving
- `AmpersandData/PrototypeContext/` - Working examples

### **Tier 3 (Specialized):**
- `testing/Travis/testcases/` - Comprehensive examples
- `src/` - Compiler implementation
- `docs/research.md` - Theoretical background
- Architecture and design documents

## Search Strategies

### Main Documentation Site: https://ampersandtarski.github.io/
- **Full-text search across all repositories**
- **Consolidated view of all documentation**
- **Most up-to-date content**

### Local File Navigation:
- Use `docs/` for user-facing documentation
- Use `src/` for implementation details
- Use `testing/` for examples and test cases
- Use `AmpersandData/` for working models

This resource map enables quick, targeted responses to any Ampersand user question by directing them to the most relevant and comprehensive resources available.
