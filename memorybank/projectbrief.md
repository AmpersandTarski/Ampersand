# Ampersand Project Brief

## Project Overview

**Project Name:** Ampersand Compiler & Ecosystem
**Organization:** AmpersandTarski (GitHub)
**Type:** Academic research platform + production-ready compiler toolchain
**Repository:** https://github.com/AmpersandTarski/Ampersand
**License:** GPL-3.0
**Status:** Actively developed (latest update: August 2025)

## Core Mission

This GitHub repository, AmpersandTarski/Ampersand, contains the source code of the Ampersand compiler.
It compiles and deploys an Ampersand script into software that serves as a prototype of the specified system.
It can also document that system by generating text, based on text fragments specified in the Ampersand script.
The purpose of Ampersand is to design information systems in a mathematically precise way, to speed up development and to prevent errors in the software as much as possible.

## AmpersandTarski Organization Overview

### Core Repositories
1. **Ampersand** - Main compiler (Haskell, GPL-3.0, 43⭐, 118 issues)
   - Source code of the Ampersand compiler
   - DevContainer and Docker support
   - Automatic builds to Docker Hub

2. **prototype** - Prototype Framework (CSS/PHP/Angular, MIT, 1⭐, 38 issues) 
   - Framework to convert Ampersand-scripts into working prototypes
   - Transforms Ampersand models into web applications
   - **Related but separate from this compiler project**

3. **project-template** - Project Template (Shell, 2⭐)
   - Boilerplate code for new prototypes
   - Clone and add Ampersand code

4. **RAP** - Rule-based Analysis & Prototyping (C#, 4⭐, 76 issues)
   - Web-based tool for Ampersand model analysis
   - Generates functional specifications
   - Primary education tool Open Universiteit Nederland

5. **Ampersand-Language-Support** - VS Code Extension (TypeScript, GPL-3.0, 5⭐)
   - Ampersand syntax support for VS Code
   - Installable via VS Code extensions

### Supporting Repositories
- **ampersand-models** - Example model collection (3⭐)
- **Project-administration** - Demo project for testing
- **AmpersandTarski.github.io** - Docusaurus documentation site
- **Publications** - LaTeX sources of academic publications

### Technical Stack Organization
- **Primary Languages:** Haskell, CSS, Shell, JavaScript, HTML
- **Primary Tools:** Docker, VS Code, Docusaurus
- **Deployment:** Docker Hub, GitHub Pages

## Domain Context

### Academic Background
- **Institution:** Open Universiteit Nederland
- **Collaboration:** TNO and Sopra Steria
- **Education:** Course "Rule Based Design"
- **Target Groups:** Students, researchers, Ampersand users, contributors

### Ampersand Methodology
Ampersand is a methodology and toolset for:
- **Rule-based design** of information systems
- **Model-driven development** - from specification to working application
- **Automatic code generation** of web interfaces
- **Relational algebra** as basis for business rules
- **Prototype generation** for validation of business requirements

### Compiler Role
The Ampersand compiler transforms:
- **Input:** Ampersand ADL (Ampersand Definition Language) scripts
- **Output:** Generated code for prototype frameworks and documentation
- **Benefits:** 
  - Mathematical precision in system design
  - Automatic validation of business rules
  - Generated documentation from specifications
  - Error prevention through formal methods

### Use Cases
1. **Educational:** Students learn rule-based design
2. **Proof of Concept:** Quick validation of business ideas  
3. **Requirements Engineering:** Stakeholder feedback on working prototypes
4. **Research:** Scientific research into formal methodologies

## Technical Architecture

### Compiler Core (Haskell)
- **Language:** Haskell with extensive type system
- **Build System:** Stack with Cabal integration
- **Extensions:** NoImplicitPrelude, OverloadedStrings
- **Output:** PHP/Angular code generation, documentation generation

### Development Environment
- **GHC Version:** 9.6.6
- **Stack LTS:** 22.39
- **DevContainer:** VS Code integration for standardized development
- **Docker:** Containerized builds and deployment

### Code Generation Targets
- **Web Applications:** PHP backend + Angular frontend
- **Documentation:** Multiple formats via Pandoc (LaTeX, HTML, etc.)
- **Database Schema:** MySQL/MariaDB with automatic constraints
- **Validation:** Business rule enforcement in generated code

## Current Status

### Active Development
- **Latest updates:** August 2025
- **Issues:** 118 open issues in compiler repo
- **Contributing:** Community-driven development
- **Integration:** Multi-repository ecosystem coordination

### Project Position
This **Ampersand Compiler** is the **core transformation engine** that enables the broader Ampersand methodology:
- **Research Foundation:** Formal methods and relational algebra
- **Educational Platform:** Hands-on learning with immediate feedback
- **Industry Application:** Rapid prototyping and requirements validation
- **Technical Foundation:** Type-safe code generation and documentation

The compiler's success directly enables adoption of rule-based design methodology across academic, research, and industry domains.
