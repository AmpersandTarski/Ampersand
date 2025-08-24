## Project Overzicht

**Project Naam:** Ampersand Prototype Framework
**Organisatie:** AmpersandTarski (op GitHub)
**Type:** Full-stack web application framework (Angular frontend + PHP backend)
**Locatie:** /Users/sjo00577/git/Prototype-framework
**Git Remote:** https://github.com/AmpersandTarski/prototype.git  
**Licentie:** MIT
**Status:** Actief ontwikkeld (laatste update: augustus 2025)

## Organisatie Overview - AmpersandTarski

### Kernrepositories
1. **Ampersand** - Hoofdcompiler (Haskell, GPL-3.0, 43⭐, 118 issues)
   - Source code van de Ampersand compiler
   - DevContainer en Docker ondersteuning
   - Automatische builds naar Docker Hub

2. **prototype** - Prototype Framework (CSS/PHP/Angular, MIT, 1⭐, 38 issues) 
   - Framework om Ampersand-scripts om te zetten in werkende prototypes
   - Transformeert Ampersand modellen naar web applicaties
   - **DIT IS HET HUIDIGE PROJECT**

3. **project-template** - Project Template (Shell, 2⭐)
   - Boilerplate code voor nieuwe prototypes
   - Clone en voeg Ampersand code toe

4. **RAP** - Rule-based Analysis & Prototyping (C#, 4⭐, 76 issues)
   - Web-based tool voor Ampersand model analyse
   - Genereert functionele specificaties
   - Primair onderwijs tool Open Universiteit Nederland

5. **Ampersand-Language-Support** - VS Code Extensie (TypeScript, GPL-3.0, 5⭐)
   - Ampersand syntax ondersteuning voor VS Code
   - Installeerbaar via VS Code extensions

### Ondersteunende Repositories
- **ampersand-models** - Voorbeeldmodellen collectie (3⭐)
- **Project-administration** - Demo project voor testing
- **AmpersandTarski.github.io** - Docusaurus documentatie site
- **Publications** - LaTeX bronnen van academische publicaties

### Technische Stack Organisatie
- **Hoofdtalen:** Haskell, CSS, Shell, JavaScript, HTML
- **Primaire Tools:** Docker, VS Code, Docusaurus
- **Deployment:** Docker Hub, GitHub Pages

## Domein Context

### Academische Achtergrond
- **Institutie:** Open Universiteit Nederland
- **Samenwerking:** TNO en Sopra Steria
- **Onderwijs:** Course "Rule Based Design"
- **Doelgroepen:** Studenten, wetenschappers, Ampersand gebruikers, contributors

### Ampersand Methodologie
Ampersand is een methodologie en toolset voor:
- **Rule-based design** van informatiesystemen
- **Model-driven development** - van specificatie naar werkende applicatie
- **Automatische codegeneratie** van web interfaces
- **Relationele algebra** als basis voor business rules
- **Prototype generatie** voor validatie van business requirements

### Prototype Framework Doel
Het prototype framework transformeert:
- **Input:** Ampersand ADL (Ampersand Definition Language) scripts
- **Output:** Volledig werkende web applicaties
- **Voordelen:** 
  - Snelle validatie van business requirements
  - Automatische database schema generatie
  - Gebruikersinterfaces gegenereerd uit relaties
  - Business rule enforcement

### Use Cases
1. **Educatief:** Studenten leren rule-based design
2. **Proof of Concept:** Snelle validatie van business ideeën  
3. **Requirements Engineering:** Stakeholder feedback op werkende prototypes
4. **Research:** Wetenschappelijk onderzoek naar formele methodieken

## Technische Architectuur

### Frontend (Angular)
- **Locatie:** `/frontend/`
- **Framework:** Angular met TypeScript
- **Styling:** SCSS, responsive design
- **Componenten:** 
  - Atomic components (herbruikbare UI elementen)
  - Box components (gegenereerde forms/tables)
  - Layout systeem met menu/topbar/sidebar
  - Admin interface voor systeem management

### Backend (PHP)
- **Locatie:** `/backend/`
- **API:** RESTful endpoints in `/backend/public/api/v1/`
- **Bootstrap:** Framework initialisatie en configuratie
- **Execution Engine:** Business rule enforcement
- **Database:** Automatisch gegenereerde schema's uit Ampersand modellen

### Deployment
- **Containerization:** Docker + Docker Compose
- **Development:** DevContainer ondersteuning voor VS Code
- **Apache:** Web server configuratie
- **Database:** MySQL/MariaDB met automatische scripts

### Development Workflow
1. **Model:** Schrijf Ampersand ADL script
2. **Generate:** Compileer naar PHP/Angular code
3. **Deploy:** Docker container met complete applicatie  
4. **Test:** Werkende web interface voor validatie
5. **Iterate:** Aanpassingen in ADL, herhaal cyclus

## Huidige Status

### Actieve Ontwikkeling
- **Laatste updates:** Augustus 2025
- **Issues:** 38 open issues in prototype repo
- **Contributing:** 3 issues labeled "good first issue"
- **Community:** Kleine maar actieve community

### Roadmap Indicaties
- Continuous integration/deployment verbeteringen
- Framework stabiliteit en performance
- Uitgebreide documentatie via GitHub Pages
- VS Code integration verbetering

## Project Relaties

Dit prototype framework project is onderdeel van het bredere Ampersand ecosysteem:
- **Upstream:** Ampersand compiler genereert code voor dit framework
- **Downstream:** Project templates gebruiken dit framework
- **Parallel:** RAP web tool biedt alternatieve interface
- **Support:** VS Code extensie verbetert development experience
- **Documentation:** Centrale docs site verzamelt alle informatie
