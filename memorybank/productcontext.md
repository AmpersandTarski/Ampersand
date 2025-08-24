## Waarom bestaat dit project?

### Kernprobleem: Van Specificatie naar Executeerbare Applicatie
Dit project levert een **prototype framework voor Ampersand** dat de kloof overbrugt tussen formele specificatie en werkende software. Het maakt het mogelijk om:

- **Ampersand ADL scripts** automatisch om te zetten naar **volledig werkende web applicaties**
- **Prototypes te draaien op willekeurige machines** (die Docker ondersteunen)  
- **Snelle validatie** van business requirements door stakeholder feedback
- **Model-driven development** waar code automatisch gegenereerd wordt uit regels

### Academische en Praktische Motivatie
- **Onderwijs:** Open Universiteit Nederland course "Rule Based Design"
- **Research:** Wetenschappelijk onderzoek naar formele methodieken (samenwerking TNO/Sopra Steria)
- **Industry:** Proof-of-concept ontwikkeling en requirements engineering
- **Methodologie:** Rule-based design van informatiesystemen

## Welk probleem lost het op?

### 1. Het Executie Probleem
**Probleem:** Een Ampersand script specificeert een informatiesysteem, maar een specificatie kun je niet executeren.
**Oplossing:** Het prototype framework ontvangt input van de Ampersand compiler en maakt er een werkende applicatie van.

### 2. Het Validatie Probleem  
**Probleem:** Stakeholders kunnen moeilijk feedback geven op abstracte specificaties.
**Oplossing:** Werkende prototypes waar gebruikers direct mee kunnen interacteren.

### 3. Het Deployment Probleem
**Probleem:** Complexe setup vereist voor het testen van Ampersand modellen.
**Oplossing:** Docker-based deployment - één commando voor complete werkende applicatie.

### 4. Het Development Cyclus Probleem
**Probleem:** Lange feedback loops tussen specificatie en werkende software.
**Oplossing:** Automatische code generatie - van ADL script naar web app in minuten.

### 5. Het Educatieve Probleem
**Probleem:** Studenten kunnen moeilijk leren van abstracte regel-specificaties.
**Oplossing:** Direct zichtbare resultaten van hun Ampersand modellen.

## Hoe werkt het?

### Technische Architectuur

#### 1. Docker-based Deployment
- **Framework als Docker image** - gestandaardiseerde runtime omgeving
- **FROM basis** in project Dockerfiles voor automatische deployments  
- **Portable** - draait op elke machine met Docker ondersteuning

#### 2. Code Generatie Pipeline
```
ADL Script → Ampersand Compiler → Generated Code → Framework → Web Application
```

**Input:** Ampersand ADL (Ampersand Definition Language) bestanden
**Processing:** Framework verwerkt gegenereerde PHP backend + Angular frontend
**Output:** Volledig werkende web applicatie met database

#### 3. Multi-tier Architecture

**Frontend (Angular + TypeScript)**
- Automatisch gegenereerde user interfaces uit relaties
- Responsive design met SCSS styling
- Component architectuur:
  - Atomic components (herbruikbare UI elementen)
  - Box components (forms, tables, data views)
  - Layout systeem (menu, topbar, sidebar)
  - Admin interface voor systeem management

**Backend (PHP + MySQL)**
- RESTful API endpoints (`/backend/public/api/v1/`)
- Execution Engine voor business rule enforcement
- Automatische database schema generatie
- Bootstrap framework voor initialisatie

**Database Layer**
- MySQL/MariaDB met auto-generated schema's
- Relationele structuur afgeleid van Ampersand concepten
- Automatische constraints en business rules

#### 4. Development Workflow
1. **Modeling:** Schrijf Ampersand ADL script met business rules
2. **Generation:** Ampersand compiler genereert PHP/Angular code  
3. **Integration:** Framework integreert gegenereerde code
4. **Deployment:** Docker container met complete applicatie
5. **Testing:** Werkende web interface voor stakeholder validatie
6. **Iteration:** Aanpassingen in ADL → automatische regeneratie

### Framework Engineering

#### Aanpasbaarheid
**Repository doel:** Software engineers kunnen het framework zelf aanpassen
- Modificatie van code generatie templates
- Uitbreiding van UI componenten  
- Performance optimalisaties
- Framework feature toevoegingen

#### Extensibiliteit
- **Template systeem** voor custom UI generatie
- **Plugin architectuur** voor custom business logic
- **API endpoints** voor externe integraties
- **Configuration management** via YAML bestanden

## Product Positie in Ampersand Ecosysteem

### Upstream Dependencies
- **Ampersand Compiler** (Haskell) - genereert code voor dit framework
- **ADL Language** - input specificatie formaat
- **Business Rules** - gecodeerd in relationele algebra

### Downstream Consumers  
- **Project Templates** - gebruiken dit framework als basis
- **Student Projects** - Open Universiteit cursus projecten
- **Research Prototypes** - academisch onderzoek toepassingen
- **Industry POCs** - commerciële proof-of-concepts

### Parallel Tools
- **RAP (Rule-based Analysis & Prototyping)** - alternatieve web-based interface
- **VS Code Extension** - development omgeving ondersteuning
- **Documentation Site** - gebruikers documentatie en tutorials

## Waardepropostitie

### Voor Studenten
- **Onmiddellijke feedback** op hun Ampersand modellen
- **Visueel leerproces** - van abstracte regels naar concrete applicatie
- **Hands-on ervaring** met model-driven development

### Voor Wetenschappers  
- **Research platform** voor formele methodieken
- **Experimentele omgeving** voor nieuwe Ampersand features
- **Publicatie ondersteuning** met werkende demonstraties

### Voor Industry
- **Rapid prototyping** voor business requirement validation
- **Stakeholder engagement** via werkende demos
- **Risk reduction** door vroege validatie van business logica
- **Time-to-market** versnelling door automatische code generatie

### Voor Developers
- **Geen handmatige UI coding** - automatisch gegenereerd
- **Consistente architectuur** - gestandaardiseerde patterns
- **Deployment automation** - Docker-based packaging
- **Framework extensibility** - aanpasbaar voor specifieke needs

## Huidige Status & Roadmap

### Actieve Ontwikkeling (Augustus 2025)
- **38 open issues** voor framework verbeteringen
- **3 "good first issue"** labels voor nieuwe contributors
- **MIT licentie** voor open source adoptie
- **Kleine maar actieve community**

### Technische Improvements
- Continuous integration/deployment optimalisaties
- Framework performance en stabiliteit
- Uitgebreide documentatie via GitHub Pages  
- Verbeterde VS Code development experience

Deze product context toont hoe het Ampersand Prototype Framework een cruciale rol speelt in het realiseren van de Ampersand methodologie - van academische theorie naar praktische, werkende software.
