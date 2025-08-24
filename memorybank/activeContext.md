## Current work focus

## Recent changes

## Next steps

## Active decisions and considerations

### Ampersand Ecosystem Architecture
- **Prototype Framework** (this project) = central transformation engine ADL → Web App
- **Upstream dependency:** Ampersand compiler (Haskell) generates code for this framework  
- **Parallel tools:** RAP (C#) provides alternative web interface, VS Code extension supports development
- **Community:** Small but active (38 open issues, 3 "good first issue" labels)

### Technical Stack Choices
- **Frontend:** Angular + TypeScript for generated interfaces
- **Backend:** PHP + MySQL for business logic + data persistence  
- **Deployment:** Docker-first approach for portability
- **Development:** VS Code + DevContainer for standardized dev environment

### Academic vs Industry Balance
- **Primary education tool** Open Universiteit Nederland course "Rule Based Design"
- **Research platform** for formal methodologies (collaboration TNO/Sopra Steria)  
- **Industry application** rapid prototyping and requirements validation
- **MIT license** promotes open source adoption

## Important patterns and preferences

### Code Generation Philosophy
- **Declarative specifications** (ADL) → **Imperative implementations** (PHP/Angular)
- **Business rules in relational algebra** → **Database constraints + execution engine**
- **Automatic UI generation** from data relations → **Consistent user experience**
- **Rapid iteration cycle:** ADL change → regenerate → deploy → validate

### Documentation Standards  
- **Concrete examples** preferred over abstract descriptions
- **Step-by-step workflows** for technical procedures
- **Architecture diagrams** showing data flow: ADL → Compiler → Framework → Web App
- **Multi-audience approach:** students, researchers, industry developers

### Quality Assurance
- **Minimal reproducible examples** required for issue reports
- **Docker reproducibility** for consistent environments across platforms  
- **Template-driven development** for standardized project structures
- **Community contribution** via "good first issue" labeling

## Learnings and project insights

### Ampersand Methodology Success Factors
1. **Visual feedback loop** - Working prototypes make abstract rules concrete for stakeholders
2. **Automatic constraint enforcement** - Database + execution engine prevent invalid data states
3. **Rapid validation cycles** - Minutes from specification change to working demo
4. **Multi-stakeholder alignment** - Students, academics, industry can all use same toolchain

### Framework Architecture Insights
- **Multi-tier separation** allows independent frontend/backend evolution
- **Docker containerization** solves deployment complexity across environments
- **Template-based generation** provides customization while maintaining consistency  
- **RESTful API design** enables future integration with external systems

### Community Development Patterns
- **Academic foundation** provides theoretical rigor and educational use cases
- **Industry collaboration** (TNO/Sopra Steria) brings practical requirements and funding
- **Open source model** (MIT license) encourages contributions and adoption
- **Documentation-first approach** via GitHub Pages reduces adoption barriers

### Current Project Position
This **Prototype Framework** is the **production runtime** that makes Ampersand specifications executable. It sits at the crucial intersection between:
- **Research** (formal methods, rule-based design) 
- **Education** (hands-on learning with immediate feedback)
- **Industry** (rapid prototyping, requirements validation)
- **Technology** (modern web stack, containerized deployment)

The framework's success directly enables the broader Ampersand methodology adoption across these diverse domains.
