# Ampersand

## Introductie
Ampersand is een project van de Open Universiteit, ondersteund door Ordina en TNO. Het is een project dat niet financieel wordt ondersteund.

Het onderzoek van Ampersand is gericht op het generen van informatiesystemen, rechtstreeks uit business rules, door middel van relatie-algebra. De Ampersand compiler is in staat om van een set business rules een database, back-end en front-end te genereren.

- [Documentatie](https://ampersandtarski.github.io/)
- [Documentatie (gitbook version)](https://ampersandtarski.gitbook.io/documentation/)
- [Github project](https://github.com/AmpersandTarski)
- [Docker hub](https://hub.docker.com/u/ampersandtarski)
- [Wiki](https://github.com/AmpersandTarski/Ampersand/wiki)

## RAP
RAP is een speciale applicatie binnen het Ampersand project, daarom wordt deze los toegelicht.

RAP is een Repository voor Ampersand-projecten, waarin studenten prototypes kunnen maken voor informatiesystemen die ze specificeren in Ampersand. Studenten en professionals gebruiken RAP om hun Ampersand-script in te dienen en te bewerken, te compileren, een prototype te genereren en te implementeren. Ze kunnen ook documentatie genereren vanuit hetzelfde Ampersand-script.

RAP is in wezen een applicatie gegenereerd door Ampersand, met een aantal add-ons. De belangrijkste is dat aan RAP de Ampersand compiler is toegevoegd, waardoor nieuwe Ampersand applicaties gegenereerd kunnen worden.

Gebruikers van RAP zijn vooral studenten van de Open Universiteit en geïnteresseerde professionals. De meeste gebruikers zijn nieuwelingen, die gemakkelijk de weg kwijtraken door relatief kleine belemmeringen.

- [RAP4 applicatie Open Universiteit](https://rap.cs.ou.nl/#/page/home)

## Aan de slag
In de [documentatie](https://ampersandtarski.github.io/) vind je alle informatie nodig om het Ampersand project te leren kennen. De RAP applicatie van de Open Universiteit kan gebruikt worden om de applicatie te leren kennen.

Als developer binnen het Ampersand project zul je de applicatie ook lokaal willen draaien. 
1. [The Tools We Use documentation](https://ampersandtarski.github.io/the-tools-we-use)

De verschillende projecten binnen Ampersand hebben een eigen onboarding.

## A-team
Ampersand valt onder de paraplu van het A-team. Een team developers van de area MTech. Veelal Young Professionals en nieuwe Ordina collega's die beschikbaar zijn.

Andere projecten die onder de paraplu van het A-team vallen:
- NUTwente: platform die huisvesting van Oekrainse vluchtelingen in de omgeving Twente ondersteund. Looptijd: februari 2022 tot juli 2022, daarna onderhoudt.
- Boswachter: ontwikkelen van een [website](https://www.ordina.com/who-we-are/social-responsibility/co2-neutral-by-2030/) die bezoekers informeert over de CO2-reductie ambitie van Ordina. Looptijd: december 2022 tot april 2023.

## A-team projecten
De projecten die door het A-team zijn opgepakt:

### Front-end
Het updaten van de Angular library die Ampersand gebruikt om de front-end te genereren. Deze werkt op basis van Angular-JS en wordt geupdate naar Angular versie 14.

- [Repository](https://github.com/AmpersandTarski/prototype-frontend)
- [Project](https://github.com/orgs/AmpersandTarski/projects/5)
- [Onboarding](https://github.com/AmpersandTarski/prototype-frontend/tree/dev/docs)

### Front-end testing
Testen van de front-end met het Robot framework.

- [Repository](https://github.com/AmpersandTarski/prototype-frontend)
- [Project](https://github.com/orgs/AmpersandTarski/projects/8)
- Onboarding

### RAP testing
Testen van de back-end met Gatling framework.

- [Repository](https://github.com/AmpersandTarski/RAP)
- [Project](https://github.com/orgs/AmpersandTarski/projects/4)
- Onboarding


### RAP deployment
Het deployen van de RAP applicatie op een public cloud, bijvoorbeeld AWS of Azure. In kaart brengen welke opties er zijn (Docker en/of Kubernetes), architectuur schetsen en deployment werken krijgen.

- [Repository](https://github.com/AmpersandTarski/RAP)
- [Project](https://github.com/orgs/AmpersandTarski/projects/6)
- [Onboarding](https://github.com/AmpersandTarski/RAP)


# HPT / Scrum
Het team in dit project werkt als een High Performance Team. Een klein multidisciplinair team die de Agile/Scrum methode hanteert.

Zie de scrum gids voor achtergrondinformatie: [Nederlandse versie scrum gids](https://scrumguides.org/docs/scrumguide/v2020/2020-Scrum-Guide-Dutch.pdf)

## Sprint
De sprints zijn 2 weken waarbij op werkdagen altijd een daily stand-up plaatsvind van maximaal een half uur.

Al het noodzakelijke werk wat nodig is om het Product Doel te bereiken, inclusief Sprint Planning, Daily Scrums, Sprint Review en Sprint Retrospective, vindt plaats binnen Sprints. Tijdens de Sprint:
- Worden geen veranderingen aangebracht die het Sprint Doel in gevaar kunnen brengen;
- Neemt de kwaliteit niet af;
- Wordt de Product Backlog naar behoefte verder uitgewerkt;
- Mag de scope worden verduidelijkt en heronderhandeld met de Product Owner naarmate meer
wordt geleerd.

## Sprint overzicht

|       | 1-Ma | 2-Di | 3-Wo | 4-Do | 5-Vr | 6-Ma | 7-Di | 8-Wo | 9-Do | 10-Vr |
| ----- | ---- | ---- | ---- | ---- | ---- | ---- | ---- | ---- | ---- | ----- |
| 9:00  | Zelfstudie | | | | | | | | Zelfstudy | Sprint review |
| 10:00 | Zelfstudie | | | | | | | | Zelfstudy | Planning voorb. |
| 11:00 | Zelfstudie | | | | | | | | Zelfstudy | Sprint planning |
| 11:30 | Wekelijks overleg | Daily | Daily | Daily | Daily | Wekelijks overleg | Daily | Daily | Daily | Daily |
| 13:00 | Zelfstudie | Scrum Poker | | | | | | | Zelfstudy | Sprint retro

Een nieuwe sprint begint bij de sprint planning, de vaste activiteiten in chronologisch volgorde:

### Daily
Het doel van de Daily Scrum is om voortgang richting het Sprint doel te inspecteren en de Sprint Backlog als nodig aan te passen, waarbij het aankomend gepland werk wordt bijgesteld. De Daily Scrum is een gebeurtenis van 15 minuten voor de Developers van het Scrum Team.

### Wekelijks overleg
Tijdens de daily van maandag zijn de product owners aanwezig ter ondersteuning van het proces en voor inhoudelijke vragen.

### Scrum poker
Tijdens scrum poker worden gewicht gehangen aan (nieuwe) backlog items. Gaat alleen door als er backlog items zijn om mee te pokeren.

| Poker | Duur  |
| ----- | ----  |
| 1     | ? uur |
| 2     | ? uur |
| 3     | ? uur |
| 5     | ? uur |
| 8     | ? uur |
| 13    | ? uur |
| 21    | ? uur |

### Sprint review
Het doel van de Sprint Review is om de uitkomst van de Sprint te inspecteren en toekomstige aanpassingen te bepalen. Het Scrum Team presenteert de resultaten van hun werk aan de belangrijkste belanghebbenden en de voortgang richting het Product Doel wordt besproken.

Een wordt een korte powerpoint presentatie voorbereidt, een kopie wordt opgeslagen in Teams *Project A & NUT-Vluchtelingen opensource*. Hier is ook een template te vinden.

Tijdens de sprint review zijn Joost, Han en Michiel aanwezig in de rol van belanghebbende.

### Sprint planning voorbereiding
Het uur voorafgaand van de sprint planning. Naar aanleiding van de sprint review wordt het backlog aangepast en het sprint board voorbereidt. Dit voorkomt dat we tijdens de sprint planning te veel bezig zijn met het backlog / sprint board.

### Sprint planning
Sprint Planning start de Sprint door het uit te voeren werk voor de Sprint uit te stippelen. Het resulterende plan wordt gemaakt door het gezamenlijke werk van het gehele Scrum Team.

Sprint Planning behandelt de volgende onderwerpen:
1. Waarom is deze sprint waardevol?
2. Wat kan deze Sprint worden afgerond?
3. Hoe zal het gekozen werk gedaan worden?

Het Sprint Doel, de Product Backlog items die geselecteerd zijn voor de Sprint, plus het plan hoe ze worden opgeleverd, worden gezamenlijk de Sprint Backlog genoemd.

Tijdens de sprint review zijn Joost, Han en Michiel aanwezig in de rol van Product Owner.

### Sprint retrospective
Het doel van de Sprint Retrospective is om manieren te bedenken om kwaliteit en effectiviteit te verhogen en in te plannen.

Voor de retrospective wordt in de Teams chat *NUTwente ontwikkelteam* een nieuw Whiteboard aangemaakt, met sjabloon *Terugblik zeilboot*. Achtereenvolgend worden sterke punten, successen, obstakels, ankers en doelen/visie ingevuld en gereflecteerd.

Een copy van het Whiteboard wordt opgeslagen in Teams *Project A & NUT-Vluchtelingen opensource*.

### Zelfstudie
Dit project is bedoeld ter ondersteuning van Young professionals en (nieuwe) werknemers van Ordina die beschikbaar zijn. Van Young professionals wordt verwacht dat ze tijd besteden aan zelfstudie.

Elke deelnemer aan het project geeft (per sprint) aan hoeveel tijd hij/zij beschikbaar heeft. Om zelfstudie zo goed mogelijk te faciliteren, zijn er standaard 2 zelfstudiedagen ingepland.

# Scrum team
De fundamentele eenheid van Scrum is een klein team van mensen; een Scrum Team. Normaliter bestaat het Scrum Team uit één Scrum Master, één Product Owner en Developers. Door de aard van het Ampersand project heeft eht project niet één, maar drie Product Owners. Daarnaast zijn de Developers vaak kortdurend (2-6 maanden) aan het project verbonden.

## Developers
De specifieke vaardigheden die nodig zijn voor de Developers, zijn vaak breed en variëren met het domein van het werk. Developers zijn altijd verantwoordelijk voor:
- Het creëren van een plan voor de Sprint; de Sprint Backlog;
- Het garanderen van kwaliteit door vast te houden aan een Definition of Done;
- Het dagelijks aanpassen van hun plan richting het Sprint Doel;
- Het elkaar verantwoordelijk houden als professionals.

Developers die aan Ampersand werken of hebben gewerkt:

| Naam | Project(en) | Start | Eind |
| ---- | ------- | ----- | ---- |
| **Andre de Nijs** | Coach | 01/09/2022 | - |
| Bas Plijnaer | RAP deployment | 01/09/2022 | 31/10/2022 |
| Dirk Suelmann | Front-end | 07/11/2022 | 16/12/2022 |
| **Emre Gul** | Testen Front-end | 16/12/2022 | - | 
| **Frank Teusink** | RAP deployment | 17/02/2023 | - | 
| **Freek Rodenburg** | RAP deployment | 07/11/2022 | - |
| Jesse Zwitserlood | RAP deployment | 01/09/2022 | 10/11/2022 |
| Kemal Yildiz | Testen RAP | 07/11/2022 | 30/12/2022 |
| **Li-Wei Yeh** | Front-end | 01/09/2022 | - |
| Mark van der Hart | Front-end | 01/09/2022 | 30/11/2022 |
| **Martijn van Andel** | Front-end | 17/01/2023 | - | 
| Naomi Prins | Front-end | 07/11/2022 | 16/12/2022 |
| Patrick Ramge | Front-end, RAP | 01/09/2022 | 30/12/2022 |
| **Sheriff Balunywa** | Front-end | 16/12/2022 | - |
| Sharvin Ramdat | Testen RAP, Testen front-end | 01/10/2022 | 13/01/2023 |

## Product owner(s)
De Product Owner is verantwoordelijk voor het maximaliseren van de waarde van het product, dat het resultaat is van het werk van het Scrum Team. Hoe dit wordt gedaan, kan sterk uiteenlopen tussen organisaties, Scrum Teams en individuen.

De Product Owner is ook verantwoordelijk voor effectief Product Backlog management, wat het volgende omvat:
- Het ontwikkelen en duidelijk overbrengen van het Product Doel;
- Het creëren en helder overbrengen van Product Backlog items;
- Het ordenen van Product Backlog items;  
- Het ervoor zorgen dat de Product Backlog transparant en zichtbaar is en begrepen wordt.

In het Ampersand project wordt echter ook van de developers verwacht zich actief bezig te houden met de inhoud van het backlog.

Product owners Ampersand project:
- [Stef Joosten](mailto:stef.joosten@ordina.nl)
- [Han Joosten](mailto:han.joosten@ordina.nl)
- [Michiel Stornebrink](mailto:michiel.stornebrink@tno.nl)

## Scrum master
De Scrum Master is verantwoordelijk voor het opzetten van Scrum, zoals staat beschreven in de Scrum Gids. Scrum Masters doen dit door iedereen te helpen om Scrum theorie en praktijk te begrijpen, zowel binnen het Scrum Team als binnen de organisatie.

De Scrum Master is verantwoordelijk voor de effectiviteit van het Scrum Team. Scrum Masters doen dit door het Scrum Team in staat te stellen zijn werkwijzen te verbeteren binnen het Scrum raamwerk.

In het Ampersand project is geen vast scrum master, deze wisselt daarom per sprint. Van de developer die de rol scrum master wordt toebedeeld, wordt verwacht dat deze hier tijd voor inpland en dit als een leerdoel ziet.

| Sprint | Start | Eind | Scrum master |
| ---- | ------- | ----- | ---- |
| 8 | 21/11/2022 | 02/12/2022 | Li-Wei Yeh |
| 9 | 05/12/2022 | 16/12/2022 | Freek Rodenburg |
| 10a | 19/12/2022 | 30/12/2022 | Patrick Ramge |
| 10b | 02/01/2023 | 15/01/2023 | Li-Wei Yeh |
| 11 | 15/01/2023 | 29/02/2023 | Sherrif Balunywa |

## Business Analyst
De Business Analyst heeft als taak systemen en processen te verbeteren. Zij doet dit door de brug te zijn tussen de product owners en developers. Taken af te stemmen en het backlog te beheren.

Business analisten die aan Ampersand werken of hebben gewerkt:

| Naam | Start | Eind | Uren /week |
| ---- | ----- | ---- | ---------- |
| Manon de Willigen | 12/12/2022 | - | 4 |

## Belanghebbenden / stakeholders
Elk project heeft belanghebbenden, bijvoorbeeld de opdrachtgever en/of gebruikers. 

Belanghebbenden van het Ampersand project:
- Open Universiteit: vertegenwoordigd door [Stef Joosten](mailto:stef.joosten@ordina.nl)
- TNO: vertegenwoordig door [Michiel Stornebrink](mailto:michiel.stornebrink@tno.nl)

# Backlog / Scrum board / issues

Taken worden bijgehouden op de bijbehorende [project pagina](https://github.com/orgs/AmpersandTarski/projects). Elke taak die aangemaakt wordt op het backlog is direct ook een issue in de repository dat hoort bij het project.

Issues en backlog items, moeten aan bepaalde voorwaarden voldoen. Een issue moet self-explaining zijn, daarvoor zijn een aantal templates gemaakt.

## Backlog: epic
```
# What is purpose: why do we need it?

# List of user stories / tasks

# What is the result?
```

## Backlog: user story / task
```
# What is the purpose: why do we need it?

# How to acclompish the task

# List of subtasks

# What is the result?
```

## Issue: Bug report
```
<!-- Thanks for contributing to this project! Please pick a clear title and proceed.-->
<!-- Please note: If your issue is about RAP3, please report it over there:-->
<!--      https://github.com/AmpersandTarski/RAP/issues -->

# What happened

# What I expected

# Version of ampersand that was used

# Steps to reproduce

1.
2.
3.
4.

# Screenshot / Video

# Context / Source of ampersand script
<!-- Optional: share your script if possible. It helps us reproduce the problem. Please try to keep the scripts tiny-->

<!-- We'd also love to know how you found the bug: #dogfooding, #manual-testing, #automated-testing, or #user-report if applicable.-->

<!-- If requesting a new feature, explain why you'd like to see it added.-->asdf
```

## Issue: Comment / error /typo in the documentation
```
<!-- Thanks for contributing to this project! We are happy with your comments on the documentation. Please pick a clear title and proceed.-->

# What is the page that your suggestion is about?

# What Is your comment?
<!-- You can provide sceenshots, or whatever you think is helpful to us to do better --!>
```

## Issue: Feature request
```
**Is your feature request related to a problem? Please describe.**

<!-- A clear and concise description of what the problem is. Ex. I'm always frustrated when [...] -->

**Describe the solution you'd like**

<!-- A clear and concise description of what you want to happen. -->

**Describe alternatives you've considered**

<!-- A clear and concise description of any alternative solutions or features you've considered. -->

**Additional context**

<!-- Add any other context or screenshots about the feature request here. -->
```