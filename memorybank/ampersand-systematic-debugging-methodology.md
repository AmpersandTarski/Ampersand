# Ampersand Systematic Debugging Methodology

## Hoofdlijn: Hoe ik debugging aanpak
Het debuggen is een zoektocht, dus niet een stappenplan.
1. Ik zoek eerst naar de oorzaak van het verschijnsel
2. Ik stel geen oplossing voor als ik nog niet begrijp wat de root-cause is.
3. Ik implementeer een oplossing pas als jij en ik samen vinden dat wij het probleem snappen en het eens zijn over de oplossing.

Het zoeken naar de oorzaak begint bij de populatie:
Ik onderzoek de POPULATION statements en de spreadsheets die mijn script via INCLUDE statements gebruikt.
Ik zoek uit welke populatie precies bij het probleem betrokken is.
Zodoende weet ik welke relaties erbij betrokken zijn.
Dat vergelijk ik zorgvuldig met de foutmelding(en), zodat ik goed snap wat er fout is gegaan.
Ik kijk goed naar de types van de relaties en de atomen, om de oorzaak van het probleem op te sporen.

Wanneer ik een verklaring heb, ga ik deze toetsen.
Kan ik een experiment verzinnen die mijn verklaring bewijst of tegenspreekt?
Ik overleg met jou over dit experment en voer het daarna uit.
Ik kijk wat we van dit experiment kunnen leren.
Als ik iets nieuws leer over Ampersand, maak ik een aantekening in mijn Ampersand instructies op mijn memorybank.
Daarna pas ik mijn diagnose van het probleem aan op basis van de resultaten van mijn experimentjes.
Voordat ik de oplossing implementeer overleg ik met jou, want jij bent de baas over de codebase.

## Details: Type Errors en Relatie Conflicten

### Analyseer van de Foutmelding
```bash
# Het commando ampersand check geeft de precieze error locatie
ampersand check project/main.adl

# Let op specifieke informatie:
# - Regel nummer
# - Concept namen
# - Relatie signatures
# - Type mismatch details: let op de foutmeldingen van de Ampersand compiler
```

### Inventariseer Alle Conflicterende Relaties
```bash
# Zoek alle relaties met dezelfde naam
grep -r "RELATION relationName" project/*.adl
grep -r "relationName\[" project/*.adl

# Zoek alle populaties van deze relatie
grep -r "POPULATION relationName" project/*.adl
```

### Controleer Include Structuur
```bash
# Identificeer welke bestanden actief zijn
grep -r "INCLUDE" project/main.adl
grep -r "-- INCLUDE" project/main.adl  # uitgeschakelde includes

# Bouw include tree op:
# main.adl
#   ├── actieve.adl
#   ├── andere.adl  
#   └── -- uitgeschakelde.adl (NIET actief)
```

### Analyseer Concept Type Signatures
Voor elke conflicterende relatie, controleer:
```adl
RELATION relatieName[SourceConcept*TargetConcept] [properties]
```

**Ampersand Relatie Gelijkheid Principe:**
- Naam + Source + Target moeten EXACT gelijk zijn
- `relatie[A*B]` ≠ `relatie[A*C]` ≠ `relatie[C*B]`

### Zoek VIEW Definities Die Relaties Gebruiken
```bash
# Zoek alle VIEWS die de problematische relatie gebruiken
grep -r "VIEW.*:" project/*.adl
grep -A 10 -B 2 "relationName" project/*.adl
```

### Stap 6: **Controleer POPULATION vs RELATION Consistency**
```adl
RELATION dekkingTekst[Dekking*Dekkingtekst]

POPULATION dekkingTekst CONTAINS
  [ ("1", "Dit is een voorbeeld dekkingstekst.") ]  -- "1" wordt geïnterpreteerd als Dekking
  --  ↑ Moet consistent zijn met source concept
```
Algemeen geldende regel:
Als een atoom (bijv. "1") links staat in een paar, en dus het eerste atoom is in dat paar, dan is dat PER DEFINITIE een atoom uit de source van de relatie. Dat is dus "Dekking" in dit voorbeeld.
Als een atoom (bijv. "tekst") rechts staat in een paar, en dus het tweede atoom is in dat paar, dan is dat PER DEFINITIE een atoom uit de target van de relatie. Dat is dus "Dit is een voorbeeld dekkingstekst." in dit voorbeeld.


### Gebruik Ampersand Type Checker
```bash
# Voor elk suspect bestand:
ampersand check project/Wijziging.adl
ampersand check project/DekkingSelectie.adl
ampersand check project/LandeneisenStructuur.adl

# Let op type error patterns:
# - "cannot match concept A and concept B"
# - "in VIEW X"
# - "Src of relation [A*B]"
```

## **Debugging Tools Checklist**

### Command Line Tools:
```bash
# Type checking
ampersand check <file.adl>

# Pattern searching  
grep -r "RELATION.*dekkingTekst" project/
grep -r "VIEW.*Dekkingscode" project/
grep -r "POPULATION.*dekkingTekst" project/

# Include tracing
grep -r "INCLUDE" project/main.adl
```

### Code Analysis Pattern:
1. **Error message** → regel nummer + concept namen
2. **Relatie definitions** → alle signatures met zelfde naam
3. **VIEW definitions** → welke concepten worden verwacht
4. **POPULATION data** → consistentie met source concept
5. **Include chain** → welke bestanden zijn actief

## **Ampersand Type Error Patterns**

### Pattern 1: VIEW Type Mismatch
```
VIEW ConceptA: ConceptA 
  { field: relation }  -- zoekt relation[ConceptA*?]

RELATION relation[ConceptB*ConceptC]  -- MISMATCH: A ≠ B
```

### Pattern 2: Multiple Relations Same Name
```
RELATION rel[A*B]  -- In bestand 1
RELATION rel[C*D]  -- In bestand 2 (andere signature!)
-- Beide bestaan in database als verschillende relaties
```

### Pattern 3: Population Concept Mismatch  
```
RELATION rel[ConceptA*ConceptB]
POPULATION rel CONTAINS [("x", "y")]  -- x moet ConceptA zijn
```

## **Preventie Strategies**

1. **Consistente naming**: Gebruik verschillende namen voor verschillende concepten
2. **Explicit concept declarations**: Declareer concepten expliciet
3. **Regular type checking**: Run `ampersand check` bij elke wijziging  
4. **Comment relatie signatures**: Documenteer source en target concepten
5. **Test include combinations**: Test verschillende include combinaties

## **Praktische Debugging Aanwijzingen**

### 1. **RELATION vs POPULATION bij Type Errors**
- **Type errors beginnen bij RELATIONs** → eerst RELATION declaraties begrijpen
- **Maar controleer ook direct POPULATION** → goede kans dat populatie ook fout zit
- **Doe beide als je zeker wilt weten** → RELATION + POPULATION samen analyseren

### 2. **Experimenten Ontwerpen**
- **Een experiment is altijd een gok** → je weet het antwoord nog niet
- **Experiment = bewijs zoeken** → test je verklaring/oplossing
- **"Als ik denk X te weten, hoe bewijs ik X?"**
- Bijvoorbeeld: "Laten we VIEW van Dekkingscode naar Dekking veranderen en kijken of fout verdwijnt"

### 3. **Excel vs ADL Populaties**
- **Excel populatie = ADL populatie** → voor Ampersand geen verschil
- **Alleen de bron verschilt** → .xlsx, .adl, .ifc allemaal gelijkwaardig
- **Blijf in ADL werken** → analyseer de resulterende populatie, niet de Excel

### 4. **Diepte van Type Tracing**
- **"Kijk verder dan je neus lang is"** → ga een slagje dieper
- **Root cause zit vaak in de diepte** → niet aan oppervlakte
- **Bijstel diagnose met diepere kennis** → zoals in deze DekkingTekst case
- **Trace door tot echte oorzaak** → stop niet bij eerste symptoom

### 5. **Iteratieve Diagnose Verfijning**
- **Eerste diagnose is vaak oppervlakkig** → ga dieper
- **Stel root-cause meerdere keren bij** → met nieuwe inzichten
- **Diepere analyse = betere oplossing** → investeer tijd in begrip

**Datum:** 4-8-2025  
**Context:** FC4 DekkingTekst type error debugging sessie
**Update:** Praktische guidelines toegevoegd na debugging reflectie
