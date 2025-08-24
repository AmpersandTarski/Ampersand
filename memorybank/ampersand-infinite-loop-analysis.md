# Ampersand Oneindige Lus Analyse Methodologie

## Overzicht
Dit document beschrijft de stap-voor-stap methodologie om oneindige lussen in Ampersand ExecEngine logs te identificeren en op te lossen. Deze aanpak gebruik ik zodra er sprake is van een "Maximum reruns exceeded" foutmelding.

## Herkenning van het Probleem

### Symptomen in Logfiles
- **Foutmelding:** `Maximum reruns exceeded. Rules fixed in last run:`
- **Gevolg door:** `APPLICATION.ERROR: Maximum reruns exceeded for ExecEngine`
- **Locatie:** Meestal aan het einde van het logbestand na herhaalde regel-uitvoeringen
Ik heb dus het logbestand nodig. Omdat ik met docker draai kan ik het logbestand ophalen met:
```bash
docker logs prototype > prototype.log
```
Dit commando slaat het logbestand op in prototype.log. Dat is relevant omdat prototype.log de actuele log met de fout erin moet bevatten, anders heeft dit onderzoek geen zin.

### Typische Logpatronen
```
[timestamp] EXECENGINE.NOTICE: ExecEngine fixed X violations for rule 'RuleA'
[timestamp] EXECENGINE.NOTICE: ExecEngine fixed X violations for rule 'RuleB'
[timestamp] EXECENGINE.ERROR: Maximum reruns exceeded. Rules fixed in last run: RuleA, RuleB
```

## Analysemethodologie

### Stap 1: Lokaliseer de Foutmelding
```bash
grep -n "Maximum reruns exceeded" prototype.log
```
Dit geeft het regelnummer waar de fout optreedt.

### Stap 2: Onderzoek de Context Rond de Fout
```bash
sed -n '[start_regel],[eind_regel]p' prototype.log
```
Ik kijk naar ~20 regels voor en na de foutmelding om te kijken of er iets interessants in de omgeving van de foutmelding te zien is.

### Stap 3: Hypothese: Conflicterende Regels
Waarschijnlijk zijn er verschillende regels die tegen elkaar in werken. Waar de ene regel paren toevoegt, verwijdert de andere regel deze paren.
Ik ga op zoek naar deze paren in het logbestand, om zo de betrokken regels te vinden.

De foutmelding toont welke regels betrokken zijn:
- **RULE naam:** Expliciete regelnamen (bijv. `synDisjunctNaam`)
- **ENFORCE regels:** Getoond als `Compute [relatie_naam] using InsPair`

Ik zoek ook naar de relatie, waarin de bewuste paren erin en dan weer eruit worden gehaald.
Alle regels, die in deze relatie paren toevoegen en/of verwijderen kunnen betrokken zijn in de ondeindige lus.

### Stap 4: Zoek Regels in Broncode
```bash
# Voor RULE regels:
grep -r "RULE regel_naam :" project/*.adl

# Voor ENFORCE regels:
grep -r "ENFORCE relatie_naam" project/*.adl
```

### Stap 5: Analyseer de Data-operaties
Ik zoek naar patronen in de log:
```bash
# Zoek naar InsPair/DelPair operaties
grep -E "(InsPair|DelPair)" logfile | tail -n 50

# Filter op specifieke relatie
grep "synoniem.*InsPair\|synoniem.*DelPair" logfile
```

## Concrete Casus: synoniem vs synDisjunctNaam
Een voorbeeld. Ik ben op zoek naar een lus en heb twee regels gevonden:

1. **ENFORCE regel** (project/Pleio.adl:44):
   ```Ampersand
   ENFORCE synoniem[Organisme*WetenschappelijkeNaam] >: organisme~;poSynoniem
   ```

2. **RULE synDisjunctNaam** (project/Organismen.adl:74-76):
   ```Ampersand
   RULE synDisjunctNaam : synoniem /\ voorkeursNaam |- -V[Organisme*WetenschappelijkeNaam]
   VIOLATION (TXT "{EX} DelPair;synoniem;Organisme;", SRC I, TXT ";WetenschappelijkeNaam;", TGT I)
   ```

Het gaat dus in alle gevallen om regels die iets toevoegen of verwijderen in de relatie synoniem.

De lus laat zich als volgt verklaren:
1. De **ENFORCE**-regel voegt synoniemen toe waar organisme = wetenschappelijke naam
2. **synDisjunctNaam** detecteert dat synoniem = voorkeursnaam en verwijdert deze
3. **ENFORCE** wordt opnieuw getriggerd door de wijziging
4. Cyclus herhaalt zich tot maximum reruns

Dit gebeurt dus in gevallen waar hetzelfde paar voorkomt in zowel `voorkeursNaam` als `synoniem`:
In het log-bestand zie ik dat gebeuren met concrete gevallen, de zgn. problematische data.
- Aspergillus flavus
- Colletotrichum circinans  
- Peronospora destructor
- Pseudocercospora abelmoschi
- Pseudomonas syringae (varianten)
Die gebruik ik straks om te valideren of mijn oplossing werkt.

## Oplossing
De regels wijzigen, om de cyclus te doorbreken.
In een relatie mag het systeem een paar toevoegen of verwijderen, maar nooit allebei tegelijk.
In dit voorbeeld kan ik dat voorkomen door de ENFORCE-regel aan te passen:
   ```Ampersand
   ENFORCE synoniem[Organisme*WetenschappelijkeNaam] >: organisme~;poSynoniem - voorkeursNaam
   ```

## Preventie

### Tijdens Ontwikkeling
1. Zoek alle regels bij elkaar die in één relatie wijzigen en kijk of er zowel inserts als deletes voorkomen.
2. Onderzoek of zulke regels een logisch conflict bevatten. Ik heb een logisch conflict als er een paar kan bestaan dat door een regel verwijderd en door een andere regel ingevoegd wordt.
3. Los het conflict op door de regels zo aan te passen dat zulke paren niet kunnen bestaan.

### Preventieve Risico-Relaties Analyse
**Doel:** Identificeer relaties waar het systeem zowel paren toevoegt als verwijdert VOORDAT er oneindige lussen ontstaan.

#### Stap 1: Zoek relaties waar paren worden toegevoegd
```bash
grep -r "ENFORCE.*>:" project/*.adl
grep -r "ENFORCE.*:=" project/*.adl
grep -r "InsPair" project/*.adl
```
Dit toont alle regels die paren aan een relatie kunnen toevoegen.

#### Stap 2: Zoek DelPair Operaties (RULE VIOLATION)
```bash
grep -r "ENFORCE.*:<" project/*.adl
grep -r "ENFORCE.*:=" project/*.adl
grep -r "DelPair" project/*.adl
```
Dit toont alle regels die paren uit een relatie kunnen verwijderen.

#### Stap 3: Onderzoek Overlappingen
Onderzoek relaties die in beide categorieën voorkomen.
- ** geen risico ** Van alle regels is er maar één die de relatie wijzigt. Bijvoorbeeld `ENFORCE r := <foo>` is de enige regel die `r` wijzigt.
- **🚨 RISICO:** De relatie komt in meerdere regels voor, waarvan sommige paren toevoegen en andere paren verwijderen.

#### Stap 4: onderzoek logische overlap voor elke risicorelatie. Zorg dat er geen logische overlap kan plaatsvinden. 

Logische overlap is een situatie waar er tenminste een paar kan bestaan, die een regel aan een relatie kan toevoegen en een andere regel uit diezelfde relatie kan verwijderen. 

### Monitoring
1. Regelmatige controle op "Maximum reruns" in logs
2. Automated alerts bij ExecEngine errors
3. Performance monitoring voor lange regel-uitvoeringstijden
4. **Preventieve risico-analyse** uitvoeren bij elke nieuwe release

## Tools en Commando's

### Log Analyse
```bash
# Vind foutmeldingen
grep "Maximum reruns exceeded" prototype.log

# Analyse rond specifiek regelnummer
sed -n '85850,85870p' prototype.log

# Filter regel-uitvoeringen
grep -E "(EXECENGINE.NOTICE|ExecEngine fixed)" prototype.log | tail -n 20

# Zoek specifieke operaties
grep -E "(InsPair|DelPair).*synoniem" prototype.log | tail -n 10
```

### Broncode Zoeken
```bash
# Zoek RULE definities
find project/ -name "*.adl" -exec grep -l "RULE.*synDisjunctNaam" {} \;

# Zoek ENFORCE statements
grep -r "ENFORCE.*synoniem" project/

# Combineer zoekacties
search_files met regex patterns in VS Code/Cline
```

### Debug Strategieën
1. **Isolatie:** Disable één regel tijdelijk om conflict te bevestigen
2. **Logging:** Voeg extra debug informatie toe aan regels
3. **Stepped execution:** Voer regels in delen uit om probleem te lokaliseren

## Testing en Validatie Methodologie

### Pre-Test: Baseline Vaststellen
1. **Documenteer de oorspronkelijke fout:**
   ```bash
   grep -n "Maximum reruns exceeded" prototype.log
   sed -n '[regel-20],[regel+5]p' prototype.log  # Context rond fout
   ```

2. **Identificeer de problematische atomen:**
   ```bash
   grep -E "(InsPair|DelPair).*[relatie_naam]" prototype.log | tail -n 20
   ```

3. **Sla specifieke test-criteria op:**
   - Welke atomen veroorzaken het conflict
   - Welke regels zijn betrokken
   - Hoe vaak de cyclus zich herhaalt

### Implementatie van de Fix
1. **Maak de code-wijziging** (bijv. voeg `- voorkeursNaam` toe aan ENFORCE)
2. **Documenteer de wijziging met commentaar** in de broncode
3. **Bouw en deploy het systeem:**
   ```bash
   docker compose up -d --build
   docker logs prototype > prototype.log  # Nieuwe logs opslaan
   ```

### Post-Test: Validatie van de Oplossing

#### Stap 1: Controleer of Hoofdprobleem is Opgelost
```bash
# Test 1: Geen "Maximum reruns exceeded" meer
grep -n "Maximum reruns exceeded" prototype.log
# Verwacht resultaat: GEEN output = succes

# Test 2: Geen oneindige lus in laatste 500 regels
tail -n 500 prototype.log | grep "Maximum reruns exceeded"
# Verwacht resultaat: GEEN output = succes
```

#### Stap 2: Verificeer Specifieke Problematische Atomen
```bash
# Test 3: Geen conflicterende operaties meer voor problematische atomen
grep -E "(Atom1|Atom2|Atom3)" prototype.log | grep "[conflicterende_relatie]" | grep -E "(InsPair|DelPair)"
# Verwacht resultaat: GEEN output = problematische operaties gestopt

# Test 4: Andere relaties werken nog steeds voor deze atomen
grep -E "(Atom1|Atom2|Atom3)" prototype.log | grep -E "(InsPair|DelPair)" | grep -v "[conflicterende_relatie]" | tail -n 10
# Verwacht resultaat: Normale operaties voor andere relaties zichtbaar
```

#### Stap 3: Controleer Systeem Functionaliteit
```bash
# Test 5: ExecEngine draait normaal
tail -n 50 prototype.log | grep -E "(EXECENGINE|ExecEngine)"
# Verwacht resultaat: "ExecEngine finished" zichtbaar, geen errors

# Test 6: Algemene systeemstatus
tail -n 100 prototype.log | grep -E "(ERROR|WARNING)" | grep -v "reliably determine.*fully qualified domain name"
# Verwacht resultaat: Geen relevante errors (DNS warnings zijn normaal)
```

### Success Criteria Checklist
- [ ] Geen "Maximum reruns exceeded" in nieuwe logs
- [ ] Geen conflicterende InsPair/DelPair voor problematische atomen
- [ ] ExecEngine start en eindigt succesvol
- [ ] Andere relaties werken nog steeds normaal
- [ ] Systeem accepteert nieuwe transacties zonder timeout

### Fail-Safe: Rollback Procedure
Als de test faalt:
1. **Revert de code-wijziging**
2. **Rebuild en redeploy**
3. **Analyseer waarom de oplossing niet werkte:**
   - Was de regel-expressie incorrect?
   - Zijn er andere conflicterende regels?
   - Is de data-inconsistentie dieper?

### Rapportage Template
```
ONEINDIGE LUS OPLOSSING RAPPORT

Datum: [datum]
Probleem: Maximum reruns exceeded tussen [regel1] en [regel2]
Problematische atomen: [lijst van atomen]

Oplossing: [beschrijving van de wijziging]
Code wijziging: [exacte regel die is aangepast]

Test resultaten:
✅/❌ Geen "Maximum reruns exceeded"
✅/❌ Conflicterende operaties gestopt
✅/❌ ExecEngine werkt normaal
✅/❌ Andere functionaliteiten intact

Status: OPGELOST / GEDEELTELIJK / GEFAALD
```

## Documentatie van Oplossingen
Bij elke opgeloste oneindige lus:
1. Documenteer de oorspronkelijke fout met specifieke log-regels
2. Leg uit waarom het conflict optrad met concrete voorbeelden
3. Beschrijf de gekozen oplossing met exacte code-wijzigingen
4. Voer de volledige test-methodologie uit en documenteer resultaten
5. Update deze methodologie met nieuwe inzichten
6. Archiveer het rapport voor toekomstige referentie
