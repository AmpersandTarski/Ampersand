# Oscillaties in Ampersand begrijpen — een uitgewerkte casus

> **Voor wie?** Studenten en ontwikkelaars die met Ampersand werken en willen begrijpen
> waarom de ExecEngine soms vastloopt met *"Maximum reruns exceeded"*.
>
> **Leerdoel.** Je leert (1) het verschijnsel herkennen, (2) het terugtraceren tot de
> botsende regels, (3) zien dat een oscillatie een vorm van **wiskundige
> onvervulbaarheid** is, en (4) zo'n oscillatie gebruiken als *kans* om je regels (of je
> data) consistent te maken. Aan het eind kun je het zelf naspelen met een minimaal script.
>
> **Kernboodschap.** Een oscillatie is geen lastige bug die je met een hogere rerun-limiet
> "weg" zet. Het is de runtime die je vertelt: *jouw invarianten spreken elkaar tegen.*
> Dat is precies de informatie die je nodig hebt om ze kloppend te maken.

---

## 0. Eerst: hoe werkt de ExecEngine? (de "waarom" onder alles)

Om het verschijnsel te begrijpen moet je weten wat de ExecEngine doet. Sla dit niet over.

In Ampersand is een **regel** een eis die altijd waar moet zijn, meestal van de vorm
`antecedent |- consequent` ("antecedent is deelverzameling van consequent"). Een toestand
(de populatie van alle relaties) die een regel schendt, heeft **violations**: de paren die
wél in de antecedent maar niet in de consequent zitten.

Er zijn twee soorten regels:

- **Invariant** (een `RULE` *zonder* `ROLE ... MAINTAINS`). Deze moet *na elke transactie*
  waar zijn. Wordt hij geschonden, dan **weigert** Ampersand de transactie en draait alles
  terug. Bij een data-import betekent dit: de hele import faalt.
- **Procesregel** (een `RULE` *met* `ROLE <rol> MAINTAINS`). Een schending is hier
  *toegestaan*; hij wordt als **signaal** aan die rol getoond. De transactie gaat door.

De **ExecEngine** is een bijzondere "rol": een robot-gebruiker. Voor elke regel met
`ROLE ExecEngine MAINTAINS` voert hij bij een violation automatisch de instructies in het
`VIOLATION`-blok uit. Die instructies beginnen met `{EX}` en roepen ingebouwde functies aan:

| Functie | Doet |
|---|---|
| `InsAtom;C` | maakt een nieuw atoom in concept `C` (placeholder `_NEW`) |
| `InsPair;r;A;a;B;b` | voegt paar `(a,b)` toe aan relatie `r[A*B]` |
| `DelPair;r;A;a;B;b` | verwijdert paar `(a,b)` uit `r` |
| `MrgAtoms;C;a;C;b` | versmelt atoom `b` in `a`: alle links van `b` gaan naar `a`, `b` verdwijnt |

**De rerun-lus.** De ExecEngine werkt iteratief:

1. Evalueer alle ExecEngine-regels en verzamel de violations.
2. Voer hun `{EX}`-fixes uit.
3. Die fixes veranderen de populatie → er kunnen *nieuwe* violations zijn ontstaan, of oude
   zijn opgelost. Dus: **rerun** (terug naar stap 1).
4. Stop zodra een ronde niets meer fixt (een **fixpunt**: violations = 0).

Werkt dit niet naartoe een fixpunt, dan zou de lus eeuwig draaien. Daarom is er een
**maximum aantal reruns**. Wordt dat bereikt, dan stopt de engine met de fout:

```
Maximum reruns exceeded. Rules fixed in last run: <regel A>, <regel B>
```

Onthoud die laatste zin goed: **"Rules fixed in last run" noemt precies de regels die in de
láátste ronde nóg steeds zaten te fixen** — dat zijn je verdachten.

---

## 1. Het verschijnsel — hoe zie je het?

In de FC5-casus dook dit op tijdens het inladen van de EPPO-codelijst. In de log van het
prototype (`docker compose logs prototype`) verschijnt:

```
EXECENGINE.ERROR: Maximum reruns exceeded. Rules fixed in last run:OrganismeUniekeEPPO, eppoCodeMaaktOrganisme)
APPLICATION.ERROR: Maximum reruns exceeded for ExecEngine
   {"Rules fixed in last run":["...Rule: OrganismeUniekeEPPO","...Rule: eppoCodeMaaktOrganisme"]}
```

Daaromheen zie je een typisch ritme van fixes die elkaar afwisselen:

```
EXECENGINE.INFO: InsAtom(Organisme)
EXECENGINE.INFO: InsPair(voorkeursNaam,Organisme,_NEW,WetenschappelijkeNaam,'Candidatus Phytoplasma solani')
EXECENGINE.INFO: InsPair(eppoCode,Organisme,_NEW,EPPOcode,PHYPSO)
...
EXECENGINE.INFO: MrgAtoms(Organisme, <id-1>, Organisme, <id-2>)
EXECENGINE.NOTICE: ExecEngine fixed 16 violations for rule 'OrganismeUniekeEPPO'
...
EXECENGINE.NOTICE: ExecEngine fixed 8 violations for rule 'eppoCodeMaaktOrganisme'
EXECENGINE.ERROR: Maximum reruns exceeded ...
```

> **Waarom is dit al verdacht?** Een gezonde ExecEngine-run *daalt* naar nul violations: elke
> ronde fixt hij minder. Hier zie je het tegenovergestelde — `MrgAtoms` (samenvoegen) en
> `InsAtom`/`InsPair` (aanmaken) blijven elkaar afwisselen. Iets wordt gemaakt, weer
> weggehaald, weer gemaakt. Dat heen-en-weer is de **oscillatie**.

Een tweede, ernstig gevolg dat je niet meteen ziet: de import draaide om een
**transactie**. Door de fout wordt die teruggedraaid. Concreet bleef in de FC5-database de
bronrelatie `eppoCode[WetenschappelijkeNaam*EPPOcode]` op **1 rij** steken (i.p.v. ~1400) —
de hele EPPO-lijst was dus niet geladen, terwijl de app gewoon "leek" te draaien.

> **Les 1.** Een oscillatie is dubbel gevaarlijk: hij stopt niet alleen de engine, hij rolt
> ook de omringende transactie terug. "De app start" betekent niet "de data is geladen".

---

## 2. De analyse — hoe trace je het terug?

Werk van symptoom naar oorzaak, niet andersom (theoretiseer niet over de fix vóór je de
oorzaak hebt vastgesteld).

**Stap 1 — Lees de twee schuldige regels uit de foutmelding.** Hier: `OrganismeUniekeEPPO`
en `eppoCodeMaaktOrganisme`. Zoek hun definitie op in de `.adl`-bron:

```
-- creatie-regel
ROLE ExecEngine MAINTAINS eppoCodeMaaktOrganisme
RULE eppoCodeMaaktOrganisme : eppoCode - voorkeursNaam~;V[Organisme*EPPOcode] |- voorkeursNaam~;I[Organisme];eppoCode
VIOLATION ( TXT "{EX} InsAtom;Organisme"
          , TXT "{EX} InsPair;voorkeursNaam;Organisme;_NEW;WetenschappelijkeNaam;", SRC I[WetenschappelijkeNaam]
          , TXT "{EX} InsPair;eppoCode;Organisme;_NEW;EPPOcode;", TGT I )

-- merge-regel
ROLE ExecEngine MAINTAINS OrganismeUniekeEPPO
RULE OrganismeUniekeEPPO : eppoCode[Organisme*EPPOcode];eppoCode[Organisme*EPPOcode]~ |- I[Organisme]
VIOLATION ( TXT "{EX} MrgAtoms;Organisme;", SRC I, TXT ";Organisme;", TGT I )
```

**Stap 2 — Vertaal elke regel naar gewone taal.**
- `eppoCodeMaaktOrganisme`: "voor elke wetenschappelijke naam in de bron die nog géén
  `voorkeursNaam` van een Organisme is: maak een Organisme met die naam én die EPPO-code."
  → dit is **naam-gestuurd**: één Organisme per *naam*.
- `OrganismeUniekeEPPO`: "twee Organismen met dezelfde EPPO-code zijn hetzelfde" → **merge
  per code**: één Organisme per *code*.

**Stap 3 — Lees de `{EX}`-acties in de log als bewijs.** De `InsPair(voorkeursNaam,…)` en
`InsPair(eppoCode,…)` laten zien wélke namen/codes het betreft. Door ze te volgen zie je
hetzelfde Organisme ontstaan, samensmelten en opnieuw ontstaan.

**Stap 4 — Controleer de invoer (de data), niet alleen de logica.** Vraag: *kan de bron de
aannames van deze regels schenden?* Query de bron:

```sql
SELECT WetenschappelijkeNaam, COUNT(DISTINCT EPPOcode)
FROM <brontabel> GROUP BY WetenschappelijkeNaam HAVING COUNT(DISTINCT EPPOcode) > 1;  -- één naam, meerdere codes?
-- en omgekeerd: één code, meerdere namen?
```

In de FC5-bron bleek: **meerdere namen wijzen naar dezelfde EPPO-code** (synoniemen). Dat is
de trigger. Houd dat vast voor de volgende paragraaf.

---

## 3. De oorzaak — concreet én wiskundig

### 3a. Concreet: het draaiboek van de lus

Neem één code `X` met twee namen `A1` en `A2` (synoniemen). Volg de ExecEngine ronde voor
ronde:

1. **Ronde 1, creatie.** `eppoCodeMaaktOrganisme` is naam-gestuurd. `A1` en `A2` zijn nog
   geen voorkeursNaam, dus de regel vuurt twee keer: er ontstaan `Org1(voorkeursNaam=A1, eppoCode=X)`
   en `Org2(voorkeursNaam=A2, eppoCode=X)`.
2. **Ronde 1, merge.** `OrganismeUniekeEPPO` ziet twee Organismen met code `X` → `MrgAtoms`.
   Het resultaat is één Organisme. Maar `voorkeursNaam` is `[UNI]` (hoogstens één per
   Organisme), dus na de merge overleeft maar één naam, zeg `A1`. **`A2` is z'n voorkeursNaam
   kwijt.**
3. **Ronde 2, creatie.** Nu is `A2` weer geen voorkeursNaam van enig Organisme → de
   creatie-regel vuurt opnieuw voor `(A2, X)` → er ontstaat wéér een Organisme voor `A2`.
4. **Ronde 2, merge.** Twee Organismen met code `X` → merge → `A2` raakt z'n naam weer kwijt.
5. → terug naar stap 3. **Maken → samenvoegen → maken → samenvoegen → …** Eeuwig.

De engine bereikt nooit nul violations en stopt bij de rerun-limiet.

### 3b. Wiskundig: de regels zijn samen onvervulbaar

Dit is de kern die je echt wilt snappen. Schrijf de eisen op als uitspraken over relaties
(in relatie-algebra; lees `;` als compositie en `~` als omdraaien):

- `voorkeursNaam` is `[UNI,INJ]` → een **injectieve partiële functie** `Organisme → Naam`:
  elk Organisme heeft hoogstens één naam, en elke naam hoort bij hoogstens één Organisme.
- `eppoCode` is `[UNI]` en `OrganismeUniekeEPPO` zegt bovendien dat verschillende Organismen
  niet dezelfde code mogen hebben → `eppoCode` is óók een **injectieve partiële functie**
  `Organisme → Code`.
- `eppoCodeMaaktOrganisme` eist dat élke `(naam, code)` uit de bron een Organisme heeft met
  precies die naam als voorkeursNaam én die code.

Tel die eisen op. Via het Organisme als tussenstap dwingen ze samen een **bijectie** af
tussen de namen en de codes die in de bron voorkomen:

```
Naam  <--(voorkeursNaam, bijectief)-->  Organisme  <--(eppoCode, bijectief)-->  Code
```

Een samenstelling van twee bijecties is een bijectie. Dus de regels eisen dat de
bron-afbeelding **naam ↔ code één-op-één** is.

Maar de **data** zegt: `A1 ↦ X` en `A2 ↦ X` — twee namen, één code. Dat is per definitie
*niet* injectief, dus *geen* bijectie. Er bestaat dus **geen enkele populatie** die alle
regels tegelijk waarmaakt. De specificatie is, gegeven deze data, **onvervulbaar
(inconsistent)**.

> **Waarom volgt daar een oneindige lus uit, en niet "gewoon" een foutmelding?**
> De ExecEngine zoekt naar een **fixpunt**: een toestand die hij niet meer hoeft te
> repareren. Dat werkt netjes als reparaties *monotoon* zijn (alleen maar feiten toevoegen):
> dan groeit de populatie naar een kleinste fixpunt en stopt (stelling van Kleene/Tarski).
> Hier is dat niet zo: `MrgAtoms` **verwijdert** een feit (een voorkeursNaam) dat de
> creatie-regel meteen weer **toevoegt**. De reparatie-afbeelding is niet-monotoon en heeft
> géén gemeenschappelijk fixpunt; in plaats daarvan beschrijft hij een **cykel met
> periode 2** (maken ↔ samenvoegen). Geen fixpunt = geen terminatie. De rerun-limiet hakt de
> oneindige lus af.
>
> Kort: **onvervulbare invarianten + automatische reparatie = oscillatie.** De oscillatie is
> het *waarneembare gevolg* van een *logische tegenspraak*.

> **Les 2.** Elke afzonderlijke regel was redelijk ("codes uniek", "namen uniek", "alles
> krijgt een Organisme"). De *combinatie* is de tegenspraak. Inconsistentie is een
> eigenschap van de *verzameling* regels, niet van één regel.

---

## 4. De oplossingskeuzes — en het principe erachter

Als regels samen onvervulbaar zijn met de data, heb je in essentie drie knoppen. Kies bewust.

**Keuze A — Verzwak het model zodat de eisen wél samen kunnen.**
De tegenspraak ontstond doordat we tegelijk "één Organisme per naam" én "één Organisme per
code" eisten. Laat één kant los: sta toe dat één code meerdere namen heeft, met één
*voorkeursnaam* en de rest als *synoniem*. Dan is de afbeelding code → {namen} legaal en
verdwijnt de bijectie-eis. → Past bij **synoniemen**, die een echt domeinverschijnsel zijn.

**Keuze B — Repareer de data zodat ze de eisen wél kan waarmaken.**
Als de tegenspraak voortkomt uit een *fout* in de bron (geen echt domeinverschijnsel), maak
de afbeelding dan kloppend. → Past bij **typefouten en placeholders** (zie §6, oscillatie #2).

**Keuze C — Degradeer een te strenge invariant tot een signaal.**
Soms wíl je niet automatisch repareren, maar een mens laten beslissen. Verander de regel van
invariant (of ExecEngine-merge) naar **procesregel** onder een menselijke rol. De schending
crasht dan niets meer; ze verschijnt als werklijst. → Past bij **checks** die eigenlijk
"meld dit aan de beheerder" betekenen.

> **Het principe.** Het doel is niet "de oscillatie wegmaken" maar **de regelverzameling
> samen vervulbaar maken** — door het model, de data, óf de strengheid aan te passen. De
> oscillatie heeft je precies verteld wélke regels je daarvoor onder de loep moet nemen.
>
> **Anti-patroon:** de rerun-limiet ophogen. Dat verbergt de tegenspraak; hij blijft bestaan
> en sloopt elders (een teruggedraaide transactie, ontbrekende data) je systeem.

---

## 5. De oplossing (oscillatie #1: synoniemen)

Gekozen: **A** (model verzwakken) — want synoniemen zijn legitiem.

**Wijziging 1 — maak de creatie code-gestuurd i.p.v. naam-gestuurd.** Niet langer "één
Organisme per naam", maar "één Organisme per code die er nog geen heeft". Alleen de
afgetrokken term in de antecedent verandert:

```
-- was (naam-gestuurd): subtract = "naam is al een voorkeursNaam"
RULE eppoCodeMaaktOrganisme : eppoCode - voorkeursNaam~;V[Organisme*EPPOcode] |- voorkeursNaam~;I[Organisme];eppoCode

-- wordt (code-gestuurd): subtract = "code zit al op een Organisme"
RULE eppoCodeMaaktOrganisme : eppoCode - V[WetenschappelijkeNaam*Organisme];eppoCode[Organisme*EPPOcode] |- voorkeursNaam~;I[Organisme];eppoCode
```

**Wijziging 2 — bewaar de niet-gekozen namen als synoniem** (anders gaan ze bij de merge
verloren):

```
ROLE ExecEngine MAINTAINS eppoCodeSynoniem
RULE eppoCodeSynoniem : eppoCode;eppoCode[Organisme*EPPOcode]~ - voorkeursNaam~ |- synoniem~
VIOLATION ( TXT "{EX} InsPair;synoniem;Organisme;", TGT I, TXT ";WetenschappelijkeNaam;", SRC I )
```

**Waarom termineert dit nu?** Volg `X` met namen `A1, A2` opnieuw:

1. Ronde 1: code-gestuurde creatie maakt (in deze ronde nog) twee Organismen voor `X`;
   `OrganismeUniekeEPPO` merget ze tot één (voorkeursNaam = `A1`); `eppoCodeSynoniem` zet
   `A2` als synoniem.
2. Ronde 2: code `X` *heeft* nu een Organisme → de creatie-regel vuurt **niet** meer (dat is
   het verschil!); de merge is tevreden; het synoniem staat er al. **Nul violations → stop.**

De cruciale verandering: de creatie hangt nu aan de **code** (die de merge intact laat), niet
aan de **naam** (die de merge juist weggooit). Daardoor "herstelt" de creatie niet meer wat
de merge net deed. De lus is verbroken omdat de regels nu **samen vervulbaar** zijn: één
code → één Organisme → één voorkeursnaam + nul-of-meer synoniemen. Geen bijectie-eis meer.

---

## 6. Hetzelfde verschijnsel, andere oorzaak (oscillatie #2)

Na de fix van #1 verscheen een **tweede** oscillatie — nu tussen `OrganismeUniekeNaam` en
`eppoCodeMaaktOrganisme`. Dit is het **spiegelbeeld**: niet meerdere namen per code, maar
**meerdere codes per naam**.

`OrganismeUniekeNaam` merget Organismen met dezelfde voorkeursNaam. Door de code-gestuurde
creatie maakt het model nu één Organisme per code; als twee codes dezelfde naam dragen,
ontstaan twee Organismen met identieke voorkeursNaam → merge → maar `eppoCode` is `[UNI]`,
dus de merge gooit een code weg → die code krijgt weer geen Organisme → creatie maakt
opnieuw → lus. Wiskundig: dezelfde bijectie-eis, nu geschonden aan de **naam-kant**.

De oorzaak bleek **vuile data** in `EPPOcodes.xlsx`:
- zes niet-bestaande codes deelden de placeholder-naam `(code niet gevonden in EPPO)`;
- een O/0-typefout: `Begomovirus coheni` stond zowel onder `TYLCV0` (cijfer nul) als `TYLCVO`
  (letter O).

Hier is **keuze B (data repareren)** juist, niet A. Belangrijk methodisch punt: **raadpleeg
de bron van waarheid.** De EPPO-database (`data.eppo.int`) gaf uitsluitsel:

| Code | EPPO `/names` | Oordeel |
|---|---|---|
| `TYLCV0` | Begomovirus coheni | echt → houden |
| `TYLCVO` | `null` | bestaat niet → typefout, verwijderen |
| de 6 placeholder-codes | `null` | bestaan niet → verwijderen |

> **Les 3.** Eén symptoom (oscillatie), twee verschillende oorzaken, twee verschillende
> juiste fixes. Synoniemen zijn een *modelkwestie* (verzwak het model); typefouten zijn een
> *datakwestie* (repareer de data). De wiskundige diagnose (welke kant van de bijectie wordt
> geschonden, en is dat een echt domeinverschijnsel of een fout?) wijst je de juiste knop.

### Robuustheid achteraf: invariant → procesregel

Wat als er in de toekomst tóch weer tegenstrijdige data binnenkomt? Dan wil je dat de import
het **meldt** in plaats van crasht. Dat is **keuze C**, toegepast op de invariant
`checkEPPOcode` (die controleert of de EPPO-code van een POcombinatie overeenkomt met de
codelijst):

```
ROLE IMPORTER MAINTAINS checkEPPOcode      -- van invariant naar procesregel
RULE checkEPPOcode : ...
```

Nu blokkeert een inconsistentie de import niet meer; ze verschijnt als signaal voor de rol
`IMPORTER`. (Let op: doe dit alléén bij *leesbare checks*. Een invariant met een `{EX}`-fix
hoort bij de ExecEngine, niet bij een mens-rol — anders verschijnt de `{EX}`-tekst als
onleesbaar signaal.)

---

## 7. De validatie — hoe weet je dat het écht klopt?

Niet "het lijkt te werken", maar meten. Na de herbouw (`./nvwa_prototype_init.sh`):

1. **Geen oscillatie meer** in de log:
   ```bash
   docker compose logs prototype 2>&1 | grep -ci "maximum reruns exceeded"   # verwacht: 0
   ```
2. **De transactie rolt niet meer terug** — de data is nu echt geladen:
   | meting | vóór | na |
   |---|---|---|
   | `eppoCode[WN*EPPO]` (rijen) | 1 | 1400 |
   | Organismen | 792 | 1496 |
   | synoniem-paren | 0 | 4295 |
3. **De invarianten kloppen nu aantoonbaar** (de eisen die de oscillatie veroorzaakten):
   ```sql
   -- elke code hoort bij precies één Organisme? -> verwacht 0
   SELECT COUNT(*) FROM (SELECT eppoCode FROM Organisme WHERE eppoCode IS NOT NULL
                         GROUP BY eppoCode HAVING COUNT(*)>1) t;
   -- geen Organisme zonder code? -> verwacht 0
   SELECT COUNT(*) FROM Organisme WHERE eppoCode IS NULL;
   ```
4. **Spot-checks tegen de bron van waarheid:** `TYLCV0 → Begomovirus coheni` (één record,
   geen dubbel), en geen Organisme meer met de naam `(code niet gevonden in EPPO)`.

> **Les 4.** Een oscillatie-fix is pas af als je kunt aantonen dat (a) de lus weg is, (b) de
> omringende transactie nu slaagt, én (c) de invarianten die botsten nu daadwerkelijk gelden.
> Punt (c) is het bewijs dat je de tegenspraak hebt *opgelost*, niet *verstopt*.

---

## 8. Zelf naspelen (minimaal voorbeeld)

Naast deze les staan twee zelfstandige, foutvrij compilerende scripts:

- **`oscillatie-buggy.adl`** — reproduceert de oscillatie. Bevat de naam-gestuurde creatie
  plus de twee merge-regels (`uniekeCode`, `uniekeNaam`), en een populatie met twee namen op
  één code. Onderaan staat een tweede populatie (één naam, twee codes) die je kunt aanzetten
  om oscillatie #2 te zien.
- **`oscillatie-fixed.adl`** — de opgeloste versie (code-gestuurde creatie + `maakSynoniem`).
  Convergeert op het synoniem-geval.

**Type-checken** (valideert alleen de syntax/typen, niet de runtime-lus). Voer dit uit in de
map waarin de scripts staan:

```bash
docker run --rm --platform linux/amd64 -v "$PWD:/scripts" \
  ampersandtarski/ampersand-compiler:latest check /scripts/oscillatie-buggy.adl
# => "contains no type errors and no population errors."
```

**De oscillatie écht zien** vergt de *runtime* (de ExecEngine draait niet bij `check`, maar
bij het uitvoeren van een prototype). Genereer en start een prototype van het script — het
eenvoudigst in een [RAP-omgeving](../../tutorial-rap4.md), of met een lokale
[prototype-deployment](../deploying-your-prototype.md). Trigger daarna de ExecEngine (de "run
execengine"-actie of een data-import) en lees de log:

- met `oscillatie-buggy.adl` verschijnt `Maximum reruns exceeded` met de twee botsende regels;
- met `oscillatie-fixed.adl` daalt de run netjes naar nul violations.

**Experimenten die het begrip vastzetten:**
1. Verander in de buggy-versie de populatie naar twee *verschillende* codes voor twee
   *verschillende* namen. → Geen oscillatie. (Waarom? De bijectie-eis wordt niet geschonden.)
2. Haal in de fixed-versie de regel `maakSynoniem` weg. → Geen oscillatie, maar de tweede
   naam gaat *verloren*. (Les: termineren is niet hetzelfde als correct.)
3. Zet in de buggy-versie de tweede populatie (één naam, twee codes) aan. → Oscillatie #2,
   nu met `uniekeNaam` als botsende regel. Repareer hem door de typefout uit de populatie te
   halen (keuze B), niet door een regel te schrappen.

---

## 9. Samenvatting — de oscillatie als kans

- Een oscillatie (`Maximum reruns exceeded`) betekent: **je invarianten zijn, gegeven de
  data, samen onvervulbaar.** De automatische reparatie vindt geen fixpunt en cykelt.
- De foutmelding noemt de **botsende regels**. Dat is een cadeau: het lokaliseert de
  tegenspraak.
- Diagnostiseer **wiskundig**: welke onmogelijke eis leggen de regels samen op (hier: een
  bijectie naam ↔ code), en welke kant schendt de data?
- Kies bewust een knop: **model verzwakken** (A, bij een echt domeinverschijnsel als
  synoniemen), **data repareren** (B, bij fouten — raadpleeg de bron van waarheid), of
  **invariant tot signaal degraderen** (C, als een mens moet beslissen).
- **Valideer** dat de lus weg is, de transactie slaagt, én de invarianten nu echt gelden.
- Hoog nooit zomaar de rerun-limiet op: dat verbergt een logische tegenspraak in plaats van
  hem op te lossen.

> Een oscillatie is geen tegenslag maar **feedback**: de runtime bewijst dat je regels elkaar
> tegenspreken en wijst aan wáár. Gebruik die kennis om je specificatie consistent te maken —
> dan is je model er aantoonbaar beter van geworden.

---

*Verwante documenten:* [Automating Rules in Ampersand](../../conceptual/automated-rules.md)
(hoe ExecEngine-regels de populatie automatisch repareren — de achtergrond onder deze casus),
en [Best practices for Ampersand modellers](../best-practices.md).
