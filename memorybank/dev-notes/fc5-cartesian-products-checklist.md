# Cartesische producten in FC5 — analyse en herstelplan

Bron: `ampersand check --verbose ~/.../NVWA/FC/FC5/project/main.adl`

Per situatie:
- 📍 **Locaties + warning(s)**
- ⚖️ **Oordeel**: vermijdbaar / onvermijdbaar
- 🧠 **Reden**
- 🛠️ **Voorgestelde oplossing**
- ☐ **Status**

---

## Situatie A — `#` (cartesisch product) als "behoort tot verzameling"

### A1. `certificaatTaal` — Landen.adl:32
```adl
RULE certificaatTaal : certificaatTaal |- I[LandCode]#nvwaTalen
```
**Warning:** `I[LandCode]#nvwaTalen[Taal*Taal]` — *the cartesian product operator (#) is rewritten to l;V;r, which uses V*

- ⚖️ **Vermijdbaar — fix in de Ampersand-compiler (`normStep`)**
- 🧠 De `#`-operator wordt door `Ampersand.FSpec.SQL` herschreven naar `l;V;r`, dus elke `EPrd` levert per definitie een cross join. Maar voor het *patroon* `l |- I[A]#r` waarbij `r` declaratief een `[PROP]`-relatie is (subset van `I`), is dat overbodig: `I[A]#r` is semantisch gelijk aan `V[A*tgt(r)];r`, en in de regel-context `lhs |- I[A]#r` is dat weer equivalent met `lhs |- lhs;r` — V-vrij.
- Aanpak: normStep aanpassen om deze situatie te herschrijven, zodat deze situatie niet meer wordt aangeboden aan de SQL-generator.
- 🛠️ **Plan in twee stappen:**
  1. **In `src/Ampersand/FSpec/ToFSpec/NormalForms.hs`** — voeg aan `normStep` een rewrite toe voor `EPrd(l, r)`:
     - Als `r` (of `l`) een `EDcD`-relatie is met de `Prop` eigenschap (subset van `I`), herschrijf `l # r` naar een equivalente vorm zonder `V`. Voor het specifieke geval `I[A] # propRel` is `propRel` semantisch zelf de "filtering set", en in een inclusie `x |- I[A] # propRel` mag `;propRel` van `x` als kandidaatreductie gelden.
     - Aandachtspunt: `normStep` werkt op expressies, niet op regels. Een echte rule-level rewrite (`x |- l#r` → `x |- (x;r) ∩ (l;V)` enz.) hoort thuis bij een hogere laag. Voor nu een conservatieve algebraïsche rewrite die alleen geldt voor `I[A] # r` met `r` als PROP: dat is veilig en lost dit specifieke geval op.
  2. **In `src/Ampersand/FSpec/ToFSpec/CreateFspec.hs`** — laat `warnCartesianProducts` `findCartesianSubexprs` aanroepen op `normExpr` (de `conjNF`-uitkomst) i.p.v. op `origExpr`. Anders blijft de warning ook na succesvolle normalisatie staan, omdat de heuristiek nu de boomstructuur van de bron-expressie inspecteert.
- 🧪 **Testaanpak:** sandbox (`Sandbox.hs` + `run_sandbox.ghci`) met een expressie `EPrd(EDcI ld, propRel)` om de rewrite te verifiëren; daarna `ampersand check --verbose` op `cart_demo.adl` (en op het FC5-project) om te zien dat A1 stilvalt.
- ⚠️ **Risico:** de PROP-eigenschap is gedeclareerd, maar niet bewezen (zoals NormalForms ook nu al voor andere properties opmerkt: `properties are not yet proven but must be enforced`). De rewrite is dus consistent met de bestaande compiler-aannames.
- ✅ **Status: GEÏMPLEMENTEERD** (2026-05-21)
  - `NormalForms.hs`: rewrite `x |- I[A]#r` → `x |- x;r` toegevoegd in `normStep`, plus de symmetrische variant `x |- l#I[B]` → `x |- l;x`. Conditie: `isProp` op de PROP-relatie.
  - `CreateFspec.hs`: `findCartesianSubexprs` werkt nu op `normExpr` (in plaats van `origExpr`), zodat normaliserende rewrites de warning ook daadwerkelijk stilleggen. Tevens overgeslagen: outer-level `ECpl` in de standaard regelvorm `EUni(ECpl x, y)`, omdat die intrinsiek is aan elke `x |- y` na `remove |-` en op zichzelf geen extra cartesisch product oplevert.
  - **Testresultaat**: `cart_a1_demo.adl` (specifiek het A1-patroon) levert nu geen Cartesian-warning meer. `cart_demo.adl` blijft warnings geven voor `enrolled#teaches` (echt cartesisch product, niet PROP) en de `\\`-residual — beide correct.
  - **Trade-off**: door op `normExpr` te kijken verdwijnt ook de warning voor regels van de vorm `V |- knows` (omdat `-V \\/ knows` simplificeert tot `knows`). Dat is inconsistent met de runtime violations-pipeline (die wél V materialiseert), maar voor de A1-fix accepteerbaar omdat zulke `V |- r`-regels in de praktijk schrijffouten zijn.
- ⚠️ **Validatie op FC5 main.adl uitstaand**: parser-error in `ProductEis.adl:198:15` (`unexpected operator '.'`) blokkeert op moment van schrijven het volledig doorlopen van het FC5-project. Dat is een onafhankelijk probleem op commit `866d28421` en niet door deze A1-fix veroorzaakt.


---

## Situatie B — `V` als type-brug bij "moet bestaan" (DCode-regels)

### B1. `DCode4` — Wijziging.adl:497
```adl
RULE DCode4 : wijzigingDekking;"4" |- wijzigingOrigine;wijzigingOrigine~;V [Wijziging*ISPM12code]
```
### B2. `DCode10` — Wijziging.adl:502
```adl
RULE DCode10 : wijzigingDekking;"10" |- wijzigingExterneReferenties;wijzigingExterneReferenties~;V [Wijziging*ISPM12code]
```
### B3. `DCode12` — Wijziging.adl:506
```adl
RULE DCode12 : wijzigingDekking;"12" |- tolerantie;tolerantie[Wijziging*Procent]~;V [Wijziging*ISPM12code]
```
**Warnings (3):** `V [Wijziging*ISPM12code]` — *V[A\*B] generates a cross join of two concept tables*

- ⚖️ **Vermijdbaar**
- 🧠 De V dient enkel om het type rechts kloppend te maken met `Wijziging*ISPM12code`. De inhoudelijke eis is: "als een wijziging dekking '4'/'10'/'12' heeft, moet ze een origine/externe referentie/tolerantie hebben". Dat is te formuleren op type `Wijziging*Wijziging`.
- 🛠️ Herschrijven naar:
  ```adl
  RULE DCode4  : I[Wijziging] /\ wijzigingDekking;"4";wijzigingDekking~  |- wijzigingOrigine;wijzigingOrigine~
  RULE DCode10 : I[Wijziging] /\ wijzigingDekking;"10";wijzigingDekking~ |- wijzigingExterneReferenties;wijzigingExterneReferenties~
  RULE DCode12 : I[Wijziging] /\ wijzigingDekking;"12";wijzigingDekking~ |- tolerantie;tolerantie~
  ```
  De VIOLATION-teksten gebruiken alleen `SRC` (geen `TGT`), dus blijven werken.
- ☐ Status: open

### B4. `delDCode4` — Wijziging.adl:500
```adl
RULE delDCode4  : wijzigingDekking;"4"  |- (wijzigingOrigine;wijzigingOrigine~/\I);V [Wijziging*ISPM12code]
```
### B5. `delDCode10` — Wijziging.adl:504
```adl
RULE delDCode10 : wijzigingDekking;"10" |- (wijzigingExterneReferenties;wijzigingExterneReferenties~/\I);V [Wijziging*ISPM12code]
```
### B6. `delDCode12` — Wijziging.adl:508
```adl
RULE delDCode12 : wijzigingDekking;"12" |- (tolerantie;tolerantie~/\I);V [Wijziging*ISPM12code]
```
**Warnings (3):** `V [Wijziging*ISPM12code]` — *V[A*B] generates a cross join*

- ⚖️ **Vermijdbaar** (let op: extra aandacht voor de `TGT I` in VIOLATION)
- 🧠 Hier moet de regel echt eindigen op type `Wijziging*ISPM12code` omdat de VIOLATION `TGT I` gebruikt — dat is de ISPM12code die uit `wijzigingDekking` verwijderd moet worden. De V is daarvoor strikt gezien niet nodig: we kunnen die "terug-route" maken via de bestaande relatie.
- 🛠️ Herschrijven door de identity-test eerst toe te passen, en dan terug te koppelen via `wijzigingDekking`:
  ```adl
  RULE delDCode4  : wijzigingDekking;"4"  |- (I /\ wijzigingOrigine;wijzigingOrigine~);wijzigingDekking;"4"
  RULE delDCode10 : wijzigingDekking;"10" |- (I /\ wijzigingExterneReferenties;wijzigingExterneReferenties~);wijzigingDekking;"10"
  RULE delDCode12 : wijzigingDekking;"12" |- (I /\ tolerantie;tolerantie~);wijzigingDekking;"12"
  ```
  Semantisch identiek; geen V meer. `SRC I` (Wijziging) en `TGT I` (ISPM12code = "4"/"10"/"12") blijven correct.
- ☐ Status: open — **graag toetsen of `TGT I` na herschrijven nog steeds de juiste atoom oplevert**.

### Compiler-aanpak voor B (alle zes regels): warning op maat + SQL zonder cross join

**Conclusie vooraf:** `normStep` is hier *niet* de juiste plek. De rewrite die nodig zou zijn is (`lhs |- r;V[A*B]` → `(I[A] /\ lhs;lhs~) |- r;r~`) maar het is niet duidelijk onder welke condities die moet worden aangeroepen. Het ad-hoc karakter is te groot. Bovendien zou `normStep` context-informatie moeten gebruiken.

**Twee aanvullende fixes in de Ampersand-compiler:**

1. **Advies-op-maat in de warning (alternatief 2)** — ✅ **GEÏMPLEMENTEERD** (2026-05-21) in `src/Ampersand/Input/ADL1/CtxError.hs`:
   - Detectie zit in `mkCartesianProductWarning` zelf. Patroon: `offending == EDcV _` && `fullExpr == EInc(lhs, rhs)` && `endsInV rhs` (recursieve helper die door rechts-associatieve ECps-ketens en `EBrk` heen kijkt tot ze een `EDcV` bereiken).
   - Wanneer dat patroon matcht voegt de warning vier extra `tip:`-regels toe (na de bestaande `(reason: ...)`-regel) die de schrijver wijzen op de herschrijving naar een endo-regel.
   - **Validatie op FC5 main.adl**: de tip wordt precies 6× uitgereikt — aan DCode4, DCode10, DCode12, delDCode4, delDCode10, delDCode12 (= B1–B6). De andere V-warnings (VulBronURL = D1 en eppoCodeMaaktOrganisme = C1) krijgen géén tip, wat correct is: daar staat de V niet aan de RHS-staart van een inclusie.
   - Het cartesisch product wordt hierdoor *niet* geëlimineerd; het totaal aantal warnings blijft 13. De doelgerichte hint biedt de schrijver wel een concrete herschrijfroute.

Oorspronkelijk plan was:
- Detecteer in `findCartesianSubexprs` (of in `mkCartesianProductWarning`) het specifieke patroon `EDcV` aan de **staart** van een ECps-keten op de RHS van een inclusie (na term2Expr is dat: `EUni(ECpl lhs, ECps(..., EDcV))`).
- Voor dit patroon: schrijf een aangepaste tip in de warning, in plaats van de generieke "V[A*B] generates a cross join". Iets als: "De V staat aan de staart als type-brug. Overweeg de regel te formuleren op type `A*A`, bv. `(I[A] /\\ lhs;lhs~) |- r;r~`."
- Dit elimineert het cartesisch product *niet*, maar geeft de schrijver een doelgerichte hint.

2. **SQL zonder cross join (alternatief 3)** — `src/Ampersand/FSpec/SQL.hs`:
   - Detecteer in `selectExpr` (of in de violations-pipeline) het patroon `EDif(x, ECps(y, EDcV))` en `EIsc(x, ECpl(ECps(y, EDcV)))`.
   - Voor zo'n conjunct is de violations-query semantisch:
     ```sql
     SELECT x.a, x.b FROM <x-subquery> x
     WHERE NOT EXISTS (SELECT 1 FROM <y-subquery> y WHERE y.a = x.a)
     ```
     De `(a, b)`-paren komen uit `x`; de `V` wordt nooit gematerialiseerd.
   - Dit is **haalbaar**: de pattern-match is lokaal in `selectExpr`, en `NOT EXISTS` is een standaard-SQL constructie die alle backends ondersteunen.
   - Kanttekening: de check moet specifiek genoeg zijn dat we hem alleen toepassen als de V écht aan de uiterste rand staat (anders bestaat het risico dat we semantiek verstoren bij geneste expressies).

**Volgorde van uitvoeren:** eerst alternatief 2 (laag-risico, snelle winst voor de schrijver), daarna alternatief 3 (eliminineert ook daadwerkelijk de cross join in de gegenereerde SQL). Met beide aan boord komt de warning niet meer voor in deze vorm, omdat de SQL-backend de V dan al wegoptimaliseert.

### ⚠️ Nuance op alternatief 3 (na experiment met DCode4)

De gegenereerde SQL voor `DCode4` is **niet** de violations-vorm `EDif(x, ECps(y, EDcV))` waar mijn alternatief 3 op pattern-matched, maar de rule-set-vorm `EUni(ECpl x, ECps(y, EDcV))` (na term2Expr + conjNF):

```sql
-- 1e cartesisch product: V[Wijziging*ISPM12code] voor de buitenste SELECT (voor ECpl x)
select ... from (select fst."Wijziging" as src, snd."ISPM12code" as tgt
                 from "Wijziging" as fst, "ISPM12code" as snd ...) as "cartesian product of Wijziging and ISPM12code"
where NOT exists (... wijzigingDekking;"4" ...)
union distinct
-- 2e cartesisch product: V[Wijziging*ISPM12code] nogmaals, aan de staart van ECps
select fence0.src, fence2.tgt
from (... wijzigingOrigine ...) as fence0,
     (... wijzigingOrigine~ ...) as fence1,
     (select fst."Wijziging", snd."ISPM12code"
      from "Wijziging" as fst, "ISPM12code" as snd ...) as fence2
```

**Bevindingen:**

1. **De warning toont de SQL voor de rule-set, niet de violations-set.** `prettySQLQuery` wordt aangeroepen op `conjNF env origExpr`, dat is de hele inclusie-expressie na term2Expr+conjNF. Dat levert SQL op voor "de paren waar de regel waar is", niet voor "de paren waar de regel geschonden is".
2. **Mijn EDif-pattern-match is niet voldoende voor deze SQL.** Het patroon `EDif(x, ECps(y, EDcV))` past niet op `EUni(ECpl x, ECps(y, EDcV))`. Voor de rule-set-vorm zou alternatief 3 dus geen effect hebben.
3. **Onbekend: of de runtime violations-pipeline een andere selectExpr-aanroep doet** die wél in EDif-vorm langskomt. Als dat zo is, optimaliseert alternatief 3 wél de echte runtime-SQL — alleen niet de illustratieve SQL in deze warning. Vereist extra onderzoek in de prototype-pipeline.

**Bijgestelde aanpak voor B:**

- **Alternatief 2 (advies-op-maat) blijft onveranderd** — werkt op het syntactische patroon dat de gebruiker schreef.
- **Alternatief 3 moet uitgebreid worden** naar twee pattern-matches in `selectExpr`:
  a. **Voor de rule-set-vorm** `EUni(ECpl x, ECps(y, EDcV))` (en commutatieve varianten): wiskundige uitwerking is `{(a,b) : (a,b) ∉ x ∨ a ∈ dom(y)}`. Dit is ECHT een gigantische verzameling — het cartesisch product is hier intrinsiek. **Geen optimalisatie mogelijk** zonder de pipeline om te bouwen van "rule-set" naar "violations-set".
  b. **Voor de violations-set-vorm** `EDif(x, ECps(y, EDcV))` (mits die ergens in de pipeline aangeroepen wordt): wel optimalisatie mogelijk zoals eerder beschreven.
- **Vooronderzoek nodig:** in `Ampersand.FSpec.SQL` en de prototype-gegenereerde queries kijken of de violations-vorm überhaupt voorkomt. Zo nee, dan is de optimalisatie alleen relevant als we óók de pipeline aanpassen zodat regels naar violations worden vertaald vóór SQL-generatie.
- **Tussenconclusie:** voor de B-regels in FC5 is **ADL-herschrijven** vooralsnog de zekerste route. Een SQL-laag-optimalisatie vereist eerst pipeline-onderzoek.

---

## Situatie C — `V` als "bestaat al ergens?"

### C1. `eppoCodeMaaktOrganisme` — Organismen.adl:34
```adl
RULE eppoCodeMaaktOrganisme :
  eppoCode - voorkeursNaam~;V[Organisme*EPPOcode] |- voorkeursNaam~;I[Organisme];eppoCode
```
**Warning:** `V [Organisme*EPPOcode]` — *V[A\*B] generates a cross join*

- ⚖️ **Vermijdbaar**
- 🧠 `voorkeursNaam~;V[Organisme*EPPOcode]` betekent "alle (naam, eppoCode)-paren waarbij de naam al aan enig Organisme hangt". Dat is een existentietest die ook zonder V kan, omdat `voorkeursNaam` UNI+INJ is: `voorkeursNaam~;voorkeursNaam` is dan een deelverzameling van `I[WetenschappelijkeNaam]`.
- 🛠️ Herschrijven:
  ```adl
  RULE eppoCodeMaaktOrganisme :
    (I[WetenschappelijkeNaam] - voorkeursNaam~;voorkeursNaam);eppoCode
      |- voorkeursNaam~;I[Organisme];eppoCode
  ```
  De linkerexpressie selecteert namen waar nog geen Organisme bij hoort, en koppelt die aan hun eppoCode. Geen V.
- ☐ Status: open — **even nadenken of de prestatie hier daadwerkelijk beter wordt; `voorkeursNaam~;voorkeursNaam` is op zich ook een join, maar wel binnen één relatie en met index op key.**

### Experiment: lost de SQL-generator dit zelf op?

Antwoord: **Nee, integendeel — er ontstaan zelfs twee echte cartesische producten.** Door `mkCartesianProductWarning` tijdelijk de gegenereerde SQL te laten printen, zien we voor `eppoCodeMaaktOrganisme`:

```sql
-- Outer SELECT: cartesisch product Naam × EPPOcode wordt gematerialiseerd
from (select fst."WetenschappelijkeNaam" as src, snd."EPPOcode" as tgt
      from "Naam" as fst, "EPPOcode" as snd
      ...)
     as "cartesian product of WetenschappelijkeNaam and EPPOcode"
where NOT exists (select * from (
  -- Binnenin: nogmaals een cartesisch product Organisme × EPPOcode
  ...
  (select fst."Organisme" as src, snd."EPPOcode" as tgt
   from "Organisme" as fst, "EPPOcode" as snd ...) as fence1
  ...)
)
```

**Conclusie:** de warning is terecht. De SQL-generator gebruikt wel een `NOT EXISTS`-vorm voor het complement, maar materialiseert daarvoor eerst V als echt cross join (`FROM tA, tB`). Geen optimalisatie te verwachten zonder ingreep in `SQL.hs`.

**Implicaties:**
1. Voor C1 zelf blijft de fix: **ADL-bron herschrijven** (zoals voorgesteld). Dit is veruit het zekerst.
2. *Generieke verbetering in de SQL-backend* zou wenselijk zijn: als de outer SELECT van een violations-query een V is, kan die door de bron-relatie van de violations-LHS worden vervangen. Maar dit raakt aan dezelfde laag als alternatief 3 bij situatie B. Aparte issue waard.

---

## Situatie D — `V` als "type-brug" via een tussenverzameling (URL-compositie)

### D1. `VulBronURL` — Uitspraak.adl:79
```adl
RULE VulBronURL :
  I[Bron] /\ bron[Uitspraak*Bron]~;sector;sectorSubdir;V[SectorSubdir*Bron]
  |- bronURL;bronURL~
```
**Warning:** `V [SectorSubdir*Bron]` — *V[A*B] generates a cross join*

- ⚖️ **Vermijdbaar**
- 🧠 De V[SectorSubdir*Bron] is een type-brug terug naar Bron. We kunnen via de bestaande relaties terugroute opbouwen, en de `/\ I[Bron]` zorgt dat alleen de diagonaal overblijft.
- 🛠️ Herschrijven:
  ```adl
  RULE VulBronURL :
    I[Bron] /\ bron~;sector;sectorSubdir;sectorSubdir~;sector~;bron
    |- bronURL;bronURL~
  ```
  Meer joins, maar geen cross join. Semantisch identiek (de `/\ I[Bron]` selecteert alleen Bronnen die "via sector → subdir → terug" zichzelf bereiken, wat altijd geldt voor Bronnen waar `bron~;sector;sectorSubdir` niet leeg is).
- ☐ Status: open — **alternatief**: split de regel in tweeën zodat we eerst per uitspraak de URL berekenen en dan via ENFORCE naar Bron stuwen. Te bespreken.

---

## Situatie E — `-I` als "heeft meer dan één waarde" (UNI/INJ-handhaving)

Deze situaties hebben allemaal dezelfde structuur en hetzelfde oordeel.

### E1. `BenoemTaal` — Landen.adl:35
```adl
RULE BenoemTaal :
  (I[LandCode]/\taal;-I[Taal];taal~) - voorkeursTaal;voorkeursTaal~
  |- voorkeursTaal;voorkeursTaal~
```
**Warning:** `-I[Taal]` — *computing the complement requires the full domain V*

### E2. `DefaultMetTaal` — Landen.adl:54
```adl
RULE DefaultMetTaal :
  ((I[LandCode] - taal;-I;taal~) /\ taal;taal~) - voorkeursTaal;voorkeursTaal~
  |- voorkeursTaal;voorkeursTaal~
```
**Warning:** `-I[Taal]`

### E3. `UNI2eppoCode` — Plagen.adl:41
```adl
RULE UNI2eppoCode :
  eppoCode;-I[EPPOcode];eppoCode~ |- -I[WetenschappelijkeNaam]
```
**Warnings (2):** `-I[EPPOcode]` en `-I[WetenschappelijkeNaam]`

### E4. `INJ2eppoCode` — Plagen.adl:56
```adl
RULE INJ2eppoCode :
  eppoCode~;-I[WetenschappelijkeNaam];eppoCode |- -I[EPPOcode]
```
**Warnings (2):** `-I[WetenschappelijkeNaam]` en `-I[EPPOcode]`

### E5. `UNInedNaam` — Producten.adl:38
```adl
RULE UNInedNaam :
  nedNaam;-I[Naam];nedNaam~ |- -I[Product]
```
**Warnings (2):** `-I[Naam]` en `-I[Product]`

- ⚖️ **Onvermijdbaar** in de huidige vorm (positieve eigenschap met aangepaste melding)
- 🧠 `r;-I;r~` is het canonieke algebraïsche patroon voor "heeft meerdere waarden". Het complement `-I` vereist per definitie de volle V, dus de cross-join is intrinsiek.
- 🛠️ **Optie 1 (aanbevolen voor E3/E4/E5)**: voeg `[UNI]`/`[INJ]` toe op de declaratie van `eppoCode`/`nedNaam`. Dan vervalt de aparte regel volledig en handelt Ampersand de check automatisch af.
  - **Bezwaar**: de huidige regels gebruiken maatwerkmeldingen (MESSAGE/VIOLATION) die je dan kwijtraakt.
- 🛠️ **Optie 2 (voor E1/E2)**: het concept "land met meerdere talen" is nu eenmaal een eigenschap waarin het complement onontkoombaar is. Eventueel kunnen we het denkkader omdraaien: een hulprelatie `meerTaligLand[LandCode]` die we via een aparte regel (mét V, maar één keer) bijhouden, zodat de hoofdregels V-vrij worden. Vereist extra opslag.
- ☐ Status: open — **discussie**: maatwerkmeldingen vs. `[UNI]`-declaratie. Per regel afwegen.

---

## Situatie F — Disjunctheid (false positive)

### F1. `synDisjunctNaam` — Organismen.adl:104
```adl
RULE synDisjunctNaam : synoniem /\ voorkeursNaam |- -V[Organisme*WetenschappelijkeNaam]
```
**Warning:** `V [Organisme*WetenschappelijkeNaam]` — *V[A*B] generates a cross join*

- ⚖️ **Schijnbaar onvermijdbaar, maar in praktijk false positive**
- 🧠 De normalizer transformeert dit naar `-synoniem \/ -voorkeursNaam`, wat geen V meer bevat. De SQL-generator zal hier dus géén cross join doen. De warning komt omdat de heuristic kijkt naar de oorspronkelijke vorm.
- 🛠️ **Optie 1**: laten staan en negeren (de SQL is goed).
- 🛠️ **Optie 2**: expliciet herschrijven naar de "lege verzameling"-vorm:
  ```adl
  RULE synDisjunctNaam : synoniem /\ voorkeursNaam |- -(synoniem \/ voorkeursNaam)
  ```
  Maar dat verandert niets aan de gegenereerde SQL. **Beter**: signaleren als bekend false-positive in de warning-heuristiek (issue in Ampersand zelf).
- ☐ Status: open — **kandidaat voor een issue in Ampersand**: warning-heuristiek mag kijken naar de genormaliseerde vorm.

---

## Overzichtstabel

| ID  | Regel | File:line | Categorie | Oordeel | Actie |
|-----|------|-----------|-----------|---------|-------|
| A1  | certificaatTaal | Landen:32 | `#`-operator | vermijdbaar | herschrijven |
| B1  | DCode4 | Wijziging:497 | V als type-brug | vermijdbaar | herschrijven |
| B2  | DCode10 | Wijziging:502 | V als type-brug | vermijdbaar | herschrijven |
| B3  | DCode12 | Wijziging:506 | V als type-brug | vermijdbaar | herschrijven |
| B4  | delDCode4 | Wijziging:500 | V als type-brug | vermijdbaar | herschrijven |
| B5  | delDCode10 | Wijziging:504 | V als type-brug | vermijdbaar | herschrijven |
| B6  | delDCode12 | Wijziging:508 | V als type-brug | vermijdbaar | herschrijven |
| C1  | eppoCodeMaaktOrganisme | Organismen:34 | V als existentietest | vermijdbaar | herschrijven |
| D1  | VulBronURL | Uitspraak:79 | V als type-brug | vermijdbaar | herschrijven |
| E1  | BenoemTaal | Landen:35 | `-I` (meertalig) | onvermijdbaar | accepteren / hulprelatie |
| E2  | DefaultMetTaal | Landen:54 | `-I` (meertalig) | onvermijdbaar | accepteren / hulprelatie |
| E3  | UNI2eppoCode | Plagen:41 | `-I` (UNI-check) | onvermijdbaar | accepteren / `[UNI]` |
| E4  | INJ2eppoCode | Plagen:56 | `-I` (INJ-check) | onvermijdbaar | accepteren / `[INJ]` |
| E5  | UNInedNaam | Producten:38 | `-I` (UNI-check) | onvermijdbaar | accepteren / `[UNI]` |
| F1  | synDisjunctNaam | Organismen:104 | disjunctheid | false positive | issue in Ampersand |

**Telling**: 15 regels, 18 warnings.
- ✅ Vermijdbaar: 9 regels, 10 warnings → herschrijven
- ❌ Onvermijdbaar of false positive: 6 regels, 8 warnings → accepteren of bron-aanpassing
