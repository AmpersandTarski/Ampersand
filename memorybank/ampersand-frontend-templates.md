# Ampersand Frontend Templates: Mechanisme en Gebruik

**Datum:** 4 maart 2026  
**Context:** Onderzoek naar templates voor de interface `DekkingStamtabellen` in het NVWA FC5-project

## Waar de templates staan

Ampersand genereert een Angular frontend op basis van StringTemplate-templates. De templates staan in:

```
~/git/PrototypeFramework/frontend/src/app/generated/.templates/
```

Elk `.html`-bestand in die map is een StringTemplate-bestand. De naam van het bestand correspondeert met een `BOX<TYPE>` of een Ampersand primitief type.

## Hoe Ampersand een template kiest

Ampersand kiest een template op basis van twee kenmerken in de ADL-interface-definitie: het box-type en het primitieve type van het doel-concept.

**Box-type → Box-template**

Het `BOX<TYPE>`-gedeelte in een interface-definitie bepaalt welk Box-template Ampersand gebruikt.

| ADL syntax | Template |
|---|---|
| `BOX<TABLE>` of `BOX<TABLE sortable>` | `Box-TABLE.html` |
| `BOX<FORM>` | `Box-FORM.html` |
| `BOX<TABS>` | `Box-TABS.html` |
| `BOX<RAW>` | `Box-RAW.html` |
| `BOX<SELECT>` | `Box-SELECT.html` |
| `BOX<FILTEREDDROPDOWN>` | `Box-FILTEREDDROPDOWN.html` |
| `BOX<NOVIEW>` | `Box-NOVIEW.html` |
| `BOX<PROPBUTTON>` | `Box-PROPBUTTON.html` |

**Primitief type → Atomic-template**

Voor elk veld in een box kiest Ampersand een Atomic-template op basis van het primitieve type van het doel-concept. Dat primitieve type staat in de `REPRESENT`-declaratie in het ADL-bestand. Als er geen `REPRESENT` staat, is het standaardtype `ALPHANUMERIC`.

| Primitief type | Template | Angular component |
|---|---|---|
| `ALPHANUMERIC` (standaard) | `Atomic-ALPHANUMERIC.html` | `<app-atomic-alphanumeric>` |
| `BIGALPHANUMERIC` | `Atomic-BIGALPHANUMERIC.html` | `<app-atomic-bigalphanumeric>` |
| `HUGEALPHANUMERIC` | `Atomic-HUGEALPHANUMERIC.html` | `<app-atomic-hugealphanumeric>` |
| `INTEGER` | `Atomic-INTEGER.html` | `<app-atomic-integer>` |
| `FLOAT` | `Atomic-FLOAT.html` | `<app-atomic-float>` |
| `BOOLEAN` | `Atomic-BOOLEAN.html` | `<app-atomic-boolean>` |
| `DATE` | `Atomic-DATE.html` | `<app-atomic-date>` |
| `DATETIME` | `Atomic-DATETIME.html` | `<app-atomic-datetime>` |
| `PASSWORD` | `Atomic-PASSWORD.html` | `<app-atomic-password>` |
| `OBJECT` | `Atomic-OBJECT.html` | `<app-atomic-object>` |

## Hoe CRUD de invulvelden bepaalt

Het `crud`-attribuut in het Atomic-template bepaalt of een veld een bewerkbaar inputveld of alleen-lezen tekst toont. Ampersand vult dit attribuut in met de CRUD-waarde uit de interface-definitie.

Een veld met `CRUD` (hoofdletter C en hoofdletter U) is bewerkbaar: de gebruiker kan een waarde invoeren of wijzigen. Een veld met `cRud` (kleine letter c en kleine letter u) is alleen-lezen.

Voorbeeld uit `Box-TABLE.html` (fragment):
```html
<app-atomic-bigalphanumeric
    [resource]="resource"
    [property]="resource.$name$"
    propertyName="$name$"
    crud="$crud$"
    ...
></app-atomic-bigalphanumeric>
```

De waarde `$crud$` komt direct uit de CRUD-annotatie in het ADL-bestand.

## Concreet voorbeeld: DekkingStamtabellen

De interface `DekkingStamtabellen` staat in `project/Dekking.adl` van het NVWA FC5-project:

```ampersand
INTERFACE DekkingStamtabellen FOR FAB : "_SESSION";V[SESSION*Dcode] cRud BOX<TABLE sortable>
  [ "Code"                                           : I[Dcode]       cRud
  , "Volledige ISPM-12 standaardtekst"               : dekkingISMP    CRUD
  , "Fragment 1 (vóór 1e invulplek)"                 : fragment1      CRUD
  , "Fragment 2 (tussen/na invulplek(ken))"           : fragment2      CRUD
  , "Fragment 3 (na 2e invulplek, alleen D4/D12)"    : fragment3      CRUD
  ]
```

De type-keten per veld:

**"Code"** heeft relatie `I[Dcode]`. `Dcode` heeft geen `REPRESENT`, dus het type is `ALPHANUMERIC`. Het veld krijgt `cRud`, dus het is alleen-lezen. Template: `Atomic-ALPHANUMERIC.html`.

**"Volledige ISPM-12 standaardtekst"** heeft relatie `dekkingISMP[Dcode*Dekkingtekst]`. `Dekkingtekst` heeft geen `REPRESENT` (zie `project/Dekking.adl`), dus het type is `ALPHANUMERIC`. Het veld krijgt `CRUD`, dus het is een bewerkbaar inputveld. Template: `Atomic-ALPHANUMERIC.html`.

**"Fragment 1/2/3"** heeft relaties `fragment1/2/3[Dcode*Tekst]`. `Tekst` heeft `REPRESENT Tekst TYPE BIGALPHANUMERIC` (zie onder andere `project/Eisen.adl`). De velden krijgen `CRUD`, dus het zijn bewerkbare inputvelden. Template: `Atomic-BIGALPHANUMERIC.html`.

De hele pagina `/dekkingstamtabellen` gebruikt dus:

- `Box-TABLE.html` voor de tabelstructuur met `<app-box-table>`
- `Atomic-ALPHANUMERIC.html` voor de velden "Code" en "Volledige ISPM-12 standaardtekst"
- `Atomic-BIGALPHANUMERIC.html` voor de velden "Fragment 1", "Fragment 2" en "Fragment 3"

## Werkwijze om templates te vinden bij een interface

Om de templates van een interface te bepalen, voer je de volgende stappen uit.

**Stap 1: zoek de interface-definitie in het ADL-bestand.**

```bash
grep -ri "INTERFACE InterfaceNaam" project/
```

**Stap 2: lees het `BOX<TYPE>`-gedeelte.** Dit geeft het Box-template.

**Stap 3: zoek per veld de relatie-declaratie.**

```bash
grep -E "RELATION veldnaam" project/*.adl
```

**Stap 4: zoek de `REPRESENT`-declaratie van het doel-concept.**

```bash
grep -E "REPRESENT.*Conceptnaam" project/*.adl
```

Als er geen `REPRESENT` staat, is het type `ALPHANUMERIC`.

**Stap 5: lees het template in de template-map.**

```bash
cat ~/git/PrototypeFramework/frontend/src/app/generated/.templates/Atomic-TYPNAAM.html
```

## Relatie tussen templates en gegenereerde code

Ampersand vult de StringTemplate-placeholders (`$naam$`, `$crud$`, etc.) in tijdens het `ampersand proto`-commando. De gegenereerde Angular-code staat daarna in `.proto/` in het projectmap. De templates zelf staan in `~/git/PrototypeFramework/frontend/src/app/generated/.templates/` en zijn de bron voor alle gegenereerde interfaces.
