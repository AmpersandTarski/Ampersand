# Handoff: een `--sql-dialect` optie voor de Ampersand database-generator

**Doel:** Ampersand zo aanpassen dat de gebruiker met een command-line optie kan kiezen of de
gegenereerde database-DDL voor **MySQL/MariaDB** (huidig gedrag, blijft de default) of voor
**Oracle** wordt geproduceerd.

**Status:** ontwerp / klaar om uit te voeren.
**Repo:** `~/git/Ampersand` (Haskell, gebouwd met `stack`).
**Doelgroep van dit document:** database-kundige ontwerpers én Claude Code. Elke taak is los uit te
voeren, noemt de exacte bestanden en functies, en heeft een eigen acceptatiecriterium plus een
expliciete lijst van vereiste voorgaande taken. De taken samen realiseren het doel.

---

## 1. Probleemstelling

De generator produceert vandaag uitsluitend MySQL/MariaDB-DDL. Dat is herkenbaar aan constructies
die op Oracle niet bestaan of anders heten: de table-opties `ENGINE = InnoDB ... ROW_FORMAT = DYNAMIC`,
de MySQL-uitbreiding `TIMESTAMP ON UPDATE CURRENT_TIMESTAMP`, en kolomtypes als `TEXT`, `MEDIUMTEXT`,
`BIGINT` en `BOOLEAN`. We willen die dialect-specifieke stukken niet langer hardcoden, maar laten
afhangen van één keuze die via de command line binnenkomt.

Het verschil is het best te zien aan één tabel. Hieronder een representatieve tabel uit een
gegenereerd `database.sql` (een "object"-tabel met een tekstkolom, een datumkolom en een index).

### 1a. Wat de generator nu produceert (MySQL/MariaDB)

```sql
CREATE TABLE "Bewijsmiddel"
     ( "Bewijsmiddel" VARCHAR(255) UNIQUE NOT NULL /* OBJECT */
     , "bewijsmiddelBeschrijving" TEXT DEFAULT NULL /* BIGALPHANUMERIC */
     , "bewijsmiddelDatum" DATE DEFAULT NULL /* DATE */
     , "bewijsmiddelNaam" VARCHAR(255) DEFAULT NULL /* ALPHANUMERIC */
     , PRIMARY KEY ("Bewijsmiddel")
     , "ts_insertupdate" TIMESTAMP ON UPDATE CURRENT_TIMESTAMP NULL DEFAULT CURRENT_TIMESTAMP
     ) ENGINE     = InnoDB DEFAULT CHARACTER SET UTF8MB4 COLLATE UTF8MB4_NOPAD_BIN
     , ROW_FORMAT = DYNAMIC
;
CREATE INDEX "Bewijsmiddel_0" ON "Bewijsmiddel" ("bewijsmiddelDatum");
```

### 1b. Wat de generator voor Oracle zou moeten produceren

```sql
CREATE TABLE "Bewijsmiddel"
     ( "Bewijsmiddel" VARCHAR2(255) NOT NULL /* OBJECT */
     , "bewijsmiddelBeschrijving" CLOB DEFAULT NULL /* BIGALPHANUMERIC */
     , "bewijsmiddelDatum" DATE DEFAULT NULL /* DATE */
     , "bewijsmiddelNaam" VARCHAR2(255) DEFAULT NULL /* ALPHANUMERIC */
     , "ts_insertupdate" TIMESTAMP DEFAULT CURRENT_TIMESTAMP
     , CONSTRAINT "Bewijsmiddel_pk" PRIMARY KEY ("Bewijsmiddel")
     )
;
CREATE INDEX "Bewijsmiddel_0" ON "Bewijsmiddel" ("bewijsmiddelDatum");
-- Trigger voor het 'laatst gewijzigd'-gedrag dat MySQL met ON UPDATE inbakt:
CREATE OR REPLACE TRIGGER "Bewijsmiddel_tsiu"
  BEFORE INSERT OR UPDATE ON "Bewijsmiddel"
  FOR EACH ROW
BEGIN
  :NEW."ts_insertupdate" := CURRENT_TIMESTAMP;
END;
/
```

De relevante verschillen, regel voor regel:

| Aspect | MySQL/MariaDB (nu) | Oracle (doel) |
|---|---|---|
| Korte string | `VARCHAR(255)` | `VARCHAR2(255)` |
| Lange tekst | `TEXT` / `MEDIUMTEXT` / `HUGE…` | `CLOB` |
| Binair | `BLOB` / `MEDIUMBLOB` / `LONGBLOB` | `BLOB` |
| Geheel getal | `BIGINT` | `NUMBER(19)` |
| Boolean | `BOOLEAN` | `NUMBER(1)` (of `CHAR(1)` + check) |
| Float | `FLOAT` | `BINARY_DOUBLE` |
| "Laatst gewijzigd" | `TIMESTAMP ON UPDATE CURRENT_TIMESTAMP` | `TIMESTAMP DEFAULT CURRENT_TIMESTAMP` + `BEFORE UPDATE`-trigger |
| Table-opties | `ENGINE = InnoDB … COLLATE …`, `ROW_FORMAT = DYNAMIC` | *(weglaten; charset/tablespace regelt de DBA)* |
| Redundante `UNIQUE` naast `PRIMARY KEY` | aanwezig | weglaten (PK impliceert uniek) |
| `DROP TABLE IF EXISTS x` | ondersteund | Oracle kent geen `IF EXISTS` (< 23c); PL/SQL-block of weglaten |
| `SHOW COLUMNS FROM x` | ondersteund | `SELECT … FROM user_tab_columns WHERE table_name = …` |
| Identifier-quoting | `"…"` (vereist `ANSI_QUOTES`) | `"…"` (standaard; let op: hoofdlettergevoelig) |

> **Belangrijk over identifiers in Oracle.** Oracle staat dubbele quotes toe (dat is juist de
> ISO-standaard), maar daardoor worden namen **hoofdlettergevoelig** en gelden er lengtegrenzen
> (30 bytes vóór Oracle 12.2, 128 bytes daarna). Tabelnamen als `wijzigingProductOrganismeEisen`
> en gegenereerde indexnamen met een suffix kunnen die 30-grens overschrijden. Zie Taak 7.

---

## 2. Scope en aanpak

### In scope
De generatie van de **databasestructuur** (`database.sql`): `CREATE TABLE`, kolomtypes, primary
keys, indexen, en de bijbehorende table-opties. Dit is wat in dit handoff-document wordt opgelost.

### Expliciet buiten scope (fase 2, apart traject)
De **runtime-queries** die de prototype-backend uitvoert. Ampersand genereert niet alleen de
structuur, maar ook `SELECT`/`INSERT`-queries (zie `src/Ampersand/FSpec/SQL.hs` en
`src/Ampersand/Prototype/PHP.hs`, die o.a. backtick-quoting en `mysqli_*`-functies gebruiken).
Volledige Oracle-ondersteuning op runtime-niveau is een veel groter project. Dit handoff-document
maakt het **DDL**-spoor dialect-bewust en laat de runtime-laag ongemoeid; de PHP-prototype-backend
blijft dus MySQL. Het ontwerp (Taak 1) wordt wel zó opgezet dat de runtime-laag later hetzelfde
`SqlDialect`-mechanisme kan hergebruiken.

### Architectuur: waar zit wat?

```
app/Ampersand/Main.hs ─ command-line entry
  └─ src/Ampersand/Options/FSpecGenOptsParser.hs   ← CLI-vlaggen (Taak 2)
       └─ src/Ampersand/Misc/HasClasses.hs         ← FSpecGenOpts-record + Has*-lenses (Taak 2,6)
            └─ src/Ampersand/Prototype/GenBackend.hs ← schrijft database.sql (Taak 6)
                 └─ src/Ampersand/Output/FSpec2SQL.hs ← databaseStructureSql (Taak 6)
                      └─ src/Ampersand/Prototype/TableSpec.hs ← ALLE DDL-bouwstenen (Taak 3,4,5,7)
                           └─ src/Ampersand/FSpec/FSpec.hs ← showSQL :: TType -> Text (Taak 3)
```

De kerngedachte: introduceer één type `SqlDialect = MySQL | Oracle`, laat dat als command-line
optie binnenkomen, en geef het door tot in de pure functies die de DDL-tekst opbouwen. Vandaag zijn
die functies puur en dialect-blind (bv. `databaseStructureSql :: FSpec -> Text`); ze krijgen er een
`SqlDialect`-parameter bij.

---

## 3. Afhankelijkheden en parallellisatie

Elke taak vermeldt hieronder welke voorgaande taken **voltooid** moeten zijn voordat hij begint.
Er zijn twee soorten afhankelijkheid, en het onderscheid is belangrijk voor parallel werken:

- **(logisch)** — de taak heeft code/typen uit een eerdere taak nodig om te compileren of te werken.
- **(bestand)** — de taak raakt hetzelfde bronbestand als een eerdere taak. Logisch zou hij parallel
  kunnen, maar om merge-conflicten te voorkomen wordt hij erná gepland. **Twee taken die elkaar niet
  als voorganger hebben, raken gegarandeerd verschillende bestanden en mogen dus parallel.**

De grote gemene deler is `src/Ampersand/Prototype/TableSpec.hs`: de Taken 3, 4, 5, 6 en 7 wijzigen
dit ene bestand. Daarom vormen die een keten (3 → 4 → 5 → 7 → 6) in plaats van echte parallellie.

```
        ┌────────────────────────────────────────────┐
T1 ─────┤                                              │
(geen)  │   T2  (parallel met de TableSpec-keten)      ├── T6 ── T8
        │   T3 → T4 → T5 → T7  (keten op TableSpec.hs) │
        └────────────────────────────────────────────┘
```

Concreet betekent dit: **T2 mag parallel met de keten T3→T4→T5→T7** (verschillende bestanden), en
alle TableSpec-werk is serieel. T6 voegt beide sporen samen; T8 sluit af.

Overzicht van geraakte bestanden per taak (zo zie je de conflicten in één oogopslag):

| Taak | Geraakte bestanden |
|---|---|
| T1 | `Types/SqlDialect.hs` *(nieuw)*, `ampersand.cabal` |
| T2 | `Misc/HasClasses.hs`, `Options/FSpecGenOptsParser.hs` |
| T3 | `FSpec/FSpec.hs`, `Prototype/TableSpec.hs` |
| T4 | `Prototype/TableSpec.hs` |
| T5 | `Prototype/TableSpec.hs` |
| T6 | `Prototype/GenBackend.hs`, `Output/FSpec2SQL.hs`, `Prototype/TableSpec.hs`, `Prototype/PHP.hs` |
| T7 | `Prototype/TableSpec.hs`, `FSpec/FSpec.hs` |
| T8 | `src/Ampersand/Test/…` *(nieuw)* |

---

## 4. Taken

---

### Taak 1 — Introduceer het type `SqlDialect`

**Vereist (blocked by):** geen. *Dit is de fundering; begin hier.*

**Waarom:** één centrale, type-veilige keuze waar alle dialect-logica aan hangt.

**Doen:**
- Maak een nieuw module `src/Ampersand/Types/SqlDialect.hs` (of plaats het type in
  `Ampersand/Core/ParseTree.hs` bij `TType`, naar voorkeur van het team) met:
  ```haskell
  data SqlDialect = MySQL | Oracle
    deriving (Eq, Ord, Show, Enum, Bounded)
  ```
- Voeg een `parseSqlDialect :: Text -> Maybe SqlDialect` en `showSqlDialect` toe, analoog aan hoe
  `Recipe` in `FSpecGenOptsParser.hs` wordt geparsed (case-insensitive prefix-match).
- Registreer de module in `ampersand.cabal` onder `exposed-modules`/`other-modules`.

**Acceptatie:** `stack build` compileert; `SqlDialect` is importeerbaar. Nog geen gedragswijziging.

---

### Taak 2 — Voeg de `--sql-dialect` command-line optie toe

**Vereist (blocked by):** Taak 1 *(logisch — gebruikt het `SqlDialect`-type)*.
**Mag parallel met:** Taken 3, 4, 5, 7 (raakt andere bestanden).

**Waarom:** de keuze moet via de CLI binnenkomen, met `MySQL` als default (geen gedragswijziging
voor bestaande gebruikers).

**Doen:**
- In `src/Ampersand/Misc/HasClasses.hs`:
  - Voeg veld `xsqlDialect :: !SqlDialect` toe aan record `FSpecGenOpts` (rond regel 291).
  - Voeg in class `HasFSpecGenOpts` een lens toe:
    `sqlDialectL = fSpecGenOptsL . lens xsqlDialect (\x y -> x {xsqlDialect = y})` (volg het
    patroon van `recipeL`, regel ~62).
  - Voeg `("--sql-dialect", tshow $ xsqlDialect opts)` toe aan de `HasOptions`-instantie (regel ~305).
- In `src/Ampersand/Options/FSpecGenOptsParser.hs`:
  - Voeg een `sqlDialectP :: Parser SqlDialect` toe naar voorbeeld van `knownRecipeP` (regel ~102),
    met `long "sql-dialect"`, `value (show MySQL)`, `showDefault`,
    `completeWith ["MySQL","Oracle"]` en passende `help`.
  - Rijg `sqlDialectP` in de applicatieve keten van `fSpecGenOptsParser` (regel ~16).
  - Voeg `xsqlDialect = MySQL` toe aan `defFSpecGenOpts` (regel ~155).

**Acceptatie:** `ampersand proto --help` toont `--sql-dialect`; `--sql-dialect Oracle` en
`--sql-dialect=mysql` parsen zonder fout; weglaten levert `MySQL`. Output verandert nog niet (de
optie wordt nog nergens gelezen — dat is Taak 6).

---

### Taak 3 — Maak de kolomtype-mapping dialect-bewust

**Vereist (blocked by):** Taak 1 *(logisch — gebruikt `SqlDialect`)*.
*Eerste bewerker van `TableSpec.hs`; start van de TableSpec-keten.*

**Waarom:** dit is het hart van het verschil (`VARCHAR(255)`→`VARCHAR2(255)`, `TEXT`→`CLOB`, enz.).

**Doen:**
- In `src/Ampersand/FSpec/FSpec.hs`, functie `showSQL :: TType -> Text` (regel ~454):
  wijzig de signatuur naar `showSQL :: SqlDialect -> TType -> Text` en geef per dialect de juiste
  mapping. Gebruik de tabel uit §1b. Concreet voor Oracle:
  `Alphanumeric/Password/Object → VARCHAR2(255)`, `BigAlphanumeric/HugeAlphanumeric → CLOB`,
  `Binary/BigBinary/HugeBinary → BLOB`, `Date → DATE`, `DateTime → TIMESTAMP`,
  `Boolean → NUMBER(1)`, `Integer → NUMBER(19)`, `Float → BINARY_DOUBLE`.
- Pas de enige call-site aan: `src/Ampersand/Prototype/TableSpec.hs`, functie `addColumn` (regel
  ~118), die nu `(showSQL . fstype) att` aanroept. `addColumn` krijgt het dialect door (zie Taak 6).

**Acceptatie:** een unit-test of `ghci`-aanroep geeft voor elk `TType` de verwachte MySQL- én
Oracle-string terug.

---

### Taak 4 — Maak de table-opties en de `ts_insertupdate`-kolom dialect-bewust

**Vereist (blocked by):** Taak 3 *(bestand — zelfde `TableSpec.hs`; logisch onafhankelijk van T3)*.

**Waarom:** `ENGINE/CHARSET/ROW_FORMAT` en `ON UPDATE CURRENT_TIMESTAMP` bestaan niet in Oracle.

**Doen:** in `src/Ampersand/Prototype/TableSpec.hs`, functie `createTableSql` (regel ~78):
- De `endings`-lijst (regel ~110-114) is nu hardcoded MySQL. Maak hem dialect-afhankelijk:
  - **MySQL:** ongewijzigd laten.
  - **Oracle:** de `ts_insertupdate`-kolom wordt `"ts_insertupdate" TIMESTAMP DEFAULT CURRENT_TIMESTAMP`
    (zonder `ON UPDATE`), en de regels met `ENGINE …` en `ROW_FORMAT …` vervallen; sluit af met
    enkel `)`.
- Het `ON UPDATE`-gedrag (kolom bijwerken bij elke UPDATE) gaat in Oracle via een trigger. Genereer
  die in Oracle-modus mee (zie het voorbeeld in §1b). Plaats de triggergeneratie het handigst in
  `tableSpec2Queries` (regel ~188), zodat hij naast de `CREATE INDEX`-statements wordt uitgestoten.
- Overweeg de redundante `UNIQUE` op de primaire kolom (zie `addColumn`, regel ~123,
  `if fsIsPrimKey att then " UNIQUE"`) in Oracle-modus weg te laten; de `PRIMARY KEY` dekt dat al.

**Acceptatie:** Oracle-output bevat geen `ENGINE`, `ROW_FORMAT`, `COLLATE` of `ON UPDATE` meer, wél
een `TIMESTAMP DEFAULT CURRENT_TIMESTAMP`-kolom en per tabel een geldige `BEFORE INSERT OR UPDATE`-trigger.

---

### Taak 5 — Maak de hulpstatements en quoting dialect-bewust

**Vereist (blocked by):** Taak 4 *(bestand — zelfde `TableSpec.hs`; logisch onafhankelijk van T4)*.

**Waarom:** `DROP TABLE IF EXISTS`, `SHOW COLUMNS`, `SET TRANSACTION …` en de indexnamen verschillen.

**Doen:** in `src/Ampersand/Prototype/TableSpec.hs`:
- `dropTableIfExistsSql` (regel ~135): Oracle kent `DROP TABLE IF EXISTS` niet (vóór 23c). Genereer
  in Oracle-modus een PL/SQL-block dat de `ORA-00942` opvangt, of laat de drop in Oracle-modus weg
  (afspraak met team — DDL is voor een verse schema-load meestal genoeg).
- `showColumsSql` (regel ~129): vervang in Oracle-modus `SHOW COLUMNS FROM x` door een query op
  `user_tab_columns`. *(Wordt alleen door de PHP-runtime gebruikt; zie scope-noot. Markeer als
  "alleen nodig zodra de runtime Oracle ondersteunt" maar maak de functie nu wél dialect-bewust.)*
- `additionalDatabaseSettings` (regel ~210): `SET TRANSACTION ISOLATION LEVEL SERIALIZABLE` is in
  Oracle geldig, maar hoort niet aan het eind van een DDL-script. In Oracle-modus weglaten of
  vervangen door een comment.
- Quoting: `doubleQuote` (regel ~213) is goed voor beide dialecten en kan blijven. Let op dat
  `singleQuote` (regel ~216) feitelijk **backticks** produceert — puur MySQL; alleen relevant voor
  de runtime-laag, hier niet aanraken.

**Acceptatie:** Oracle-output bevat geen `SHOW COLUMNS`, geen `DROP TABLE IF EXISTS` en geen
losse `SET TRANSACTION`-regel; MySQL-output is byte-identiek aan vóór de wijziging.

---

### Taak 6 — Rijg het dialect door van CLI tot in de DDL-functies

**Vereist (blocked by):** Taak 2 *(logisch — heeft de `sqlDialectL`-lens nodig)* **én** Taak 7
*(bestand + logisch — laatste bewerker van `TableSpec.hs` vóór het doorrijgen; vereist via T7
transitief dat T3, T4, T5 klaar zijn)*. **Dit is het samenvoegpunt van beide sporen.**

**Waarom:** de pure generator-functies moeten de keuze uit Taak 2 daadwerkelijk ontvangen.

**Doen (van buiten naar binnen):**
- `src/Ampersand/Prototype/GenBackend.hs`, `doGenBackend` (regel ~19-29): lees het dialect uit de
  env via `view sqlDialectL` (de `HasFSpecGenOpts`-constraint zit al in het type-bereik) en geef het
  mee aan `databaseStructureSql`.
- `src/Ampersand/Output/FSpec2SQL.hs`:
  - `databaseStructureSql :: FSpec -> Text` → `databaseStructureSql :: SqlDialect -> FSpec -> Text`
    (regel ~13).
  - `generateDBstructQueries` (regel ~20) → idem een `SqlDialect`-parameter, doorgeven aan
    `tableSpec2Queries`.
- `src/Ampersand/Prototype/TableSpec.hs`: geef `SqlDialect` door aan `tableSpec2Queries`,
  `createTableSql`, `addColumn`, `dropTableIfExistsSql`, `additionalDatabaseSettings` en `showSQL`
  (de functies uit Taken 3/4/5).
- `src/Ampersand/Prototype/PHP.hs`, `createTablePHP` (regel ~19) roept `createTableSql` en
  `dropTableIfExistsSql` aan: geef hier `MySQL` mee (de PHP-runtime blijft MySQL — zie scope).

**Acceptatie:** `ampersand proto --sql-dialect Oracle MODEL.adl` schrijft een `database.sql` met
Oracle-DDL; zonder de vlag (of met `--sql-dialect MySQL`) is de output identiek aan de huidige.

---

### Taak 7 — Dek de Oracle-specifieke valkuilen af

**Vereist (blocked by):** Taak 5 *(bestand — zelfde `TableSpec.hs`)*, en logisch Taak 3
*(boolean-mapping in `FSpec.hs`)* en Taak 4 *(indexgeneratie)*.

**Waarom:** Oracle heeft regels die MySQL niet kent; zonder dit draait de DDL mogelijk niet.

**Doen:**
- **Identifier-lengte.** Controleer of tabel- en indexnamen binnen de Oracle-grens vallen (30 bytes
  vóór 12.2, 128 daarna). De index-suffix-logica in `tableSpec2Queries` (regel ~191-205,
  `tsName <> "_" <> tshow i`) kan namen over de grens duwen. Kies een strategie (afkappen +
  hash-suffix, of een doelversie ≥ 12.2 vooronderstellen en documenteren) en leg die vast.
- **Gereserveerde woorden.** Omdat alles al dubbel-gequote is, zijn reserved words geen blokker,
  maar documenteer dat de quoting in Oracle namen hoofdlettergevoelig maakt.
- **Boolean.** Bevestig de keuze `NUMBER(1)` vs `CHAR(1)`+check met het team (Taak 3 hangt hieraan).

**Acceptatie:** een notitie in de PR die per valkuil de gekozen oplossing en de veronderstelde
Oracle-versie benoemt; gegenereerde namen overschrijden de gekozen lengtegrens aantoonbaar niet.

---

### Taak 8 — Verificatie: golden-file test voor beide dialecten

**Vereist (blocked by):** Taak 6 *(logisch — heeft de werkende end-to-end generatie nodig)* en
Taak 7 *(logisch — de Oracle-output moet definitief zijn)*. **Sluitstuk.**

**Waarom:** garanderen dat MySQL niet regresseert en Oracle correct is.

**Doen:**
- Kies een klein representatief `.adl`-model (met een object-tabel, een lange-tekstkolom, een
  boolean en een index) en genereer `database.sql` in beide dialecten.
- Leg beide outputs vast als golden files in de testsuite (zie `src/Ampersand/Test/` en de
  `ampersand-test`-target) en assert byte-gelijkheid.
- **MySQL golden file = exact de huidige output** (regressiebewaking).
- Valideer de Oracle-output bij voorkeur tegen een echte Oracle (of `sqlplus`/SQLcl in een
  container): draait `CREATE TABLE` + index + trigger zonder fout?

**Acceptatie:** `stack test` slaagt; de MySQL-golden is ongewijzigd t.o.v. `main`; de Oracle-golden
laadt foutloos in een Oracle-instantie.

---

## 5. Bouwen en draaien

```sh
cd ~/git/Ampersand
stack build
stack exec ampersand -- proto --sql-dialect Oracle path/to/MODEL.adl
# resultaat: <protodir>/generics/database.sql met Oracle-DDL
```

> **Release notes (CI).** Zodra deze taken in een PR naar `main` gaan, vereist de
> `changelog-enforcer` een regel onder `## Unreleased` in `ReleaseNotes.md` voor elke wijziging
> buiten `docs/`. Voeg die toe bij Taak 2 of Taak 6.
