# Ampersand Modern Syntax Rules

## RELATION Syntax (Moderne Stijl)

### ❌ OBSOLETE (niet meer gebruiken):
```adl
organisme :: POcombinatie -> Organisme
productNaam :: POcombinatie -> Product  
eppoCode :: Organisme -> EPPOcode
```

### ✅ MODERNE SYNTAX (altijd gebruiken):
```adl
RELATION organisme[POcombinatie*Organisme] [UNI,TOT]
RELATION productNaam[POcombinatie*Product] [UNI,TOT]  
RELATION eppoCode[Organisme*EPPOcode] [UNI]
```

## Multiplicity Properties

### Basis Properties
- **UNI** (Univalent): Elk source element → **maximaal één** target element
- **TOT** (Total): Elk source element → **minstens één** target element  
- **INJ** (Injective): Elk target element ← **maximaal één** source element
- **SUR** (Surjective): Elk target element ← **minstens één** source element

### Combinaties
```adl
RELATION voorkeursNaam[Organisme*WetenschappelijkeNaam] [UNI,TOT]  -- totale functie
RELATION eppoCode[Organisme*EPPOcode] [UNI]                        -- partiële functie  
RELATION synoniem[Organisme*WetenschappelijkeNaam]                 -- many-to-many
RELATION cbbCode[Organisme*CBBcode] [INJ]                          -- één-op-veel omgekeerd
```

### Betekenis in Business Context

#### UNI,TOT (Totale Functie)
```adl
RELATION voorkeursNaam[Organisme*WetenschappelijkeNaam] [UNI,TOT]
-- "Elk organisme heeft precies één voorkeurnaam"
```

#### UNI (Partiële Functie)  
```adl
RELATION eppoCode[Organisme*EPPOcode] [UNI]
-- "Elk organisme heeft maximaal één EPPO code (sommige hebben er geen)"
```

#### INJ (Unieke Target Values)
```adl
RELATION eppoCode[Organisme*EPPOcode] [INJ]  
-- "Elke EPPO code behoort aan maximaal één organisme"
```

#### Geen Properties (Many-to-Many)
```adl
RELATION synoniem[Organisme*WetenschappelijkeNaam] 
-- "Organismen kunnen meerdere synoniemen hebben, synoniemen kunnen bij meerdere organismen horen"
```

## Pattern Template

```adl
PATTERN ModernPatternName
   CONCEPT EntityName "Business definitie" "Bron"
   
   RELATION basicAttribute[EntityName*AttributeType] [UNI,TOT]
   MEANING "Business betekenis van deze relatie"
   
   RELATION optionalAttribute[EntityName*AttributeType] [UNI]  
   MEANING "Optioneel attribuut - niet elk entity heeft dit"
   
   RELATION uniqueReference[EntityName*ReferenceType] [UNI,INJ]
   MEANING "Unieke referentie - één-op-één relatie"
   
   RELATION multiValueAttribute[EntityName*ValueType]  
   MEANING "Multi-value attribuut - many-to-many"
   
   PURPOSE RULE RuleName
   {+ Business uitleg waarom deze regel nodig is +}
   ROLE RoleName MAINTAINS RuleName  
   RULE RuleName : left-expression |- right-expression
   VIOLATION (TXT "Violation message template")
ENDPATTERN
```

## FC4 Specific Examples

### Organisme Relations
```adl
RELATION voorkeursNaam[Organisme*WetenschappelijkeNaam] [UNI,TOT]  -- verplicht, uniek
RELATION nederlandseNaam[Organisme*Naam] [UNI]                      -- optioneel, uniek  
RELATION eppoCode[Organisme*EPPOcode] [UNI,INJ]                     -- optioneel, maar uniek per code
RELATION synoniem[Organisme*WetenschappelijkeNaam]                  -- meerdere synoniemen mogelijk
RELATION cbbCode[Organisme*CBBcode] [UNI]                           -- maximaal één CBB code
```

### POcombinatie Relations  
```adl
RELATION organisme[POcombinatie*Organisme] [UNI,TOT]      -- elke combinatie heeft organisme
RELATION productNaam[POcombinatie*Product] [UNI,TOT]      -- elke combinatie heeft product
RELATION vorm[POcombinatie*Vorm] [UNI]                    -- vorm is optioneel
```

### ESEid Relations
```adl
RELATION organisme[ESEid*Organisme] [UNI,TOT]             -- elke export eis heeft organisme  
RELATION landnaam[ESEid*Landnaam] [UNI,TOT]               -- elke export eis heeft land
RELATION eisNaam[ESEid*Eisnaam] [UNI,TOT]                 -- elke export eis heeft eis naam
RELATION bron[ESEid*Bron] [UNI]                           -- bron is optioneel
```

## Development Workflow

### Syntax Validation
```bash
# Test syntax en type checking van ADL bestanden
ampersand check project/main.adl
ampersand check project/Organismen.adl
ampersand check project/Producten.adl

# Voor alle bestanden in project directory  
cd project && find . -name "*.adl" -exec ampersand check {} \;
```

### Development Cycle
1. **Write/Edit** ADL bestand
2. **Check syntax**: `ampersand check <filename.adl>`
3. **Fix errors** als er syntax/type fouten zijn
4. **Test logic** door prototype te herbouwen
5. **Verify behavior** in browser interface

### Typische Check Outputs
```bash
# ✅ Succesvolle check
$ ampersand check Organismen.adl
Checking file: Organismen.adl
✓ Syntax OK
✓ Type checking OK

# ❌ Syntax error
$ ampersand check Organismen.adl  
Error: Line 45, Column 23
Unexpected token: Expected ']' but found '}'
RELATION eppoCode[Organisme*EPPOcode} [UNI]
                                    ^

# ❌ Type error  
$ ampersand check Organismen.adl
Type error: Line 67
Cannot unify types: Organisme and Product in expression
```

## Migration Checklist

✅ Vervang `::` syntax door `RELATION` syntax  
✅ Vervang `->` door `*` in type signature  
✅ Voeg multiplicity properties toe waar relevant  
✅ Documenteer business betekenis in MEANING  
✅ Gebruik PURPOSE voor complexe regels  
✅ **Test met `ampersand check`** na elke wijziging
✅ Consistent gebruik van moderne syntax door gehele codebase

## Concept Populaties en Relatie Populaties

### ⚠️ BELANGRIJKE REGEL: Automatische Concept Populatie Afleiding

Voor een willekeurige a,b,r,A en B geldt **ALTIJD** dat:
Als een paar (a,b) in relatie r zit, en r is gedeclareerd als `r[A*B]`, dan zit a in het concept A en b in het concept B. 

**Deze eigenschap ligt wiskundig vast en de Ampersand software garandeert dat dit op ieder moment blijft gelden.**

### Praktisch Gevolg: Overbodig om Concept Populaties te Specificeren

❌ **OVERBODIG** (hoeft niet):
```adl
POPULATION Dekkingscode CONTAINS ["1", "2", "3", "4", "5a", "5b", ...]
POPULATION Dekkingtekst CONTAINS ["The consignment was inspected...", ...]

POPULATION dekkingTekst CONTAINS
  [ ("1", "The consignment was inspected...")
  , ("2", "The consignment was tested...")
  , ...
  ]
```

✅ **VOLDOENDE** (automatische afleiding):
```adl
-- Alleen de relatie populatie is nodig
POPULATION dekkingTekst CONTAINS
  [ ("1", "The consignment was inspected...")
  , ("2", "The consignment was tested...")
  , ("3", "The growing media...")
  , ...
  ]
-- Ampersand leidt automatisch af:
-- - Dekkingscode bevat: "1", "2", "3", ...  
-- - Dekkingtekst bevat: "The consignment was inspected...", "The consignment was tested...", ...
```

### Waarom Dit Werkt

1. **Mathematische garantie**: Als `("1", "tekst")` in `dekkingTekst[Dekkingscode*Dekkingtekst]` staat, dan moet `"1"` een `Dekkingscode` zijn en `"tekst"` een `Dekkingtekst`
2. **Runtime verificatie**: Ampersand prototype controleert dit voortdurend
3. **Code reductie**: Minder duplicatie, minder onderhoud
4. **Consistentie**: Onmogelijk om inconsistentie tussen concept en relatie populaties te krijgen

### Best Practice

**Specificeer alleen relatie populaties**, laat concept populaties automatisch afleiden door Ampersand.

## Interface Syntax Regels

### ❌ FOUT: TXT in BOX Context
```adl
-- TXT kan NIET gebruikt worden binnen BOX elementen
INTERFACE Example : I[Entity] BOX<FORM>
  [ "Label" : TXT "Vaste tekst"                    -- ❌ SYNTAX ERROR
  , "Info" : TXT "Certificaat vereist"            -- ❌ SYNTAX ERROR  
  ]
```

### ✅ CORRECT: TXT alleen in VIEWS en VIOLATIONS
```adl
-- TXT is toegestaan in VIEWS
VIEW EntityView: Entity DEFAULT
  { naam: entityNaam
  , separator: TXT " - "                          -- ✅ CORRECT
  , info: TXT "Extra informatie"                  -- ✅ CORRECT
  }
ENDVIEW

-- TXT is toegestaan in VIOLATIONS  
RULE ExampleRule : I[Entity] |- condition
VIOLATION (TXT "Foutmelding: ", SRC entityNaam)   -- ✅ CORRECT
```

### Interface Item Structuur
**Elk interface item heeft de structuur: `<label> : <term>`**

```adl
INTERFACE Example : I[Entity] BOX<FORM>
  [ "Entity ID" : I                               -- term = I (identity)
  , "Naam" : entityNaam                           -- term = relatie
  , "Type" : entityType                           -- term = relatie
  , "Details" : entityDetails BOX<TABLE>          -- term = relatie met sub-box
      [ "Detail" : I                              -- sub-term
      , "Waarde" : detailWaarde                   -- sub-term
      ]
  ]
```

### Alternatieve Oplossingen voor Vaste Tekst in Interface

#### Optie 1: Gebruik Relations naar Tekstconstanten
```adl
-- Definieer relaties naar vaste waarden
RELATION certificaatExport[LandCode*Tekst] 
POPULATION certificaatExport CONTAINS [("NL", "1")]

INTERFACE Example : I[Entity] BOX<FORM>
  [ "Certificaat Export" : certificaatExport      -- ✅ CORRECT
  ]
```

#### Optie 2: Gebruik VIEWS voor Formatting
```adl
VIEW LandHeader: LandCode DEFAULT
  { certificaatExport: TXT "1"
  , certificaatReexport: TXT "20"  
  , taal: TXT "E"
  }
ENDVIEW

INTERFACE Example : I[Entity] BOX<FORM>
  [ "Land Info" : landCode                        -- ✅ CORRECT - gebruikt VIEW
  ]
```

### Belangrijke Syntax Regel
**TXT keyword hoort NIET tot een term in interface context. TXT is alleen geldig in VIEWS en VIOLATIONS.**

## Ampersand Relatie Gelijkheid Principe (Hard Ingebakken)

### ⚠️ KRITIEKE REGEL: Relatie Identiteit
**Twee relaties in Ampersand zijn gelijk als hun naam, source EN target gelijk zijn.**

**Er zijn GEEN uitzonderingen op deze regel - dit is hard ingebakken in Ampersand.**

### Voorbeelden van VERSCHILLENDE Relaties:
```adl
RELATION dekkingTekst[Dekkingcode*Dekkingnaam] [UNI]        -- Relatie A
RELATION dekkingTekst[Dekking*Dekkingtekst] [UNI,TOT]       -- Relatie B  
RELATION dekkingTekst[Dekkingscode*Dekkingtekst] [UNI]      -- Relatie C
```

**Dit zijn 3 TOTAAL VERSCHILLENDE relaties**, ondanks dezelfde naam `dekkingTekst`.

### Database Consequenties:
1. **Alle gedeclareerde relaties bestaan** in de database
2. **Geen automatische samenvoeging** van relaties met dezelfde naam
3. **Elke combinatie naam+source+target** krijgt eigen database tabel
4. **Queries moeten exact de juiste signature gebruiken**

### Foutmelding Debug Tip:
Als je een foutmelding krijgt over een missende relatie zoals:
```
"Kan geen relatie dekkingTekst[Dekkingscode*Dekkingtekst] vinden"
```

Dan zoekt het systeem naar **exact die signature**, niet naar:
- `dekkingTekst[Dekkingcode*Dekkingnaam]` (andere target)  
- `dekkingTekst[Dekking*Dekkingtekst]` (andere source)

### Best Practice: Consistente Naming
```adl
-- ✅ GOED: Verschillende namen voor verschillende concepten
RELATION dekkingcodeNaam[Dekkingcode*Dekkingnaam] [UNI]     -- standaard namen  
RELATION dekkingSpecifiekeTekst[Dekking*Dekkingtekst] [UNI] -- specifieke teksten

-- ❌ VERWARREND: Zelfde naam voor verschillende signatures  
RELATION dekkingTekst[Dekkingcode*Dekkingnaam] [UNI]        -- verschillende
RELATION dekkingTekst[Dekking*Dekkingtekst] [UNI]           -- relaties!
```

**Datum toegevoegd:** 4-8-2025 (FC4 DekkingTekst debugging sessie)

## Voordelen Moderne Syntax

1. **Expliciete constraints**: UNI/TOT/INJ/SUR maken business regels duidelijk
2. **Betere code documentatie**: MEANING en PURPOSE leggen intent uit  
3. **Ampersand optimalisatie**: Compiler kan beter optimaliseren met expliciete properties
4. **Maintenance**: Makkelijker te begrijpen en onderhouden
5. **Tool support**: Betere IDE en tooling ondersteuning voor moderne syntax
6. **Automatische concept populatie**: Relatie populaties leiden concept populaties automatisch af
