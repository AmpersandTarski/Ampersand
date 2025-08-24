# Ampersand Concept vs Type Mapping

## Belangrijke Leerervaring: CONCEPT vs TYPE

### Kern Principe
In Ampersand zijn **CONCEPT** en **TYPE** verschillende dingen:
- **CONCEPT:** De logische entiteit/begrip in je domein model
- **TYPE:** De technische datatype representatie in de database/interface

### Default Gedrag van Ampersand

#### Basis Rule: ALPHANUMERIC Default
```ampersand
CONCEPT Product  -- Wordt automatisch ALPHANUMERIC type
```
Als je geen REPRESENT statement definieert, krijgt elk concept automatisch het type `ALPHANUMERIC`.

#### OBJECT Type voor Sleutelwaarden
```ampersand
-- Object type wordt automatisch gebruikt voor sleutelwaarden
-- Het is niet bedoeld om zichtbaar te zijn voor eindgebruikers
```
Het `OBJECT` type wordt gebruikt voor sleutelwaarden en is niet bedoeld om zichtbaar te zijn voor eindgebruikers. Het krijgt VARCHAR(255) opslag in de database.

### REPRESENT Statements

#### Wanneer Nodig
Je hebt een REPRESENT statement nodig wanneer je wilt dat een concept een ander type krijgt dan ALPHANUMERIC:

```ampersand
REPRESENT Datum TYPE DATE
REPRESENT Getal TYPE INTEGER  
REPRESENT Procent TYPE INTEGER
REPRESENT Tekst TYPE BIGALPHANUMERIC
REPRESENT BOOLEAN TYPE BOOLEAN
```

#### Praktisch Voorbeeld: BOOLEAN Probleem
**Foutmelding:** `Error in cell 'ProductEisen!H2': Concept 'BOOLEAN' is not defined`

**Oorzaak:** Excel bevat boolean waarden, maar CONCEPT BOOLEAN bestaat wel maar heeft geen TYPE definitie

**Oplossing:**
```ampersand
REPRESENT BOOLEAN TYPE BOOLEAN
```

**Verklaring:** 
- `CONCEPT BOOLEAN` = logische entiteit (kan bestaan door gebruik in Excel)
- `TYPE BOOLEAN` = technische representatie (moet expliciet gedefinieerd worden)

### Officiële Ampersand Atomic Types

```ampersand
-- Tekst Types (van kort naar lang)
REPRESENT Naam TYPE ALPHANUMERIC             -- Korte strings (<255 chars)
REPRESENT Tekst TYPE BIGALPHANUMERIC         -- Middelgrote tekst (<64kb)  
REPRESENT Document TYPE HUGEALPHANUMERIC     -- Zeer grote tekst

-- Numeriek
REPRESENT Getal TYPE INTEGER                 -- Gehele getallen
REPRESENT Bedrag TYPE FLOAT                  -- Decimale getallen

-- Datum/Tijd (ISO8601 compatibel)
REPRESENT Datum TYPE DATE                    -- Alleen datum
REPRESENT Tijdstip TYPE DATETIME             -- Datum en tijd

-- Boolean
REPRESENT JaNee TYPE BOOLEAN                 -- True/False waarden
REPRESENT Actief TYPE BOOLEAN

-- Beveiliging
REPRESENT Wachtwoord TYPE PASSWORD           -- Beveiligde opslag

-- Binaire Data (van klein naar groot)
REPRESENT Bestand TYPE BINARY                -- Kleine binaire data
REPRESENT GrootBestand TYPE BIGBINARY        -- Middelgrote binaire data  
REPRESENT HugeBestand TYPE HUGEBINARY        -- Zeer grote binaire data

-- Object (sleutelwaarden, niet zichtbaar voor gebruikers)
-- Object type wordt automatisch gebruikt voor sleutelwaarden
```

### Diagnostiek Tips

#### Symptomen van Ontbrekende REPRESENT
1. **Excel import errors:** "Concept 'X' is not defined"
2. **Type mismatch errors** in interfaces
3. **Database constraint violations**

#### Oplossing Strategie
1. **Identificeer het concept** uit de foutmelding
2. **Bepaal het gewenste type** (meestal uit context/Excel data)
3. **Voeg REPRESENT statement toe** aan relevant .adl bestand
4. **Rebuild prototype** en test

### Best Practices

#### Groepeer REPRESENT Statements
```ampersand
-- Zet alle REPRESENT statements bovenaan context
REPRESENT Tekst TYPE BIGALPHANUMERIC
REPRESENT Datum TYPE DATE
REPRESENT Getal, Procent TYPE INTEGER
REPRESENT BOOLEAN TYPE BOOLEAN
```

#### Consistent Naming
```ampersand
-- Gebruik duidelijke concept namen die het type reflecteren
CONCEPT JaNee          -- Voor boolean waarden
CONCEPT ProductNaam    -- Voor tekst
CONCEPT AantalStuks    -- Voor integer
CONCEPT LeverDatum     -- Voor datum
```

#### Vermijd Type Conflicts
```ampersand
-- FOUT: Product wordt BOX concept (OBJECT) én krijgt ander type
INTERFACE Products : "_SESSION";V[SESSION*Product] BOX<TABLE> [...]
REPRESENT Product TYPE ALPHANUMERIC  -- Conflict!

-- GOED: Gebruik attribuut voor type mapping
INTERFACE Products : "_SESSION";V[SESSION*Product] BOX<TABLE>
  [ code : productCode ]  -- productCode kan ALPHANUMERIC zijn
REPRESENT ProductCode TYPE ALPHANUMERIC
```

### Troubleshooting Checklist

Bij type-gerelateerde fouten:
- [ ] Is er een REPRESENT statement voor het concept?
- [ ] Wordt het concept gebruikt als BOX-concept in interfaces?
- [ ] Matcht het TYPE met de data in Excel/database?
- [ ] Zijn er conflicterende REPRESENT statements?
- [ ] Is de syntax correct? (`REPRESENT ConceptNaam TYPE TypeNaam`)

### Gerelateerde Concepten
- **Excel Import Mapping:** Zie `memorybank/ampersandExcelImportMapping.md`
- **Ampersand Syntax:** Zie `memorybank/ampersandSyntax.md` 
- **Database Schema:** Types bepalen database kolom types

---
*Documentatie gebaseerd op praktijkervaring met BOOLEAN type fout in landeneisen_AZ.xlsx import*
