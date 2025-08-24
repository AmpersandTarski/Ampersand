# Ampersand Population Importer - Excel to ADL Mapping

## Hoe de Population Importer Relaties Herkent

### **BASIS HERKENNING:**

**1. Concept Identificatie:**
```
[ConceptNaam] in eerste kolom = Dit wordt het concept
```

**2. Relatie Herkenning via Kolomkoppen:**
- Kolomkop = relatienaam in ADL script
- Tweede rij = target concept van de relatie
- Tweede rij eerste kolom = source concept van de relatie
- Twee relaties met dezelfde naam, source en target hebben op ieder moment in de tijd exact dezelfde populatie. Zij zijn dus identiek. Deze regel is hard en geldt ALTIJD binnen de Ampersand context. In een database moet je ze dus op dezelfde tabel afbeelden om te garanderen dat ze altijd op ieder moment dezelfde inhoud hebben.

### **CONCRETE VOORBEELDEN:**

#### **Producten.xlsx - Sheet "Producten":**
```
[Product]   | voorkeursNaam  | spToevoeging | vorm | synoniem           | eppoCode | ...
Product     | ProductNaam    | SPaffix      | Vorm | WetenschappelijkeNaam | EPPOcode | ...
pr00001     | Solanum...     |              | zaad | Lycopersicon...    | LYPES    | ...
```

**Mapping:**
- `[Product]` → **Product concept**
- `voorkeursNaam[Product*ProductNaam]` → kolomkop "voorkeursNaam", target "ProductNaam"
- `spToevoeging[Product*SPaffix]` → kolomkop "spToevoeging", target "SPaffix" 
- `eppoCode[Product*EPPOcode]` → kolomkop "eppoCode", target "EPPOcode"

#### **Organismen.xlsx - Sheet "Organismen 1":**
```
[Organisme] | spToevoeging | voorkeursNaam         | eppoCode | soort | ...
Organisme   | SPaffix      | WetenschappelijkeNaam | EPPOcode | Soort | ...
Abutilon... |              | Abutilon theophrasti  | ABUTH    | ONKRUIDEN | ...
```

**Mapping:**
- `[Organisme]` → **Organisme concept**
- `voorkeursNaam[Organisme*WetenschappelijkeNaam]` → kolomkop "voorkeursNaam", target "WetenschappelijkeNaam"
- `eppoCode[Organisme*EPPOcode]` → kolomkop "eppoCode", target "EPPOcode"

#### **Pleio.xlsx - Sheet "Organismen":**
```
[Organisme] | voorkeursNaam         | eppoCode | peststatus
Organisme   | WetenschappelijkeNaam | EPPOcode | Pest
Cercospora  | Cercospora kikuchii   | CERCKI   | absent
```

### **HERKENNINGSREGELS:**

1. **Eerste kolom met `[...]`** = Source concept identificatie
2. **Kolomkop** = Relatienaam in ADL
3. **Tweede rij** = Target concept  
4. **Data rijen** = Daadwerkelijke relatie instanties

### **VOORDELEN VAN DEZE AANPAK:**
- ✅ **Automatische mapping** - geen handmatige configuratie
- ✅ **Type-safe** - relaties worden correct getypeerd
- ✅ **Flexibel** - nieuwe relaties door kolom toevoegen
- ✅ **Leesbaar** - Excel structuur volgt ADL logica

### **KRITIEKE VUISTREGELS:**
1. `[ConceptNaam]` in eerste cel van eerste kolom
2. Kolomkoppen = exacte relatienamen uit ADL
3. Tweede rij = target concepten (moeten matchen met ADL)
4. Tweede rij eerste kolom = source concept (moet matchen met eerste cel)
5. Data vanaf rij 3
6. Lege cellen = geen relatie instantie
7. **SIGNATURE MATCHING:** Excel construeert `kolomkop[rij1_cel1*rij2_kolom]` en zoekt EXACTE match in ADL
8. **GEEN FUZZY MATCHING:** Elk onderdeel (naam, source, target) moet precies overeenkomen

### **VEELGEMAAKTE FOUT - CONCEPT MISMATCH:**
```
// FOUT: Excel gebruikt [Dekking] maar ADL definieert:
RELATION dekkingEisNummer[LandeneisenDekking*EisNummer]

// OPLOSSING 1: Excel sheet aanpassen naar [LandeneisenDekking]
// OPLOSSING 2: ADL relatie herdefiniëren naar [Dekking*EisNummer]
// OPLOSSING 3: Bridge relatie toevoegen in ADL
```

**Dit levert op runtime fouten op zoals:** `Relation 'dekkingEisNummer[Dekking*EisNummer]' is not defined`

## BOOLEAN waarden vs PROP relaties

### PROP Relaties (Eigenschappen)
PROP relaties zijn eigenschappen, bijvoorbeeld: `RELATION standaard[ProductEis] [PROP]`

**Kenmerken:**
- Source en target zijn gelijk (endorelatie)  
- [PROP] = [SYM,ASY] (symmetrisch en antisymmetrisch)
- Altijd deelverzameling van I (identiteitsrelatie)
- Voor elk paar (a,b) geldt: a=b

**Excel invulling voor PROP relaties:**
```
[ProductEis]           | standaard
ProductEis             | ProductEis
PRODUCT_EIS_AZ_001     | PRODUCT_EIS_AZ_001    // TRUE - herhaal eerste kolom
PRODUCT_EIS_AZ_002     |                       // FALSE - lege cel  
PRODUCT_EIS_AZ_003     | PRODUCT_EIS_AZ_003    // TRUE - herhaal eerste kolom
```

**KRITISCH:** Bij PROP relaties NOOIT "TRUE"/"FALSE" als tekst gebruiken!

### Reguliere BOOLEAN Types
Voor gewone boolean relaties: `RELATION actief[Product*BOOLEAN]`

**Excel invulling:**
```
[Product]     | actief
Product       | BOOLEAN
prod001       | TRUE      // Tekstwaarde "TRUE"
prod002       | FALSE     // Tekstwaarde "FALSE"  
prod003       |           // Lege cel = geen relatie
```

**Vergelijking:**
- **PROP relatie:** Herhaal eerste kolom waarde (TRUE) of lege cel (FALSE)
- **BOOLEAN type:** Gebruik "TRUE"/"FALSE" als tekstwaarden

Deze fout verklaart waarschijnlijk de "Concept 'BOOLEAN' is not defined" error - een PROP relatie werd behandeld als BOOLEAN type.
