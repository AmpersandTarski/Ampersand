# Ampersand Relatie Compositie - Belangrijke Leerpunten

## Relatie Compositie met `~` en `;`

### Wat betekent `organisme~;poSynoniem`?

**Definitie**: Alle paren (x,y) waarvoor een POcombinatie p bestaat zodat:
- (x,p) zit in `organisme~` 
- (p,y) zit in `poSynoniem`

### Inverse Relatie (`~`)
Als het paar (a,b) in de relatie `organisme` zit, dan zit het paar (b,a) in `organisme~`.

**Voorbeeld:**
- `organisme[POcombinatie*Organisme]` bevat (poc1, org1)  
- Dan bevat `organisme~[Organisme*POcombinatie]` het paar (org1, poc1)

## Deelverzameling Regel (`|-`)

### Betekenis van `|-`
Het symbool `|-` betekent **"is een deelverzameling van"**.

```adl
RULE PleioSynoniem2Organisme : organisme~;poSynoniem |- synoniem[Organisme*WetenschappelijkeNaam]
```

**Dit betekent:** Elk paar uit `organisme~;poSynoniem` moet ook in `synoniem[Organisme*WetenschappelijkeNaam]` zitten.

## VIOLATION Mechanisme

### Hoe werkt de VIOLATION?
```adl
VIOLATION ( TXT "{EX} InsPair;synoniem;Organisme;"
          , SRC I
          , TXT ";WetenschappelijkeNaam;"
          , TGT I
          )
```

- **SRC I** = het linker atoom van het ontbrekende paar
- **TGT I** = het rechter atoom van het ontbrekende paar
- De ExecEngine voegt ontbrekende paren automatisch in

## Veelgemaakte Fout

❌ **FOUT**: Denken dat complexe expressies automatisch "omslachtig" zijn
✅ **CORRECT**: Begrijpen dat expressies correct of incorrect zijn, niet "omslachtig"

Mijn oorspronkelijke poging was **fout**, niet omslachtig. De correcte versie volgt de logica van relationele compositie.

## Praktijk Voorbeeld

Gegeven:
- `organisme[POcombinatie*Organisme]` bevat (poc1, org1)
- `poSynoniem[POcombinatie*WetenschappelijkeNaam]` bevat (poc1, syn1)

Dan bevat `organisme~;poSynoniem`:
- Het paar (org1, syn1)

En dit paar moet volgens de regel ook in `synoniem[Organisme*WetenschappelijkeNaam]` komen.

## ENFORCE vs RULE + VIOLATION Patroon

### Veel voorkomend patroon
Wanneer de overtredingen van een regel direct aan een relatie moeten worden toegevoegd, bestaat er een ENFORCE regel om dit korter en duidelijker op te schrijven.

**Patroon:**
```adl
ROLE ExecEngine MAINTAINS PleioSynoniem2Organisme
RULE PleioSynoniem2Organisme : organisme~;poSynoniem |- synoniem[Organisme*WetenschappelijkeNaam]
VIOLATION ( TXT "{EX} InsPair;synoniem;Organisme;"
          , SRC I
          , TXT ";WetenschappelijkeNaam;"
          , TGT I
          )
```

**Kort (ENFORCE patroon):**
```adl
ENFORCE synoniem[Organisme*WetenschappelijkeNaam] >: organisme~;poSynoniem
```

### Voordelen van ENFORCE
- **Veel korter** - minder code om te onderhouden
- **Duidelijker intentie** - direct zichtbaar wat er geënforceerd wordt
- **Standaard patroon** - Ampersand herkent dit patroon en genereert automatisch de juiste ExecEngine acties
- **Minder foutgevoelig** - geen handmatige VIOLATION constructies

### Wanneer ENFORCE gebruiken?
✅ **Gebruik ENFORCE** als je overtredingen van een regel direct aan één relatie wilt toevoegen
✅ **Gebruik RULE + VIOLATION** alleen als de logica niet in ENFORCE past

### Syntax
- `ENFORCE relatie >: expressie` betekent: relatie moet minstens de paren uit expressie bevatten. De ExecEngine zal de ontbrekende paren toevoegen aan het linkerlid.
- `ENFORCE relatie := expressie` betekent: relatie moet exact gelijk zijn aan expressie. De ExecEngine wijzigt de populatie van relatie en zorgt er zo voor dat links en rechts steeds aan elkaar gelijk blijven.
- `ENFORCE relatie :< expressie` betekent: relatie moet hooguit de paren uit expressie bevatten. De ExecEngine zal de overtollige paren verwijderen uit het linkerlid.

