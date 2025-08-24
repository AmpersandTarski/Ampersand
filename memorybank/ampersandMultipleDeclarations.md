# Ampersand Multiple Declarations - Belangrijke Regel

## Geen Conflicten bij Dubbele Declaraties

### Kernprincipe
**Ampersand staat meerdere declaraties van dezelfde relatie toe zonder conflicten.**

### Wanneer zijn declaraties identiek?
Twee declaraties zijn identiek als hun **naam, source en target** identiek zijn:
```adl
-- Deze twee declaraties zijn identiek:
RELATION productNaam[POcombinatie*ProductNaam] [UNI]  -- in Plagen.adl
RELATION productNaam[POcombinatie*ProductNaam]   -- in Pleio.adl
```
De multipliciteits-eigenschappen (UNI, TOT, enzovoorts) in alle declaraties van dezelfde relatie gelden. Het is dus niet erg dat een declaratie niet alle multipliciteits-eigenschappen vermeldt als die in een andere declaratie (van dezelfde relatie) al vermeld wordt. 

### Voordelen van Multiple Declarations
1. **Context-specifieke documentatie** - elke module kan zijn eigen PURPOSE en MEANING hebben
2. **Modulaire ontwikkeling** - modules kunnen onafhankelijk relaties declareren
3. **Flexibiliteit** - geen zorgen over "waar staat deze relatie al?"

### Nadelen van Multiple Declarations
1. **Verwarrend** - je weet niet zeker of een relatie misschien ergens anders gedeclareerd is.

### Praktijk Voorbeeld
```adl
-- In Plagen.adl:
RELATION productNaam[POcombinatie*ProductNaam] [UNI]
PURPOSE RELATION productNaam 
{+ Voor koppeling van plagen aan producten +}

-- In Pleio.adl:
RELATION productNaam[POcombinatie*ProductNaam] [UNI]  
PURPOSE RELATION productNaam
{+ Voor import van Pleio Excel data +}
```
**Resultaat:** Dezelfde relatie met twee PURPOSE statements - beide geldig!

### Wat NIET mag
```adl
-- FOUT - verschillende signatures:
RELATION productNaam[POcombinatie*ProductNaam] [UNI]
RELATION productNaam[ESEid*ProductNaam] [UNI]  -- andere source!
```

### Conclusie
✅ **Wel oké:** Identieke relaties op meerdere plekken declareren  
✅ **Wel oké:** Meerdere PURPOSE en MEANING voor dezelfde relatie  
❌ **Niet oké:** Verschillende signatures voor dezelfde relatienaam

**Dit is een krachtige feature van Ampersand die modulariteit ondersteunt!**
