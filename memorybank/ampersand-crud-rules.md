# Ampersand CRUD Regels

## CRUD annotaties in interfaces

**Losstaande relaties vs. samengestelde expressies:**

### Losstaande relaties
- Een **losstaande relatie** is een enkele relatie, al dan niet met flip erachter (tilde symbool: `~`)
- Voorbeelden: `productSynoniem`, `productSynoniem~`, `nedNaam`
- Als een losstaande relatie, stel `r[A*B]` in een interface voorkomt, leidt dat tot een veld in de user-interface in het prototype dat Ampersand genereert.
- Als in het woord crud de c (Create) als hoofdletter voorkomt (bijvoorbeeld CRud), dan mag de gebruiker van het prototype een nieuw atoom aanmaken. Dat atoom is dan van het concept `B`, dus de target van de relatie `r[A*B]`.
- Als in het woord crud de r (Read) als hoofdletter voorkomt (bijvoorbeeld cRuD), dan mag de gebruiker de paren lezen (Read). In het veldje in zijn prototype krijgt hij dan het target atoom te zien.
- Als in het woord crud de u (Update) als hoofdletter voorkomt (bijvoorbeeld cRUd), dan mag de gebruiker de paren updaten. In zijn prototype mag hij dan het target atoom wijzigen. Hij kan er dan bijvoorbeeld een ander paar voor in de plaats zetten, of het paar uit de relatie verwijderen, of een nieuw paar toevoegen.
- Als in het woord crud de d (Delete) als hoofdletter voorkomt (bijvoorbeeld CRUD), dan mag de gebruiker het target atoom verwijderen. Als hij dat doet, verdwijnen ook alle paren (in de hele context) waarin dat atoom voorkomt.
- Alleen bij losstaande relaties mag je **alle 16 combinaties** van CRUD gebruiken (crud, cRud, cRUd, CRUD, etc.)

### Samengestelde relaties  
- Een **samengestelde relatie** bestaat uit meerdere relaties gekoppeld met operatoren zoals `;`, `/\`, `\/`, `<>`, enzovoorts.
- Voorbeelden: `productSynoniem;synoniemNaam`, `voorkeursNaam;eppoCode~`.
- Bij samengestelde relaties mag je **alleen cRud** gebruiken omdat de gebruiker niets kan wijzigen in een samengestelde relatie.
- Om dezelfde reden mag je alleen cRud gebruiken bij een complement, bijvoorbeeld `-voorkeursnaam`. Ook daar kan de gebruiker niets wijzigen.

Er zijn 16 mogelijke combinaties van hoofd- en kleine letters van het woord "crud", maar slechts **cRud** mag gebruikt worden voor samengestelde relaties.
