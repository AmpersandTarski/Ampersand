# Ampersand Concept-Database Mapping Analysis

## Methodologie
** Welke bestanden horen tot je Ampersand Script?**
Om alle bestanden te vinden van het Ampersand scipt, kijk je naar de bestandsnaam in de aanroep van de compiler, bijvoorbeeld: `main.adl`. Daarin bevinden zich `INCLUDE` statements, bijvoorbeeld `INCLUDE "recepten/appeltaart.adl".
De daarin genoemde bestanden horen ook tot het script. Deze bestanden kunnen op hun beurt (recursief dus) ook weer `INCLUDE` statements bevatten, waarvan de bestanden ook tot het script behoren. Je gaat recursief de `INCLUDE` structuur af om alle bestanden te vinden.

** Hoe zie je het verschil tussen een concept en de naam van een relatie?**
De naam van een relatie in Ampersand begint altijd met een kleine letter.
Een conceptnaam begint altijd met een hoofdletter.
In de naamgeving in de database vind je dat niet terug omdat de database case-insensitive werkt.
Ampersand genereert database schema's, dus alle relaties en concepten komen een op een terug in de database.

** Hoe vind je alle concepten in je script?**
Je declareert een concept door:
1. het concept te gebruiken als source of target in de declaratie van een relatie, of
2. het concept te declareren in een `CONCEPT`-statement.
3. Twee concepten met dezelfde naam zijn hetzelfde concept.

** Hoe vind je alle relaties in je script?**
1. Je kunt een relatie alleen in een `RELATION`-statement declareren.
2. Binnen een context mag geen relaties gebruiken als ze niet binnen die context gedeclareerd zijn.
3. Twee relaties met dezelfde naam, source en target zijn dezelfde relatie

** Hoe beeldt de Ampersand compiler concepten en relaties af op database tabellen?**
Elk concept krijgt precies één conceptentabel.
Het prototype slaat alle atomen van een concept op in de concepttabel van dat concept, zodat hij zonodig complementen kan berekenen.
De Ampersand compiler beeldt een univalente relatie af op een attribuut in de conceptentabel van zijn source concept. De Ampersand compiler beeldt een injectieve relatie af op een attribuut in de conceptentabel van zijn target concept. De naam van het attribuut komt overeen met de naam van de relatie. Als er dubbele namen zijn gebruikt de compiler cijfers achter de naam om ze uniek identifeerbaar te maken. Een relatie die niet univalent noch injectief is krijgt een zelfstandige tabel: een koppeltabel.

Het prototype slaat paren uit een relatie op in de tabel die deze relatie bevat. Als dat een conceptentabel is, dan is de relatie een attribuut. Als dat een koppeltabel is, dan representeert deze de relatie in z'n geheel.

**Waarom hebben sommige tabellen of attributen een cijfer achter de naam?**
De Ampersand compiler gebruikt nummering om naamconflicten op te lossen tussen conceptnamen en relatienamen die dezelfde tabelnaam zouden krijgen. Bijvoorbeeld: concept "Student" en relatie "student" krijgen beide een tabelnaam - de compiler voegt cijfers toe voor unieke identificatie.


## Actieve ADL bestanden in FC4 project
Via recursieve analyse van main.adl:
- `Plagen.adl` → includeert `Organismen.adl`, `Producten.adl`, `Pleio.xlsx`
- `Exporteisen.adl`
- `Landen.adl` → includeert `Landen.xlsx`
- `DekkingSelectie.adl` → includeert `SnelCodes.xlsx`, `Plagen.adl`
- `ExportEisen.ifc` → includeert `Landen.xlsx`

**Nota bene:** `FytoSan.adl` is uitgecommentarieerd en dus niet actief!







## Referenties
- Ampersand documentatie: ampersandtarski.github.io

## Datum analyse: 2025-01-21
