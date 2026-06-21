# Lessen uit een gezamenlijke zoektocht naar een typechecker-bug

**Datum:** 1 mei 2026  
**Aanleiding:** Foutmelding over ambiguïteit in de Ampersand-typechecker — twee signatures voor `productnaam`, terwijl grep slechts één declaratie vond.

---

## De situatie

De typechecker meldde: _"This expression can have signatures [Eis\*ProductNaam] or [Eis\*WetenschappelijkeNaam]."_ Een zoekopdracht door de volledige werkdirectory vond uitsluitend `[Eis*ProductNaam]`. De vraag was: is dit een bug in de typechecker?

De zoektocht naar het antwoord duurde meerdere sessies en liep via een aantal foute aflagen. De lessen die hieruit te trekken zijn gelden voor alle situaties waarbij een fout diep verborgen zit in een compilatiepijplijn.

---

## Les 1 — Verifieer eerst de data, dan pas de redenering

In de eerste fase las ik broncode om te begrijpen hoe de typechecker werkt. Dat leverde een theorie op: ISA-hiërarchie (`ProductNaam ISA WetenschappelijkeNaam`) zorgt ervoor dat de typechecker `[Eis*WetenschappelijkeNaam]` als kandidaat-signature beschouwt.

De theorie klopte — maar leidde direct tot een fix, zonder dat ik eerst de werkelijke data had bekeken. De fix was: in `constrain`, eis een exacte match voor gedeclareerde relaties (`EDcD`) in plaats van een breder-of-gelijke match.

Na compileren en testen: geen effect. Source was in beide gevallen `Eis`. De fix filterden niets weg.

De les: een _logisch correcte_ fix op basis van een _ongecontroleerde aanname_ pakt niet uit. Controleer altijd eerst welke data de verdachte code-plek daadwerkelijk ontvangt, voordat je een fix schrijft.

---

## Les 2 — Trace zo vroeg mogelijk, niet als laatste redmiddel

Pas nadat de eerste fix faalde, voegden we een trace statement toe bij `relsList` in `resultPrim`. Die trace liet direct zien: `[Eis*WetenschappelijkeNaam]` staat al in `declMap`. De typechecker verzon niets — de declaratie was er gewoon.

Die trace had tien stappen eerder kunnen worden toegevoegd. Dan hadden we niet hoeven gissen over ISA-propagatie in de typechecker, SignOrd-vergelijking, of de `constrain`-functie.

De les: wanneer de fout diep in een pijplijn zit, is tracing de goedkoopste manier om vast te stellen waar de fout echt zit. Doe dat aan het begin van een onderzoek, niet aan het eind. Code lezen geeft inzicht in hoe iets bedoeld is; tracing laat zien wat er feitelijk gebeurt.

---

## Les 3 — Verklein de zoekruimte met elke stap

De zoektocht ging door vier niveaus:

1. Waar worden ambiguïteitsfouten gegooid? → `findAmbiguities` in P2A_Converters.hs.
2. Waar komen de twee signatures vandaan? → ISA-hiërarchie; `relsList` bevat beide.
3. Hoe komt `[Eis*WetenschappelijkeNaam]` in `declMap`? → `pDecl2aDecl` met een specifieke `origin`.
4. Wie maakt die declaratie? → Waarschijnlijk de XLSX-parser via `genericRelations`.

Elke stap stelde een nieuwe gerichte vraag. Een trace bij de juiste stap beantwoordde die vraag in één compilatie. De fout was niet in de typechecker-logica; hij zat een stap eerder, in de constructie van `declMap`.

De les: formuleer bij elke stap één concrete vraag die een binaire of kleine keuzeset oplevert. Kies de trace-plek zo dat het antwoord die vraag beantwoordt en de volgende stap dicteert.

---

## Les 4 — Laat je corrigeren op het niveau van semantiek, niet syntax

De gebruiker corrigeerde meermaals de richting van het onderzoek:

- _"Je moet zoeken waar de typechecker aanneemt dat de relatie bestaat — niet waar de ISA-propagatie plaatsvindt."_
- _"Term TermPrim is geen Relation. Je kunt `declMap` niet direct bevragen vanuit een term."_
- _"Fix het op de plek waar de OpTree wordt geconstrueerd, niet downstream in `findAmbiguities`."_
- _"Gedeclareerde relaties mogen niet breder zijn — ze moeten precies kloppen."_

Al deze correcties gingen over semantiek: wat betekent het dat een relatie "bestaat"? Ik redeneerde telkens vanuit de code-structuur; de gebruiker redeneerde vanuit het datamodel. Die kloof gaf de vertraging.

De les: als je merkt dat je steeds van richting wordt bijgestuurd, stel dan expliciet de semantische vraag: _"Wat zou de juiste uitkomst zijn als alle data correct is?"_ Dat geeft het referentiepunt waaraan je de code toetst.

---

## Les 5 — Lees een bestand één keer goed, gebruik het daarna als referentie

Tijdens de sessie las ik `P2A_Converters.hs` vijf keer. Bij de derde en vierde lezing gaf het systeem een "DUPLICATE READ"-waarschuwing. Dat wijst op een patroon: ik laadde opnieuw in wat ik al had, omdat ik niet gericht genoeg naar de juiste sectie keek.

De les: lees een bestand één keer met een gerichte start- en eindregel. Noteer (of onthoud) welke functies relevant zijn. Gebruik bij vervolgvragen een gerichte grep of een kleine leesopdracht met regelnummers in plaats van een volledige bestandslees.

---

## Les 6 — De fout zit vaak een laag eerder dan je denkt

De eerste hypothese was: de typechecker maakt een fout in de ambiguïteitsdetectie. Na onderzoek bleek het probleem niet in de typechecker zelf te zitten, maar in de invoer: `declMap` bevatte een declaratie die niet in de ADL-bronbestanden stond.

Dit patroon — "de fout zit een stap eerder dan de foutmelding" — is gebruikelijk in systemen met een compilatiepijplijn. De foutmelding beschrijft wat het systeem ziet; de oorzaak ligt bij wie de data heeft aangeleverd.

De les: vraag bij elke foutmelding niet alleen "waarom gaat dit fout?" maar ook "klopt de input van deze functie?" Dat tweede is vaak sneller te controleren dan het eerste.

---

## Les 7 — Beslissingen zijn beter als ze traceerbaar zijn

Twee keer veronderstelden we de oorzaak zonder het te bevestigen. Beide keren leidde dat tot een fix die niets deed. Beide keren hadden we eerst een `trace` kunnen toevoegen die de aanname bevestigde.

Traceerbaarheid gaat niet alleen over debugging-output. Het gaat ook over het bewust vastleggen, voor jezelf en de gebruiker, wat de aanname is die de volgende stap legitimeert. In dit geval: _"We nemen aan dat `[Eis*WetenschappelijkeNaam]` alleen passeert vanwege het ISA-mechanisme in `constrain`. Laten we dat bevestigen."_

Als die zin hardop was uitgesproken, had de gebruiker direct kunnen antwoorden: "Maar beide hebben source = Eis, dus dat filter helpt niet."

De les: maak aannames expliciet, ook als ze voor de hand lijken te liggen. Dat is niet omslachtig; het bespaart tijd.

---

## Samenvatting

| Stap | Goed | Beter |
|---|---|---|
| Hypothese vormen | Snel | Bevestig eerst met data |
| Fix implementeren | Logisch consistent | Wacht op bevestiging |
| Trace toevoegen | Uiteindelijk gedaan | Direct aan het begin |
| Richting aanpassen | Bereidheid om te corrigeren | Proactief de semantische vraag stellen |
| Bestand lezen | Grondig | Gerichter, minder duplicaten |

Het patroon dat werkt: formuleer een vraag, kies de minimale trace die antwoord geeft, compileer, concludeer, stap door. Zo bouw je begrip op zonder te gissen.
