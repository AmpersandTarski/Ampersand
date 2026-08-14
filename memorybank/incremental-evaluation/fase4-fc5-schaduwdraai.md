# Fase 4 — schaduwdraai van het delta-onderhoud op FC5

Status: stappen 0-3 uitgevoerd, 2026-08-14; uitslagen staan per stap hieronder.
Hoort bij fase 4 van [plan.md](plan.md) en bouwt voort
op de fase-3-praktijktest ([fc5-praktijktest.md](fc5-praktijktest.md)). De
vertrouwelijkheids- en niet-verstoorregels van dat plan gelden hier onverkort:
de FC5-werkmap blijft leesalleen, er verschijnt geen FC5-inhoud buiten de
lokale machine, de test draait in een eigen databaseomgeving, en gedeelde
bronnen serialiseren via de afgesproken `mkdir`-lock.

## Wat er sinds fase 3 bij is gekomen

Het prototype-framework onderhoudt de violation-cache nu zelf met het
delta-protocol (branch `feat-delta-conjunct-maintenance` in de
prototype-repo). De schakelaar `transactions.deltaConjunctMaintenance` kent
drie standen. In `off` verandert er niets. In `shadow` draaien beide routes:
het delta-protocol onderhoudt de cache, daarna evalueert het framework
volledig zoals vandaag; elk verschil komt als `DELTA SHADOW MISMATCH` in de
log en het volledige resultaat blijft leidend. In `on` vervalt de volledige
evaluatie voor conjuncts binnen de ondersteunde klasse.

De rooktest op `transactional-demo` (2026-08-13, zelf waargenomen) toonde:
relatie-only transacties onderhouden hun conjuncts via het delta-protocol met
schaduwoordeel "identical", een schendende transactie wordt ook in `on`-modus
geblokkeerd vanuit de delta-onderhouden cache, delta-tabellen zijn na commit
leeg, en de cache is gelijk aan een volledige her-evaluatie.

## Het schaduwplan voor FC5

**Stap 0 — bouwvoorwaarden.** De schaduwdraai vereist een FC5-image met (a) de
delta-sql-compiler (branch `delta-sql`, voor de generics met `deltaQueries` en
de delta-tabellen in het schema) en (b) het framework van
`feat-delta-conjunct-maintenance`. Beide zijn nog niet gereleased; de bouw
gebeurt dus lokaal met een lokaal gebouwd framework-image, buiten de
productie- en ontwikkelcontainers van het project om.

*Uitslag (2026-08-14):* de opstelling staat en wijkt bewust af van het
image-recept hierboven: een kopie op API-niveau in plaats van een volledig
FC5-image (ontwerpkeuze DC-12 in [DesignChoices.md](DesignChoices.md)). De
framework-branch draait als gemounte worktree in een kaal framework-image,
de generics komen van de delta-sql-compiler op de host, en de replay gaat
door de volledige request- en transactiepijplijn via een eigen
replay-endpoint; een Angular-frontend doet niet mee, want de schaduwdraai
rijdt uitsluitend op de API. De generatie reproduceert de fase-3-census
exact (626 conjuncts, 611 met delta-queries; 232 relaties met delta-tabel)
en de installer neemt de delta-tabellen mee in het schema. Twee bewuste
configuratie-afwijkingen: `session.loginEnabled` staat uit (replay zonder
SIAM; conjunct-onderhoud is rol-onafhankelijk) en de schakelaar staat op
`shadow`. Bekend artefact daarvan: een staande schending van de
PrototypeContext-regel over actieve rollen.

**Stap 1 — dekkingsmeting op de echte transactiemix.** De fase-3-census
(626 conjuncts, 611 met delta-queries) zegt welke conjuncts *kunnen*; de
conservatieve regels van het framework bepalen wat er per transactie
*gebeurt*: een conjunct valt terug op volledige evaluatie zodra de transactie
een concept raakt dat het conjunct noemt, of een bulkmutatie doet. Meet op de
replay-transactiestroom van fase 3 welk aandeel van de
(transactie, conjunct)-paren werkelijk het delta-pad neemt. Dit getal bepaalt
of de concept-terugval eerst verfijning verdient (tabel-loze concepten niet
als affected aanmerken — hun populatie is voor geen enkele query zichtbaar).

*Uitslag (2026-08-14):* over de schaduwdraai van 1043 transacties nam 61%
van de (transactie, conjunct)-instanties het delta-pad (3684 van 6002); een
kleinere proef van 99 transacties gaf 79%, het aandeel hangt dus merkbaar
van de getrokken mix af. Dat is ruim boven het niveau waarop de verfijning
van de concept-terugval de praktijktest zou moeten voorafgaan; zij blijft
staan als optimalisatiepunt voor daarna.

**Stap 2 — schaduwdraai op de FC5-kopie.** Zelfde opzet als de
fase-3-praktijktest: volledige populatie, productie-tabellayout, minstens
duizend replay-transacties door de API van het draaiende prototype (niet
rechtstreeks op de database), met `transactions.deltaConjunctMaintenance:
'shadow'`. Slagingscriterium: nul `DELTA SHADOW MISMATCH`-regels; elke
mismatch is een bevinding die eerst verklaard en verholpen wordt (en waar hij
op een kandidaat-volledigheidsgat wijst, terugvloeit naar de C-obligaties van
fase 3).

*Uitslag (2026-08-14): gehaald.* De schaduwdraai omvatte 1043
replay-transacties (3×350 + 25, seeds 46-49) op de volledige scriptpopulatie
en de productie-tabellayout, na een schone herinstallatie, plus een
voorafgaande proef van 99 transacties: samen 1142 transacties met **nul
`DELTA SHADOW MISMATCH`-regels** en 3684 + 383 schaduwchecks met oordeel
"identical". Veertien transacties rolden terug op een invariantschending;
ook daar oordeelden beide routes gelijk. De replay verwijdert en herplaatst
bestaande paren door de request-pijplijn (ExecEngine inbegrepen); de paren
komen uit de exporter, omdat de ExecEngine bij installatie een deel van de
scriptpopulatie herschrijft en een replay uit `populations.json` daardoor
op niet-bestaande paren kan stuiten (één keer waargenomen, daarna
afgevangen).

**Stap 3 — meting van de fase-4-maatstaf.** De eerlijke vergelijking is
`on`-modus tegenover `off`-modus, beide inclusief cache-onderhoud: vandaag
betaalt het framework her-evaluatie plus integrale cachevervanging
(DELETE-alles + INSERT-alles per conjunct); het delta-pad betaalt de
kandidaatquery's plus twee scoped statements. Meet per transactie de
`close()`-tijd in beide standen op dezelfde replay-stroom. De
fase-3-vervolgpunten gelden ook hier: kandidaatset per (conjunct, relatie)
éénmalig materialiseren, kandidaat-groottes bemonsteren, en dezelfde meting op
een gegroeide populatie.

*Uitslag (2026-08-14): het delta-pad wint op deze populatiegrootte nog
niet.* Dezelfde geseede stream draaide in beide standen na een schone
herinstallatie; gemeten is de tijd van `runExecEngine()->close()` in het
replay-endpoint, dus inclusief cache-onderhoud aan beide kanten. Over 1043
gepaarde transacties: mediaan 4,22 ms (`on`) tegenover 3,05 ms (`off`),
p90 10,4 tegenover 6,7 ms; de paarsgewijze verhouding heeft mediaan 1,41
met p10 0,85 en p90 3,03 — een minderheid van de transacties is onder `on`
dus wél sneller. De commit-beslissing was in alle 1043 paren gelijk, wat de
gedragsgelijkheid van de delta-onderhouden cache nogmaals bevestigt, nu in
`on`-modus zonder vangnet. Het beeld spoort met fase 3: op ~1100 paren zijn
FC5's volledige queries snel en overtreft de kandidaatprijs de winst; de
winst moet van databasegroei komen. De meting op een gegroeide populatie is
daarmee het eerstvolgende meetpunt.

**Stap 4 — besluit en borging.** Rapportage inhoudsvrij en na review, zoals in
fase 3. Bij een divergentievrije schaduwperiode én een gunstige meting volgt
het echte fase-4-slot uit het plan: `on` met een periodieke steekproef
(zelfcontrole met alarm en automatische cache-rebuild) en de schakelaar per
applicatie. Valt de meting ongunstig uit, dan is dat een uitkomst, geen
mislukking: de schaduwmachinerie blijft staan en de optimalisatiepunten uit
stap 3 zijn de agenda.

*Uitslag (2026-08-14):* de schaduwperiode is divergentievrij, de meting is
op de huidige populatiegrootte nog niet gunstig — dus geen `on` voor een
echte applicatie nu. De agenda daarheen bestaat uit de al genoemde punten:
de kandidaatset per (conjunct, relatie) éénmalig materialiseren, de
concept-terugval verfijnen voor tabel-loze concepten, en de meting herhalen
op een gegroeide populatie — dáár moet de kruising liggen die de
in-Haskell-schaalcurves voorspellen. Deze notitie is de rapportage; wat
ervan naar issue #1684 gaat, bepaalt Stef na review. De opstelling blijft
staan (containers `fc5schaduw-prototype`/`-db` op poort 8490/3490, worktree
`PrototypeFramework-delta`, artefacten en draaiboeken in `/tmp/fc5-fase4/`)
en is met `docker compose -f /tmp/fc5-fase4/docker-compose.yml down` en
`git worktree remove` op te ruimen zodra zij niet meer dient.

## Wat deze schaduwdraai níét is

Geen productie-inzet: FC5's eigen containers, database en gebruikers blijven
buiten schot. De schakelaar staat in élk gepubliceerd image standaard op
`off`, dus een release van dit framework verandert het gedrag van bestaande
prototypes niet.
