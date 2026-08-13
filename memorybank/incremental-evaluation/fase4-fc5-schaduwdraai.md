# Fase 4 — schaduwdraai van het delta-onderhoud op FC5

Status: plan, 2026-08-13. Hoort bij fase 4 van [plan.md](plan.md) en bouwt voort
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

**Stap 1 — dekkingsmeting op de echte transactiemix.** De fase-3-census
(626 conjuncts, 611 met delta-queries) zegt welke conjuncts *kunnen*; de
conservatieve regels van het framework bepalen wat er per transactie
*gebeurt*: een conjunct valt terug op volledige evaluatie zodra de transactie
een concept raakt dat het conjunct noemt, of een bulkmutatie doet. Meet op de
replay-transactiestroom van fase 3 welk aandeel van de
(transactie, conjunct)-paren werkelijk het delta-pad neemt. Dit getal bepaalt
of de concept-terugval eerst verfijning verdient (tabel-loze concepten niet
als affected aanmerken — hun populatie is voor geen enkele query zichtbaar).

**Stap 2 — schaduwdraai op de FC5-kopie.** Zelfde opzet als de
fase-3-praktijktest: volledige populatie, productie-tabellayout, minstens
duizend replay-transacties door de API van het draaiende prototype (niet
rechtstreeks op de database), met `transactions.deltaConjunctMaintenance:
'shadow'`. Slagingscriterium: nul `DELTA SHADOW MISMATCH`-regels; elke
mismatch is een bevinding die eerst verklaard en verholpen wordt (en waar hij
op een kandidaat-volledigheidsgat wijst, terugvloeit naar de C-obligaties van
fase 3).

**Stap 3 — meting van de fase-4-maatstaf.** De eerlijke vergelijking is
`on`-modus tegenover `off`-modus, beide inclusief cache-onderhoud: vandaag
betaalt het framework her-evaluatie plus integrale cachevervanging
(DELETE-alles + INSERT-alles per conjunct); het delta-pad betaalt de
kandidaatquery's plus twee scoped statements. Meet per transactie de
`close()`-tijd in beide standen op dezelfde replay-stroom. De
fase-3-vervolgpunten gelden ook hier: kandidaatset per (conjunct, relatie)
éénmalig materialiseren, kandidaat-groottes bemonsteren, en dezelfde meting op
een gegroeide populatie.

**Stap 4 — besluit en borging.** Rapportage inhoudsvrij en na review, zoals in
fase 3. Bij een divergentievrije schaduwperiode én een gunstige meting volgt
het echte fase-4-slot uit het plan: `on` met een periodieke steekproef
(zelfcontrole met alarm en automatische cache-rebuild) en de schakelaar per
applicatie. Valt de meting ongunstig uit, dan is dat een uitkomst, geen
mislukking: de schaduwmachinerie blijft staan en de optimalisatiepunten uit
stap 3 zijn de agenda.

## Wat deze schaduwdraai níét is

Geen productie-inzet: FC5's eigen containers, database en gebruikers blijven
buiten schot. De schakelaar staat in élk gepubliceerd image standaard op
`off`, dus een release van dit framework verandert het gedrag van bestaande
prototypes niet.
