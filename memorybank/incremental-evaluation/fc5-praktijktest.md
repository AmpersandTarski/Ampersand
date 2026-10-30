# Praktijktest van delta-SQL op FC5

Status: plan, 2026-08-13. Hoort bij issue
[#1684](https://github.com/AmpersandTarski/Ampersand/issues/1684) (fase 3) en
bereidt fase 4 voor. FC5 is het productiemodel van een lopend klantproject; de
naam is publiek (release notes v5.9.4), de inhoud niet.

## Vertrouwelijkheid en niet-verstoren — harde regels

1. De lokale FC5-werkmap is voor deze test **strikt leesalleen**: geen
   bestanden erin, geen git-operaties erop, geen wijziging aan wat daar draait.
2. Er verschijnt **geen FC5-inhoud** buiten de lokale machine: geen
   relatienamen, atomen, populaties of querytekst in issues, commits,
   memorybank of chatkanalen. Publieke rapportage bevat uitsluitend
   inhoudsvrije aggregaten (aantallen, percentages, tijden) en pas na review
   door Stef. Ook lokale machinepaden blijven uit publieke bestanden.
3. De test gebruikt een **eigen database-omgeving**: de bestaande
   test-container (of een eigen, met uniek poortnummer), tijdelijke databases
   met een herkenbare prefix, opgeruimd na afloop. De productie- en
   ontwikkelcontainers van het project zelf worden niet aangeraakt.
4. Runs die de gedeelde test-database claimen, serialiseren via de afgesproken
   `mkdir`-lock, zodat parallelle sessies elkaar niet raken.
5. Testresultaten landen lokaal; wat ervan in de projectwerkmap of elders
   hoort, beslist Stef.

## Gereedheidstoets (stand 2026-08-13)

**Klaar:** de delta-SQL-keten staat end-to-end (candidate-calculus, contract,
schema, harnas) en het referee-harnas draait groen op vijf testmodellen tegen
echte MariaDB, subtyping en complementen inbegrepen.

**Nog niet klaar — en bepalend voor dit plan:**

- **De dekkingscensus op FC5 is nog niet gedraaid.** De sessie-permissies
  blokkeerden het aanroepen van de compiler op de FC5-map, dus onbekend is
  welk deel van FC5's conjuncts in de ondersteunde klasse valt. Zonder dat
  getal is niet te zeggen of de praktijktest 10% of 90% van de regels
  incrementeel toetst. Dit is stap 0.
- **Drie harnas-beperkingen knellen op FC5** (stap 1):
  1. Het harnas vereist `--sql-bin-tables`; FC5 draait productie met brede
     tabellen. Voor de correctheidsvraag is bin-tables een geldige toets
     (zelfde relatiealgebra-semantiek), maar representatieve mutaties op brede
     tabellen (UPDATE/NULL op kolommen volgens `RelStore`) zijn een
     uitbreiding.
  2. Synthetische atomen passen niet bij getypeerde concepten (datums e.d.):
     de generator faalt daar bewust. Nodig is een **replay-modus**: transacties
     zijn verwijderen/terugplaatsen van bestaande paren uit de echte
     populatie — type-veilig én het realistische mutatieprofiel.
  3. De referee her-evalueert nu per transactie álle ondersteunde conjuncts
     volledig; op FC5-schaal onbetaalbaar. Nodig: alleen de geráákte conjuncts
     per transactie (dat is volledig: een ongeraakte conjunct kan niet
     veranderen), een instelbare cadans, en één integrale eindcheck.
- **Parallelle sporen** die de volgorde raken, niet de inhoud: de lopende
  release v5.9.7 (de `delta-sql`-branch merget daarna), de #1683-bewijssessie
  in haar eigen worktree, en de destijds nog onbewezen K-obligaties
  (kandidaat-volledigheid; tot 2026-08-14 "C-obligaties" geheten) — de
  praktijktest is juist hun empirische toets. Inmiddels zijn zij
  machine-checked: `proofs/incremental/Candidates.thy`, registerclaim PRF-7.

Oordeel: **nog niet toe aan de praktijktest zelf**; wel aan stap 0 en stap 1,
die samen de weg vrijmaken.

## Het plan

**Stap 0 — Census (leesalleen, geen database).** Compileer FC5 met de
delta-sql-compiler en tel: conjuncts totaal, conjuncts met delta-queries, en
de constructies waarop de rest afvalt. Het harnas doet dit vóór enig
database-werk; zonder `--sql-bin-tables` stopt het na de telling vanzelf:

    stack exec ampersand -- incremental-bench <FC5>/project/main.adl --sql

*Beslispunt:* bij dekking onder circa 60% eerst de D-regels uitbreiden (de
residu-antijoins liggen het meest voor de hand; hun S-lemma's zijn al bewezen)
en de census herhalen.

*Uitslag (2026-08-13):* het model compileert met `--build-recipe Prototype`
(de SIAM-koppeling gebruikt PrototypeContext-relaties) en telt **626
conjuncts, waarvan 611 (97,6%) met delta-queries**; 15 vallen buiten de
ondersteunde klasse. Het beslispunt is gehaald: geen D-regel-uitbreiding
nodig vóór de praktijktest.

**Stap 1 — Harnas-uitbreidingen** (in de compiler-worktree, zonder FC5):
replay-modus (`--replay`: transacties gesampled uit de scriptpopulatie; de
schaalparameter wordt sample-grootte), affected-only referee met instelbare
cadans plus integrale eindcheck, en mutaties op brede tabellen. Elke
uitbreiding eerst groen op de bestaande vijf testmodellen.

*Uitslag (2026-08-13):* gereed (commit 208f9bac1): `--replay`,
`--referee-every K` (met altijd de integrale eindcheck), UPDATE-mutaties op
brede tabellen, en tijdmeting per transactie. De matrix is groen: vier
modellen × drie modi (synthetisch/bin, replay/bin, replay/brede-layout).
Een lege replay-stroom beëindigt de run in plaats van te blijven draaien
(gevonden doordat een populatieloos testmodel de lus liet spinnen).

**Stap 2 — Rooktest op een FC5-deelmodel.** Eén registerdeel met zijn
xlsx-populatie in een tijdelijke database op de eigen container;
replay-transacties; referee groen; runtijd meten om stap 3 te dimensioneren.

*Uitslag (2026-08-13):* de rooktest draaide meteen op het volledige model met
de volledige populatie (25 replay-transacties, productie-tabellayout): groen.

**Stap 3 — Volledige FC5-populatie.** Drie meetdoelen: (a) **correctheid** —
referee nul verschillen over minstens duizend replay-transacties; (b)
**kandidaat-groottes** per conjunct per transactie (de efficiëntie-indicator
van het protocol); (c) **tijdverhouding** delta-protocol tegenover integrale
herberekening per transactie, beide in SQL op dezelfde database. Daarna, als
stap 1's wide-table-mutaties er zijn, dezelfde run op de productie-tabellayout.

*Uitslag (2026-08-13):*

- **(a) Correctheid: gehaald.** 1075 replay-transacties (3×350 + 25) op de
  volledige populatie en de productie-tabellayout, referee elke tiende
  transactie op de geraakte conjuncts plus per chunk een integrale eindcheck
  over alle 611 delta-onderhouden caches: **nul verschillen**. De
  K-obligaties (kandidaat-volledigheid) zijn daarmee empirisch stevig
  ondersteund; sinds 2026-08-14 zijn zij bovendien machine-checked
  (`proofs/incremental/Candidates.thy`, registerclaim PRF-7).
- **(c) Tijdverhouding: het delta-protocol wint hier nog niet.** Medianen per
  transactie over de drie chunks: delta-protocol 547/567/596 ms; referee
  (cache-pull plus volledige her-evaluatie van de geraakte conjuncts)
  236/449/242 ms. Op de huidige populatiegrootte zijn FC5's volledige queries
  al snel, terwijl het protocol per (conjunct, relatie) twee statements
  uitvoert die elk de kandidaatquery opnieuw evalueren — de kandidaatprijs
  overtreft de winst. De in-Haskell schaalcurves (bench/RESULTS.md) laten de
  kruising met de databasegroei meegroeien; wanneer die kruising voor FC5
  valt, is een open meetvraag.
- **(b) Kandidaat-groottes: niet gemeten** — open, zie vervolgwerk.

*Vervolgwerk uit stap 3:* (1) de kandidaatverzameling per (transactie,
conjunct) éénmalig materialiseren in een tijdelijke tabel en beide scoped
statements daartegen draaien (halveert de kandidaatevaluatie); (2)
kandidaat-groottes bemonsteren; (3) dezelfde meting op een gegroeide
populatie, want de winst schaalt met de databasegrootte; (4) de fase-4-maatstaf
is het huidige framework-gedrag — integrale her-evaluatie plus integrale
cache-vervanging — dat aan de referee-kant nog cache-schrijfkosten toevoegt.
Runtijd per chunk: 36-47 minuten, gedomineerd door modelcompilatie,
populatie-installatie en de twee integrale referees.

**Stap 4 — Rapportage en borging.** Resultaten lokaal; in overleg naar de
projectwerkmap. Naar issue #1684 alleen inhoudsvrije aggregaten na review.
Bevindingen die het ontwerp raken (dekkingsgaten, kandidaat-explosies) komen
als ontwerpkeuzes of plan-bijstellingen in dit dossier terug.

## Wat deze test níét is

Geen fase-4: er draait geen framework mee, er verandert niets aan het
FC5-project, en er ontstaat geen productie-afhankelijkheid. De test toetst
uitsluitend of de gegenereerde delta-SQL op industriële data correct en
zuinig is — de voorwaarde om fase 4 (framework-adoptie en schaduwdraaien)
met vertrouwen te beginnen.
