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
  in haar eigen worktree, en de nog onbewezen C-obligaties
  (candidate-volledigheid) — de praktijktest is juist hun empirische toets.

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

**Stap 1 — Harnas-uitbreidingen** (in de compiler-worktree, zonder FC5):
replay-modus (`--replay`: transacties gesampled uit de scriptpopulatie; de
schaalparameter wordt sample-grootte), affected-only referee met instelbare
cadans plus integrale eindcheck, en mutaties op brede tabellen. Elke
uitbreiding eerst groen op de bestaande vijf testmodellen.

**Stap 2 — Rooktest op een FC5-deelmodel.** Eén registerdeel met zijn
xlsx-populatie in een tijdelijke database op de eigen container;
replay-transacties; referee groen; runtijd meten om stap 3 te dimensioneren.

**Stap 3 — Volledige FC5-populatie.** Drie meetdoelen: (a) **correctheid** —
referee nul verschillen over minstens duizend replay-transacties; (b)
**kandidaat-groottes** per conjunct per transactie (de efficiëntie-indicator
van het protocol); (c) **tijdverhouding** delta-protocol tegenover integrale
herberekening per transactie, beide in SQL op dezelfde database. Daarna, als
stap 1's wide-table-mutaties er zijn, dezelfde run op de productie-tabellayout.

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
