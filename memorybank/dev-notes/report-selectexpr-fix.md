# Verslag: Eliminatie van onnodige cartesische producten in ENFORCE-regels

**Datum:** 19 mei 2026
**Auteur:** Cline (geautomatiseerd)
**Bestand gewijzigd:** `src/Ampersand/ADL1/P2A_Converters.hs`
**Taak-prompt:** `docs/sandbox-prompt-selectexpr-fix.md`

---

## 1. Probleem

In het door Ampersand gegenereerde prototype bevatten de violation-SQL-queries voor sommige ENFORCE-regels een **cartesisch product** (een join zonder `ON`-clause, in SQL zichtbaar als `from A as fst, A as snd`). Dit leidt tot O(n²) databasekosten bij runtime — een zware en onnodige belasting voor elke populatie die enige omvang heeft.

### Voorbeeldcasus

```
ENFORCE parentStar[A*A] >: I[A] \/ parentPlus[A*A]
```

Gegenereerde violation-SQL voor de bijbehorende conjunct:

```sql
select distinct t1.src as src, t1.tgt as tgt
from (select fst."A" as src, snd."A" as tgt        --  ← cartesisch product!
      from "A" as fst, "A" as snd
      where (fst."A" is not null) and (snd."A" is not null)) as t1
left join (
  select "SrcA" as src, "TgtA" as tgt from "parentStar" …
  union
  select … /* EDif (EDcV …) */
) as t2 on …
```

De query is 1937 tekens lang voor één simpele ENFORCE-regel.

---

## 2. Analyse

### 2.1 Bron van het cartesisch product

De oorzaak ligt in de normaliseerstap *"All-negative intersection"* in
`src/Ampersand/FSpec/ToFSpec/NormalForms.hs` (regels 1457-1465):

```haskell
-- All-negative intersection
... ECpl a, ECpl b ...  -- alle leden zijn complementen
  -> ECpl (EUni (a, b)) -- → V - (a ∪ b)
```

Bij het uitwerken van de violations van een ENFORCE-regel met union in de LHS
ontstaat (na `notCpl . conjNF . pushDownCpl`) een uitdrukking `-a /\ -b` die
door deze regel wordt gemaakt tot `V - (a \/ b)`. Hier wordt **V** (de
cartesische totale relatie) geïntroduceerd, die in SQL eindigt als een cross
join.

### 2.2 Bewijs met GHCi

In een Sandbox.hs is met `notCpl . conjNF . pushDownCpl` getoond:

| Invoer | Resultaat | V's | Cost |
|---|---|---|---|
| `(I[A] \/ I[A]~) \|- I*` | `V - (I* \/ (V - (I~ \/ I)))` | **2** | O(n²) |
| `I[A] \|- I*` + `I[A]~ \|- I*` | `I - I*` + `I~ - I*` | **0** | O(n) |

Dit bevestigt: door de ENFORCE *vóór* de normalisatie te splitsen in afzonderlijke regels per disjunct, treedt de "All-negative" regel niet op, en blijft de uitdrukking V-vrij.

### 2.3 Algebraïsche correctheid

De splitsing is algebraïsch sluitend:
- `(a ∪ b) ⊆ r  ⇔  (a ⊆ r) ∧ (b ⊆ r)`
- `r ⊆ (a ∩ b)  ⇔  (r ⊆ a) ∧ (r ⊆ b)`

Beide volgen direct uit de definities van union/intersectie en inclusion.

### 2.4 Nieuw ontdekt nevenprobleem

Naast de cartesisch-productkwestie bleek bij testen dat **het handmatig splitsen van een ENFORCE in ADL niet werkt**, omdat Ampersand in `mkRule` (zelfde functie) regelnamen genereert als:

```haskell
rrnm = "Compute" <> hash("Compute " <> tshow rel <> " using " <> command)
```

Twee ENFORCE-statements voor dezelfde relatie `r` met dezelfde command (`InsPair`) krijgen daardoor identieke namen — de tweede overschrijft de eerste in de regelverzameling. Dit is een latent probleem dat met deze fix wordt opgelost.

---

## 3. Oplossing

In `src/Ampersand/ADL1/P2A_Converters.hs`, functie `pEnforce2aEnforce`
(rond regels 1043-1076), wordt de ENFORCE-regel **vóór de normalisatie**
gesplitst per disjunct/conjunct:

### Vóór

```haskell
enfRules = case oper of
            IsSuperSet {} -> [insPair]
            IsSubSet {} -> [delPair]
            IsSameSet {} -> [insPair, delPair]
  where
    insPair = mkRule "InsPair" (EInc (expr, EDcD rel))
    delPair = mkRule "DelPair" (EInc (EDcD rel, expr))
    mkRule command fExpr = Rule { … , rrnm = hash lbl' , … }
      where lbl' = "Compute " <> tshow rel <> " using " <> command
```

### Na

```haskell
enfRules = case oper of
            IsSuperSet {} -> insPairs
            IsSubSet {} -> delPairs
            IsSameSet {} -> insPairs <> delPairs
  where
    -- (a \/ b) |- r  →  [a |- r, b |- r]
    insPairs = [ mkRule "InsPair" sub (EInc (sub, EDcD rel))
               | sub <- NE.toList (exprUni2list expr) ]
    -- r |- (a /\ b)  →  [r |- a, r |- b]
    delPairs = [ mkRule "DelPair" sub (EInc (EDcD rel, sub))
               | sub <- NE.toList (exprIsc2list expr) ]
    mkRule command subExpr fExpr = Rule { … , rrnm = hash lbl' , … }
      where lbl' = "Compute " <> tshow rel <> " using " <> command
                                <> " for " <> tshow subExpr  -- ← uniek per sub-expressie
```

De toevoeging `" for " <> tshow subExpr` aan `lbl'` zorgt dat elke gesplitste regel een unieke naam krijgt, zodat ze niet meer over elkaar heenschrijven.

---

## 4. Verificatie

Test ADL:
```
RELATION parentPlus[A*A]
RELATION parentStar[A*A]
ENFORCE parentStar >: I[A] \/ parentPlus
```

### Voor de fix
- 1 Compute-regel: `I\/parentPlus |- parentStar` → conj_0
- conj_0: **1937 tekens MET cartesisch product** (`from "A" as fst, "A" as snd`)

### Na de fix
- 2 Compute-regels:
  - `I[A] |- parentStar` → conj_43 (245 tekens, V-vrij)
  - `parentPlus |- parentStar` → conj_42 (290 tekens, V-vrij)
- **Geen V-conjuncts meer afkomstig uit deze ENFORCE-regel.**

De resterende V-conjuncts in het prototype (conj_12, conj_13) zijn afkomstig
uit de PrototypeContext-module en bestaan ook in een lege ADL-context — die
horen daar en zijn onafhankelijk van dit probleem.

### Reductie

| Maat | Voor | Na | Reductie |
|---|---:|---:|---:|
| Aantal regels | 1 | 2 | (gewenst) |
| Totale SQL-lengte | 1937 chars | 535 chars (245+290) | **72%** korter |
| Cartesische producten | 1 | 0 | **100%** |
| Asymptotische cost | O(n²) | O(n) | **n× sneller** |

### Regressietest

Twee bestaande testcases compileren succesvol zonder fouten:
- `testing/Travis/testcases/prototype/shouldSucceed/Try1.adl` ✅
- `testing/Travis/testcases/prototype/shouldSucceed/CartesianTest.adl` ✅

---

## 5. Reikwijdte

Deze fix is **minimaal-invasief**: het verandert alleen de structuur van
ENFORCE-regels in de A-structuur, niet de semantiek van de taal of de
normaliseerstappen. Andere uitdrukkingen (RULE, IDENT, VIEW, …) zijn niet
geraakt, omdat zij niet via `pEnforce2aEnforce` lopen.

Bijwerkingen:
- Bij ENFORCE met enkel-disjuncte expressie (geen `\/`/`/\` op top-niveau)
  blijft het gedrag exact gelijk (`exprUni2list e = [e]`).
- Bij union/intersectie op top-niveau worden er meer afzonderlijke regels
  gegenereerd — elk met een uniek hash. Dit kan leiden tot meer entries in
  `rules.json` en meer (kleinere) conjuncts in `conjuncts.json`, maar dat
  vermindert totale workload zoals hierboven aangetoond.

---

## 6. Geraakte bestanden

- `src/Ampersand/ADL1/P2A_Converters.hs` — fix in `pEnforce2aEnforce`/`toAEnforce`
- `docs/analysis-selectexpr-v-problem.md` — eerdere analyse (al aanwezig)
- `docs/report-selectexpr-fix.md` — dit verslag

## 7. Vervolgwerk (optioneel)

De achterliggende oorzaak — de "All-negative intersection" normaliseerstap die
V introduceert — staat nog steeds in `NormalForms.hs` regel 1457. Andere
fronten waar deze regel ongewenste V's introduceert (bijvoorbeeld binnen
gewone RULE's met een unionnegatie) zijn met deze patch *niet* geadresseerd.

Een meer principiële fix zou zijn: in `NormalForms.hs` de "All-negative"-regel
voorwaardelijk maken (alleen toepassen als beide operanden klein/eindig of
totaal zijn), of helemaal afschaffen en de SQL-generator alternatieve
patronen aanbieden voor `-a /\ -b` (bv. `NOT EXISTS`). Dat is een grotere
ingreep en valt buiten de scope van deze taak.
