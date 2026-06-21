# Probleemanalyse: selectExpr voor EDif(EDcV, x) — performantieverlies

## Samenvatting

In `src/Ampersand/FSpec/SQL.hs` zit op lijn 1148–1150 een `case`-arm die
`EDif(EDcV(sgn), x)` — "de universele relatie minus x" — vertaalt naar
`selectExpr fSpec (notCpl x)`.  
Voor een **positieve** x (zoals `EUni(l, r)`) produceert `notCpl x = ECpl(x)`
een gecorreleerde `NOT EXISTS`-query met de volledige cartesiaanse ruimte `V×V`
als buitenste tabel.  
Dit leidt tot een exponentielle uitvoeringstijd: **O(n² × m)** in plaats van
de haalbare **O(n² + m)** met LEFT JOIN.

---

## 1. De relevante code (lijn 1148–1150 SQL.hs)

```haskell
-- in nonSpecialSelectExpr, SQL.hs
(EDif (EDcV _, x)) ->
  traceComment ["case: EDif (EDcV _,x)"]
    $ selectExpr fSpec (notCpl x)
```

De bedoeling is correct: `V - x = V ∧ (¬x)`.  
De implementatie hergebruikt `notCpl x` om het complement op te bouwen.

---

## 2. `notCpl` — twee fundamenteel verschillende gevallen

`notCpl` is gedefinieerd in `Ampersand.ADL1.Expression`:

```haskell
notCpl :: Expression -> Expression
notCpl (ECpl e) = e       -- dubbel complement cancelt: -(-e) = e
notCpl e        = ECpl e  -- voeg complement toe
```

Dit leidt tot twee gedragspaden:

| x             | notCpl x       | levert op                                  |
|---------------|----------------|--------------------------------------------|
| `ECpl e`      | `e`            | `selectExpr e` — direct, efficiënt       |
| `EUni(l, r)`  | `ECpl(EUni(l,r))` | `selectExpr(ECpl(EUni(...)))` — **PROBLEEM** |
| `EIsc(l, r)`  | `ECpl(EIsc(...))` | `selectExpr(ECpl(EIsc(...)))` — **PROBLEEM** |
| `EDcI c`      | `ECpl(EDcI c)` | speciaal geval (cross product met ≠) — efficiënt |
| `EDcD r`      | `ECpl(EDcD r)` | `selectExpr(ECpl(EDcD r))` → `_ ->` branch — PROBLEEM |

**Bewijs (ghci):**
```
notCpl (EUni(R,S))          = ECpl(EUni(I[R], I[S]))    ← positief geval
notCpl (ECpl R)             = I[R]                       ← efficiënt
notCpl (ECpl (EUni(R,S)))   = EUni(I[R], I[S])          ← efficiënt
```

---

## 3. De problematische code-chain

### Stap voor stap voor `EDif(V[A*B], EUni(l, r))`

```
selectExpr fSpec (EDif(EDcV sgn, EUni(l,r)))
  │
  ├─ maybeSpecialCase: EDif(...) heeft geen speciaal patroon → Nothing
  │
  └─ nonSpecialSelectExpr
       │
       └─ case (EDif (EDcV _, x)) met x = EUni(l,r)
            │
            └─ selectExpr fSpec (notCpl (EUni(l,r)))
                 = selectExpr fSpec (ECpl (EUni(l,r)))
                 │
                 ├─ maybeSpecialCase: ECpl(EUni(...)) heeft geen speciaal patroon → Nothing
                 │
                 └─ nonSpecialSelectExpr
                      │
                      └─ case (ECpl e) met e = EUni(l,r), geen EDcV/EDcI → _ branch
                           │
                           theClosedWorldExpression = EDcV(sign(EUni(l,r)))
                           │
                           └─ GENEREERT:
                              SELECT cw.src, cw.tgt
                              FROM (cartesiaans product V[A*B]) AS cw   ← O(n²) rijen
                              WHERE NOT EXISTS (
                                SELECT * FROM (l UNION r) AS pos
                                WHERE cw.src = pos.src AND cw.tgt = pos.tgt
                              )                                          ← gecorreleerd!
```

**Kernobservatie:** De buitenste tabel is de volledige cartesiaanse ruimte V×V
(O(n²) rijen), en voor elke rij wordt de gecorreleerde subquery opnieuw
geëvalueerd → **O(n² × m)** vergelijkingen totaal.

---

## 4. Wanneer treedt dit patroon op?

### 4a. Expliciete ADL-code

Als de gebruiker `V - (r \/ s \/ t)` schrijft in ADL.

### 4b. Via normalisatie (NormalForms.hs)

`NormalForms.hs` bevat de normalisatieregel (lijn ~"All-negative intersection"):

```haskell
| null posList && not (null negList) && all isNeg rs =
    let negInners = map notCpl negList
     in ( EDif (EDcV (sign l), foldr1 (.\/.) (head negInners NE.:| tail negInners)),
          ["All-negative intersection: -x/\\-y = V-(x\\/y)"],
```

Dit converteert **`-r /\ -s /\ ...`** → **`EDif(EDcV, EUni(r, s, ...))`**

**Bewijs (ghci):**
```
Input: -R /\ -S
conjNF tree:  EDif(V[[R]], EUni(I[S], I[R]))
              ↑ pattern: EDif(V, EUni x) => PROBLEEM: V*V + NOT EXISTS

Input: -R /\ -S /\ -T
conjNF tree:  EDif(EDif(V[[S]], EUni(I[T], I[S])), I[R])
              ↑ cascade: outer EDif niet V-patroon, maar inner wél!
```

De 3-term cascade is interessant:
- `EDif(EDif(V, EUni(T,S)), I[R])` — outer EDif heeft links een EDif, niet EDcV
- Maar de **inner** `EDif(V, EUni(T,S))` IS het problematische patroon!
- De fix corrigeert de inner expressie via recursie.

### 4c. CartesianTest-patroon

In `CartesianTest.adl`:
```adl
ENFORCE product := vorm;vorm~ /\ productnaam;voorkeursNaam~
```
Levert deletie-check op:
```
product /\ -(vorm;vorm~ /\ productnaam;voorkeursNaam~)
= product /\ (-vorm;vorm~ \/ -productnaam;voorkeursNaam~)
```

Hier genereert elke `ECpl(ECps(...))` direct een V×V-cartesiaan.
Met de normStep-fix kan dit herschreven worden naar `EDif(V, EUni(...))`,
waarna de selectExpr-fix de LEFT JOIN genereert.

---

## 5. Semantische grondslag

De equivalentie `EDif(V, x) = EIsc(V, ECpl(x))` is een algebraïsche tautologie:

```
V - x
= { definitie verschil: a - b = a ∧ ¬b }
V ∧ ¬x
= EIsc(V, ECpl(x))
```

Dit is de basis voor de fix: in plaats van `selectExpr (notCpl x)` aan te roepen
(wat ECpl(EUni) produceert), construeert de fix `EIsc(V(sgn), ECpl(x))` en laat
`maybeSpecialCase` die als LEFT JOIN vertalen.

---

## 6. De ECpl general case — de eigenlijke boosdoener

In `nonSpecialSelectExpr` (lijn 983-1029):

```haskell
(ECpl e) ->
  case e of
    EDcV _ -> emptySet                        -- V = ∅ na complement
    EDcI c -> [cross product c ≠ c']          -- concept × concept  
    _ ->                                      -- HIER: EUni, EIsc, ECps, etc.
      BinSelect {
        ...,
        bseTbl = [(toTableRef . selectExpr fSpec) (EDcV (sign e)) `as` closedWorldName],
        bseWhr = Just $ selectNotExists (toTableRef (selectExpr fSpec e) `as` posName) ...
      }
      where theClosedWorldExpression = EDcV (sign e)
```

De `_ ->` branch is generiek correct maar performantie-zwak:
- **Buitenste tabel:** `EDcV(sign e)` = cartesiaans product van src- en tgt-concept
- **Filter:** `NOT EXISTS (correlated subquery)`
- **MySQL/MariaDB:** evalueert gecorreleerde subqueries rij voor rij

---

## 7. Performance-analyse

| Methode | Outer tabel | Filter | Complexiteit |
|---------|-------------|--------|--------------|
| NOT EXISTS (huidig) | V×V = O(n²) rijen | gecorreleerde subquery O(m) per rij | **O(n² × m)** |
| LEFT JOIN (gewenst) | V×V = O(n²) rijen | hash-probe O(1) per rij, O(m) build | **O(n² + m)** |

**Concreet voorbeeld (ghci-bewijs):**
```
n = 1000 instanties, m = 10 paren in EUni

NOT EXISTS: 1.000.000 × 10 = 10.000.000 vergelijkingen
LEFT JOIN:  1.000.000 + 10 = 1.000.010 vergelijkingen
Speedup:    ~10x
```

> Noot: Bij MySQL/MariaDB is het verschil in de praktijk nog groter omdat de
> query-optimizer NOT EXISTS slecht optimaliseert voor gecorreleerde subqueries
> met een cartesiair product als buitenste tabel.

---

## 8. Randgevallen en interactie met andere code

### 8a. x = ECpl e (negatief geval — geen probleem)

```
EDif(V, ECpl e)
→ notCpl(ECpl e) = e     [dubbel complement cancelt]
→ selectExpr fSpec e     [direct, efficiënt]
```

De bestaande aanpak werkt perfect voor negatieve x.

**Bewijs (ghci):**
```
notCpl (ECpl R)   = I[R]     ← geen ECpl wrapper
```

### 8b. x = EDcV sgn2 (V - V = ∅)

- Huidig: `notCpl(EDcV) = ECpl(EDcV)` → `ECpl(EDcV _)` case → `emptySet` (O(1))
- Na fix (positief geval): `maybeSpecialCase(EIsc(V, ECpl(V)))` → LEFT JOIN V met V → semantisch lege verzameling, maar O(n²)
- **Mitigatie:** V - V wordt normaal door de normalizer vereenvoudigd naar ∅ vóór SQL-generatie

### 8c. x = EDcD rel (directe relatie)

- Huidig: `ECpl(EDcD rel)` → NOT EXISTS met V×V als outer + EDcD als inner (met subquery)
- Na fix: `go False V (EDcD rel)` → EDcD branch in `go` → LEFT JOIN V direct met de relatietabel (geen subquery!)
- **Dit is snéller dan de huidige code** — directe tabel-JOIN i.p.v. subquery

### 8d. 3-term cascade (ghci-bewijs)

```
conjNF(-R /\ -S /\ -T) = EDif(EDif(V[[S]], EUni(I[T], I[S])), I[R])
  inner check:    EDif(V, EUni x)   => PROBLEEM: ECpl(EUni) => V*V + NOT EXISTS
```

De cascade-structuur `EDif(EDif(V, EUni(T,S)), I[R])` heeft:
- Inner `EDif(V, EUni)` → wordt door de fix gecorrigeerd naar LEFT JOIN EUni
- Outer `EDif(inner, I[R])` → valt in de algemene `EDif(l,r)` case:
  `selectExpr (l ./\. ECpl r)` = `EIsc(inner, ECpl(I[R]))`
  → `maybeSpecialCase EIsc(e, ECpl e2)` → LEFT JOIN inner met I[R]

Beide niveaus profiteren van de fix (via recursie).

---

## 9. `go` altijd een `Just` — fallback nooit nodig

`go` in `maybeSpecialCase` retourneert **altijd** `Just`:

```haskell
go :: Bool -> Expression -> Expression -> Maybe BinQueryExpr
go isFlipped' expr1 expr2 =
  Just                           -- ← altijd Just!
    . traceComment [...]
    $ BinSelect { ... }
```

De fallback `Nothing -> selectExpr fSpec (notCpl x)` in de voorgestelde fix
is **defensief** maar kan in de praktijk nooit optreden.

---

## 10. Overzicht per benadering

De taakdocument stelt één specifieke oplossing voor, maar er zijn meerdere
benaderingen denkbaar:

### Benadering A: Fix in `nonSpecialSelectExpr` (de voorgestelde fix)

Verander de EDif(EDcV, x) case:
```haskell
-- Huidig
(EDif (EDcV _, x)) ->
  traceComment ["case: EDif (EDcV _,x)"]
    $ selectExpr fSpec (notCpl x)

-- Verbeterd
(EDif (EDcV sgn, x)) ->
  traceComment ["case: EDif (EDcV _,x)"] $
    case x of
      ECpl _ -> selectExpr fSpec (notCpl x)         -- bestaande aanpak (efficiënt)
      _      -> fromMaybe
                  (selectExpr fSpec (notCpl x))       -- fallback (defensief)
                  (maybeSpecialCase fSpec (EIsc (EDcV sgn, ECpl x))) -- LEFT JOIN
```

**Voordelen:** Minimale wijziging, gedragsbehouds voor ECpl-gevallen.  
**Nadelen:** Werkt alleen als `EDif(V, x)` het patroon is; genereert geen LEFT JOIN voor nested gevallen direct.

### Benadering B: Fix in `maybeSpecialCase` — voeg `EDif(EDcV, x)` toe

Voeg een nieuw patroon toe aan `maybeSpecialCase`:
```haskell
EDif (EDcV sgn, x) | not (isCpl x) ->
  go False (EDcV sgn) x
```

**Voordelen:** Past binnen de bestaande structuur van `maybeSpecialCase`, overschrijft automatisch.  
**Nadelen:** `EDif(V, ECpl e)` valt buiten `not (isCpl x)` maar zou `EIsc(V, ECpl(ECpl e)) = EIsc(V, e)` genereren — dit is ook correct maar anders dan de huidige aanpak.

### Benadering C: Fix in `ECpl` general case

De onderliggende oorzaak is de ECpl `_ ->` branch. Men zou daar
`EIsc(V, ECpl(e))` via LEFT JOIN herkennen als het canonieke patroon.
Maar dat verandert de `ECpl` case fundamenteel en raakt meer code.

**Voordelen:** Aanpak bij de bron.  
**Nadelen:** Riskanter, raakt meer gevallen.

---

## 11. Conclusies

1. **Het probleem is reëel en meetbaar:** `EDif(EDcV, EUni(l,r))` produceert O(n²×m) vs. O(n²+m) met LEFT JOIN.

2. **De oorzaak is precies geïdentificeerd:** De `EDif(EDcV, x)` case roept `selectExpr(notCpl x)` aan, wat voor positieve x leidt tot de ECpl `_ ->` branch met V×V als outer tabel en gecorreleerde NOT EXISTS.

3. **De semantische equivalentie is bewezen:** `EDif(V, x) = EIsc(V, ECpl(x))`, en `maybeSpecialCase EIsc(V, ECpl(x))` genereert al een correcte LEFT JOIN.

4. **Randgeval ECpl is probleemloos afgehandeld:** `notCpl(ECpl e) = e`, dus voor x = ECpl e is de bestaande aanpak al efficiënt en mag niet worden veranderd.

5. **De 3-term cascade werkt recursief:** `EDif(EDif(V, EUni(T,S)), I[R])` profiteert indirect van de fix via recursie in `selectExpr`.

6. **`go` geeft altijd `Just`:** De fallback in de fix is defensief maar overbodig.

7. **Bijzonder voordeel voor x = EDcD rel:** De fix genereert een directe tabel-JOIN (geen subquery), wat nog sneller is dan de huidige NOT EXISTS aanpak.
