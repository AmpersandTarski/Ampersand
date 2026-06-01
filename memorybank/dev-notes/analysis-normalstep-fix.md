# Probleemanalyse: normStep en all-negative conjuncten

## Samenvatting

Het probleem zit in `normStep` in `NormalForms.hs`. De "Avoid complements"-regel
(`x /\ -y = x - y`) vereist minstens één **positieve** term in een conjunctie om te
kunnen vuren. Wanneer **alle** termen negatief zijn (`-r /\ -s /\ -t`), blijft de
expressie hangen als `EIsc(ECpl r, EIsc(ECpl s, ECpl t))`. Dit leidt in de SQL-backend
tot meerdere aparte cartesiaanse producten — één per ECpl-term — terwijl de wiskundig
equivalente vorm `EDif(V, EUni(r, EUni(s, t)))` tot slechts één cartesiaans product leidt.

---

## Bewijsvoering uit ghci

### Bewijs 1: De all-negative case is een **fixpunt**

```
DeMorgan(e1) tree:  EIsc(ECpl(I[R]), EIsc(ECpl(I[S]), ECpl(I[T])))
conjNF(DeMorgan(e1)) tree: EIsc(ECpl(I[R]), EIsc(ECpl(I[S]), ECpl(I[T])))
Is fixpunt?: True
cfProof DeMorgan(e1):
  <=>   -I[R]/\ -I[S]/\ -I[T]
```

Na De Morgan geeft `conjNF` de expressie **ongewijzigd terug**. Er is maar één stap
(de startexpressie zelf) — geen enkele rewrite-regel vurt.

### Bewijs 2: N-term all-negative — confirmeert het patroon voor N=2 en N=4

```
cfProof -(r\/s):
  <=>  -I[R]\/I[S]   -- [De Morgan]
  <=>  -I[R]/\ -I[S]
conjNF tree: EIsc(ECpl(I[R]), ECpl(I[S]))

cfProof -(r\/s\/t\/u):
  <=>  -I[R]\/I[S]\/I[T]\/I[U]   -- [De Morgan]
  <=>  -I[R]/\ -I[S]/\ -I[T]/\ -I[U]
conjNF tree: EIsc(ECpl(I[R]), EIsc(ECpl(I[S]), EIsc(ECpl(I[T]), ECpl(I[U]))))
```

Voor **elk** N blijft de all-negative conjunctie als een geketende `EIsc(ECpl ...)`.
De rij groeit **lineair** met het aantal termen.

### Bewijs 3: isNeg-classificatie bevestigt de root cause

```
deMorganEUni(e1) = -I[R]/\ -I[S]/\ -I[T]
  -I[R]  isCpl=True isNeg=True
  -I[S]  isCpl=True isNeg=True
  -I[T]  isCpl=True isNeg=True

deMorganEUni(e2) = -I[R]/\ -I[S]/\I[T]
  -I[R]  isCpl=True isNeg=True
  -I[S]  isCpl=True isNeg=True
  I[T]   isCpl=False isNeg=False
```

De "Avoid complements"-regel (regel 15 in de EIsc-guard-keten) controleert:
```haskell
(negList, posList) = NE.partition isNeg (exprIsc2list l <> exprIsc2list r)
-- Avoid complements: x/\\-y = x-y
| (not . null) negList && (not . null) posList = ...
```

Voor `e1`: `posList = []` → conditie `(not . null) posList = False` → **regel vurt niet**.  
Voor `e2`: `posList = [I[T]]` → conditie True → Avoid-complements **vurt wel**.

### Bewijs 4: e2, e3, e4 produceren EDif-kettingen (één positieve term volstaat)

```
e1 (all-neg): EIsc(ECpl(I[R]), EIsc(ECpl(I[S]), ECpl(I[T])))   ← stuck
e2 (1 pos):   EDif(EDif(I[T], I[S]), I[R])                       ← efficiënt
e3 (1 pos):   EDif(EDif(I[R], I[S]), I[T])                       ← efficiënt
e4 (1 pos):   EDif(EDif(I[S], I[T]), I[R])                       ← efficiënt
```

De **inconsistentie** is glashelder: de functie `normStep` gedraagt zich structureel
anders op basis van het al dan niet aanwezig zijn van één positieve term.

### Bewijs 5: EDif(V, x) is een **stabiele normaalvorm**

```
EDif(V[R*R], I[R]) cfProof:
  <=>  V [R*R]-I[R]
conjNF tree: EDif(V [R*R], I[R])
conjNF == input?: True

EDif(V, I[R]\/I[R]\/I[R]) cfProof:
  <=>  V [R*R]-I[R]\/I[R]\/I[R]
conjNF tree: EDif(V [R*R], EUni(I[R], EUni(I[R], I[R])))
```

`normStep` heeft **geen enkel rewrite-patroon** voor EDif op het top-niveau:
- geen expansie naar ECpl
- geen destructurering naar EIsc
- de EUni in de tweede operand wordt ook niet vereenvoudigd (normStep recurseert
  niet in de deelexpressies van EDif)

**Gevolg:** Als de fix `EDif(V, EUni(r, EUni(s, t)))` produceert, blijft dit
stabiel staan. `conjNF` zal het **niet ongedaan maken**.

### Bewijs 6: Grensgevallen — bestaande regels remmen niet

```
-r/\-r tree: EIsc(ECpl(I[R]), ECpl(I[R]))
cfProof:
  <=>  -I[R]/\ -I[R]   -- [ -I[R] /\  -I[R] =  -I[R]]
  <=>  -I[R]
```

Als de twee ECpl-termen **dezelfde** expressie zijn, vurt de "Absorb equals"-regel
**vóór** onze nieuwe regel kan vuren. Correct — `EIsc(ECpl r, ECpl r) = ECpl r`,
niet `EDif(V, EUni(r,r))`.

```
r/\-r tree: EIsc(I[R], ECpl(I[R]))
cfProof:
  <=>  I[R]/\ -I[R]   -- [I[R] /\  -I[R] = V-]
  <=>  -V [R*R]
```

De "Inconsistency"-regel (`r /\ -r = -V`) vurt ook **vóór** onze nieuwe regel.

---

## Analyse van de SQL-impact

De SQL-backend in `SQL.hs` handelt `ECpl` als volgt af:

```haskell
(ECpl e) -> case e of
    EDcV _ -> emptySet        -- -V = ∅ (special case)
    EDcI c -> BinSelect ...   -- -I[C] (special case: non-cartesian)
    _ ->
      -- CARTESIAN PRODUCT!
      traceComment ["case: ECpl e"] BinSelect ...
      -- "cartesian product of " <> tshow (source e)
```

Voor `ECpl(r)` en `ECpl(s)` en `ECpl(t)` waar r,s,t **niet** `EDcV` of `EDcI` zijn:
elk genereert een **apart cartesiaans product** over het hele universum van begrippen.

`EIsc(ECpl r, EIsc(ECpl s, ECpl t))` genereert via de EIsc-handler:
- `EIsc(ECpl expr1, expr2)` → `go False expr2 expr1`
- De positieve kant `expr2 = EIsc(ECpl s, ECpl t)` vereist zelf weer recursief
  de ECpl-generatoren → **N aparte cartesiaanse producten**

`EDif(EDcV _, x)` heeft een speciale handler:
```haskell
(EDif (EDcV _, x)) ->
    traceComment ["case: EDif (EDcV _,x)"]
      $ selectExpr fSpec (notCpl x)
```

Dit evalueert direct `notCpl(EUni(r,s,t)) = ECpl(EUni(r,s,t))` → één cartesiaans
product, waarvan de UNION van r,s,t wordt afgetrokken. In plaats van N aparte
cartesiaanse producten die worden samengevoegd, is dit **één** cartesiaans product
met een gecombineerde filter:

| Vorm | Cartesiaanse producten | SQL-complexiteit |
|------|----------------------|-----------------|
| `EIsc(ECpl r, EIsc(ECpl s, ECpl t))` | N (één per term) | O(N × n²) |
| `EDif(V, EUni(r, EUni(s, t)))` | 1 (voor de hele union) | O(n²) |
| `EDif(EDif(t, s), r)` (1 pos term) | 0 (LEFT JOIN chain) | O(n) |

Waarbij n het aantal atomen van het betrokken concept is.

---

## Wiskundige correctheid van de fix

De gelijkheid die we willen vastleggen:

```
-r /\ -s /\ -t
= -(r \/ s \/ t)        [De Morgan, achteruit]
= V - (r \/ s \/ t)     [-x = V - x in relatie-algebra]
= EDif(EDcV(sign), EUni(r, EUni(s, t)))
```

Dit is **exact equivallent** — geen insluiting, maar volle gelijkheid (<=>).

---

## Exacte locatie van de fix in normStep

De `nM posCpl x@(EIsc (l, r)) rs` handler in `normStep` heeft een reeks guards.
De relevante sectie (vereenvoudigd):

```
Guard  1: Absorb equals      (r/\r = r)         — vurt bij duplicaten
Guard  2: isTrue l           (V/\x = x)
Guard  3: isTrue r           (x/\V = x)
Guard  4: Inconsistency      (r/\-r = -V)       — vurt bij tegenstrijdigheid
Guard  5: isFalse l          (-V/\x = -V)
Guard  6: isFalse r          (x/\-V = -V)
Guard  7: sub-expr changed   (recursieve stap)
Guard  8: AbsorbAsy          (niet-eq)
Guard  9: AbsorbAsyRfx
Guard 10: Absorb (x\/y)/\y = y
Guard 11: Absorb (x\/y)/\x = x
Guard 12: Absorb (x\/-y)/\y
Guard 13: Absorb (x\\/-y)/\x
Guard 14: Avoid complements  (x/\-y = x-y)      ← vereist posList niet-leeg
Guard 15: otherwise                               ← all-negative valt hier
```

De nieuwe regel gaat tussen 14 en 15:

```haskell
-- All-negative: -x/\-y/\.../\-z = V - (x\/y\/z)
| null posList && not (null negList) =
    ( EDif (EDcV (sign x), foldr1 (.\/.) (map notCpl negList)),
      ["All-negative: -x/\\-y = V-(x\\/y)"],
      "<=>"
    )
```

waarbij `negList` = alle ECpl-subexpressies uit de conjunctie,
en `map notCpl negList` de binnenste termen (met de ECpl eraf) geeft.

---

## Benaderd vanuit meerdere hoeken

### Hoek 1: De rewrite-regel zelf
De bestaande regel `x /\ -y = x - y` heeft een "blinde vlek" voor het geval `x = -z`.
Dat geeft `-z /\ -y = -z - y`, wat op zichzelf nog no een ECpl-term als linkeroperand
heeft. De regel propageert maar convergeert niet naar een complement-vrije vorm voor
het all-negative geval.

### Hoek 2: Symmetrie met De Morgan
`conjNF` past De Morgan toe: `ECpl(EUni r s t)` → `EIsc(ECpl r, EIsc(ECpl s, ECpl t))`.
De omgekeerde stap (De Morgan, terugwaarts) bestaat ook als rewrite-regel in
`tceDerivRules`:
```
"-r[A*B]/\\-s[A*B] = -(r[A*B]\\/s[A*B])"   --  De Morgan
```
Deze zou theoretisch de all-negative EIsc kunnen terugbrengen naar `ECpl(EUni(...))`.
Maar dat maakt de situatie erger, niet beter — we eindigen weer bij het ECpl-probleem.

De juiste aanpak is niet "terugstappen naar ECpl" maar "doorstappen naar EDif(V,...)".

### Hoek 3: De EDif-vorm is stabiel maar niet "blind"
`EDif(V, EUni(r,s,t))` wordt door `normStep` niet vereenvoudigd — noch expanded
(naar EIsc+ECpl) noch intern vereenvoudigd (de EUni in de rechteroperand blijft
