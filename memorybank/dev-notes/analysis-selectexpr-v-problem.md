# Probleemanalyse: V-introductie door "All-negative intersection" normalisatieregel

## Samenvatting

De gegenereerde SQL voor ENFORCE-regels met union in de LHS bevat onnodige cartesiaanse producten (`V[A*A]`). Dit komt **niet** door SQL.hs, maar door de normalisatieregel "All-negative intersection" in `NormalForms.hs` (lijn 1457-1465).

De `selectExpr`-fix in SQL.hs (LEFT JOIN voor `EDif(V,x)`) is **al toegepast** en werkt correct. Maar zelfs met LEFT JOIN blijft `V[A*A]` een cartesiaans product van O(n²) rijen.

## Het probleem stap voor stap

### Input: ENFORCE met union in LHS
```adl
ENFORCE parentStar >: I[A] \/ parentPlus[A*A]
```
Genereert regel: `(I \/ parentPlus) |- parentStar`

### Stap 1: conjNF normaliseert de regel (= rcConjunct)

Bewezen in ghci met `cfProof`:

```
<=> I[A] \/ I[A]~ |- I[A]*          -- [remove |-]
<=>  -(I[A] \/ I[A]~) \/ I[A]*      -- [De Morgan]
<=>  -I[A] /\ -I[A]~ \/ I[A]*       -- [All-negative intersection: -x/\-y = V-(x\/y)]
<=> V[A*A] - (I[A]~ \/ I[A]) \/ I[A]*
```

**Hier introduceert de "All-negative" regel de EERSTE V.**

rcConjunct = `EUni(EDif(V[A], EUni(I~, I)), I*)`

### Stap 2: violations = conjNF(notCpl(rcConjunct))

```
notCpl(rcConjunct) =  -(V[A*A] - (I~ \/ I) \/ I*)
```

Bewezen in ghci met `cfProof`:

```
<=>  -(V[A*A] - (I~ \/ I)) \/ I*    -- [De Morgan]
<=>  -(V[A*A] - (I~ \/ I)) /\ -I*   -- [All-negative intersection: -x/\-y = V-(x\/y)]
<=> V[A*A] - (I* \/ V[A*A] - (I~ \/ I))
```

**Hier introduceert de "All-negative" regel de TWEEDE V.**

### Resultaat
```
violations = EDif(V[A], EUni(I*, EDif(V[A], EUni(I~, I))))
           = V - (I* \/ (V - (I~ \/ I)))
```

**Twee V's = twee cartesiaanse producten = O(n⁴) worst case!**

### Wat het IDEALE resultaat zou zijn

Algebraïsch: violations van `(I \/ pp) |- ps` = `(I \/ pp) /\ -ps` = `(I \/ pp) - ps`

```
ideal = EDif(EUni(I, I~), I*)
      = (I \/ I~) - I*
```

Bewezen in ghci: `conjNF((I \/ I~) - I*) = (I \/ I~) - I*` — **onveranderd, V-vrij!**

| | Expressie | V's | Complexiteit |
|---|---|---|---|
| **Ideaal** | `(I \/ I~) - I*` | **0** | O(n) |
| **Actueel** | `V - (I* \/ (V - (I~ \/ I)))` | **2** | O(n²) of erger |

## Root cause: "All-negative intersection" regel

**Locatie**: `NormalForms.hs` lijn 1457-1465

```haskell
-- All-negative: -x/\-y/\.../\-z = V-(x\/y\/...\/z)
| null posList && not (null negList) && all isNeg rs =
    let negInners = map notCpl negList
     in ( EDif (EDcV (sign l), foldr1 (.\/.) ...),
          ["All-negative intersection: -x/\\-y = V-(x\\/y)"],
          "<=>"
        )
```

### Wanneer vuur deze regel?

De regel vuur wanneer een `EIsc` ALLEEN negatieve termen bevat (`-a /\ -b /\ ...`).

| Regelpatroon | Triggert? | Reden |
|---|---|---|
| `r |- s` | **Nee** | Na remove `\|-`: `-r \/ s` = één neg + één pos in EUni |
| `(r /\ s) |- t` | **Nee** | Na De Morgan: `-r \/ -s \/ t` = allemaal in EUni, geen EIsc |
| `(r \/ s) |- t` | **JA** | Na De Morgan: `-r /\ -s \/ t` → `-r /\ -s` is all-negative EIsc |
| `(r \/ s \/ t) |- u` | **JA** | Na De Morgan: `-r /\ -s /\ -t \/ u` → all-negative EIsc |

**Patroon**: elke regel met EUni in de linkerhelft van `|-` triggert de All-negative regel.

## Waarom de bestaande selectExpr-fix onvoldoende is

De fix in SQL.hs (lijn 1148-1159) maakt `EDif(V, x)` efficiënter door LEFT JOIN i.p.v. NOT EXISTS te gebruiken. Maar:

1. V[A*A] blijft een cartesiaans product van n² rijen
2. LEFT JOIN met n² rijen is nog steeds O(n²)
3. Bij geneste V's (violations-case) wordt het O(n²) per level

De fix is correct en nuttig, maar lost het kernprobleem niet op.

## Mogelijke oplossingsrichtingen

(Gedachten, nog niet uitgewerkt)

### Richting 1: Fix de normalizer
Verwijder of conditionaliseer de "All-negative" regel zodat V niet geïntroduceerd wordt. Risk: andere delen van het systeem verwachten mogelijk dit patroon.

### Richting 2: Bypass de normalizer voor violations
Bereken violations direct als `lhs - rhs` zonder `conjNF . notCpl . rcConjunct`. Vereist wijziging in Conjuncts.hs.

### Richting 3: Post-processing op de expressie
Na `conjNF . notCpl . rcConjunct`, herschrijf `V - (x \/ (V - (y \/ z)))` terug naar `(y \/ z) - x`. Hermkenbaar patroon dat algebraïsch correct terug te schrijven is.

### Richting 4: SQL-level optimalisatie
Detecteer in SQL.hs dat `V LEFT JOIN x` equivalent is aan "al wat niet in x zit" en genereer een NOT IN of EXCEPT query i.p.v. cartesiaans product.

## Bewijs (ghci-sessie)

```haskell
-- rcConjunct bevat V
> let cA = mkCpt 'A'; iA = EDcI cA
> showTree (conjNF () (EInc (EUni (iA, EFlp iA), EKl0 iA)))
"EUni(EDif(V[[A]], EUni(I[A]~, I[A])), I[A]*)"

-- Violations bevat TWEE V's
> showTree (conjNF () (notCpl (conjNF () (EInc (EUni (iA, EFlp iA), EKl0 iA))))))
"EDif(V[[A]], EUni(I[A]*, EDif(V[[A]], EUni(I[A]~, I[A]))))"

-- Ideale expressie is V-vrij en conjNF laat het met rust
> showTree (conjNF () (EDif (EUni (iA, EFlp iA), EKl0 iA)))
"EDif(EUni(I[A], I[A]~), I[A]*)"

-- cfProof toont de exacte stappen waar V ontstaat
> mapM_ printProofStep (cfProof (EInc (EUni (iA, EFlp iA), EKl0 iA)))
<=> I[A]\/I[A]~ |- I[A]*         -- [remove |-]
<=>  -I[A]\/I[A]~\/I[A]*         -- [De Morgan]
<=>  -I[A]/\ -I[A]~\/I[A]*       -- [All-negative intersection: -x/\-y = V-(x\/y)]
<=> V[A*A]-I[A]~\/I[A]\/I[A]*
```
