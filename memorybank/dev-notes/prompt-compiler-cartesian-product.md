# Bug report: ENFORCE >: genereert onnodig Cartesiaans product in SQL

## Samenvatting

De Ampersand compiler (v5.5.6, build 19-May-26) genereert voor een `ENFORCE r >: expr` regel een SQL-query die het volledige Cartesiaans product `V[A*A]` bevat, terwijl dat wiskundig niet nodig is. Bij grote populaties (duizenden atomen) loopt deze query eindeloos vast.

## De regel

In het bestand `Organismen.adl` staat deze ENFORCE-regel voor de reflexieve transitieve afsluiting:

```adl
RELATION parent[EPPOcode*EPPOcode] [IRF,ASY,UNI]
RELATION parentPlus[EPPOcode*EPPOcode] [IRF,ASY]
RELATION parentStar[EPPOcode*EPPOcode] [ASY]

ROLE ExecEngine MAINTAINS berekenParentPlus
RULE berekenParentPlus : parent |- parentCopy
VIOLATION (TXT "{EX} TransitiveClosure;parent;EPPOcode;parentCopy;parentPlus")

ENFORCE parentStar >: I[EPPOcode] \/ parentPlus
```

De `>:` operator hoeft alleen de insertiekant te berekenen: paren die in `I[EPPOcode] \/ parentPlus` zitten maar nog niet in `parentStar`. De wiskundige expressie is:

```
(I[EPPOcode] \/ parentPlus) - parentStar
```

## Wat de compiler genereert

De compiler genereert conjunct `conj_0` met de volgende SQL (opgeschoond):

```sql
SELECT DISTINCT t1.src, t1.tgt
FROM (
    -- V[EPPOcode*EPPOcode]: Cartesiaans product (PROBLEEM!)
    SELECT fst."EPPOcode" AS src, snd."EPPOcode" AS tgt
    FROM "EPPOcode" AS fst, "EPPOcode" AS snd
    WHERE fst."EPPOcode" IS NOT NULL AND snd."EPPOcode" IS NOT NULL
) AS t1
LEFT JOIN (
    -- parentStar
    SELECT "SrcEPPOcode" AS src, "TgtEPPOcode" AS tgt FROM "parentStar"
    WHERE "SrcEPPOcode" IS NOT NULL AND "TgtEPPOcode" IS NOT NULL
    UNION
    -- V[EPPOcode*EPPOcode] - (parentPlus \/ I): ook Cartesiaans product!
    SELECT t1.src, t1.tgt
    FROM (
        SELECT fst."EPPOcode" AS src, snd."EPPOcode" AS tgt
        FROM "EPPOcode" AS fst, "EPPOcode" AS snd
        WHERE fst."EPPOcode" IS NOT NULL AND snd."EPPOcode" IS NOT NULL
    ) AS t1
    LEFT JOIN (
        SELECT "SrcEPPOcode" AS src, "TgtEPPOcode" AS tgt FROM "parentPlus"
        WHERE "SrcEPPOcode" IS NOT NULL AND "TgtEPPOcode" IS NOT NULL
        UNION
        SELECT "EPPOcode" AS src, "EPPOcode" AS tgt FROM "EPPOcode"
        WHERE "EPPOcode" IS NOT NULL
    ) AS t2
    ON t1.src = t2.src AND t1.tgt = t2.tgt
    WHERE t2.src IS NULL OR t2.tgt IS NULL
) AS t2
ON t1.src = t2.src AND t1.tgt = t2.tgt
WHERE t2.src IS NULL OR t2.tgt IS NULL
```

De compiler berekent:

```
V - (parentStar \/ (V - (parentPlus \/ I)))
```

Dit is wiskundig equivalent aan `(parentPlus \/ I) - parentStar`, maar de SQL bevat **tweemaal** het Cartesiaans product `V[EPPOcode*EPPOcode]`. Bij N EPPOcode-atomen is dat N² rijen per keer.

## Wat de compiler zou moeten genereren

De equivalente, maar efficiënte SQL is:

```sql
SELECT t1.src, t1.tgt
FROM (
    -- parentPlus \/ I[EPPOcode]
    SELECT "SrcEPPOcode" AS src, "TgtEPPOcode" AS tgt FROM "parentPlus"
    WHERE "SrcEPPOcode" IS NOT NULL AND "TgtEPPOcode" IS NOT NULL
    UNION
    SELECT "EPPOcode" AS src, "EPPOcode" AS tgt FROM "EPPOcode"
    WHERE "EPPOcode" IS NOT NULL
) AS t1
LEFT JOIN "parentStar" AS t2
    ON t1.src = t2."SrcEPPOcode" AND t1.tgt = t2."TgtEPPOcode"
WHERE t2."SrcEPPOcode" IS NULL
```

Geen Cartesiaans product, directe berekening van `expr - r`.

## Impact

In het NVWA-prototype bevat het EPPOcode-concept enkele duizenden atomen na het inladen van de default populatie en Producten.xlsx. Het Cartesiaans product levert miljoenen rijcombinaties op. De query draaide in ons geval meer dan 850 seconden voordat we hem handmatig moesten killen. Ondertussen houdt de query een transaction-lock vast die alle andere imports (Producten.xlsx, Organismen.xlsx, etc.) blokkeert met "Lock wait timeout exceeded".

## Analyse van de interne expressie

De SQL-comments in de gegenereerde query bevestigen het probleem:

```
/* EDif (EDcV [EPPOcode], EUni (EDcD parentPlus[EPPOcode*EPPOcode], EDcI EPPOcode)) */
/* Expression: V [EPPOcode*EPPOcode] - parentPlus[EPPOcode*EPPOcode] \/ I[EPPOcode] */
```

De compiler herschrijft `(I \/ parentPlus) - parentStar` kennelijk als `V - (parentStar \/ (V - (I \/ parentPlus)))` via de relatie-algebra-identiteit `A - B = V - (B \/ (V - A))`. Die identiteit klopt, maar leidt tot een inefficiënte SQL-vertaling omdat V als Cartesiaans product wordt geïmplementeerd.

Een directere vertaling zou `A LEFT JOIN B WHERE B IS NULL` opleveren, zonder V.

## Reproduceren op deze machine

De Ampersand repository staat lokaal op deze computer. Om het probleem te reproduceren:

```bash
# 1. Ga naar het NVWA project
cd "/Users/stef/Library/CloudStorage/GoogleDrive-stefjoosten1@gmail.com/Mijn Drive/cloudDrive/NVWA/FC/FC5"

# 2. Compileer met de huidige compiler en bekijk de gegenereerde conjuncts
bash generate_proto.sh

# 3. Inspecteer conj_0 in de output

# 4. Of minimaler: maak een test.adl met alleen het patroon
cat > /tmp/test.adl << 'EOF'
CONTEXT TestCartesian IN ENGLISH
RELATION parent[A*A] [IRF,ASY,UNI]
RELATION parentCopy[A*A]
RELATION parentPlus[A*A] [IRF,ASY]
RELATION parentStar[A*A] [ASY]
ROLE ExecEngine MAINTAINS calc
RULE calc : parent |- parentCopy
VIOLATION (TXT "{EX} TransitiveClosure;parent;A;parentCopy;parentPlus")
ENFORCE parentStar >: I[A] \/ parentPlus
ENDCONTEXT
EOF
```

Zoek in de gegenereerde conjuncts.json naar een SQL-query die `fst."A" as src, snd."A" as tgt` bevat. Dat is het Cartesiaans product `V[A*A]`. Het zou er niet in moeten zitten.

## Compiler versie

```
Ampersand-v5.5.6 [no git info], build time: 19-May-26 13:00:51 UTC
```

## Gerelateerde context

In hetzelfde project is eerder een vergelijkbaar probleem opgelost door `ENFORCE product[Uitspraak*Product] := expr` te veranderen naar `>:`. In dat geval loste `>:` het Cartesiaans product probleem op omdat de expressie `vorm;vorm~ /\ productnaam;voorkeursNaam~` twee verschillende concepten (Uitspraak en Product) betreft. Het complement in de `:=` verwijderingscontrole genereerde daar V[Uitspraak*Product].

Bij `ENFORCE parentStar >: I[EPPOcode] \/ parentPlus` treedt het probleem ook bij `>:` op, omdat de compiler de insertie-expressie `(I \/ parentPlus) - parentStar` vertaalt via een tussenstap die V[EPPOcode*EPPOcode] bevat.
