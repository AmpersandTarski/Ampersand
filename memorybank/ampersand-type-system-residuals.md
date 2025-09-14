# Ampersand Type System - Residuals en Concept Hiërarchie

## Key Insight van testTypes.adl analyse

**Probleem dat ik verkeerd begreep:**
Ik dacht dat het probleem was dat concepten ontbraken uit de conceptGraph, maar dat was fout.

**De echte situatie:**
1. In AdjacencyMap (Algebra.Graph) staan alle nodes die in edges voorkomen automatisch ook als vertices
2. Het `makeGraph` gedrag is volledig correct
3. Het type systeem werkt zoals bedoeld

## Hoe Left Residual Type Checking werkt

Voor `enrolled/requires` waar:
- `enrolled[Student*Course]` (target = Course)  
- `requires[Course*Subject]` (source = Course)

**Zonder CLASSIFY Course ISA Subject:**
- Er is geen relatie tussen Course en Subject in de concept hiërarchie
- De join van Course (target van enrolled) en Course (source van requires) bestaat wel
- Maar er is geen relatie die Subject verbindt met de rest
- Result: Type error - kan geen consistente signature afleiden

**Met CLASSIFY Course ISA Subject:**
- Er wordt een edge toegevoegd: (Course, Subject) in de concept hiërarchie  
- Nu bestaat er een join tussen Course en Subject
- De residual `enrolled/requires` krijgt signature `[Student*Subject]`
- Result: Type correct

## Belangrijke Lessen

1. **CLASSIFY statements zijn essentieel voor type checking** - ze definiëren de concept hiërarchie die het type systeem gebruikt voor join/meet operaties

2. **Het type systeem werkt hiërarchisch** - zonder expliciete CLASSIFY relaties kunnen concepten niet met elkaar gerelateerd worden

3. **De conceptGraph bevat alle informatie** - als concepten niet via edges verbonden zijn, kunnen ze niet in expressies gecombineerd worden

4. **Left residuals vereisen compatibele concepten** - de target van de linker expressie moet een join hebben met de source van de rechter expressie

## Test Output Analyse

```
conceptsGraph: overlay (vertices [X,Y,Z,ONE]) (edges [(AdvancedCourse,Course),(Course,Subject),(Student,Person),(Teacher,Person)])
```

Dit laat zien dat:
- Course → Subject edge bestaat (door CLASSIFY Course ISA Subject)  
- Student → Person edge bestaat
- Teacher → Person edge bestaat  
- AdvancedCourse → Course edge bestaat

Deze edges maken de benodigde joins mogelijk voor type checking.

## Conclusie

Het systeem werkt correct. De CLASSIFY statements zijn niet optioneel voor complexe expressies - ze definiëren de type hiërarchie die bepaalt welke expressies type-correct zijn.
