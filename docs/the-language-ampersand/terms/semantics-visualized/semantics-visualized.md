# Semantics of residuals visualized

Consider two relations: `traveler[Trip*Person]` and `dest[Trip*Destination]`. The first relation tells which persons have traveled on which trip. The diagram shows this as red dashed lines. The second relation links trips to destinations. It is depicted by dotted blue lines in the diagram.

![Venn-diagram for 'traveler' and 'dest'](<../../../.gitbook/assets/venntrips (1) (1).svg>)

From this diagram, we can tell which statements are true (i.e. facts). The statements are given both formally and in natural language. The elaborate version is a literate translation of the [semantics in logic](../semantics-in-logic/residual-operators.md). The ordinary version tells the same in a more human sounding manner.

| Formal statement                   | Elaborate natural language                                                 | Ordinary natural language                  |
| ---------------------------------- | -------------------------------------------------------------------------- | ------------------------------------------ |
| `"Peter" (traveler~/dest~) "Rome"` | For each trip, if it has destination Rome, then it has been made by Peter. | Every trip to Rome has been made by Peter. |

The following statements do _**NOT**_  follow from the population shown in the diagram:

| Formal statement                    | Elaborate natural language                                                | Ordinary natural language                                         |
| ----------------------------------- | ------------------------------------------------------------------------- | ----------------------------------------------------------------- |
| `"Peter" (traveler\dest) "Rome"`    | For each trip, if Peter has made the trip then its destination is Rome.   | Every trip that Peter made has Rome as destination.               |
| `"Peter" (traveler\dest) "Paris"`   | For each trip, if Peter has made the trip then its destination is Paris.  | <p>Every trip that Peter made has</p><p>Paris as destination.</p> |
| `"Peter" (traveler~/dest~) "Paris"` | For each trip, if Paris is the destination then Peter has made that trip. | Every trip to Paris has been made by Peter.                       |

## Other explanation

Would you like a different explanation of the residual operators? [This page](../semantics-in-natural-language/residual-operators.md) explains them in natural language. [Click here](../semantics-in-logic/residual-operators.md) for the explanation of residual operators in logic.
