# Semantics of relational operators visualized

Consider two relations: `traveler[Trip*Person]` and `dest[Trip*Destination]`. The first relation tells which persons have traveled on which trip. The diagram shows this as red dashed lines. The second relation links trips to destinations. It is depicted by dotted blue lines in the diagram.

![Venn-diagram for 'traveler' and 'dest'](<../../../.gitbook/assets/venntrips (1) (1).svg>)

Each pair (fact) in the diagram can be written as a fact in two ways, using the converse operator:&#x20;

| Fact                          | Fact                         |
| ----------------------------- | ---------------------------- |
| `"Peter" traveler~ "LBD-199"` | `"LBD-199" traveler "Peter"` |
| `"Peter" traveler~ "TSS-730"` | `"TSS-730" traveler "Peter"` |
| `"TSS-730" dest "Rome"`       | `"Rome" dest~ "TSS-730"`     |
| `"TSS-730" dest "Paris"`      | `"Paris" dest~ "TSS-730"`    |
| `"QRA-492" dest "Paris"`      | `"Paris" dest~ "QRA-492"`    |

From the diagram, we assume that each pair represents a true statement (i.e. a fact). The statements are given both formally and in natural language. The elaborate version is a literate translation of the [semantics in logic](../semantics-in-logic/residual-operators.md). The ordinary version tells the same in a more human sounding manner.

| Formal statement                   | Elaborate natural language                                           | Ordinary natural language      |
| ---------------------------------- | -------------------------------------------------------------------- | ------------------------------ |
| `"Peter" (traveler~;dest) "Rome"`  | There is a trip that Peter has made, which has Rome as destination.  | Peter has made a trip to Rome. |
| `"Peter" (traveler~;dest) "Paris"` | There is a trip that Peter has made, which has Paris as destination. | Peter has made a trip to Paris |
