# Semantics of boolean operators visualized

Consider two relations: `authorized[Account*Person]` and `beneficiary[Account*Person]`. The first relation tells which persons are authorized to which accounts. The diagram shows this as red dashed lines. The second relation tells which persons stand to benefit from which accounts.. It is depicted by dotted blue lines in the diagram.

![](<../../../.gitbook/assets/Untitled Diagram (2).png>)

This diagram gives an example population of the relations `authorized[Account*Person]` and `beneficiary[Account*Person]`. Bob is authorized for account DE9382991 and Ann is authorized for account RS746620. Carl stands to benefit from account NL19RABO03992844 and Ann stands to benefit from account RS746620. Formally, we say:

| statements                              |
| --------------------------------------- |
| `"NL19RABO03992844" beneficiary "Carl"` |
| `"DE9382991" authorized "Bob"`          |
| `"RS746620" authorized "Ann"`           |
| `"RS746620" beneficiary "Ann"`          |

By combining the relations `authorized` and `beneficiary`, we can derive the following true statements.

| statement                                             |  natural language                                                          |
| ----------------------------------------------------- | -------------------------------------------------------------------------- |
| `"RS746620" (authorized/\beneficiary) "Ann"`          | Ann is authorized for and stands to benefit from for account RS746620.     |
| `"NL19RABO03992844" (authorized\/beneficiary) "Carl"` | Carl is authorized for or stands to benefit from account NL19RABO03992844. |
| `"RS746620" (authorized\/beneficiary) "Ann"`          | Ann is authorized for or stands to benefit from account RS746620.          |
| `"DE9382991" (authorized\/beneficiary) "Bob"`         | Bob is authorized for or stands to benefit from account DE9382991.         |



A different way to state the same is:

|                                                                                                                                                                                             |
| ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
|  `authorized/\beneficiary = {("RS746620", "Ann")}`                                                                                                                                          |
| <p><code>authorized\/beneficiary =</code> </p><p>  <code>{ ("NL19RABO03992844", "Carl")</code></p><p>  <code>, ("RS746620", "Ann")</code></p><p>  <code>, ("DE9382991", "Bob") }</code></p> |

## Other explanations

Would you like a different explanation of the boolean operators? [This page](../semantics-in-sets/boolean-operators-sets.md) explains them in set theory. [Click here](../semantics-in-algebra/boolean-operators-in-algebra.md) for the semantics of the boolean operators in algebra. [Here](../semantics-in-logic/boolean-operators.md) you get their definitions in logic.
