## Current work focus
The current work focuses on refactoring the disambiguation algorithm.
In the new situation, a function term2Expr :: ContextInfo -> Term TermPrim -> Guarded Expression will replace the code for disambiguation and type checking of terms. A term is a Haskell object of type Term Termprim.
We are halfway that change. The function term2Expr has been written and sits in P2A_Converters.hs. We are busy to resolve the type  until it is error-free and warning-free.
## Recent changes
## Next steps
As soon as we have resolved errors from P2A_Converters.hs and made it free of warnings, we will do the same for the other files in ./Ampersand/src/, so the entire package will be error-free and warning-free.
## Active decisions and considerations
## Important patterns and preferences
## Learnings and project insights
