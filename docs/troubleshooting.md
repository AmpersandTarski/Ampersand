# Troubleshooting

Whenever you run into problems with Ampersand, and you cannot find what you are looking for in this documentation, have a look at the common problems below. When your problem isn't there, see if you can find an issue for it. Otherwise, do one of:

  1. If your problem is with this documentation (you miss some explanation), [file an issue that is specific for this documentation](http://github.com/AmpersandTarski/Ampersand/issues).
  2. If your problem is with the ampersand method or tooling, you can [file an issue that is specific for Ampersand itself](http://github.com/AmpersandTarski/ampersand/issues).

## Common Problems
<!---
This is meant to become a list that helps users with frequently asked questions
-->
 1. **`Maximum reruns exceeded` while running a prototype.** The ExecEngine is caught in an
    *oscillation*: your invariants are, given the data, jointly unsatisfiable, so the automatic
    repair never reaches a fixpoint. Do **not** raise the rerun limit — that hides the
    contradiction. The worked case [Diagnosing oscillations](guides/oscillations/README.md)
    shows how to trace it back to the colliding rules and fix it properly.
 1. The documentation is far from complete. We are working on it though. If you are stuck, please create an issue! You are more than welcome!

## Understanding "The relations defined in prototypecontext.adl are not in sync"

The compiler reports this error when running commands that use the Prototype recipe, such as `validate` or `proto`. Despite the message text, the cause is usually not a sync problem between relations and transformers. The PrototypeContext metamodel files failed to parse or typecheck, which prevents the compiler from loading the metamodel.

Check the error output above this message. The compiler shows parser or type checker errors such as "Ambiguous term might be one of: label, label, label" or "Interface is incompatible" or "The TYPE of the concept must be OBJECT". Fix these parser and type errors to resolve the issue. For ambiguous references, add explicit signatures to relation uses. Report the issue if you cannot fix it yourself. See [Metamodel and Meatgrinder](./reference-material/metamodel-and-meatgrinder.md) for background.

## Understanding "Ambiguous term might be one of"

The compiler reports this error when a relation name is used without a signature, and multiple relations with that name exist. For example, if your script declares `label[Interface*Label]`, `label[Role*Label]`, and `label[NavMenuItem*Label]`, then using just `label` without specifying which one creates ambiguity. The compiler cannot determine which relation you mean.

Add the signature explicitly to fix this. Write `label[Interface*Label]` instead of just `label`. See [Syntax of Ampersand](./reference-material/syntax-of-ampersand.md) for details on relation syntax.

## Understanding "Violations in database"

The compiler reports this error when the initial population violates invariant rules. Your `POPULATION` statements create data that breaks the rules defined in your script. Check which rule the error message identifies as violated. Either fix the population data or adjust the rule. During development, use the `--allow-invariant-violations` flag to bypass this check. See [Truth](./reference-material/truth.md) for background on rules and violations.
