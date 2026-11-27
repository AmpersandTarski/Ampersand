# Design choices — role hierarchies and role terms

Register of design choices for issue [#1376](https://github.com/AmpersandTarski/Ampersand/issues/1376):
the statement `ROLE a, b ISA c` and the operators `\/`, `/\` and `-` on the roles after `FOR`.
Numbers are stable and never reused; the current state stands here, the history lives in git.

## Meaning

**The compiler computes the roles of every interface from its role terms, and the application checks a plain list of roles.**
*DC-1 · valid · 2026-11-27 · origin: issue #1376, decided by Stef Joosten*

A role in a role term stands for itself and every role that is transitively ISA it,
and `\/`, `/\` and `-` are the union, intersection and difference of those sets of roles.
The roles of an interface are the union of what its role terms denote.
`roles.json`, `openapi.json` and the metapopulation (`PrototypeContext.ifcRoles`) hold that list,
and the prototype framework admits a user to an interface when one of the user's active roles is on it.
A difference therefore excludes a role and not a person:
a user with both `User` and `TechnicalMaintainer` active uses an interface `FOR User - TechnicalMaintainer` through the role `User`.

*Considerations:*

1. The purpose is that a modeller writes the rights of related roles once, in the hierarchy, instead of in every `FOR` clause.
2. With a plain list per interface the framework needs no change, which it would otherwise need, as the comment of Michiel on the issue anticipated.
3. The alternative set aside is to pass the role term to the framework and evaluate it per session,
   so that a user with `TechnicalMaintainer` active is refused `FOR User - TechnicalMaintainer`.
   That treats a difference as an exclusion of persons, but it needs a new representation of interface rights in the framework backend and a framework issue to carry it.

*Impact on the specification:* the parse tree has `RoleTerm` after `FOR` and `P_RoleIsa` for the ISA statement;
an `Interface` keeps its role terms as written (`ifcRoleTerms`) next to the roles they denote (`ifcRoles`),
because the denoted roles need not be closed under ISA once a difference excludes a role, and a script printed back from the A-structure must mean the same.

*Impact in production:* none for the framework; a generated prototype carries the computed lists.

**A role that is ISA another role maintains every rule that role maintains.**
*DC-2 · valid · 2026-11-27 · origin: issue #1376, decided by Stef Joosten*

`maintainersOf` in `ADL2FSpec.hs` adds every role below a maintaining role, for every kind of rule,
so a sub-role sees the violations of the rules of its generic role.
FormalAmpersand states it as the rule `inheritMaintains`: `isaRole;maintains |- maintains`.

*Considerations:*

1. The issue says that all restrictions that apply to a role also apply to the roles that are ISA it; the rules a role maintains are among them.
2. The inheritance holds for every kind of rule, including the rules that the ExecEngine maintains, so that `inheritMaintains` holds without exception.
3. No such rule holds for interfaces: `FOR User - Maintainer` excludes a sub-role of `User` from an interface for `User` on purpose.

*Impact on the specification:* FormalAmpersand has `isaRole`, `isaRolePlus` (exactly its transitive closure, irreflexive) and `inheritMaintains`.

*Impact in production:* the `maintains` lists in `roles.json` contain the inherited rules.

**A cycle in the hierarchy and a role term that denotes no role are compile errors.**
*DC-3 · valid · 2026-11-27 · origin: issue #1376*

The compiler gives one error per cycle, naming the roles on it,
and one error for an interface whose non-empty `FOR` clause denotes no role.

*Considerations:*

1. An empty list of roles means that an interface is for every role (`isPublic` in FormalAmpersand),
   so a term such as `Maintainer - User` would otherwise open an interface to everybody.
2. A cycle makes every role on it ISA itself, which `isaRolePlus [IRF]` forbids.
3. The alternative set aside for the empty case is a warning with an interface that no role can use; the framework has no representation for that, because an empty list already means public.

*Impact on the specification:* the regression tests in `testing/roleHierarchy/fail` pin both errors (exit code 10).

*Impact in production:* none; a script with either error does not compile.
