# Design considerations

The design considerations of Ampersand are treated as principles, not as laws. In case of conflicting design considerations, choices have been made.

## Constraints

An Ampersand context presents itself as a universe with constraints. This was chosen to allow for incremental development. Adding a constraint changes nothing to the semantics of other constraints.

One example of this is found in the CRUD annotations: A service provides all possible CRUD-functions, which are constrained by CRUD annotations.

## No obligations

The Ampersand engineer gets as much freedom as possible, to facilitate incremental development. Every obligation imposed on the Ampersand-engineer is a necessity.

One example is the freedom to define `PURPOSE` statements. Although the Ampersand team feels strongly about the need to specify a purpose for almost everything, the choice is left to the user. Since there is no hard technical reason for having them, the compiler does not impose its use.

As a consequence, the Ampersand engineer can choose the order in which she adds statements. This is consistent with the order-free semantics of Ampersand.

## Reliable semantics

The semantics of a script is well-defined and self-contained, to ensure predictable behaviour of information systems defined in Ampersand.

The prime example is the relation algebraic semantics of terms, which is defined unambiguously in a well-studied formalism that is over a century of age.

## Automated design process

Tools must be reliable and may not distract their users from their real work: designing information systems. For this reason we have paid much attention to making reliable tools and to automate the process of designing and building.

An example is the generation of software by the Ampersand compiler, which makes Ampersand more than a design tool. But also the deployment automation with Docker exemplifies this principle.

## Working systems over comprehensive documentation

Every Ampersand script, that passes the compiler without errors yields a working system. This makes it easy to focus on working systems, because partial implementations can be demonstrated and tried. (prototyping).

This principle makes it easy to neglect documentation.
