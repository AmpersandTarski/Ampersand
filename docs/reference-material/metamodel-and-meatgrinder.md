# The Metamodel and Meatgrinder

## The Metamodel

Ampersand uses two metamodels to describe the structure of Ampersand scripts. FormalAmpersand describes the complete Ampersand language itself. PrototypeContext describes the runtime components needed to run a prototype. The metamodels are written in Ampersand itself and reside in `AmpersandData/FormalAmpersand/` and `AmpersandData/PrototypeContext/`.

## The Meatgrinder

The meatgrinder transforms a compiled Ampersand specification into population data for the metamodel. This process breaks down the specification into the binary relations that constitute the metamodel. The meatgrinder is part of the Ampersand compiler, written in Haskell in `src/Ampersand/FSpec/Transformers.hs`.

## How Grinding Works

The compilation process follows four steps. First, the parser reads the Ampersand script and produces a P_Context. Second, the type checker validates the P_Context and produces an FSpec. Third, the meatgrinder extracts data from the FSpec and produces population data as a P_Context. Fourth, the grinded population merges back into the original context.

When you write `RELATION manages[Person*Project]`, the meatgrinder extracts this declaration and creates population data. It populates `name[Relation*RelationName]` with the pair `("manages", "manages")`. It populates `source[Relation*Concept]` with `("manages", "Person")`. It populates `target[Relation*Concept]` with `("manages", "Project")`. This population data goes into the metamodel database, allowing tools like RAP to analyze and display your specification.

## Build Recipes

The Ampersand compiler uses different recipes depending on the use case.

The Standard recipe compiles the script without loading any metamodel. This recipe is used for the `check` command, quick validation of syntax and types, and generating documentation or diagrams.

The Grind recipe compiles the script, type-checks it, then grinds it using the FormalAmpersand transformers. This recipe is used for analyzing scripts in detail and populating the FormalAmpersand metamodel.

The Prototype recipe loads the PrototypeContext metamodel, grinds the combined script, and merges the result. This recipe is used for the `proto` command, the `validate` command, and generating working prototypes. The PrototypeContext provides the runtime infrastructure for prototypes, including interface definitions, navigation menu items, role management, and session handling.

The RAP recipe combines FormalAmpersand, PrototypeContext, and the user script, then grinds everything together. This recipe is used for the RAP application and running scripts in a shared repository environment.

## The Metamodel-Transformer Correspondence

Each relation declared in the metamodel ADL files must have a corresponding transformer in the Haskell code. The transformers generate the population data for these relations. For example, `FormalAmpersand/Relations.adl` declares `RELATION name[Relation*RelationName]` and `Transformers.hs` contains code that populates this relation from the FSpec.

The compiler checks this correspondence when building prototypes. If a relation exists in the ADL but has no transformer, or if a transformer exists without a corresponding relation, the build fails with an error message.

## Why This Matters

The metamodel population enables RAP to store and analyze user scripts in a database. It enables the Atlas to display concepts, relations, and rules in an interface. It enables documentation generators to extract structured information from scripts. It enables prototype frameworks to configure interfaces and navigation. Without grinding, these tools cannot access the structure of your Ampersand script.

## Common Issues

When metamodel files contain syntax errors or ambiguous references, the compiler cannot load the metamodel. This prevents the grinding process from running. The error message "The relations defined in prototypecontext.adl are not in sync with the transformers" appears when the metamodel files fail to parse, even though the actual cause is a parser error rather than a sync problem.

If you add a relation to the metamodel ADL files but forget to add the corresponding transformer in Haskell, the build fails. The compiler reports which relations are missing transformers. If you add a transformer in Haskell but forget to declare the relation in the ADL files, the build fails. The compiler reports which transformers have no matching relation.

## For Contributors

When modifying the metamodel, add the relation declaration in the appropriate ADL file in `AmpersandData/`. Add the transformer in `src/Ampersand/FSpec/Transformers.hs`. Compile with the appropriate recipe to verify the sync. Test the change by running `stack exec ampersand -- validate <test-file>`. The sync check runs automatically during builds that use the Prototype or RAP recipes.
