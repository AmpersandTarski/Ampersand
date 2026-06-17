---
description: >-
  How the Ampersand compiler turns an FSpec into an OpenAPI 3.0 description of the
  prototype REST API, why the mapping is the way it is, and how to extend the
  generator in OpenAPI.hs.
---

# Generating an OpenAPI description

The compiler writes a file `generics/openapi.json` when it generates a backend for a
development build. That file is an [OpenAPI](https://www.openapis.org/) 3.0 description
of the REST API that the prototype framework serves for your context. For a production
build (`--production`) the file is omitted by default, so the prototype publishes nothing
(see [The `--[no-]openapi` flag](#the---no-openapi-flag) below).

## What OpenAPI is, and why we generate it

A running Ampersand prototype is a web application with a REST backend. The frontend
talks to that backend over HTTP: it reads a resource with `GET`, changes it with
`PATCH`, creates one with `POST`, and removes one with `DELETE`.

OpenAPI is an industry-standard, machine-readable way to write down such an HTTP API.
A single JSON (or YAML) document lists every path the server exposes, every operation
on each path, the shape of every request and response, and the data types involved.
Tools read that document to generate client code, to render interactive documentation
(Swagger UI, Redoc), to run contract tests, and to validate requests at a gateway.

The Ampersand compiler generates this JSON document to keep the API description always in step with
the model. When you change an `INTERFACE`, the description follows automatically.

## Where the generator lives

| Concern | Location |
| --- | --- |
| The generator itself | [`src/Ampersand/Output/ToJSON/OpenAPI.hs`](https://github.com/AmpersandTarski/Ampersand/blob/main/src/Ampersand/Output/ToJSON/OpenAPI.hs) |
| Where it is called | `doGenBackend` in [`src/Ampersand/Prototype/GenBackend.hs`](https://github.com/AmpersandTarski/Ampersand/blob/main/src/Ampersand/Prototype/GenBackend.hs) |
| The on/off switch | `--[no-]openapi`; default follows the build target `--[no-]production` (see below) |

The single entry point is

```haskell
openAPIToJSON :: env -> FSpec -> BL.ByteString
```

It takes the FSpec (the compiler's fully elaborated model) and returns the pretty-printed
JSON document as a lazy bytestring. `doGenBackend` writes that bytestring next to the other
`generics/*.json` files.

### The `--[no-]production` and `--[no-]openapi` flags

The OpenAPI document describes the backend, so it is part of the backend output. Whether
it is written depends on the **build target**:

- A **development build** (the default) writes `openapi.json`, so the prototype publishes
  its API description and Swagger UI.
- A **production build** (`--production`) does not write it, so a deployment aimed at end
  users exposes no API description by default.

An explicit `--openapi` / `--no-openapi` always overrides that default, in either direction:

```sh
ampersand proto model.adl                       # development: openapi.json written
ampersand proto model.adl --production           # production: no openapi.json
ampersand proto model.adl --production --openapi  # production build, but force the spec
ampersand proto model.adl --no-openapi           # development build, but suppress the spec
```

Because the document only makes sense when there is a backend to describe, it is written
inside `doGenBackend`. Running `proto --no-backend` therefore produces no `openapi.json`
either, which is the intended behaviour.

#### How the compiler and the framework stay consistent

The `--production` flag is passed on to the prototype framework: the compiler writes
`global.productionEnv` into `generics/settings.json`. The framework uses that setting both
to hide developer interfaces and to decide whether to publish the spec. The framework only
serves `GET /api/v1/openapi.json` and the Swagger UI at `GET /api/v1/docs` when the build is
not a production build *and* `openapi.json` is present. So a single compiler flag drives both
sides: generation (here) and publication (in the framework).

The flags follow the same pattern as `--[no-]backend` and `--[no-]frontend`. The places to
touch are: the `xproduction` / `xgenerateOpenAPI` fields in `Ampersand.Misc.HasClasses`, the
parsers `productionP` and `generateOpenAPIP` in `Ampersand.Options.ProtoOptsParser` (where
the openapi default is derived from `--production`), the `ExtendedRunner` instance in
`Ampersand.Types.Config`, and the `global.productionEnv` field in
`Ampersand.Output.ToJSON.Settings` that carries the flag to the framework.

## How the document is built

The OpenAPI generator builds the `aeson` `Value` directly with `object` and `(.=)`.
The generator
assembles the tree 'by hand' because field names such as `$ref`,
`200`, `application/json`, `x-crud` are not valid Haskell field names.
That makes using `aeson` and its `amp2Jason` machinery unneccessarily complicated.
Read `openApiDoc` top to bottom and you are reading the
structure of the output document: `openapi`, `info`, `servers`, `tags`, `paths`,
`components`.

One import subtlety is worth knowing in advance: `Object` is both a `TType` constructor
(an Ampersand concept stored as an object) and an `aeson` `Value` constructor. The module
imports `Data.Aeson (Value, object, (.=))` without the constructors, so `Object` always
means the Ampersand one.

The generator stays on `aeson` plus `aeson-pretty`, which the compiler already depends on
and which the other `ToJSON` generators use. A dedicated library such as `openapi3` would
give type-safe OpenAPI records, but it adds a dependency for little gain here, since the generator generates type-safe results anyway. 

## The mapping, and why it is what it is

This is the part to understand before changing anything. Every decision below is a
deliberate choice about how an Ampersand concept maps onto an HTTP concept.

### Which interfaces become paths

Every `INTERFACE` in your script becomes one path with a `GET`. The interfaces whose name
starts with `PrototypeContext.` are excluded, because the framework injects those itself;
they are not part of the API your model defines.

### The root concept decides the URL shape

The source concept of an interface's top-level term decides how you address it:

- a `SESSION`-rooted interface lives at `/resource/SESSION/1/{name}` (there is exactly one
  session resource);
- a `ONE`-rooted interface lives at `/resource/ONE/1/{name}` (the singleton);
- any other interface lives at `/resource/{root}/{resourceId}/{name}`, where `resourceId`
  is the atom you are addressing.

### CRUD rights drive both visibility and operations

Interface fields carry CRUD rights (Create, Read, Update, Delete). These are explained in
the [interfaces reference](../reference-material/interfaces.md). A CRUD annotation says what
a client may *do* with the target atoms and the pairs of a term; it says nothing about
cardinality. Keeping that distinction straight is the key to the whole mapping:

- **C** — a new target atom may be created.
- **R** — the field may be read. A lowercase `r` means the field is writable but not
  returned, so it is left out of the read schema.
- **U** — pairs `(src, tgt)` may be added or removed. This changes the *link*, without
  deleting the atom on either end.
- **D** — the target atom itself (and every pair it occurs in) may be deleted.

The generator turns those rights into HTTP operations exactly as the framework does:

- **R** drives presence in the read schema: only readable fields appear in the `GET`
  response.
- **U** gives the interface a `PATCH` (`replace`/`add`/`remove` operations on links).
- **C** gives the interface a `PATCH` (`create` operation) and the concept a
  `POST /resource/{Concept}`.
- **D** gives the interface a `DELETE`.

The CRUD rights of every field also travel with the field as an `x-crud` annotation, so a
client can see per field which operations are allowed.

### Rights aggregate over the whole interface tree

Whether an interface gets a `PATCH`, `POST` or `DELETE` does not depend on the top-level
field alone. A box deep inside the interface may carry an update right. `treeRights` folds
the Create/Update/Delete rights over the entire subtree of the interface, because the
framework accepts a patch that touches any writable field anywhere in the tree.

### Univalence decides list-versus-single, and nothing else

A field whose term is univalent holds at most one value, so it is rendered as a single
value. A non-univalent field is rendered as a JSON array.

Univalence is used **only** for that distinction. In particular, totality is deliberately
**not** used to mark fields as `required` or non-`nullable`. Totality and univalence are
properties of relations; the OpenAPI document is a contract at the interface level, and the
framework does not guarantee that a total relation is already populated for the atom you
fetch. The only field that is always present is `_id_`, the resource identity. Every other
single-valued field is `nullable`. Inventing `required` constraints from relation totality
would produce a contract the server does not actually honour.

### A PROP relation on an object concept becomes a boolean

When a field is backed by a relation declared with the `PROP` property and its target is an
object concept, the framework renders it as a boolean: the pair is either present or not.
The generator mirrors that.

There is a real trap here, and it is worth spelling out because it caused a bug. The
property must be read from the **declared relation**, exactly as `relations.json` does with
`isProp (EDcD decl)`. It must **not** be read from `isProp` applied to the normalized field
term. A complex term can normalize to a symmetric-and-antisymmetric shape and so look like a
property even though no `PROP` relation backs it; reading `isProp` off that term flags
ordinary object fields as booleans. The generator therefore computes the flag as
`maybe False (isProp . EDcD) mDecl`, where `mDecl` is the relation found behind the field,
and falls back to "not a property" when the field is a complex term with no single relation.

### Scalar types are deliberately coarse

A field with a scalar target is typed from the concept's representation type (`TType`) in
`scalarPairs`. The mapping is intentionally coarse — every textual `TType` becomes a plain
`string`. That is a starting point, not a limit: deriving stricter types from the model (a
`JaNee` concept as an `enum`, a date-like concept with `format`) is the obvious place to
extend the generator, and the FSpec carries enough information to do it.

### Field names match the backend's keys exactly

`fieldName` runs each property key through `escapeIdentifier`, the same escaping the backend
applies to the keys it returns (a space becomes `_32_`, an underscore becomes `__`). This is
a deliberate choice: the schema uses the exact key a client receives on the wire, not a
prettier name that would fail to match the actual payload.

### Every resource carries the same envelope

The framework returns more than the modelled fields. Each resource is wrapped in an
envelope: `_id_` (identity), `_label_` (a human-readable label from a `VIEW`), `_path_`
(the canonical resource path), `_view_` (the `VIEW` result), `_ifcs_` (interfaces you can
navigate to), and `_sortValues_`. The generator adds this envelope to every box object and
defines it once as the reusable `AtomRef` schema. When a field points at another resource
rather than inlining it, the generator references `AtomRef`.

`_view_` is polymorphic, so its schema is a `oneOf`: an interface without a `VIEW` returns
an empty array there, while an interface with a `VIEW` returns an object. The schema admits
both rather than guessing one.

### Why object fields are wrapped in `allOf`

A subtlety of OpenAPI 3.0 explains a piece of the code that otherwise looks redundant. When
a Schema Object contains a `$ref`, the specification says every sibling keyword next to that
`$ref` is ignored. So you cannot write `{ "$ref": "…/AtomRef", "nullable": true, "x-crud": … }`
and expect tooling to honour the `nullable` or the `x-crud` — they are silently dropped.

To attach those annotations to a referenced schema, the generator wraps the reference in an
`allOf`: `{ "allOf": [ { "$ref": "…/AtomRef" } ], "nullable": true, "x-crud": … }`. The
`allOf` is a real schema object, so its siblings are respected. That is why `coreSchema`
returns an `allOf` form for object-valued fields instead of a bare `$ref`. Removing the
`allOf` would "simplify" the output into something tools quietly misread.

### The write side mirrors the framework

The shapes of write requests and responses are not invented; they match the
`ResourceController` and `Resource` implementation in the
[prototype framework](https://github.com/AmpersandTarski/prototype). A `PATCH` body is a
JSON-Patch array of `{op, path, value}` operations. Each `op` maps onto a CRUD right:
`replace` sets a univalent field, `add`/`remove` insert or delete a pair (the **U** right),
and `create` makes a new target atom (the **C** right).

The response is a `WriteResult`; a `DELETE` returns the same without `content`/`patches`;
a `POST` create returns `{_id_}`. These shapes live as reusable schemas in `sharedSchemas`,
so a change to the framework's response shape is a one-place edit there.

### Roles are informative only, for now

Each operation lists the interface's roles as `x-ampersand-roles`. This is documentation,
not an enforced security model. A real OpenAPI `security` model waits until the roles
system is reworked; until then the annotation tells a reader which roles the model attaches
to an interface without pretending to be an access-control contract.

## How to verify a change

The agreed target shape was first written as a throwaway Python generator that consumes the
same `generics/*.json` files. That generator is the **oracle**: when you change
`OpenAPI.hs`, you check your output against it. The reference artifacts (the Python
generator, the design note, contract tests) are kept with the project they were derived
from, not in this repository.

The one rule that makes the comparison trustworthy: run the Python oracle on the **same**
generics you just produced. A reference `openapi.json` that was generated months ago by an
older compiler differs from your fresh output for reasons that have nothing to do with the
generator — the model or the compiler drifted in between. Comparing generator-to-generator
on identical input isolates the logic you actually changed.

A practical recipe:

```sh
# 1. Build and generate fresh backend files.
stack build --fast
ampersand proto path/to/main.adl --no-frontend --proto-dir /tmp/out

# 2. Run the oracle on the SAME generics.
python3 gen_openapi.py /tmp/out/generics /tmp/ref

# 3. Compare counts first.
for f in /tmp/out/generics/openapi.json /tmp/ref/openapi.json; do
  jq -r '"GET \([.paths[]|select(.get)]|length)  PATCH \([.paths[]|select(.patch)]|length)  schemas \(.components.schemas|length)"' "$f"
done
```

When you diff the documents, ignore differences that are allowed to vary and collapse the
ones that are equivalent:

- `description` and `summary` text may differ; strip them before diffing.
- Key order may differ; sort keys (`jq -S`).
- An inlined envelope and a `$ref` to `AtomRef` are equivalent, because `AtomRef` *is* that
  envelope. The Python oracle inlines it; the Haskell generator references it. Canonicalize
  one form into the other before concluding there is a real difference.

A clean result is one where the only remaining differences are deliberate: the
`x-ampersand-roles` annotation that the Haskell generator adds, and the inline-versus-`$ref`
spelling of the envelope.

## How to extend the generator

A few concrete starting points:

- **Add a field to every resource.** Edit `metaProps`. Because `AtomRef` and every box use
  it, the new field appears everywhere at once.
- **Add a reusable schema** (for a new write shape, say). Add it to `sharedSchemas` and
  reference it with `ref "YourSchema"`.
- **Change how a field's type is chosen.** That logic is in `coreSchema` and `scalarPairs`.
  `scalarPairs` maps an Ampersand `TType` to an OpenAPI `type`/`format`; extend the `case`
  there for a new representation type.
- **Add an operation** (a new HTTP method on a path). Build the operation object alongside
  `getO`/`patchO`/`deleteO` in `mkInfo` and add it to the `methods` list, guarded by the
  CRUD condition that should enable it.

After any change, build with `stack build --fast` and re-run the verification recipe above.
Two compile-time things tend to surprise newcomers: the `Object` name clash described
earlier, and the `isProp` source described in the PROP section. Both are about reading a
property from the declared relation rather than from a normalized term.

## Known limitations

- **Interface references are not expanded.** When one interface includes another by
  reference, `interfaces.json` does not carry the referenced interface's fields, so neither
  the oracle nor this generator lists those sub-fields. A referenced resource is described
  by the `AtomRef` envelope only. Expanding references would require resolving them against
  the referenced interface during generation.
- **No `security` model.** Roles are exposed only as the informative `x-ampersand-roles`
  annotation, pending the rework of the roles system.
- **Write endpoints are described from the framework source, not yet from live runs.** The
  read schemas were validated against real responses; the `PATCH`/`DELETE` shapes follow the
  framework implementation and still deserve live confirmation.
