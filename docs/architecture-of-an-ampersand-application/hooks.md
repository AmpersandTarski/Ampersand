# Hooks

Hookpoint use the following naming convention:

* camelCasing
* start with _pre_ or _post_ to define if hooks are called before or after the following position
* specify classname or file where hookpoint is positioned
* specify functionname where hookpoint is positioned
* optionally specify postion within function where hookpoint is positioned

Current list of hookpoints:

| Hookpoint | Extensions that use it |
| :--- | :--- |
| postDatabaseReinstallDB | ExecEngine |
| postDatabaseUpdate | Mutation \(experimental\), Mqtt \(experimental\) |
| postDatabaseInsert | Mutation \(experimental\), Mqtt \(experimental\) |
| postDatabaseDelete | Mutation \(experimental\), Mqtt \(experimental\) |
| preDatabaseCloseTransaction | ExecEngine |
|  | postDatabaseCloseTransaction |
| postDatabaseAddAtomToConceptInsert | Mqtt \(experimental\) |
|  | postDatabaseAddAtomToConceptSkip |
| postDatabaseDeleteAtom | Mqtt \(experimental\) |
|  | postDatabaseStartTransaction |
| postDatabaseCommitTransaction | Mqtt \(experimental\) |
|  | postDatabaseRollbackTransaction |

More hookpoints will be defined when needed.

