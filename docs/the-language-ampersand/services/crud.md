# CRUD

CRUD annotations are used in services to constrain the functionality of fields and boxes in an `INTERFACE`-statement. This allows you to minimize the functionality for your users, to design for easy learning.

Each CRUD annotation comes right after a [term](../terms/), so we can always refer to "the term" to which a CRUD annotation belongs. A CRUD annotation constrains the things your user can do with the target atoms and the pairs of its term.

The CRUD-annotation specifies Create, Read, Update, and Delete rights for the term it follows. Capital = allowed, Non-capital = not allowed. CRUD is the default, so if you specify nothing, everything is allowed. The following service definition illustrates this.

```
INTERFACE Overview : "_SESSION"                  cRud
BOX <TABS>
     [ Students : V[SESSION*Student]             cRuD
       BOX <TABLE>
                [ "Student" : I[Student]         cRud
                , "Enrolled for" : isEnrolledFor cRUD
                , "Course" : takes CRUD
                ]
     , Course : V[SESSION*Course]                cRuD   -- used for a box
       BOX <TABLE>
                [ "Course" : I                   cRud   -- used for a field
                , "Modules" : isPartOf~                 -- CRUD is default
                ]
     , Modules : V[SESSION*Module]               cRud
       BOX <TABLE>
                [ "Modules" : I                  cRuD
                , "Course" : isPartOf            cRUd
                , "Students" : isEnrolledFor~    CRUD
                ]
     ]
```

The user interface defined by this service is shown as a screenshot below. Notice that the lowercase r in the annotation of the Students box prevents showing the underlying box. The full CRUD functionality in Course yields 'create' functionality (the green plus-button), 'remove pair' functionality (red minus button), and 'delete atom' functionality (the red trash can button). The lowercase c, u, and d in the Modules box prevents displaying that functionality in the user interface.

![Column-oriented layout of a user interface with columns in each row](<../../.gitbook/assets/COLS layout example.png>)

The next sections give some more detailed information on the run time semantics for CRUD annotations as implemented in Ampersand.

## Create

| CRUD | for a box                                                                                                                                                                                                 | for a field.                                                                                                                                                                                                                                                  |
| ---- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| C    | ![Creating atoms is done by pressing the + button](../../.gitbook/assets/box-crud-create.png) A + (plus) button is displayed that lets you create a new atom, but only if the box-expression is editable. | ![Creating atoms is done by pressing the + button](<../../.gitbook/assets/Create Field.png>) Enter a new atom and a `+` button appears. Click the + to add that atom to the listed set of atoms. If you enter an atom that exists (Peter), you can select it. |
| c    | Atoms cannot be created                                                                                                                                                                                   | Atoms cannot be created                                                                                                                                                                                                                                       |

## Read

| Read | CRUD for boxes      |   | CRUD for fields     |
| ---- | ------------------- | - | ------------------- |
| R    | Read is allowed     |   | Read is allowed     |
| r    | Read is not allowed |   | Read is not allowed |

## Update

| Update | CRUD for boxes                                                                                                                                                                                                                                               | CRUD for fields                                                                                                                                                                                                                                                |
| ------ | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| U      | ![Deleting a pair is done with the - button](../../.gitbook/assets/box-crud-update.png) Removing and/or adding a pair (src,tgt) is allowed if expr is editable and the atom exists. Deleting a pair is done with the - button; the atom will NOT be deleted. | ![Deleting a pair is done with the - button](../../.gitbook/assets/field-crud-update.png) Removing and/or adding a pair (src,tgt) is allowed if expr is editable and the atom exists. Deleting a pair is done with the - button; the atom will NOT be deleted. |
| u      | Update is not allowed                                                                                                                                                                                                                                        | Update is not allowed                                                                                                                                                                                                                                          |

## Delete

| Delete | CRUD for boxes                                                                                                                                                                          | CRUD for fields                                                                                                                                |
| ------ | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------- |
| D      | ![Deleting a pair is done with the - button](../../.gitbook/assets/box-crud-delete.png) Deleting a pair is done with the - (minus) button. Deleting an atom is done with the trash bin. | ![Deleting a pair is done with the - button](../../.gitbook/assets/field-crud-delete.png) Delete atom (tgt) and all pairs in which it is used. |
| d      | delete not allowed                                                                                                                                                                      | delete not allowed                                                                                                                             |

A top-level Update or Create are common in my own scripts, e.g. to create an overview of People and be able to create a new Person: `INTERFACE "People" : V[SESSION*Person] CRud COLS []`. And update is also possible.

## Things to remember

1. The red minus is enabled by `U`. It unlinks an atom (by deleting a pair from a relation) and leaves the atom alone.
2. The red trash bin is enabled by `D`. It removes an atom and all pairs in which that atom is used.

## Background

Motivations for CRUD-functionality are found in the [GitHub discussions on CRUD](https://github.com/AmpersandTarski/Ampersand/issues?utf8=%E2%9C%93\&q=is%3Aissue+label%3Acrud+) functionality.
