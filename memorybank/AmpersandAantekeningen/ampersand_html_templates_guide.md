# Ampersand HTML Templates: User Guide

Use templates to turn a plain Ampersand interface into a professional, interactive
user experience with tables, forms, tabs, dropdowns and buttons.
This guide describes the templates that the current Angular Prototype Framework
actually supports, and how to use them from your `.adl` script.

> **Ground truth.** The attributes below are the ones the shipped `Box-*.html`
> templates and their Angular components honour. The Ampersand compiler passes
> *any* attribute you write straight through to the template, so a misspelled or
> obsolete attribute compiles without error but has **no effect**. When in doubt,
> check the template files under `frontend/src/app/generated/.templates/` in the
> Prototype Framework.

## 1. Template System Overview

Ampersand uses three kinds of templates to generate a user interface:

1. **BOX templates** structure a container (`FORM`, `TABLE`, `TABS`, …). Use them
   after the `BOX` keyword, in sharp brackets: `BOX<FORM>`, `BOX<TABLE>`,
   `BOX<TABS>`. A BOX template defines the layout of the whole box. BOX templates
   are written in HTML and shipped with the framework.
2. **Atomic templates** display a single value (a text field, checkbox, date
   picker, …). The compiler knows the type of a leaf field from its `REPRESENT`
   statement and attaches the matching atomic template automatically. You write
   nothing extra.
3. **VIEW templates** are custom display formats for a concept. You apply a view
   inside a term, as in `manager<Person>`, to display the target of `manager`
   through the view `Person`:
   ```ampersand
   VIEW Person: Person
     { first : firstname
     , space : TXT " "
     , last  : lastname
     }
   ENDVIEW
   ```

### Syntax pattern

```ampersand
INTERFACE Name : expr BOX<TEMPLATE attributes>
  [ "fieldname1" : expr1 cRud
  , "fieldname2" : expr2 cRud BOX<SUBTEMPLATE>
      [ "subfieldname" : subexpr cRud
      ]
  ]
```

### Key points

- Template names are case-sensitive: `BOX<FORM>` uses the built-in `FORM`
  template; `BOX<form>` looks for a custom template named `form` and errors if you
  did not supply one.
- Some templates require **prescribed field names** (exact labels the template
  looks for, e.g. `"label"`, `"property"`, `"selectFrom"`). Using the wrong name
  breaks the template.
- CRUD annotations control what the user may do.
- You can nest templates inside other templates.

---

## 2. BOX Templates

| Template | Purpose | Attributes / prescribed fields |
| --- | --- | --- |
| `FORM` | vertical form layout (default) | none |
| `TABLE` | tabular layout, one row per atom | `sortable`, `sortBy`, `order` |
| `TABS` | one tab per sub interface | none |
| `RAW` | plain `<div>`s, no styling or editing | none |
| `PROPBUTTON` | button that sets/clears/toggles a property | fields `label`, `property`; attribute `action` |
| `FILTEREDDROPDOWN` | filterable dropdown, create-on-the-fly | fields `selectFrom`, `setRelation` |
| `SELECT` | plain select list | fields `selectFrom`, `setRelation` |

### 2.1 FORM

The default BOX template. It displays one form per target atom, with each sub
interface on its own line (a vertical layout). It has **no attributes**.

```ampersand
INTERFACE EditProject : I[Project] BOX<FORM>
  [ "Project Name" : projectName cRUd
  , "Description"  : description cRud
  , "Start Date"   : startDate cRud
  ]
```

### 2.2 TABLE

Displays data as a table: one row per target atom, one column per sub interface.

**Attributes**:

- `sortable` — make column headers clickable so the user can sort. Only applies to
  univalent fields.
- `sortBy="Column Label"` — the column the table is sorted by initially. Use
  together with `sortable`.
- `order="asc"` or `order="desc"` — the initial sort direction (`asc` is the
  default). Use together with `sortBy`.

```ampersand
INTERFACE ProjectList : "_SESSION";V[SESSION*Project] BOX<TABLE sortable sortBy="Project Name">
  [ "Project Name" : projectName cRud
  , "Status"       : status cRud
  , "Team Size"    : teamMembers;teamMembers~ cRud
  ]
```

### 2.3 TABS

Organises the sub interfaces into tabs; one tab is shown at a time. Best used with
a univalent interface term, because a full tab set is shown per target atom. It has
**no attributes**.

```ampersand
INTERFACE ProjectDetails : I[Project] BOX<TABS>
  [ "General" : I cRud BOX<FORM>
      [ "Name"        : projectName cRud
      , "Description" : description cRud
      ]
  , "Team" : teamMembers cRud BOX<TABLE>
      [ "Employee" : I cRud
      , "Role"     : role cRud
      ]
  ]
```

### 2.4 RAW

Renders each sub interface inside plain `<div>` elements, without styling or
editing functionality. Use it when you supply your own layout or embed the output
elsewhere. It has **no attributes**.

```ampersand
expr BOX<RAW>
  [ "Field" : fieldexpr cRud
  ]
```

---

## 3. Atomic Templates

Atomic templates render leaf fields automatically, based on the concept's
`REPRESENT` type. You do not select them yourself.

| `REPRESENT … TYPE` | Widget |
| --- | --- |
| `ALPHANUMERIC` | single-line text input |
| `BIGALPHANUMERIC` | multi-line textarea |
| `HUGEALPHANUMERIC` | large textarea |
| `BOOLEAN` | checkbox |
| `DATE` | date picker |
| `DATETIME` | date/time picker |
| `INTEGER` | integer input |
| `FLOAT` | number input |
| `PASSWORD` | masked password input |
| *(no `REPRESENT`; an object)* | `OBJECT` — see below |

`TYPEOFONE` is a special atomic template for the singleton `ONE` atom; you rarely
need it.

### OBJECT

For a leaf whose target is an object (a concept without a `REPRESENT` statement),
Ampersand renders each atom through the concept's `DEFAULT` VIEW if one exists. If
there is no default view, it falls back to the atom's internal identifier (the
"ugly id"). Define a `DEFAULT` VIEW for the concept to show a readable label. When
the field is editable, the object field also offers navigation to an interface for
the target concept, if one exists.

---

## 4. Built-in VIEW Templates

VIEW templates create custom display formats for a concept. You reference them from
a term or attach them with a `HTML TEMPLATE` clause on a `VIEW`.

### 4.1 FILEOBJECT — upload and download files

```ampersand
IDENT FileObjectName: FileObject (filePath)
RELATION filePath[FileObject*FilePath] [UNI,TOT]
RELATION originalFileName[FileObject*FileName] [UNI,TOT]
REPRESENT FilePath,FileName TYPE ALPHANUMERIC

VIEW FileObject: FileObject DEFAULT
  { apiPath  : TXT "api/v1/file"
  , filePath : filePath
  , fileName : originalFileName
  } HTML TEMPLATE "View-FILEOBJECT.html" ENDVIEW
```

### 4.2 LINKTO — navigate to another interface

```ampersand
"label" : expr LINKTO INTERFACE "InterfaceName"
```

`expr`'s target concept must match the source concept of the referenced interface.

```ampersand
INTERFACE ProjectList : "_SESSION";V[SESSION*Project] BOX<TABLE>
  [ "Project Name" : projectName cRud
  , "Edit"         : I LINKTO INTERFACE "EditProject"
  ]
```

### 4.3 PROPERTY — a property as a checkbox

The built-in VIEW template for a `[PROP]` relation. It renders the property as a
boolean (a checkbox), so the user sees and edits a true/false value.

---

## 5. Dropdown Templates

Dropdowns populate a relation from a selectable set. Both dropdown templates use the
same two **prescribed field names**:

- `"selectFrom"` — the term whose target atoms the user may choose from.
- `"setRelation"` — the relation that is filled with the chosen atom. If the
  relation is `[UNI]`, a newly selected atom replaces the current value; otherwise
  selected atoms are added.

> These templates replace the former `OBJECTDROPDOWN` and `VALUEDROPDOWN`
> templates. The prescribed field names are `selectFrom` and `setRelation`
> (camelCase), not the old `selectfrom`/`setrelation`.

### 5.1 FILTEREDDROPDOWN

A dropdown the user can filter by typing. When the typed value does not exist yet,
the user can create a new target atom on the fly.

```ampersand
INTERFACE AssignProjectLead : I[Project] BOX<FORM>
  [ "Project Name" : projectName cRud
  , "Assign Leader" : I cRud BOX<FILTEREDDROPDOWN>
      [ "selectFrom"  : V[Project*Person]
      , "setRelation" : projectLead cRUd
      ]
  ]
```

### 5.2 SELECT

The same behaviour through a plain (non-filtering) select list.

```ampersand
"Priority" : I cRud BOX<SELECT>
  [ "selectFrom"  : V[Task*Priority]
  , "setRelation" : taskPriority cRUd
  ]
```

---

## 6. PROPBUTTON

`PROPBUTTON` renders a button that changes a single property-relation (a relation
that is `[PROP,UNI]`) when clicked.

**Prescribed fields**:

- `"label"` — the text on the button.
- `"property"` — the `[PROP,UNI]` relation the button changes. Its CRUD annotation
  must allow Update (capital `U`).

**Attribute**:

- `action="toggle"` (default) flips the property; `action="set"` always makes it
  true; `action="clear"` always makes it false.

```ampersand
RELATION isCompleted[Task] [PROP,UNI]

INTERFACE TaskList : "_SESSION";V[SESSION*Task] cRud BOX<TABLE>
  [ "Task Name" : taskName cRud
  , "Status"    : I cRud BOX<PROPBUTTON>
      [ "label"    : TXT "Mark Complete"
      , "property" : isCompleted cRUd
      ]
  ]
```

> The current built-in `PROPBUTTON` supports only `label`, `property` and `action`.
> Older attributes such as `color`, `popovertext`, `hide`, `disabled`, and the
> multi-relation `fliprop*`/`setprop*`/`clrprop*` fields are no longer honoured. For
> richer buttons, write a custom BOX template.

---

## 7. A Combined Example

```ampersand
CONCEPT Project ""
CONCEPT Person ""

RELATION projectName[Project*ProjectName] [UNI,TOT]
RELATION teamMember[Project*Person]
RELATION projectLead[Project*Person] [UNI]
RELATION isActive[Project] [PROP,UNI]
REPRESENT ProjectName TYPE ALPHANUMERIC

INTERFACE ProjectOverview : "_SESSION";V[SESSION*Project] cRud BOX<TABLE sortable>
  [ "Project"   : projectName cRud
  , "Team Size" : teamMember;teamMember~ cRud
  , "Active"    : I cRud BOX<PROPBUTTON>
      [ "label"    : TXT "Toggle Active"
      , "property" : isActive cRUd
      ]
  , "Edit"      : I LINKTO INTERFACE "EditProject"
  ]

INTERFACE EditProject : I[Project] cRud BOX<TABS>
  [ "General" : I cRud BOX<FORM>
      [ "Project Name" : projectName cRUd
      , "Project Lead" : I cRud BOX<FILTEREDDROPDOWN>
          [ "selectFrom"  : V[Project*Person]
          , "setRelation" : projectLead cRUd
          ]
      ]
  , "Team" : teamMember cRud BOX<TABLE>
      [ "Team Member" : I cRud
      ]
  ]
```

---

## 8. Troubleshooting

**Template not found or ignored.**
Check the name and casing: `BOX<FORM>`, not `BOX<form>` or `expr FORM [...]`.
Remember that an unknown *attribute* does not error — it is simply ignored.

**PROPBUTTON does nothing.**
The property relation must be `[PROP,UNI]`, the field names must be exactly
`"label"` and `"property"`, and the property's CRUD must include a capital `U`.

**Dropdown shows no options.**
Use the exact field names `"selectFrom"` and `"setRelation"` (camelCase).

**Table sorting does not work.**
Add the `sortable` attribute, and remember it applies only to univalent fields.

**A field is not editable.**
CRUD letters are case-sensitive: use a capital `U` to allow updating
(`fieldExpr cRUd`), lowercase to forbid it.
