# Ampersand HTML Templates: Complete User Guide

Use templates to transform your basic Ampersand interfaces into professional, interactive user experiences with buttons, dropdowns, tables, forms, and custom layouts that make your applications intuitive and efficient for end users.
For this purpose, Ampersand lets you create rich user interfaces using pre-built HTML templates and HTML templates of your own.
You will will learn the template system architecture, use every available template type, and build sophisticated interfaces step by step.


## 1. Template System Overview

### How Templates Work

Ampersand uses three types of templates to generate user interfaces:

1. **BOX Templates** - Structure containers (FORM, TABLE, TABS, etc.). Use these in conjunction with the `BOX` keyword, as in `BOX<FORM>`, `BOX<TABLE>`, `BOX<TABS>, etc., to make the template work for the entire BOX. Note that you specify a BOX template in HTML.
2. **Atomic Templates** - Display individual data values (text fields, buttons, etc.).
   For data fields, the Ampersand compiler selects the appropriate atomic template based on the datatype.
3. **VIEW Templates** - Custom display formats for specific concepts.
   Use these in conjunction with a term in an interface, as in `project~<ProjectView>`, to apply the view ProjectView to the target of the term `project~`.
   Note that you specify a VIEW template in Ampersand.

Templates communicate with the Angular frontend through **prescribed field names**. These are exact field names that each template expects to find in your interface definition. Using the wrong field name breaks the template.

### Basic Syntax Pattern

All templates follow this pattern:

```ampersand
INTERFACE Name : expr BOX<TEMPLATE attributes>
  [ "fieldname1" : expr1 cRud
  , "fieldname2" : expr2 cRud BOX<SUBTEMPLATE>
      [ "subfieldname" : subexpr cRud
      ]
  ]
```

Key points:
- Template names are case-sensitive: `BOX<FORM>` not `BOX<form>`
- Prescribed field names must match exactly: `"label"` not `"Label"`
- CRUD annotations control user permissions
- You can nest templates inside other templates

---

## 2. BOX Templates

BOX templates structure your interface layout. They organize fields into forms, tables, tabs, or custom layouts.

### 2.1 FORM Template

**Purpose**: Display fields vertically like a traditional form.

**Syntax**:
```ampersand
expr BOX<FORM attributes>
  [ "Field 1" : field1expr cRud
  , "Field 2" : field2expr cRud
  ]
```

**Available Attributes**:
- `hideOnNoRecords` - Hide entire form when no data exists
- `hideSubOnNoRecords` - Hide individual fields when they have no data
- `hideLabels` - Remove field labels
- `title="Your Title"` - Add form title
- `noRootTitle` - Remove auto-generated title for root interfaces
- `showNavMenu` - Add navigation menu button

**Example**:
```ampersand
INTERFACE EditProject : I[Project] BOX<FORM hideSubOnNoRecords>
  [ "Project Name" : projectName cRUd
  , "Description" : description cRud
  , "Start Date" : startDate cRud
  ]
```

**When to use**: For data entry forms, detail views, and editing interfaces.

### 2.2 TABLE Template

**Purpose**: Display data in rows and columns like a spreadsheet.

**Syntax**:
```ampersand
expr BOX<TABLE attributes>
  [ "Column 1" : col1expr cRud
  , "Column 2" : col2expr cRud
  ]
```

**Available Attributes**:
- `hideOnNoRecords` - Hide table when no data exists
- `noHeader` - Remove column headers
- `title="Your Title"` - Add table title
- `noRootTitle` - Remove auto-generated title
- `sortable` - Make columns clickable for sorting
- `sortBy="column name"` - Default sort column
- `order="asc"` or `order="desc"` - Default sort direction
- `showNavMenu` - Add navigation menu button

**Example**:
```ampersand
INTERFACE ProjectList : "_SESSION";V[SESSION*Project] BOX<TABLE sortable sortBy="Project Name">
  [ "Project Name" : projectName cRud
  , "Status" : status cRud
  , "Team Size" : teamMembers;teamMembers~ cRud
  ]
```

**When to use**: For lists, overviews, and data comparison interfaces.

### 2.3 TABS Template

**Purpose**: Organize related information into separate tabs.

**Syntax**:
```ampersand
expr BOX<TABS attributes>
  [ "Tab 1" : tab1expr cRud BOX<FORM>
      [ "Field" : fieldexpr cRud ]
  , "Tab 2" : tab2expr cRud BOX<TABLE>
      [ "Column" : colexpr cRud ]
  ]
```

**Available Attributes**:
- `title="Your Title"` - Add title above tabs
- `noRootTitle` - Remove auto-generated title
- `hideOnNoRecords` - Hide tabs when no data exists
- `hideSubOnNoRecords` - Hide individual tabs when they have no data

**Example**:
```ampersand
INTERFACE ProjectDetails : I[Project] BOX<TABS>
  [ "General" : I cRud BOX<FORM>
      [ "Name" : projectName cRud
      , "Description" : description cRud
      ]
  , "Team" : teamMembers cRud BOX<TABLE>
      [ "Employee" : I cRud
      , "Role" : role cRud
      ]
  ]
```

**When to use**: For complex objects with multiple aspects or when screen space is limited.

### 2.4 RAW Template

**Purpose**: Display data without styling or editing functionality.

**Syntax**:
```ampersand
expr BOX<RAW attributes>
  [ "Field" : fieldexpr cRud
  ]
```

**Available Attributes**:
- `form` - Use simple form structure (default)
- `table` - Use simple table structure

**When to use**: For read-only displays, custom styling, or embedding in other systems.

---

## 3. Atomic Templates

Atomic templates display individual data values. Ampersand automatically selects the appropriate template based on your concept's `REPRESENT` statement.

### 3.1 ALPHANUMERIC, BIGALPHANUMERIC, HUGEALPHANUMERIC

**Purpose**: Display and edit text values.

**Auto-selected for**:
```ampersand
REPRESENT ConceptName TYPE ALPHANUMERIC
```

**Display**: Single-line text input (ALPHANUMERIC), multi-line textarea (BIGALPHANUMERIC), large textarea (HUGEALPHANUMERIC)

### 3.2 BOOLEAN

**Purpose**: Display and edit true/false values.

**Auto-selected for**:
```ampersand
REPRESENT ConceptName TYPE BOOLEAN
```

**Display**: Checkbox

### 3.3 DATE, DATETIME

**Purpose**: Display and edit date/time values.

**Auto-selected for**:
```ampersand
REPRESENT ConceptName TYPE DATE
REPRESENT ConceptName TYPE DATETIME
```

**Display**: Date picker (DATE), date/time picker (DATETIME)

### 3.4 INTEGER, FLOAT

**Purpose**: Display and edit numeric values.

**Auto-selected for**:
```ampersand
REPRESENT ConceptName TYPE INTEGER
REPRESENT ConceptName TYPE FLOAT
```

**Display**: Number input field with appropriate validation

### 3.5 PASSWORD

**Purpose**: Display and edit sensitive text.

**Auto-selected for**:
```ampersand
REPRESENT ConceptName TYPE PASSWORD
```

**Display**: Password input field (masked text)

### 3.6 OBJECT

**Purpose**: Display and edit references to other objects.

**Auto-selected for**: Concepts without a `REPRESENT` statement

**Display**: Shows object identifier with navigation link if interface exists

**Special Features**:
- **Create-and-add functionality**: Users can type new values directly into the dropdown and create new objects on-the-fly
- **Filtered dropdowns**: Options exclude already selected items and support search filtering
- **Uni vs Non-Uni behavior**: Univalent relations show single dropdown, non-univalent show multiple selection interface
- **Remove vs Delete**: Remove disconnects the relation, Delete removes the object entirely

**Usage Requirements**:
- Set the `tgtResourceType` attribute to specify the target concept type
- Ensure target concept has appropriate interfaces for navigation
- Consider setting a meaningful `placeholder` text for better user experience

**Example with create functionality**:
```ampersand
RELATION projectLead[Project*Person] [UNI]

INTERFACE ProjectDetails : I[Project] BOX<FORM>
  [ "Project Name" : projectName cRud
  , "Project Lead" : projectLead cRud  -- Users can type new person names and create them
  ]
```

When users type a name that doesn't exist in the dropdown, they can press Enter or click the + button to create a new Person object and immediately assign it to the project.

---

## 4. VIEW Templates

VIEW templates create custom display formats for specific concepts. You define them separately and reference them in interfaces.

### 4.1 FILEOBJECT

**Purpose**: Upload and download files.

**Required Setup**:
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

**Usage in Interface**:
```ampersand
INTERFACE DocumentManager : I[Document] BOX<FORM>
  [ "Title" : title cRud
  , "File" : attachment <FileObject> cRud
  ]
```

**When to use**: For document management, file attachments, and file upload/download functionality.

### 4.2 LINKTO

**Purpose**: Create navigation links to other interfaces.

**Syntax**:
```ampersand
"label" : expr LINKTO INTERFACE "InterfaceName"
```

**Example**:
```ampersand
INTERFACE ProjectList : "_SESSION";V[SESSION*Project] BOX<TABLE>
  [ "Project Name" : projectName cRud
  , "Edit" : I LINKTO INTERFACE "EditProject"
  ]

INTERFACE EditProject : I[Project] BOX<FORM>
  [ "Name" : projectName cRud
  , "Description" : description cRud
  ]
```

**When to use**: For navigation between interfaces, creating edit links, and building hierarchical interfaces.

### 4.3 PROPERTY

**Purpose**: Display boolean properties in a specialized format.

**Auto-selected for**: Relations with `[PROP]` constraint.

**Example**:
```ampersand
RELATION isActive[Project*Project] [PROP]

INTERFACE ProjectStatus : I[Project] BOX<FORM>
  [ "Project Name" : projectName cRud
  , "Active" : isActive cRud
  ]
```

### 4.4 STRONG

**Purpose**: Display text in bold formatting.

**Usage**: Applied automatically to specific VIEW definitions or can be used in custom VIEWs.

### 4.5 URL

**Purpose**: Display clickable web links.

**Auto-selected for**: Concepts representing URLs.

**Example**:
```ampersand
REPRESENT WebsiteURL TYPE ALPHANUMERIC

INTERFACE CompanyInfo : I[Company] BOX<FORM>
  [ "Company Name" : name cRud
  , "Website" : website cRud  -- Displays as clickable link
  ]
```

---

## 5. Dropdown Templates

Dropdown templates create selection lists for choosing from predefined options.

### 5.1 OBJECTDROPDOWN

**Purpose**: Select from a list of objects (concepts without REPRESENT statements).

**Prescribed Field Names**:
- `"selectfrom"` - Expression defining available options
- `"setrelation"` - Relation to populate when user selects
- `"instruction"` - Placeholder text when nothing selected
- `"selectflag"` - Property relation that toggles when object selected
- `"deselectflag"` - Property relation that toggles when nothing selected

**Syntax**:
```ampersand
expr cRud BOX<OBJECTDROPDOWN>
  [ "selectfrom" : optionsExpr cRud <ObjectView>
  , "setrelation" : targetRelation cRUd
  , "instruction" : TXT "Choose an option"
  , "selectflag" : selectedFlag cRUd
  , "deselectflag" : deselectedFlag cRUd
  ]
```

**Example**:
```ampersand
RELATION projectLead[Project*Person] [UNI]
RELATION isAssigned[Person*Person] [PROP]

INTERFACE AssignProjectLead : I[Project] BOX<FORM>
  [ "Project Name" : projectName cRud
  , "Assign Leader" : I cRud BOX<OBJECTDROPDOWN>
      [ "selectfrom" : V[Project*Person] cRud
      , "setrelation" : projectLead cRUd
      , "instruction" : TXT "Select project leader"
      , "selectflag" : projectLead;isAssigned cRUd
      ]
  ]
```

**When to use**: For selecting employees, categories, or any objects from a predefined list.

### 5.2 VALUEDROPDOWN

**Purpose**: Select from a list of values (concepts with REPRESENT statements).

**Prescribed Field Names**: Same as OBJECTDROPDOWN but for value-type concepts.

**Syntax**:
```ampersand
expr cRud BOX<VALUEDROPDOWN>
  [ "selectfrom" : optionsExpr cRud <ValueView>
  , "setrelation" : targetRelation cRUd
  , "instruction" : TXT "Choose a value"
  , "selectflag" : selectedFlag cRUd
  , "deselectflag" : deselectedFlag cRUd
  ]
```

**Example**:
```ampersand
REPRESENT Priority TYPE ALPHANUMERIC
RELATION taskPriority[Task*Priority] [UNI]

INTERFACE EditTask : I[Task] BOX<FORM>
  [ "Task Name" : taskName cRud
  , "Priority" : I cRud BOX<VALUEDROPDOWN>
      [ "selectfrom" : V[Task*Priority] cRud
      , "setrelation" : taskPriority cRUd
      , "instruction" : TXT "Select priority level"
      ]
  ]
```

**When to use**: For selecting from predefined text values, status codes, or enumerated options.

---

## 6. Advanced Templates

### 6.1 PROPBUTTON Template

**Purpose**: Create interactive buttons that can set, clear, or flip property relations.

**Prescribed Field Names**:
- `"label"`, `"label1"`, `"label2"`, `"label3"` - Button text components
- `"property"` - Property relation to flip (backward compatible)  
- `"fliprop1"`, `"fliprop2"`, `"fliprop3"` - Property relations to flip
- `"setprop1"`, `"setprop2"`, `"setprop3"` - Property relations to set (make true)
- `"clrprop1"`, `"clrprop2"`, `"clrprop3"` - Property relations to clear (make false)
- `"color"` - Button color (primary, secondary, success, warning, danger, info, light, dark)
- `"hide"` - Expression to hide button when true
- `"disabled"` - Expression to disable button when true
- `"disabledcolor"` - Button color when disabled
- `"popovertext"` - Tooltip text when enabled
- `"disabledpopovertext"` - Tooltip text when disabled

**Syntax**:
```ampersand
expr cRud BOX<PROPBUTTON>
  [ "label" : TXT "Button Text"
  , "property" : propRelation cRUd
  , "color" : TXT "primary"
  , "hide" : hideCondition cRud
  , "disabled" : disableCondition cRud
  , "popovertext" : TXT "Click to activate"
  ]
```

**Example**:
```ampersand
RELATION isApproved[Document*Document] [PROP]
RELATION isRejected[Document*Document] [PROP]
RELATION canApprove[Document*Document] [PROP]

INTERFACE DocumentReview : I[Document] BOX<FORM>
  [ "Document Title" : title cRud
  , "Approve" : I cRud BOX<PROPBUTTON>
      [ "label" : TXT "Approve Document"
      , "setprop1" : isApproved cRUd
      , "clrprop1" : isRejected cRUd
      , "color" : TXT "success"
      , "hide" : isApproved cRud
      , "disabled" : (I - canApprove) cRud
      , "popovertext" : TXT "Approve this document"
      , "disabledpopovertext" : TXT "You don't have approval rights"
      ]
  , "Reject" : I cRud BOX<PROPBUTTON>
      [ "label" : TXT "Reject Document"
      , "setprop1" : isRejected cRUd
      , "clrprop1" : isApproved cRUd
      , "color" : TXT "danger"
      , "hide" : isRejected cRud
      ]
  ]
```

**When to use**: For workflow buttons, status changes, approval processes, and interactive state management.

---

## 7. Template Tutorials

### 7.1 Building a Project Management Interface

This tutorial combines multiple templates to create a complete project management system.

**Step 1: Define the data model**
```ampersand
CONCEPT Project "A project with team members and tasks"
CONCEPT Person "A person who can work on projects"
CONCEPT Task "A work item within a project"

RELATION projectName[Project*ProjectName] [UNI,TOT]
RELATION teamMember[Project*Person]
RELATION projectLead[Project*Person] [UNI]
RELATION taskTitle[Task*TaskTitle] [UNI,TOT]
RELATION taskProject[Task*Project] [UNI,TOT]
RELATION taskAssignee[Task*Person] [UNI]
RELATION isCompleted[Task*Task] [PROP]
RELATION isActive[Project*Project] [PROP]

REPRESENT ProjectName, TaskTitle TYPE ALPHANUMERIC
```

**Step 2: Create the main project overview**
```ampersand
INTERFACE ProjectOverview : "_SESSION";V[SESSION*Project] BOX<TABLE sortable>
  [ "Project" : projectName cRud
  , "Status" : isActive cRud
  , "Team Size" : teamMember;teamMember~ cRud
  , "Tasks" : taskProject~;taskProject cRud
  , "Edit" : I LINKTO INTERFACE "EditProject"
  ]
```

**Step 3: Create the project detail interface with tabs**
```ampersand
INTERFACE EditProject : I[Project] BOX<TABS>
  [ "General" : I cRud BOX<FORM>
      [ "Project Name" : projectName cRUd
      , "Project Lead" : projectLead cRud
      , "Activate" : I cRud BOX<PROPBUTTON>
          [ "label" : TXT "Activate Project"
          , "property" : isActive cRUd
          , "color" : TXT "success"
          , "hide" : isActive cRud
          ]
      ]
  , "Team" : teamMember cRud BOX<TABLE>
      [ "Team Member" : I cRud
      , "Remove" : I cRud BOX<PROPBUTTON>
          [ "label" : TXT "Remove"
          , "color" : TXT "danger"
          ]
      ]
  , "Tasks" : taskProject~ cRud BOX<TABLE>
      [ "Task" : taskTitle cRud
      , "Assignee" : taskAssignee cRud
      , "Complete" : I cRud BOX<PROPBUTTON>
          [ "label" : TXT "Mark Complete"
          , "property" : isCompleted cRUd
          , "color" : TXT "success"
          , "hide" : isCompleted cRud
          ]
      ]
  ]
```

**Step 4: Add team member assignment interface**
```ampersand
INTERFACE AssignTeamMember : I[Project] BOX<FORM>
  [ "Project" : projectName cRud
  , "Add Team Member" : I cRud BOX<OBJECTDROPDOWN>
      [ "selectfrom" : V[Project*Person] cRud
      , "setrelation" : teamMember cRUd
      , "instruction" : TXT "Select team member to add"
      ]
  ]
```

### 7.2 Combining Dropdown and PROPBUTTON Templates

This example shows how to create a task assignment workflow:

```ampersand
RELATION taskStatus[Task*TaskStatus] [UNI]
RELATION canAssign[Task*Task] [PROP]
REPRESENT TaskStatus TYPE ALPHANUMERIC

INTERFACE TaskAssignment : I[Task] BOX<FORM>
  [ "Task Title" : taskTitle cRud
  , "Assign To" : I cRud BOX<OBJECTDROPDOWN>
      [ "selectfrom" : taskProject;teamMember cRud
      , "setrelation" : taskAssignee cRUd
      , "instruction" : TXT "Choose assignee"
      ]
  , "Set Status" : I cRud BOX<VALUEDROPDOWN>
      [ "selectfrom" : V[Task*TaskStatus] cRud
      , "setrelation" : taskStatus cRUd
      , "instruction" : TXT "Select status"
      ]
  , "Start Task" : I cRud BOX<PROPBUTTON>
      [ "label" : TXT "Start Working"
      , "setprop1" : canAssign cRUd
      , "color" : TXT "primary"
      , "disabled" : (I - taskAssignee;taskAssignee~) cRud
      , "popovertext" : TXT "Begin work on this task"
      , "disabledpopovertext" : TXT "Task must be assigned first"
      ]
  ]
```

---

## 8. Troubleshooting Guide

### 8.1 Common Template Errors

**Error**: Template not found or not working
**Cause**: Incorrect template name or missing BOX keyword
**Solution**: Check template name spelling and ensure you use `BOX<TEMPLATENAME>`

```ampersand
-- Wrong:
expr FORM [ ... ]
expr BOX<form> [ ... ]

-- Correct:
expr BOX<FORM> [ ... ]
```

**Error**: Fields not displaying in PROPBUTTON
**Cause**: Wrong prescribed field names
**Solution**: Use exact field names as documented

```ampersand
-- Wrong:
BOX<PROPBUTTON>
  [ "text" : TXT "Click me"      -- Should be "label"
  , "prop" : myProperty cRUd     -- Should be "property"
  ]

-- Correct:
BOX<PROPBUTTON>
  [ "label" : TXT "Click me"
  , "property" : myProperty cRUd
  ]
```

**Error**: Dropdown not showing options
**Cause**: Missing required prescribed field names
**Solution**: Include both "selectfrom" and "setrelation" fields

```ampersand
-- Wrong:
BOX<OBJECTDROPDOWN>
  [ "options" : V[Project*Person] cRud  -- Should be "selectfrom"
  ]

-- Correct:
BOX<OBJECTDROPDOWN>
  [ "selectfrom" : V[Project*Person] cRud
  , "setrelation" : projectLead cRUd
  ]
```

### 8.2 CRUD Permission Issues

**Problem**: Users can't edit fields that should be editable
**Solution**: Check CRUD annotations - use uppercase for allowed operations

```ampersand
-- Wrong: User can't update
"Field Name" : fieldExpr crud

-- Correct: User can update  
"Field Name" : fieldExpr cRUd
```

**Problem**: Users see fields they shouldn't access
**Solution**: Use lowercase CRUD letters to restrict access

```ampersand
-- Hide field completely:
"Sensitive Data" : sensitiveField c

-- Show but don't allow editing:
"Read Only" : readOnlyField cRud
```

### 8.3 Template Attribute Problems

**Problem**: Table sorting not working
**Solution**: Add `sortable` attribute and ensure fields are univalent

```ampersand
-- Wrong: No sortable attribute
BOX<TABLE>

-- Correct: Sortable table
BOX<TABLE sortable>
```

**Problem**: Form hiding when it shouldn't
**Solution**: Check `hideOnNoRecords` attribute usage

```ampersand
-- This hides the entire form when no data:
BOX<FORM hideOnNoRecords>

-- This only hides individual empty fields:
BOX<FORM hideSubOnNoRecords>
```

### 8.4 Debugging Template Issues

**Step 1**: Verify template name spelling and case
**Step 2**: Check all prescribed field names against documentation  
**Step 3**: Validate CRUD annotations for permissions
**Step 4**: Ensure relations exist and have correct signatures
**Step 5**: Test with minimal example to isolate the issue

### 8.5 Best Practices

1. **Use descriptive field labels**: Make interface purpose clear to users
2. **Apply consistent CRUD patterns**: Follow same permission logic throughout your application
3. **Test with different data states**: Empty data, single records, multiple records
4. **Combine templates thoughtfully**: Don't mix too many different templates in one interface
5. **Document custom templates**: If you create custom templates, document their prescribed field names

---

## Reference Materials

For complete syntax reference and additional examples, see:
- [Ampersand Syntax Reference](#) - Complete language syntax
- [Template Attribute Reference](#) - All available template attributes  
- [CRUD Annotation Guide](#) - Permission system details
- [Interface Examples Collection](#) - More complex interface patterns

This guide covers the essential templates and patterns for building rich Ampersand interfaces. Start with basic FORM and TABLE templates, then gradually incorporate advanced features like PROPBUTTON and dropdown templates as your application requirements grow.
