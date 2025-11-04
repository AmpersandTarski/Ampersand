# Ampersand Validate Command - Database Inspection Guide

## Overview
When Ampersand runs the `validate` command, it creates a temporary MySQL database to test SQL expressions against Haskell evaluations. This guide explains how to inspect that database for debugging purposes.

## How Validation Works

1. **Database Creation**: Creates `TempDB_<ContextName>` where `<ContextName>` is from the ADL file's CONTEXT statement
2. **Table Generation**: Creates tables for all concepts and relations (via PlugSQLs)
3. **Population**: Populates tables with data from POPULATION statements
4. **Validation**: Evaluates expressions in both SQL and Haskell, comparing results
5. **Cleanup**: Database may be dropped after validation completes

## Database Inspection Procedure

### Method 1: PHP Inspection (RECOMMENDED - Most Robust)

**Why PHP?** 
- Ampersand internally uses PHP to connect to MySQL
- Same authentication method as Ampersand uses
- Works even when MySQL command-line client has authentication issues

**Steps:**

1. **Create PHP inspection script** (`inspect_temp_db.php`):

```php
<?php
// Replace CONTEXT_NAME with the actual context name from your ADL file
$DB_name = 'TempDB_CONTEXT_NAME';

// Standard Ampersand connection settings
$DB_host = '127.0.0.1';
$DB_user = 'root';
$DB_pass = '';

$DB_link = mysqli_connect($DB_host, $DB_user, $DB_pass, $DB_name);
if (mysqli_connect_errno()) {
    die('Failed to connect: ' . mysqli_connect_error());
}

echo "=== Tables ===\n";
$result = mysqli_query($DB_link, "SHOW TABLES");
while ($row = mysqli_fetch_array($result)) {
    echo "  - " . $row[0] . "\n";
}

// To inspect a specific table:
$table_name = 'YourConceptName';
echo "\n=== Contents of $table_name ===\n";
$result = mysqli_query($DB_link, "SELECT * FROM `$table_name`");
if ($result) {
    while ($row = mysqli_fetch_assoc($result)) {
        print_r($row);
    }
}

mysqli_close($DB_link);
?>
```

2. **Run validation in background and inspect:**

```bash
#!/bin/bash
CONTEXT_NAME="YourContextName"  # From CONTEXT statement in ADL
ADL_FILE="path/to/your/file.adl"

# Start validation in background
ampersand validate --verbose "$ADL_FILE" > validate.log 2>&1 &
VALIDATE_PID=$!

# Wait for database to be created
sleep 3

# Inspect database
php inspect_temp_db.php

# Wait for validation to complete
wait $VALIDATE_PID
echo "Validation exit code: $?"
```

### Method 2: Direct MySQL (May Fail on Some Systems)

If MySQL command-line client works with your authentication setup:

```bash
mysql -u root -h 127.0.0.1 -D TempDB_CONTEXT_NAME -e "SHOW TABLES;"
mysql -u root -h 127.0.0.1 -D TempDB_CONTEXT_NAME -e "SELECT * FROM ConceptName;"
```

**Note:** This may fail with authentication errors on newer MySQL versions (9.x+).

## Common Database Schema Patterns

### Concept Tables
- **Table name**: Same as concept name (e.g., `Idee`, `Persoon`)
- **Columns**: 
  - Concept name column (VARCHAR(255))
  - `ts_insertupdate` (TIMESTAMP)
- **Purpose**: Stores atoms of that concept

### Relation Tables  
- **Table name**: Relation name or qualified name
- **Columns**:
  - Source column
  - Target column  
  - `ts_insertupdate` (TIMESTAMP)
- **Purpose**: Stores pairs for binary relations

### Special Tables
- `ONE`: Contains single row with value '1'
- `SESSION`: Session management
- Tables prefixed with `PrototypeContext.`: Framework tables for prototype functionality

## Determining Context Name

Extract from ADL file:
```bash
grep "^CONTEXT" your_file.adl | awk '{print $2}'
```

For Issue746.adl:
```
CONTEXT Issue746 IN DUTCH
```
→ Database name: `TempDB_Issue746`

## Troubleshooting

### Database Not Found
- Validation may have completed too quickly
- Increase sleep time between starting validation and inspection
- Check if database creation succeeded in validation log

### Authentication Errors (MySQL CLI)
- Use PHP method instead
- MySQL 9.x has deprecated mysql_native_password plugin
- PHP's mysqli uses compatible authentication

### Empty Tables
- Check if POPULATION statements exist in ADL
- Verify validation hasn't failed during FSpec creation (before DB creation)
- Look for violations in validation log

### Database Already Exists
- Previous validation may have crashed without cleanup
- Manually drop: `mysql -u root -h 127.0.0.1 -e "DROP DATABASE IF EXISTS TempDB_CONTEXT_NAME;"`

## Ready-to-Use Tool

A generic inspection tool is available at: `memorybank/tools/inspect_validate_db.sh`

**Usage:**

```bash
# Inspect all tables
./memorybank/tools/inspect_validate_db.sh path/to/file.adl

# Inspect specific table only
./memorybank/tools/inspect_validate_db.sh path/to/file.adl TableName
```

**Examples:**

```bash
# Show all tables in Issue746 database
./memorybank/tools/inspect_validate_db.sh testing/Travis/testcases/prototype/shouldSucceed/Issue746.adl

# Show only the Idee table
./memorybank/tools/inspect_validate_db.sh testing/Travis/testcases/prototype/shouldSucceed/Issue746.adl Idee
```

**Features:**
- Automatically extracts context name from ADL file
- Runs validation in background
- Uses PHP for robust database connectivity
- Shows table structure and contents
- Limits output to 50 rows per table (configurable in script)
- Reports validation exit code
- Cleans up temporary files automatically

## Source Code References

- **Database creation**: `src/Ampersand/Prototype/PHP.hs` → `createTempDatabase`
- **Database name**: `src/Ampersand/Prototype/PHP.hs` → `tempDbName`
- **Table creation**: `src/Ampersand/Prototype/TableSpec.hs` → `plug2TableSpec`
- **Table population**: `src/Ampersand/FSpec/ToFSpec/ADL2FSpec.hs` → `tblcontents`
- **Validation logic**: `src/Ampersand/Prototype/ValidateSQL.hs` → `validateRulesSQL`
