#!/bin/bash
# Generic tool to inspect Ampersand validate temporary database
# Usage: ./inspect_validate_db.sh <adl_file> [table_name]
#
# Examples:
#   ./inspect_validate_db.sh Issue746.adl
#   ./inspect_validate_db.sh Issue746.adl Idee
#   ./inspect_validate_db.sh path/to/MyContext.adl MyTable

set -e

ADL_FILE="${1:?Usage: $0 <adl_file> [table_name]}"
SPECIFIC_TABLE="${2:-}"

if [ ! -f "$ADL_FILE" ]; then
    echo "Error: File not found: $ADL_FILE"
    exit 1
fi

# Extract context name from ADL file
CONTEXT_NAME=$(grep "^CONTEXT" "$ADL_FILE" | awk '{print $2}' | head -1)

if [ -z "$CONTEXT_NAME" ]; then
    echo "Error: Could not extract CONTEXT name from $ADL_FILE"
    echo "Make sure the file contains a line like: CONTEXT YourName"
    exit 1
fi

DB_NAME="TempDB_$CONTEXT_NAME"
echo "=== Database Inspection Setup ==="
echo "ADL file: $ADL_FILE"
echo "Context name: $CONTEXT_NAME"
echo "Database name: $DB_NAME"
echo ""

# Create temporary PHP inspection script
PHP_SCRIPT="/tmp/inspect_ampersand_$$.php"
cat > "$PHP_SCRIPT" <<'PHPEOF'
<?php
$DB_name = getenv('DB_NAME');
$specific_table = getenv('SPECIFIC_TABLE');

$DB_host = '127.0.0.1';
$DB_user = 'root';
$DB_pass = '';

$DB_link = mysqli_connect($DB_host, $DB_user, $DB_pass, $DB_name);
if (mysqli_connect_errno()) {
    die("Failed to connect to $DB_name: " . mysqli_connect_error() . "\n");
}

echo "=== Successfully connected to $DB_name ===\n\n";

// Get all tables
echo "=== Tables in database ===\n";
$result = mysqli_query($DB_link, "SHOW TABLES");
$tables = [];
while ($row = mysqli_fetch_array($result)) {
    $tables[] = $row[0];
    echo "  - " . $row[0] . "\n";
}
echo "\n";

// Function to display table info
function display_table($DB_link, $table) {
    echo "=== Table: $table ===\n";
    
    // Show structure
    echo "Structure:\n";
    $result = mysqli_query($DB_link, "DESCRIBE `$table`");
    if ($result) {
        while ($row = mysqli_fetch_assoc($result)) {
            echo "  " . $row['Field'] . " (" . $row['Type'] . ")\n";
        }
    }
    
    // Show contents (limit 50 rows)
    echo "\nContents (max 50 rows):\n";
    $result = mysqli_query($DB_link, "SELECT * FROM `$table` LIMIT 50");
    if ($result) {
        $num_rows = mysqli_num_rows($result);
        if ($num_rows == 0) {
            echo "  (empty table)\n";
        } else {
            $fields = [];
            while ($field = mysqli_fetch_field($result)) {
                $fields[] = $field->name;
            }
            echo "  Columns: " . implode(", ", $fields) . "\n";
            
            $row_num = 0;
            while ($row = mysqli_fetch_array($result, MYSQLI_NUM)) {
                $row_num++;
                $values = array_map(function($v) { 
                    return $v === null ? "NULL" : "'" . addslashes($v) . "'"; 
                }, $row);
                echo "  Row $row_num: " . implode(", ", $values) . "\n";
            }
        }
    } else {
        echo "  Error: " . mysqli_error($DB_link) . "\n";
    }
    echo "\n";
}

// Display specific table or all tables
if (!empty($specific_table)) {
    if (in_array($specific_table, $tables)) {
        display_table($DB_link, $specific_table);
    } else {
        echo "Error: Table '$specific_table' not found in database.\n";
        echo "Available tables: " . implode(", ", $tables) . "\n";
    }
} else {
    // Display all tables
    foreach ($tables as $table) {
        display_table($DB_link, $table);
    }
}

mysqli_close($DB_link);
?>
PHPEOF

# Export environment variables for PHP
export DB_NAME="$DB_NAME"
export SPECIFIC_TABLE="$SPECIFIC_TABLE"

# Start validation in background
echo "=== Starting Ampersand validation in background ==="
VALIDATE_LOG="/tmp/validate_ampersand_$$.log"
ampersand validate --verbose "$ADL_FILE" > "$VALIDATE_LOG" 2>&1 &
VALIDATE_PID=$!

echo "Validation PID: $VALIDATE_PID"
echo "Waiting 3 seconds for database to be created..."
sleep 3

# Run PHP inspection
echo ""
echo "=== Running Database Inspection ==="
php "$PHP_SCRIPT"

# Wait for validation to complete
echo "=== Waiting for validation to complete ==="
wait $VALIDATE_PID
VALIDATE_EXIT=$?

echo ""
echo "=== Validation Result ==="
echo "Exit code: $VALIDATE_EXIT"

if [ $VALIDATE_EXIT -ne 0 ]; then
    echo ""
    echo "=== Validation Failed - Last 30 lines of log ==="
    tail -30 "$VALIDATE_LOG"
fi

# Cleanup
rm -f "$PHP_SCRIPT" "$VALIDATE_LOG"

exit $VALIDATE_EXIT
