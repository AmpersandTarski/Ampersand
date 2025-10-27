#!/bin/bash
# Stop the Ampersand MariaDB database
# This script stops the MariaDB container but preserves all data

echo "Stopping Ampersand MariaDB database..."
docker-compose down

echo "✓ MariaDB stopped."
echo ""
echo "Note: All data is preserved. Run './db-start.sh' to start it again."
echo "To completely remove the database and all data, run: docker-compose down -v"
