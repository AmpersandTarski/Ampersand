#!/bin/bash
# Start the Ampersand MariaDB database
# This script starts the MariaDB container defined in docker-compose.yml

echo "Starting Ampersand MariaDB database..."
docker-compose up -d

# Wait for the database to be ready
echo "Waiting for database to be ready..."
timeout=30
counter=0

while [ $counter -lt $timeout ]; do
    if docker exec ampersand-mariadb mariadb -u root -e "SELECT 1" >/dev/null 2>&1; then
        echo "✓ MariaDB is ready!"
        echo ""
        echo "Database connection details:"
        echo "  Host: localhost (127.0.0.1)"
        echo "  Port: 3306"
        echo "  Database: ampersand"
        echo "  User: root"
        echo "  Password: (empty)"
        echo ""
        echo "You can now run 'stack test' or individual tests."
        exit 0
    fi
    sleep 1
    counter=$((counter + 1))
done

echo "⚠ Warning: Database may not be fully ready yet. Check with: docker-compose logs mariadb"
exit 0
