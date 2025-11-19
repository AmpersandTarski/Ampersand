# Database Setup for Testing

Ampersand generates PHP prototypes that connect to a MariaDB or MySQL database. The test suite validates SQL expressions against a running database instance. This document describes how to set up a local MariaDB instance for development and testing.

## Prerequisites

You need Docker installed and running on your system.

## Quick Start

Start the database:

```bash
./db-start.sh
```

The script starts a MariaDB container, waits for it to initialize, and displays the connection details.

Stop the database:

```bash
./db-stop.sh
```

This preserves all data in the database.

## Configuration

The setup uses these connection settings:

- Host: localhost (127.0.0.1)
- Port: 3306  
- User: root
- Password: (empty)
- Default Database: ampersand

These settings match the defaults in `src/Ampersand/Prototype/PHP.hs` where Ampersand generates PHP code that connects to the database.

## Running Tests

Run all tests:

```bash
stack test
```

Run a single test:

```bash
stack exec ampersand -- validate --verbose testing/Travis/testcases/prototype/shouldFail/ParserOrTypecheckFailures/typecheckerror.adl
```

## Docker Configuration

The `docker-compose.yml` file defines the database service with MariaDB 11.5. This matches the version used in GitHub Actions CI (see `.github/workflows/ci2.yml`).

The database container restarts automatically when Docker starts, unless you stop it manually with `./db-stop.sh`.

## Data Persistence

Docker stores database data in a volume named `ampersand_mariadb_data`. The data persists between container restarts.

Reset the database and delete all data:

```bash
docker-compose down -v
./db-start.sh
```

## Troubleshooting

Check if the container runs:

```bash
docker ps | grep ampersand-mariadb
```

View database logs:

```bash
docker-compose logs mariadb
```

Test the connection:

```bash
docker exec ampersand-mariadb mariadb -u root -e "SELECT 1"
```

## Files

The setup consists of these files in the project root:

- `docker-compose.yml` - Defines the MariaDB service
- `init-db.sql` - Configures root user permissions
- `db-start.sh` - Script to start the database
- `db-stop.sh` - Script to stop the database

The `.gitignore` file excludes the `mariadb_data/` directory from version control.

## Comparison with CI

The GitHub Actions workflow in `.github/workflows/ci2.yml` uses the `shogo82148/actions-setup-mysql@v1` action to install MariaDB 11.5 directly on the runner. The local setup replicates this environment using Docker.
