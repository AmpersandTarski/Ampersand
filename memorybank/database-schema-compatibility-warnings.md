# Database Schema Compatibility Warnings

## Critical Warning Signs - Schema Incompatibility Risk

### HIGH RISK Actions That Require Database Volume Reset:

1. **Framework Version Changes**
   - Changing Ampersand prototype framework versions (e.g., `ampersandtarski/prototype-framework:main` → `v1.19`)
   - Switching between different framework approaches (Angular frontend vs no-frontend)
   - Upgrading/downgrading Ampersand compiler versions

2. **Docker Configuration Changes**
   - Modifying base Docker image in `project/Dockerfile`
   - Changing Ampersand compilation parameters (`--crud-defaults`, `--proto-dir`, schema options)
   - Switching between different prototype generation modes

3. **Schema-Affecting Code Changes**
   - Major modifications to `.adl` files that alter database structure
   - Changes to database initialization scripts in `db-init-scripts/`
   - Modifications to table relationships or constraints

### WARNING: When These Changes Occur

**BEFORE proceeding with framework changes:**
1. **Backup current database**: `docker exec prototype-db mysqldump -u ampersand -p[password] [database] > backup.sql`
2. **Document current setup**: Note current framework version, Ampersand version, schema version
3. **Test in isolation**: Use fresh database volume for testing new framework
4. **Plan volume reset**: Be prepared to delete `db-data` volume if incompatibility occurs

### Symptoms of Schema Incompatibility:
- Container starts successfully but application fails
- Database connection works but queries fail silently
- Missing tables/columns errors in logs
- Foreign key constraint violations
- Prototype appears broken despite successful container startup

### Resolution:
```bash
# Stop containers
docker-compose down

# Remove database volume (DESTRUCTIVE - will lose all data)
docker volume rm fc4_db-data

# Restart with fresh database
docker-compose up -d
```

### Prevention Strategy:
- Always test framework changes with fresh database first
- Use explicit version tags instead of `:latest` or `:main`
- Keep separate volume names for different framework versions during testing
- Document all framework version changes in project history

---
**Last Updated**: July 25, 2025
**Context**: This knowledge comes from experience where switching from one Ampersand prototype framework to another caused database schema incompatibility, requiring volume deletion to resolve.
