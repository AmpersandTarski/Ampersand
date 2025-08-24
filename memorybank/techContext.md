## Technologies Used

### Core Framework
- **Ampersand**: Domain-specific language for rapid prototyping (Haskell-based compiler)
- **ADL (Ampersand Definition Language)**: Declarative business rule specification
- **PHP**: Generated backend application runtime
- **Apache**: Web server for prototype hosting
- **Angular + TypeScript**: Frontend framework voor generated user interfaces
- **SCSS**: Styling framework for responsive design

### AmpersandTarski Ecosystem Stack
- **Compiler:** Haskell (Ampersand repository, GPL-3.0, 43★)
- **Framework:** PHP/Angular (prototype repository, MIT, 1★) - **THIS PROJECT**
- **Templates:** Shell scripts (project-template repository, 2★)
- **Analysis:** C# web tool (RAP repository, 4★) 
- **IDE Support:** TypeScript VS Code extension (5★)
- **Documentation:** JavaScript/Docusaurus (GitHub Pages)
- **Examples:** ADL model collection (ampersand-models, 3★)

### Database & Data
- **MariaDB 10.4**: Primary database with generated schema
- **Excel Import**: Runtime data loading via population scripts
- **CSV Processing**: Alternative data import format

### Deployment & Infrastructure
- **Docker**: Containerization platform
- **Docker Compose**: Local orchestration
- **Kubernetes**: Production deployment (optional)
- **Git**: Version control via Sopra Steria InnerSource

## Development Setup

### Prerequisites
```bash
# Required tools
docker --version          # Docker Desktop or Engine
docker compose version    # Compose V2 
git --version             # Git SCM
```

### Environment Configuration
```bash
# Initial setup
git clone https://innersource.soprasteria.com/stef.joosten/nvwa-fc.git
cd nvwa-fc
cp .env.example .env

# Configure .env variables
MYSQL_ROOT_PASSWORD=<secure_random_password>
MYSQL_AMPERSAND_PASSWORD=<secure_random_password>
SERVER_HOST_NAME=localhost
```

### Build & Run Workflow
```bash
# Development cycle
docker build project -t ampersand-prototype:latest  # Build container
docker compose up -d                               # Start services
docker logs prototype                              # Check build logs
docker logs prototype-db                          # Check DB logs

# Access points
http://localhost        # Prototype application
http://localhost:8080   # phpMyAdmin interface

# Cleanup
docker compose down     # Stop services
docker compose down -v # Stop and remove volumes
```

### Performance Considerations
```bash
# Fast development builds (use smaller datasets)
# Comment out large Excel files in main.adl:
# -- INCLUDE "ExportEisen.xlsx"  # 10+ minute compile time
# Use ExportEisen1-4.xlsx instead for faster iteration

# Memory allocation for large datasets
docker run --memory="4g" ampersand-prototype:latest
```

## Technical Constraints

### Ampersand Framework Limitations
- **Compilation Time**: ExportEisen.xlsx requires 10+ minutes compile time
- **Memory Requirements**: Large Excel files need 4GB+ RAM during build
- **Schema Generation**: Database schema is auto-generated, no manual optimization
- **PHP Runtime**: Generated application runs on specific PHP/Apache stack

### Database Constraints
- **MariaDB Version**: Fixed at 10.4 for Ampersand compatibility
- **Case Sensitivity**: `--lower-case-table-names=1` required
- **SQL Mode**: `ANSI,TRADITIONAL` mode enforced
- **Character Set**: UTF-8 encoding required for international characters

### Development Constraints
- **Build Context**: Must build from `project/` directory
- **File Paths**: All Excel imports relative to ADL script location
- **Port Conflicts**: Ports 80 and 8080 must be available
- **Volume Persistence**: Database data stored in Docker volume `db-data`

## Dependencies

### Docker Images
```yaml
# Core services
prototype:
  build: project/
  image: ampersand-prototype:latest

prototype-db:
  image: mariadb:10.4
  
phpmyadmin:
  image: phpmyadmin/phpmyadmin:latest
```

### Data Dependencies
```
# Critical import order
EPPOcodes.xlsx     → Must be first (organism standardization)
Organismen.xlsx    → After EPPOcodes (organism definitions)
Producten.xlsx     → After EPPOcodes (product definitions)
Pleio.xlsx         → After Organismen + Producten (combinations)
ExportEisen.xlsx   → After all base data (requirements)
```

### Network Dependencies
```yaml
# Docker network
networks:
  prototype:
    # Internal communication between containers
    # prototype ↔ prototype-db
    # phpmyadmin ↔ prototype-db
```

## Tool Usage Patterns

### Development Commands
```bash
# Quick rebuild cycle
docker compose down && docker compose up -d --build

# Database access
docker exec -it prototype-db mysql -u ampersand -p

# Log monitoring
docker compose logs -f prototype     # Application
