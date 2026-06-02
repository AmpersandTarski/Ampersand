## Systeem Architectuur

### High-level Architecture
```
┌─────────────────┐    ┌──────────────────┐    ┌─────────────────────┐
│ Excel Data      │───▶│ Ampersand        │───▶│ Generated Web App   │
│ (Runtime Import)│    │ Prototype        │    │ (Apache/PHP)        │
└─────────────────┘    │ Compiler         │    └─────────────────────┘
                       └──────────────────┘              │
                                 │                       │
                       ┌──────────────────┐    ┌─────────────────────┐
                       │ ADL Scripts      │    │ MariaDB Database    │
                       │ (.adl files)     │    │ (Generated Schema)  │
                       └──────────────────┘    └─────────────────────┘
```

### Container Architecture
- **prototype**: Ampersand web application (port 80)
- **prototype-db**: MariaDB 10.4 database
- **phpmyadmin**: Database management interface (port 8080)

## Technische Sleutelbeslissingen

### Data Management Strategy
1. **Excel-driven Import**: Runtime data loading from Excel sheets
2. **Generated Database Schema**: Ampersand auto-generates MariaDB tables
3. **Dual Population**: Static (.pop files) + Runtime (Excel imports)

### Business Logic Implementation
1. **Declarative Rules**: Business constraints via Ampersand RULE statements
2. **Automatic Enforcement**: ENFORCE rules maintain data consistency automatically
3. **ExecEngine Automation**: PHP functions for complex data transformations

### User Interface Approach
1. **Generated Interfaces**: Ampersand auto-generates CRUD interfaces
2. **Custom Templates**: HTML templates for UI customization
3. **Role-based Authorization**: Different views per user role

## Design Patterns

### Domain Pattern: Product-Organisme-Land Triangle
```
ProductNaam ──────────── POcombinatie ──────────── Organisme
     │                        │                        │
     │                   ESEid (Export Eis)            │
     │                        │                        │
     └── Product Form ────── LandCode ──────── EPPOcode ─┘
```

### Data Consistency Pattern: EPPO Code Validation
```
WetenschappelijkeNaam ←──[UNI]──→ EPPOcode ←──[INJ]──→ Nederlandse Naam
                                      │
                                 External Standard
                                 (EPPO Database)
```

### Auto-population Pattern: PLEIO Integration
```
Pleio.xlsx Input → POcombinatie → ENFORCE Rules → Auto-generated Relations
     │                                │
     ├── waardplant="waard"     ──→   waard[ProductNaam*Organisme]
     ├── peststatus="present"   ──→   pest[ProductNaam*Organisme]  
     └── seedborne="seed-borne" ──→   seed_borne[ProductNaam*Organisme]
```

## Relaties tussen Componenten

### Core Domain Entities
1. **POcombinatie** (Central Hub)
   - Links ProductNaam to Organisme
   - Foundation for all business rules
   - Source for ESEid generation

2. **ESEid** (Export Requirements)
   - Extends POcombinatie with land-specific rules
   - Contains permit requirements and trace info
   - Main business output

3. **EPPOcode** (Standardization)
   - International organism identifier
   - Ensures data consistency across systems
   - Links to external EPPO database

### Data Flow Dependencies
```
EPPOcodes.xlsx → Organismen.xlsx → Pleio.xlsx → POcombinatie
                                              ↓
Landen.xlsx → LandCode → ESEid ← ProductNaam ← Producten.xlsx
```

## Kritische Implementatiepaden

### 1. Data Import Workflow
```
Container Build → Excel File Loading → Population Scripts → 
Database Schema Generation → Business Rule Validation → UI Generation
```

**Critical Points:**
- Excel file format consistency
- EPPO code validation during import
- Memory usage during large file processing

### 2. Business Rule Enforcement
```
User Input → Rule Validation → ENFORCE Execution → 
ExecEngine Actions → Database Updates → UI Refresh
```

**Critical Points:**
- Rule cycle detection
- ExecEngine performance
- Constraint violation handling

### 3. Performance Optimization Path
```
Small Excel Files (Development) → Rule Optimization → 
Indexing Strategy → Production Excel Files → Load Testing
```

**Critical Points:**
- ExportEisen.xlsx size (10+ min compile time)
- Database query optimization
- Memory management during compilation

## Ampersand Database Structuur

### Conceptentabellen vs Koppeltabellen
**Conceptentabellen** (Concept Tables):
- Representeren een enkel concept/entiteit
- Elk atoom komt precies **één keer** voor in het sleutelveld
- **1 kolom**: Concept zonder attributen (bijv. `dekkingsoort`)  
- **3+ kolommen**: Concept met attributen (bijv. `dekking3` met 28 kolommen)
- **2 kolommen**: Concept met 1 attribuut (ALLEEN als relatie UNI of INJ is)

**Koppeltabellen** (Relation Tables):
- Hebben altijd precies **twee kolommen** (exclusief timestamp)
- Atomen kunnen **nul of meer keer** voorkommen
- Representeren relaties tussen concepten
- **2 kolommen**: Relatie die NIET univalent of injectief is (bijv. `dekking2`)

### Waarom Koppeltabel ≠ Conceptentabel
Een koppeltabel kan nooit een conceptentabel zijn omdat:

1. **Structureel verschil**: 
   - Conceptentabel: Unieke atomen (1:1 mapping concept↔atoom)
   - Koppeltabel: Relaties tussen concepten (M:N mappings mogelijk)

2. **Data duplicatie**:
   - Conceptentabel: Elk atoom is uniek en identificeert precies één concept-instantie
   - Koppeltabel: Atomen kunnen herhaald worden om verschillende relaties uit te drukken

3. **Semantische functie**:
   - Conceptentabel: Definieert "wat bestaat er"
   - Koppeltabel: Definieert "wat is gerelateerd aan wat"

### Database Pattern Recognition
```
Conceptentabel Pattern:
┌─────────────┬──────────────┐
│ SleutelAtom │ Eigenschap1  │ ← Elk SleutelAtoom is uniek
├─────────────┼──────────────┤
│ A001        │ WaardeA      │
│ A002        │ WaardeB      │  
│ A003        │ WaardeC      │
└─────────────┴──────────────┘

Koppeltabel Pattern:
┌─────────────┬─────────────┐
│ ConceptA    │ ConceptB    │ ← Atomen kunnen herhalen
├─────────────┼─────────────┤
│ A001        │ B001        │
│ A001        │ B002        │ ← A001 komt meerdere keren voor
│ A002        │ B001        │ ← B001 komt meerdere keren voor
└─────────────┴─────────────┘
```

## Architecture Constraints

### Ampersand Framework Limitations
- **Compilation Time**: Large datasets cause long build times
- **Memory Usage**: Excel processing requires significant RAM
- **Rule Complexity**: Complex business logic can cause performance issues

### Database Constraints  
- **Schema Generation**: Cannot manually optimize generated schema
- **MariaDB Compatibility**: Must use compatible SQL features
- **Transaction Management**: Limited control over transaction boundaries

### Deployment Constraints
- **Container Dependencies**: Tight coupling between prototype and database
- **Environment Variables**: Sensitive data management via .env files
- **Port Management**: Fixed port allocation for services
