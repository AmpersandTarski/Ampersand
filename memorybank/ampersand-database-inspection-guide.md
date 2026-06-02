# Ampersand Database Inspection Guide

## Overzicht
Deze handleiding beschrijft hoe je database gegevens kunt bekijken in een draaiend Ampersand prototype via Docker containers.

## Voorwaarden
- Docker is geïnstalleerd en draait
- Er draait een Ampersand prototype met een database container
- Je hebt toegang tot de database credentials (meestal `ampersand` / `ampersand`)

## Stap-voor-stap instructies

### 1. Controleer draaiende containers
```bash
docker ps
```
Zoek naar je database container (bijvoorbeeld `prototype-db` met MariaDB/MySQL).

### 2. Bekijk beschikbare databases
```bash
docker exec -it <database-container-name> mysql -u <username> -p'<password>' -e "SHOW DATABASES;"
```

Voorbeeld:
```bash
docker exec -it prototype-db mysql -u ampersand -p'ampersand' -e "SHOW DATABASES;"
```

### 3. Bekijk tabellen in een database
```bash
docker exec -it <database-container-name> mysql -u <username> -p'<password>' <database-name> -e "SHOW TABLES;"
```

Voorbeeld:
```bash
docker exec -it prototype-db mysql -u ampersand -p'ampersand' fc -e "SHOW TABLES;"
```

### 4. Bekijk tabel structuur
```bash
docker exec -it <database-container-name> mysql -u <username> -p'<password>' <database-name> -e "DESCRIBE <table-name>;"
```

Voorbeeld:
```bash
docker exec -it prototype-db mysql -u ampersand -p'ampersand' fc -e "DESCRIBE landcode;"
```

### 5. Query specifieke data
```bash
docker exec -it <database-container-name> mysql -u <username> -p'<password>' <database-name> -e "SELECT * FROM <table-name> WHERE <condition>;"
```

Voorbeeld:
```bash
docker exec -it prototype-db mysql -u ampersand -p'ampersand' fc -e "SELECT * FROM landcode WHERE LandCode = 'BE';"
```

### 6. Tel records in een tabel
```bash
docker exec -it prototype-db mysql -u ampersand -p'ampersand' fc -e "SELECT COUNT(*) FROM <table-name>;"
```

## Veelvoorkomende Ampersand tabellen

### Standaard Ampersand tabellen:
- `__ampersand_model_history__` - Model versie geschiedenis
- `__conj_violation_cache__` - Cached regel violaties
- `session` - Sessie informatie
- `role` - Rollen definitie

### Concept tabellen:
In Ampersand worden concepten omgezet naar database tabellen. De tabelnamen komen overeen met de concept namen in je ADL files.

### Relatie tabellen:
Relaties tussen concepten worden ook als tabellen opgeslagen, meestal met samengestelde primary keys.

## Troubleshooting

### Geen output bij queries:
- Controleer of de tabel data bevat: `SELECT COUNT(*) FROM <table-name>;`
- Mogelijk zijn er geen records die voldoen aan je WHERE clausule

### Connection errors:
- Controleer of de container draait: `docker ps`
- Verificeer database credentials
- Controleer of je de juiste database naam gebruikt

### Command not found:
Als `mysql` niet beschikbaar is op je systeem, gebruik dan altijd `docker exec` om commands uit te voeren binnen de database container.

## Interactieve database sessie

Voor complexere queries kun je een interactieve sessie starten:
```bash
docker exec -it prototype-db mysql -u ampersand -p'ampersand' fc
```

Dan kun je interactief SQL queries uitvoeren.

## Praktische tips

1. **Database backup maken:** Voor je grote wijzigingen doet
2. **Primary keys controleren:** Gebruik `DESCRIBE` om te zien welke kolommen de primary key vormen
3. **Foreign key relaties:** Ampersand gebruikt vaak samengestelde keys voor relaties
4. **Timestamps:** Veel tabellen hebben een `ts_insertupdate` kolom voor audit trails
5. **Case sensitivity:** Let op hoofdletters/kleine letters in tabelnamen

## Voorbeeld debugging scenario

Bij infinite loops in ExecEngine:
1. Bekijk de `__conj_violation_cache__` tabel voor actieve violations
2. Controleer de relevante concept tabellen voor conflicterende data
3. Zoek naar records die alternerende waardes kunnen veroorzaken

```bash
# Voorbeeld: debugging defaultTaal infinite loop
docker exec -it prototype-db mysql -u ampersand -p'ampersand' fc -e "SELECT * FROM landcode WHERE defaultTaal IS NULL;"
docker exec -it prototype-db mysql -u ampersand -p'ampersand' fc -e "SELECT * FROM officieletaal WHERE LandCode = 'BE';"
```

## Beveiliging

- Gebruik nooit productie credentials in voorbeelden
- Database passwords niet hardcoden in scripts
- Gebruik omgevingsvariabelen voor credentials in productie
- Beperk database toegang tot alleen benodigde IP adressen

---

**Opmerking:** Deze guide is gebaseerd op standaard Ampersand prototype opzet met MariaDB/MySQL. Voor andere database systemen kunnen de commando's licht verschillen.
