# Ampersand Prototype Administration

## Database Reinstallation Process

**BELANGRIJKE PROCEDURE**: Wanneer het datamodel wijzigt (gele melding "Generated model is changed. You SHOULD reinstall or migrate your application"), moet de database opnieuw geïnstalleerd worden.

## Methoden voor Herinstallatie

### Methode 1: Via Webinterface (GUI)

1. **Navigeer naar de Installer pagina**
   - URL: `http://localhost/#/admin/installer`
   - Deze route is standaard beschikbaar in alle Ampersand prototypes

2. **Alternatief: Via Tool Menu**  
   - Het Tool menu is het **derde icoon van rechts** in de navigatiebalk 
   - Klik op "Reinstall application" in het dropdown menu

3. **Start herinstallatie**
   - Klik op de rode "Reinstall application" knop
   - Wacht tot de groene "Application reinstalled" melding verschijnt

### Methode 2: Via API/Command Line (Aanbevolen voor Automatisering) ✅

**API Endpoint voor herinstallatie:**
```bash
curl http://localhost/api/v1/admin/installer
```

**Belangrijke details:**
- HTTP Method: `GET` (niet POST zoals verwacht)
- URL: `http://localhost/api/v1/admin/installer`
- Response: JSON met succes/fout meldingen
- Geen authentication headers nodig voor lokale development

**Voorbeeld response bij succes:**
```json
{
    "errors": [],
    "warnings": [],
    "infos": [],
    "successes": [
        {
            "message": "Application successfully reinstalled"
        }
    ],
    "invariants": [],
    "signals": []
}
```

**Voordelen van API methode:**
- Volledig geautomatiseerd (geen GUI interactie)
- Geschikt voor CI/CD pipelines
- Kan worden geïntegreerd in scripts
- JSON response voor programmatische verwerking

### Stappen voor Database Reinstallation:

1. **Detectie van wijzigingen**
   - Gele waarschuwing: "Generated model is changed. You SHOULD reinstall or migrate your application"
   - Verschijnt bovenaan de interface

2. **Uitvoering herinstallatie**
   - Via webinterface: `http://localhost/#/admin/installer`
   - Via API: `curl http://localhost/api/v1/admin/installer`

3. **Verificatie**
   - Groene melding "Application reinstalled"
   - Nieuwe menu-items verschijnen in navigatie (indien van toepassing)
   - Gele waarschuwing verdwijnt

### Waarom nodig?
- Wanneer relaties, concepten of datastructuren wijzigen in .adl bestanden
- Na het toevoegen van nieuwe POPULATION declaraties
- Bij wijzigingen in relation signatures (UNI, TOT constraints, etc.)
- Na wijzigingen in INTERFACE declaraties

### Technische Achtergrond
- Herinstallatie genereert nieuwe database schema
- Bestaande data wordt gewist en vervangen door nieuwe POPULATION data
- Nieuwe interfaces worden gegenereerd op basis van gewijzigde INTERFACE declaraties

### Alle Ampersand prototypes werken op deze manier
Deze procedure is standaard voor alle Ampersand prototype frameworks, ongeacht de onderliggende database (SQLite, MySQL, PostgreSQL).

## Excel Populatie Upload (Runtime Data Import)

**BELANGRIJKE PROCEDURE**: Voor dit specifieke FC4 prototype moeten na herinstallatie 5 Excel bestanden handmatig geüpload worden voor runtime populatie data.

### Benodigde bestanden (in volgorde):
1. **Organismen.xlsx** - Basis organisme data
2. **Producten.xlsx** - Basis product data  
3. **Plagen.xlsx** - POcombinatie data (gebruikt organismen)
4. **EPPOcodes.xlsx** - EPPO code referentie data
5. **landeneisen_AZ.xlsx** - Landeneisen datamigratie experiment data

### API Upload Commando's:
```bash
# 1. Upload Organismen.xlsx
curl -X POST http://localhost/api/v1/admin/import -F "file=@project/Organismen.xlsx"

# 2. Upload Producten.xlsx  
curl -X POST http://localhost/api/v1/admin/import -F "file=@project/Producten.xlsx"

# 3. Upload Plagen.xlsx
curl -X POST http://localhost/api/v1/admin/import -F "file=@project/Plagen.xlsx"

# 4. Upload EPPOcodes.xlsx
curl -X POST http://localhost/api/v1/admin/import -F "file=@project/EPPOcodes.xlsx"

# 5. Upload landeneisen_AZ.xlsx
curl -X POST http://localhost/api/v1/admin/import -F "file=@landeneisen_AZ.xlsx"
```

### Wat NIET uploaden:
- **Pleio.xlsx, Landen.xlsx, SnelCodes.xlsx** - Deze zijn al op compile-time geüpload via INCLUDE statements
- Alle bestanden in `parkeren/` directory (experimenteel/backup)
- Backup/test bestanden in root directory (landeneisen_*.xlsx, steekproef*.xlsx, etc.)

### API Details:
- **Endpoint:** `POST http://localhost/api/v1/admin/import`
- **Parameter:** `file=@filepath` (multipart/form-data)
- **Response:** JSON met succesmelding en eventuele warnings
- **Verwachte warnings:** EPPOcodes.xlsx kan duplicate organisme namen warnings geven (automatisch gemerged)

### Verificatie:
- Controleer JSON response voor `"isCommitted": true`
- Controleer voor `"invariantRulesHold": true`
- Succesmelding: `"message": "Imported [filename].xlsx successfully"`
