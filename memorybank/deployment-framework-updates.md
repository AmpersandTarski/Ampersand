# Deployment Framework & Update Strategie
## Van Ampersand Prototype Framework naar Landeneisenprototype

*Gegenereerd op: 22 juli 2025*

---

## 🏗️ **Huidige Deployment Architectuur**

### **Docker Compose Setup** (`docker-compose.yml`)
```yaml
services:
  prototype:
    image: ampersand-prototype:latest
    build:
      context: project
    ports:
      - "80:80"
    environment:
      - AMPERSAND_DBHOST=prototype-db
      - AMPERSAND_DBUSER=ampersand
      - AMPERSAND_LOG_CONFIG=logging.debug.yaml
      - AMPERSAND_DEBUG_MODE=true

  prototype-db:
    image: mariadb:10.4
    command: ["--lower-case-table-names=1", "--sql-mode=ANSI,TRADITIONAL"]
    volumes:
      - db-data:/var/lib/mysql
      - ./db-init-scripts:/docker-entrypoint-initdb.d

  phpmyadmin:
    image: phpmyadmin/phpmyadmin:latest
    ports:
      - "8080:80"
```

### **Dockerfile Setup** (`project/Dockerfile`)
```dockerfile
FROM ampersandtarski/prototype-framework:v1.14

# Project bestanden kopiëren
COPY . /usr/local/project/
WORKDIR /usr/local/project

# Prototype genereren uit main.adl
RUN ampersand proto main.adl \
  --proto-dir /var/www \
  --verbose --trim-cellvalues

# Permissies instellen
RUN chown -R www-data:www-data /var/www/log /var/www/data /var/www/generics
```

---

## 🔄 **Framework Update Strategie**

### **1. Upstream Source Tracking**
- **Framework Repository**: https://github.com/AmpersandTarski/prototype
- **Repository Status**: MIT license, 1⭐, 38 open issues (3 "good first issue")
- **Last Activity**: Augustus 2025 (active development)
- **Current Version**: `ampersandtarski/prototype-framework:v1.14`
- **Documentation**: https://ampersandtarski.github.io/
- **Community**: Small but active, contributors welcome
- **Update Frequency**: Per release tag controleren, check GitHub releases page

### **2. Versie Upgrade Process**

#### **Step 1: Framework Version Update**
```dockerfile
# In project/Dockerfile wijzigen:
FROM ampersandtarski/prototype-framework:v1.15  # new version
```

#### **Step 2: Compatibility Check**
```bash
# Test lokaal
cd /path/to/FC4
docker-compose build prototype
docker-compose up -d
# Test interfaces en functionaliteit
```

#### **Step 3: Compiler Version Alignment**
Als nieuwe Ampersand compiler versie nodig is:
```dockerfile
# Optie A: Specifieke compiler van Github releases
ADD https://github.com/AmpersandTarski/Ampersand/releases/download/Ampersand-v4.2.0/ampersand /usr/local/bin/ampersand
RUN chmod +x /usr/local/bin/ampersand

# Optie B: Van development image
COPY --from=ampersandtarski/ampersand:development /bin/ampersand /usr/local/bin
```

---

## ⚠️ **Update Risico's & Mitigatie**

### **Potentiële Breaking Changes**
1. **API wijzigingen** in prototype framework
2. **Database schema** wijzigingen  
3. **Template systeem** updates
4. **Dependency conflicts**

### **Mitigatie Strategie**
```bash
# 1. Backup voor update
docker-compose exec prototype-db mysqldump --all-databases > backup.sql

# 2. Staged rollout
docker-compose stop prototype
docker-compose build prototype
docker-compose up -d

# 3. Rollback indien nodig
git checkout HEAD~1 project/Dockerfile
docker-compose build prototype
```

---

## 🛠️ **Customization Preservation**

### **Template Customizations**
```dockerfile
# Als custom templates bestaan:
COPY ./templates /var/www/templates/
RUN cp -r -v /usr/local/project/shared/templates /var/www/
```

### **Frontend Customizations**  
```dockerfile
# Indien customizations:
RUN cd /var/www \
  && composer install --prefer-dist --no-dev --optimize-autoloader \
  && npm install \
  && gulp build-ampersand \
  && gulp build-project
```

### **Configuration Preservatie**
- Environment variables in `.env`
- Database init scripts in `db-init-scripts/`
- Custom logging config: `logging.debug.yaml`

---

## 📋 **Update Checklist**

### **Pre-Update**
- [ ] Check nieuwe release notes op GitHub
- [ ] Backup database: `docker-compose exec prototype-db mysqldump...`
- [ ] Backup huidige config bestanden
- [ ] Test current state werkt correct

### **During Update**
- [ ] Update Dockerfile `FROM` versie
- [ ] Check deprecated/removed features in release notes
- [ ] Update compiler versie indien nodig
- [ ] Rebuild container: `docker-compose build prototype`

### **Post-Update Testing**
- [ ] Container start succesvol: `docker-compose up -d`
- [ ] Database connectie werkt
- [ ] Alle interfaces toegankelijk op http://localhost
- [ ] CRUD operaties werken
- [ ] Excel import functionaliteit
- [ ] Pleio interface functionality
- [ ] ExecEngine regels werken correct

### **Production Deployment**  
- [ ] Staged deployment in test omgeving
- [ ] Performance check (geen regressies)
- [ ] User acceptance test met key interfaces
- [ ] Monitoring na deployment

---

## 🔧 **Automation Mogelijkheden**

### **GitHub Actions Workflow** (suggestie)
```yaml
name: Framework Update Check
on:
  schedule:
    - cron: '0 9 * * MON'  # Elke maandag om 9:00
  workflow_dispatch:

jobs:
  check-updates:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - name: Check for new framework version
        run: |
          CURRENT=$(grep "FROM ampersandtarski/prototype-framework" project/Dockerfile | cut -d: -f2)
          LATEST=$(curl -s https://api.github.com/repos/AmpersandTarski/Prototype/releases/latest | jq -r .tag_name)
          if [ "$CURRENT" != "$LATEST" ]; then
            echo "New version available: $LATEST (current: $CURRENT)"
            # Create issue or PR
          fi
```

---

## 🎯 **Specific Update Considerations voor FC4**

### **Critical Components**
- **Pleio Interface**: Test POcombinatie CRUD
- **ExecEngine Rules**: Test ENFORCE regel execution  
- **Excel Import**: Test `.xlsx` file processing
- **Template System**: Check custom HTML templates in `/templates/`
- **Database Migrations**: Check schema compatibility

### **Performance Monitoring**
- Container startup time
- Query performance (vooral grote tabellen)
- Memory usage tijdens Excel imports
- Response times voor complexe interfaces

### **Rollback Strategy**
```bash
# Quick rollback
git revert HEAD
docker-compose build prototype  
docker-compose up -d

# Database rollback indien nodig
docker-compose exec prototype-db mysql < backup.sql
```

---

## 📝 **Update Log Template**

```markdown
## Framework Update Log - [DATE]

**From**: ampersandtarski/prototype-framework:v1.14
**To**: ampersandtarski/prototype-framework:v1.15

### Changes Applied:
- [ ] Dockerfile FROM version updated
- [ ] Compiler version: [version]  
- [ ] Custom templates preserved: [Y/N]
- [ ] Database backup created: [filename]

### Testing Results:
- [ ] Container builds successfully
- [ ] All interfaces accessible  
- [ ] Pleio functionality working
- [ ] Excel imports working
- [ ] Performance acceptable

### Issues Found:
- [List any issues]

### Resolution:
- [How issues were resolved]

**Status**: ✅ SUCCESS / ❌ ROLLED BACK
**Deployed**: [timestamp]
**Tested by**: [name]
```

---

## 📝 **Deployment Notes & Lessons Learned**

*Deze sectie wordt bijgehouden met praktische ervaringen, bevindingen en updates*

### **Update History**
*Hier komen logs van daadwerkelijke framework updates*

### **Performance Metrics**  
*Baseline en post-update performance data*

### **Issues & Solutions**
*Problemen tegengekomen en hoe ze zijn opgelost*

### **Best Practices Discovered**
*Nieuwe inzichten tijdens deployment processen*

---

**🎯 Doel**: Deze documentatie zorgt ervoor dat framework updates **vlot en veilig** kunnen worden verwerkt zonder functionaliteit te verliezen.
