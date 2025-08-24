# Ampersand Prototype Framework Switching Guide

This document explains how to switch between the new Angular-based prototype framework and the old PHP-based framework by modifying the Dockerfile.

## Current Status
- **Active Framework**: New Angular-based (prototype-framework:main)
- **Inactive Framework**: Old PHP-based (prototype-framework:v1.19) - commented out

## How to Switch Frameworks
All you need is to switch dockerfiles. I implemented that by commenting out the inactive dockerfile.
So, the entire operation can be done by uncommenting one part and commenting the other, effectively switching the docker files within the Dockerfiile.

### Switch to NEW Angular Framework (Currently Active)
**In project/Dockerfile, ensure this section is UNCOMMENTED:**

```dockerfile
FROM ampersandtarski/prototype-framework:main
 
COPY . /usr/local/project/
 
# Run ampersand compiler to generated backend json model files (in generics folder)
RUN ampersand proto --no-frontend /usr/local/project/main.adl \
  --proto-dir /var/www/backend \
  --crud-defaults cRud \
  --verbose
 
# Run ampersand compiler to generated new frontend
RUN ampersand proto --frontend-version Angular --no-backend /usr/local/project/main.adl \
  --proto-dir /var/www/frontend/src/app/generated \
  --crud-defaults cRud \
  --verbose

WORKDIR /var/www/frontend
 
# Build + bundle Angular frontend
RUN npx ng build
 
# Copy Angular frontend to public folder in web server
RUN cp -r /var/www/frontend/dist/prototype-frontend/* /var/www/html/
```

### Switch to OLD PHP Framework
**Comment out the above section and UNCOMMENT:**

```dockerfile
FROM ampersandtarski/prototype-framework:v1.19

COPY . /usr/local/project/

WORKDIR /usr/local/project

RUN mkdir -p /var/www/log

# Generate prototype application from folder
RUN ampersand proto main.adl \
  --proto-dir /var/www \
  --verbose --trim-cellvalues

RUN chown -R www-data:www-data /var/www/log /var/www/data /var/www/generics \
  && cd /var/www
```

## Key Differences

| Aspect | New Angular Framework | Old PHP Framework |
|--------|----------------------|-------------------|
| Base Image | `prototype-framework:main` | `prototype-framework:v1.19` |
| Frontend | Angular (separate build) | PHP-based (integrated) |
| Backend | Separate JSON generation | Integrated with frontend |
| Build Process | Two ampersand commands | Single ampersand command |
| Commands | `--no-frontend` + `--frontend-version Angular --no-backend` | Single `proto` command |
| Directories | `/var/www/backend` + `/var/www/frontend` | `/var/www` |

## Switching Process
1. Open `project/Dockerfile`
2. Comment out the current active section (prefix lines with `#`)
3. Uncomment the desired framework section (remove `#` from lines)
4. Save the file
5. Rebuild Docker container

## Notes
- The user can switch back and forth as needed
- Only one framework should be active at a time
- The choice affects the entire prototype generation and serving mechanism
- The new Angular framework provides better frontend capabilities and the atomic object improvements discussed in the GitHub PR #251
