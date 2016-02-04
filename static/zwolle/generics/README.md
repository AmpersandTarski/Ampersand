The .htaccess file in this folder prevents outside access (not from backend itself) to this folder. DO NOT REMOVE/EDIT this .htaccess file.

All generated config and ampersand model files will be placed in this folder. This includes:
* concepts.json
* conjuncts.json
* interfaces.json
* mysql-installer.json
* relations.json
* rules.json
* settings.json
* tables.json
* views.json

The following files are still in use, but will be phased out and replaced by json files specified above (see ticket #197 and #198):
* Generics.php (contains ampersand model, needed by the framework)
* dbSettings.php
* InstallerDBstruct.php
* InstallerDefPop.php