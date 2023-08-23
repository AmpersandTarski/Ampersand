---
title: Configuring your application
id: configuring-your-application
---
# Configuring your application

## Local Settings for the client-side application

The file localSettings.php contains code and switches to be used in the client-side application:

1. a switch `debug-mode` to switch debugging information on and off
2. code to switch different forms of logging on and off
3. the time zone used at runtime
4. a switch `display_errors` 
5. a time limit for running PHP.
6. a switch `productionEnv` to switch between development and production mode
7. database credentials for the application to log into the database
8. a switch loginEnabled to allow logging into the system
9. a variable `allowedRolesForImporter` to specify roles with access to the [Excel importer](../the-excel-importer.md).
10. runtime parameters for the [Exec engine](architecture-of-an-ampersand-application#the-execengine).
11. runtime parameters for extensions such as OAuth.