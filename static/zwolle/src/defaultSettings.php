<?php

use Ampersand\Config;

try{
    Config::set('pathToGeneratedFiles', 'global', dirname(dirname(__FILE__)) . '/generics/');
    Config::set('pathToAppFolder', 'global', dirname(dirname(__FILE__)) . '/app/');
    
    // Load settings.json
    $settings = file_get_contents(Config::get('pathToGeneratedFiles') . DIRECTORY_SEPARATOR . 'settings.json');
    $settings = json_decode($settings, true);

    // Settings
    Config::set('versionInfo', 'global', $settings['versionInfo']); // e.g. "Ampersand v3.2.0[master:acbd148], build time: 07-Nov-15 22:14:00 W. Europe Standard Time"
    Config::set('contextName', 'global', $settings['contextName']); // set the name of the application context

    // Mysql settings,  can be overwritten in localSettings.php
    Config::set('dbHost', 'mysqlDatabase', $settings['mysqlSettings']['dbHost']);
    Config::set('dbUser', 'mysqlDatabase', $settings['mysqlSettings']['dbUser']);
    Config::set('dbPassword', 'mysqlDatabase', $settings['mysqlSettings']['dbPass']);
    Config::set('dbName', 'mysqlDatabase', $settings['mysqlSettings']['dbName']);
    Config::set('dbsignalTableName', 'mysqlDatabase', $settings['mysqlSettings']['dbsignalTableName']);

    // Other default configuration
    Config::set('serverURL', 'global', 'http://localhost/' . Config::get('contextName')); // set the base url for the application
    Config::set('apiPath', 'global', '/api/v1'); // relative path to api

    Config::set('sessionExpirationTime', 'global', 60*60); // expiration time in seconds
    Config::set('productionEnv', 'global', false); // set environment as production deployment (or not = default)
    Config::set('debugMode', 'global', true); // set debugMode (or not = default). Impacts the way errors are returned by API

    Config::set('absolutePath', 'global', __DIR__ . '/../');
    Config::set('uploadPath', 'global', 'uploads/'); // absolute path to folder, without trailing slash
    Config::set('allowedMimeTypes', 'global', array('application/vnd.ms-excel'
            ,'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'
            ,'application/excel'
            ,'application/pdf'
            ,'text/xml'
    ));

    Config::set('loginEnabled', 'global', false); // enable/disable login functionality (requires Ampersand script, see localSettings.php)


    Config::set('checkDefaultPopulation', 'transactions', true); // For debugging set to false (commits the initial population, regardless if it has invariant violations)
    Config::set('ignoreInvariantViolations', 'transactions', false); // for debugging can be set to true (transactions will be committed regardless off invariant violations)
    Config::set('interfaceAutoCommitChanges', 'transactions', true); // specifies whether changes in an interface are automatically commited when allowed (all invariants hold)
    Config::set('interfaceCacheGetCalls', 'transactions', false); // specifies whether GET calls should be cached by the frontend (e.g. angular) application

    // Default CRUD rights for interfaces
    Config::set('defaultCrudC', 'transactions', true);
    Config::set('defaultCrudR', 'transactions', true);
    Config::set('defaultCrudU', 'transactions', true);
    Config::set('defaultCrudD', 'transactions', true);

    // Default notification settings
    Config::set('defaultShowSignals', 'notifications', true);
    Config::set('defaultShowInfos', 'notifications', true);
    Config::set('defaultShowWarnings', 'notifications', true);
    Config::set('defaultShowSuccesses', 'notifications', true);
    Config::set('defaultAutoHideSuccesses', 'notifications', true);
    Config::set('defaultShowErrors', 'notifications', true);
    Config::set('defaultShowInvariants', 'notifications', true);

}catch(Exception $e){
    throw $e;
}

?>