<?php

/* FUNCTIONS OF NEWER VERSIONS OF PHP */
require_once (__DIR__ . '/functions/array_column.php'); //TODO: can be removed after PHP update

/* OTHER GENERIC FUNCTIONS */
require_once (__DIR__ . '/functions/getDirectoryList.php');
require_once (__DIR__ . '/functions/JsonPatch.php'); // to compare 2 different Json filesS

/* INCLUDES OF AMPERSAND FRAMEWORK */

require_once (__DIR__ . '/../ampersand/Generics.php'); // loading the Ampersand model

require_once (__DIR__ . '/../db/Database.php');

require_once (__DIR__ . '/Concept.php');
require_once (__DIR__ . '/Atom.php');
require_once (__DIR__ . '/ErrorHandling.php');
require_once (__DIR__ . '/Relation.php');
require_once (__DIR__ . '/Role.php');
require_once (__DIR__ . '/RuleEngine.php');
require_once (__DIR__ . '/Session.php');
require_once (__DIR__ . '/ObjectInterface.php');
require_once (__DIR__ . '/Viewer.php');

require_once (__DIR__ . '/../localSettings.php');

?>