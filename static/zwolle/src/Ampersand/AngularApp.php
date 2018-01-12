<?php

/*
 * This file is part of the Ampersand backend framework.
 *
 */

namespace Ampersand;

use Exception;
use Ampersand\Core\Concept;
use function Ampersand\Misc\getDirectoryList;
use Ampersand\AmpersandApp;
use Ampersand\Misc\Config;
use Psr\Log\LoggerInterface;
use Ampersand\Interfacing\InterfaceObject;

/**
 *
 * @author Michiel Stornebrink (https://github.com/Michiel-s)
 *
 */
class AngularApp {

    protected $html;
    
    /**
     * 
     * @var \Psr\Log\LoggerInterface
     */
    private $logger;
    
    /**
     * List of css files that must be included in the Angular webapplication
     *
     * @var array
     */
    protected $cssFiles = [];

    /**
     * List of javascript files that must be included in the Angular webapplication
     *
     * @var array
     */
    protected $jsFiles = [];
    
    /**
     * List of items for the extensions menu (in navbar)
     * 
     * @var array 
     */
    protected $extMenu = [];
    
    /**
     * List of items for the refresh menu (in navbar)
     * 
     * @var array
     */
    protected $refreshMenu = [];
    
    /**
     * List of items for the role menu (in navbar)
     * 
     * @var array
     */
    protected $roleMenu = [];

    /**
     * Contains information for the front-end to navigate the user in a certain case (e.g. after COMMIT)
     * 
     * @var array
     */
    protected $navToResponse = [];

    /**
     * Instantiation of Ampersand backend application
     *
     * @var \Ampersand\AmpersandApp
     */
    protected $ampersandApp;

    /**
     * Undocumented function
     *
     * @param \Ampersand\AmpersandApp $ampersandApp
     * @param \Psr\Log\LoggerInterface $logger
     */
    public function __construct(AmpersandApp $ampersandApp, LoggerInterface $logger){
        $this->logger = $logger;
        $this->logger->debug("## BUILD ANGULAR APP ##################################################");
        $this->ampersandApp = $ampersandApp;
    }

    /**
     * Function is called when object is treated as a string
     * @return string
     */
    public function __toString(){
        return $this->html;
    }

    public function addCSS($relativePath){
        $this->cssFiles[] = $relativePath;
    }

    public function addJS($relativePath){
        $this->jsFiles[] = $relativePath;
    }
    
    /**
     * @param string $menu specifies to which part of the menu (navbar) this item belongs to
     * @param string $itemUrl location of html template to use as menu item
     * @param callable function which returns true/false determining to add the menu item or not
     */
    public function addMenuItem(string $menu, string $itemUrl, callable $function){
        switch ($menu) {
            case 'ext':
                $this->extMenu[] = ['url' => $itemUrl, 'function' => $function];
                break;
            case 'refresh':
                $this->refreshMenu[] = ['url' => $itemUrl, 'function' => $function];
                break;
            case 'role':
                $this->roleMenu[] = ['url' => $itemUrl, 'function' => $function];
                break;
            default:
                throw new Exception("Cannot add item to menu. Unknown menu: '{$menu}'", 500);
                break;
        }
    }
    
    public function getMenuItems($menu){
        switch ($menu) {
            case 'ext':
                $arr = $this->extMenu;
                break;
            case 'refresh':
                $arr = $this->refreshMenu;
                break;
            case 'role':
                $arr = $this->roleMenu;
                break;
            default:
                throw new Exception("Cannot get menu items. Unknown menu: '{$menu}'", 500);
                break;
        }
        
        // Filter menu items
        global $container;
        $ampersandApp = $container['ampersand_app'];
        $result = array_filter($arr, function($item) use ($ampersandApp){
            // Execute function which determines if item must be added or not
            return call_user_func_array($item['function'], [$ampersandApp]);
        });
        
        return array_values($result); // reindex array
    }
    
    public function getNavBarIfcs($menu){
        global $container;

        // Filter interfaces for requested part of navbar
        $interfaces = array_filter($container['ampersand_app']->getAccessibleInterfaces(), function(InterfaceObject $ifc) use ($menu){
            switch ($menu) {
                case 'top':
                    if(($ifc->srcConcept->isSession() || $ifc->srcConcept->name == 'ONE') && $ifc->crudR()) return true;
                    else return false;
                case 'new':
                    // crudC, otherwise the atom cannot be created
                    // isIdent (interface expr = I[Concept]), because otherwise a src atom is necesarry, which we don't have wiht +-menu
                    if($ifc->crudC() && $ifc->isIdent()) return true;
                    else return false;
                default:
                    throw new Exception("Cannot get navbar interfaces. Unknown menu: '{$menu}'", 500);
            }
        });
        
        // Create return object
        $result = array_map(function($ifc){
            return array('id' => $ifc->id, 'label' => $ifc->label, 'link' => '/' . $ifc->id);
        }, $interfaces);
        
        return array_values($result); // reindex array
    }

    public function getNavToResponse($case){
        switch ($case) {
            case 'COMMIT':
            case 'ROLLBACK':
                if(array_key_exists($case, $this->navToResponse)) return $this->navToResponse[$case];
                else return null;
                break;
            default:
                throw new Exception("Unsupported case '{$case}' to getNavToResponse", 500);
        }
    }
    
    public function setNavToResponse($navTo, $case = 'COMMIT'){
        switch ($case) {
            case 'COMMIT':
            case 'ROLLBACK':
                $this->navToResponse[$case] = $navTo;
                break;
            default:
                throw new Exception("Unsupported case '{$case}' to setNavToResponse", 500);
        }
    }

    public function buildHtml(){
        $this->addHtmlLine("<!doctype html>");
        $this->addHtmlLine('<html ng-app="AmpersandApp">');
        $this->addHtmlLine('<head>');

        $this->addHtmlLine('<title>'.Config::get('contextName').'</title>');

        // Meta tags
        $this->addHtmlLine('<meta name="viewport" content="width=device-width, initial-scale=1.0"/>');
        $this->addHtmlLine('<meta charset="UTF-8">');
        $this->addHtmlLine('<meta http-equiv="Expires" content="0"/>');
        $this->addHtmlLine('<meta http-equiv="Cache-Control" content="no-store"/>');

        // initSessionId
        $this->addHtmlLine('<script type="text/javascript">var initSessionId = \'' . session_id() . '\';</script>');

        // JQuery
        $this->addHtmlLine('<script src="app/lib/jquery/jquery-1.11.0.min.js"></script>');
        //$this->addHtmlLine('<script src="app/lib/jquery/jquery-migrate-1.2.1.js"></script>');
        $this->addHtmlLine('<script src="app/lib/jquery/jquery-ui-1.10.4.custom.js"></script>');

        // Bootstrap (requires Jquery, loaded above)
        $this->addHtmlLine('<link href="app/lib/bootstrap-3.3.5-dist/css/bootstrap.min.css" rel="stylesheet" media="screen">'); // load boostrap.css before app specific css files that overwrite bootstrap.css
        $this->addHtmlLine('<script src="app/lib/bootstrap-3.3.5-dist/js/bootstrap.min.js"></script>');

        /*
         ********** Angular *********************************
         *
        */
        $this->addHtmlLine('<script src="app/lib/angular/angular.min.js"></script>');
        $this->addHtmlLine('<script src="app/lib/angular/angular-resource.min.js"></script>');
        $this->addHtmlLine('<script src="app/lib/angular/angular-route.min.js"></script>');
        $this->addHtmlLine('<script src="app/lib/angular/angular-sanitize.min.js"></script>');
        /* Third party directives/libraries for angular */
        // angular-ui-switch
        $this->addHtmlLine('<script src="app/lib/angular/angular-ui-switch/angular-ui-switch-adapted.js"></script>');
        $this->addHtmlLine('<link href="app/lib/angular/angular-ui-switch/angular-ui-switch.css" rel="stylesheet" media="screen" type="text/css">');
            
        // angular-busy
        $this->addHtmlLine('<script src="app/lib/angular/angular-busy/angular-busy.min.js"></script>');
        $this->addHtmlLine('<link href="app/lib/angular/angular-busy/angular-busy.min.css" rel="stylesheet" media="screen" type="text/css">');

        // si-table
        $this->addHtmlLine('<script src="app/lib/angular/si-table/si-table.js"></script>');

        // angular-code-mirror
        $this->addHtmlLine('<script src="app/lib/angular/angular-code-mirror/angular-code-mirror.min.js"></script>');
        $this->addHtmlLine('<link href="app/lib/angular/angular-code-mirror/angular-code-mirror.css" rel="stylesheet" media="screen" type="text/css">');

        // ng-storage
        $this->addHtmlLine('<script src="app/lib/angular/angular-ng-storage/ngStorage.min.js"></script>');

        // angular-file-upload
        $this->addHtmlLine('<script src="app/lib/angular/angular-file-upload/angular-file-upload.min.js"></script>');

        // Restangular (with depency for lodash)
        $this->addHtmlLine('<script src="app/lib/restangular/restangular.min.js"></script>');
        $this->addHtmlLine('<script src="app/lib/restangular/lodash.min.js"></script>');

        // jquery UI & bootstrap in native AngularJS
        $this->addHtmlLine('<script src="app/lib/ui-bootstrap/ui-bootstrap-tpls-0.14.3.min.js"></script>');
        // datetimepicker
        $this->addHtmlLine('<script src="app/lib/ui-bootstrap/datetimepicker/datetimepicker.js"></script>');
        $this->addHtmlLine('<link href="app/lib/ui-bootstrap/datetimepicker/datetimepicker.css" rel="stylesheet" media="screen" type="text/css">');
        
        // markdown support
        $this->addHtmlLine('<script src="app/bower_components/marked/lib/marked.js"></script>');
        $this->addHtmlLine('<script src="app/bower_components/angular-marked/dist/angular-marked.js"></script>');

        /*
         ********** CSS *********************************
         *
        */
        // CSS files from app directory
        $files = getDirectoryList(Config::get('pathToAppFolder') . 'css');
        $cssFiles = [];
        foreach ((array)$files as $file){
            if (substr($file,-3) !== 'css') continue;
            if ($file == 'ampersand.css') array_unshift($cssFiles, 'app/css/' . $file); // make sure ampersand.css is listed first
            else $cssFiles[] = 'app/css/' . $file;
        }
        // Add css files to html output
        foreach ($cssFiles as $file) $this->addHtmlLine('<link href="'.$file.'" rel="stylesheet" media="screen" type="text/css">');
            
        // Other css files (from extensions)
        foreach ($this->cssFiles as $file) $this->addHtmlLine('<link href="'.$file.'" rel="stylesheet" media="screen" type="text/css">');

        /*
         ********** App specific javascript ***************
         *
        */
        // AmpersandApp
        $this->addHtmlLine('<script src="app/AmpersandApp.js"></script>');
        $this->addHtmlLine('<script src="app/RouteProvider.js"></script>');

        // AngularApp controler files (both static and generated)
        $files = getDirectoryList(Config::get('pathToAppFolder') . 'controllers');
        foreach ((array)$files as $file){
            if (substr($file,-2) !== 'js') continue;
            $this->addHtmlLine('<script src="app/controllers/'.$file.'"></script>');
        }

        // Javascript files
        $files = getDirectoryList(Config::get('pathToAppFolder') . 'js');
        foreach ((array)$files as $file){
            if (substr($file,-2) !== 'js') continue;
            $this->addHtmlLine('<script src="app/js/'.$file.'"></script>');
        }

        // Add js files to html output
        foreach ($this->jsFiles as $file) $this->addHtmlLine('<script src="'.$file.'"></script>');

        $this->addHtmlLine('</head>');

        $this->addHtmlLine('<body>');

        $this->addHtmlLine(file_get_contents(Config::get('pathToAppFolder') . 'AmpersandApp.html'));

        $this->addHtmlLine('</body>');

        $this->addHtmlLine('</html>');

        return $this->html;
    }

    private function addHtmlLine($htmlLine){
        $this->html .= $htmlLine;
    }

}
