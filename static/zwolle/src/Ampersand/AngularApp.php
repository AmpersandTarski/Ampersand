<?php

/*
 * This file is part of the Ampersand backend framework.
 *
 */

namespace Ampersand;

use Exception;
use Ampersand\Interfacing\InterfaceObject;
use Ampersand\Log\Logger;
use function Ampersand\Helper\getDirectoryList;

/**
 *
 * @author Michiel Stornebrink (https://github.com/Michiel-s)
 *
 */
class AngularApp {

	private $html;
	
	/**
	 * 
	 * @var \Psr\Log\LoggerInterface
	 */
	private $logger;
	
	private static $cssFiles = array();
	private static $jsFiles = array();
    
    /**
     * @var array $extMenu contains potential items for the extensions menu (in navbar)
     */
    private static $extMenu = array();
    
    /**
     * @var array $refreshMenu contains potential items for the refresh menu (in navbar)
     */
    private static $refreshMenu = array();
    
    /**
     * @var array $roleMenu contains potential items for the role menu (in navbar)
     */
    private static $roleMenu = array();

	public function __construct(){
	    $this->logger = Logger::getLogger('FW');
	    $this->logger->debug("## BUILD ANGULAR APP ##################################################");
		$this->buildHtml();
	}

	public function __toString(){
		return $this->html;
	}

	public static function addCSS($relativePath){
		AngularApp::$cssFiles[] = $relativePath;
	}

	public static function addJS($relativePath){
		AngularApp::$jsFiles[] = $relativePath;
	}
    
    /**
     * @param string $menu specifies to which part of the menu (navbar) this item belongs to
     * @param string $itemUrl location of html template to use as menu item
     * @param function function which returns true/false determining to add the menu item or not
     */
    public static function addMenuItem($menu, $itemUrl, $function){
        switch ($menu) {
            case 'ext':
                self::$extMenu[] = array('url' => $itemUrl, 'addItem' => $function);
                break;
            case 'refresh':
                self::$refreshMenu[] = array('url' => $itemUrl, 'addItem' => $function);
                break;
            case 'role':
                self::$roleMenu[] = array('url' => $itemUrl, 'addItem' => $function);
                break;
            default:
                throw new Exception("Cannot add item to menu. Unknown menu: '{$menu}'", 500);
                break;
        }
    }
    
    public static function getMenuItems($menu){
        $session = Session::singleton();
        switch ($menu) {
            case 'ext':
                $arr = self::$extMenu;
                break;
            case 'refresh':
                $arr = self::$refreshMenu;
                break;
            case 'role':
                $arr = self::$roleMenu;
                break;
            default:
                throw new Exception("Cannot get menu items. Unknown menu: '{$menu}'", 500);
                break;
        }
        
        // Filter menu items
        $result = array_filter($arr, function($item) use ($session){
            // Execute function which determines if item must be added or not
            return $item['addItem']($session);
        });
        
        return array_values($result); // reindex array
    }
    
    public static function getNavBarIfcs($menu){
        $session = Session::singleton();
        $navBarIfcs = array();
        
        // Add public interfaces
        $interfaces = InterfaceObject::getPublicInterfaces();
        
        // Add interfaces for active roles
        foreach($session->getActiveRoles() as $role){
            $interfaces = array_merge($interfaces, $role->interfaces());
        }
        
        // Filter duplicate interfaces
        $interfaces = array_unique($interfaces); 
        
        // Filter interfaces for requested part of navbar
        $interfaces = array_filter($interfaces, function($ifc) use ($menu){
            switch ($menu) {
                case 'top':
                    if(($ifc->srcConcept->name == 'SESSION' || $ifc->srcConcept->name == 'ONE') && $ifc->crudR) return true;
                    else return false;
                case 'new':
                    // crudC, otherwise the atom cannot be created
                    // isIdent (interface expr = I[Concept]), because otherwise a src atom is necesarry, which we don't have wiht +-menu
                    if($ifc->crudC && $ifc->isIdent) return true;
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
		$this->addHtmlLine('<script src="app/lib/jquery/jquery-migrate-1.2.1.js"></script>');
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
		$cssFiles = array();
		foreach ((array)$files as $file){
			if (substr($file,-3) !== 'css') continue;
			if ($file == 'ampersand.css') array_unshift($cssFiles, 'app/css/' . $file); // make sure ampersand.css is listed first
			else $cssFiles[] = 'app/css/' . $file;
		}
		// Add css files to html output
		foreach ($cssFiles as $file) $this->addHtmlLine('<link href="'.$file.'" rel="stylesheet" media="screen" type="text/css">');
			
		// Other css files (from extensions)
		foreach (AngularApp::$cssFiles as $file) $this->addHtmlLine('<link href="'.$file.'" rel="stylesheet" media="screen" type="text/css">');

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
		foreach (AngularApp::$jsFiles as $file) $this->addHtmlLine('<script src="'.$file.'"></script>');

		$this->addHtmlLine('</head>');

		$this->addHtmlLine('<body>');

		$this->addHtmlLine(file_get_contents(Config::get('pathToAppFolder') . 'AmpersandApp.html'));

		$this->addHtmlLine('</body>');

		$this->addHtmlLine('</html>');

	}

	private function addHtmlLine($htmlLine){
		$this->html .= $htmlLine;
	}

}
?>