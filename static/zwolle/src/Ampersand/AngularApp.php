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
        global $container;
        $ampersandApp = $container['ampersand_app'];

        switch ($menu) {
            // Items for extension menu
            case 'ext':
                $result = array_filter($this->extMenu, function($item) use ($ampersandApp){
                    return call_user_func_array($item['function'], [$ampersandApp]); // execute function which determines if item must be added or not
                });
                break;
            
            // Items for refresh menu
            case 'refresh':
                $result = array_filter($this->refreshMenu, function($item) use ($ampersandApp){
                    return call_user_func_array($item['function'], [$ampersandApp]); // execute function which determines if item must be added or not
                });
                break;
            
            // Items for role menu
            case 'role':
                $result = array_filter($this->roleMenu, function($item) use ($ampersandApp){
                    return call_user_func_array($item['function'], [$ampersandApp]); // execute function which determines if item must be added or not
                });
                break;
            
            // Items in menu to create new resources (indicated with + sign)
            case 'new':
                $result = [];
                foreach ($this->getNavBarIfcs('new') as $ifc) {
                    /** @var \Ampersand\Interfacing\InterfaceObject $ifc */
                    $sort = $ifc->tgtConcept->name; // or sort by classification tree: $sort = $ifc->tgtConcept->getLargestConcept()->name;

                    if(!isset($result[$sort])) $result[$sort] = ['label' => "New {$ifc->tgtConcept->label}", 'ifcs' => []];

                    $result[$sort]['ifcs'][] = ['id' => $ifc->id
                                               ,'label' => $ifc->label
                                               ,'link' => '/' . $ifc->id
                                               ,'resourceType' => $ifc->tgtConcept->name
                                               ];
                }
                break;
            
            // Top level items in menu bar
            case 'top':
                $result = array_map(function(InterfaceObject $ifc){
                    return [ 'id' => $ifc->id
                           , 'label' => $ifc->label
                           , 'link' => '/' . $ifc->id
                           ];
                }, $this->getNavBarIfcs('top'));
                break;
            default:
                throw new Exception("Cannot get menu items. Unknown menu: '{$menu}'", 500);
                break;
        }
        
        return array_values($result); // reindex array
    }
    
    /**
     * Get interfaces for certain use cases
     
     * @param string $menu
     * @return \Ampersand\Interfacing\InterfaceObject[]
     */
    protected function getNavBarIfcs(string $menu): array {
        global $container;

        // Filter interfaces for requested part of navbar
        return array_filter($container['ampersand_app']->getAccessibleInterfaces(), function(InterfaceObject $ifc) use ($menu){
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

    /**
     * Determine if frontend app needs to refresh the session information (like navigation bar, roles, etc)
     * 
     * True when session variable is affected in a committed transaction
     * False otherwise
     * 
     * @return boolean
     */
    public function getSessionRefreshAdvice(): bool {
        static $skipRels = ['lastAccess[SESSION*DateTime]']; // these relations do not result in a session refresh advice

        $affectedRelations = [];
        foreach (Transaction::getTransactions() as $transaction) {
            if(!$transaction->isCommitted()) continue;
            $affectedRelations = array_merge($affectedRelations, $transaction->getAffectedRelations());
        }
        
        foreach (array_unique($affectedRelations) as $relation) {
            // Advise session refresh when src or tgt concept of this relation is SESSION
            if(($relation->srcConcept->isSession() || $relation->tgtConcept->isSession())
                && !in_array($relation->getSignature(), $skipRels)) return true;
        }

        return false;
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

        // Libraries
        $this->addHtmlLine('<link href="app/dist/lib/lib.min.css" rel="stylesheet" media="screen" type="text/css">');
        $this->addHtmlLine('<script src="app/dist/lib/lib.min.js"></script>');
        
        // Ampersand
        $this->addHtmlLine('<link href="app/dist/ampersand.min.css" rel="stylesheet" media="screen" type="text/css">');
        $this->addHtmlLine('<script src="app/dist/ampersand.min.js"></script>');

        // Project specific
        $this->addHtmlLine('<link href="app/dist/project.min.css" rel="stylesheet" media="screen" type="text/css">');
        $this->addHtmlLine('<script src="app/dist/project.min.js"></script>');

        $this->addHtmlLine('</head>');

        $this->addHtmlLine('<body ng-include="\'app/src/app.html\'"></body>');

        $this->addHtmlLine('</html>');

        return $this->html;
    }

    private function addHtmlLine($htmlLine){
        $this->html .= $htmlLine . PHP_EOL;
    }

}
