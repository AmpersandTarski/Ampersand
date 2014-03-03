<?php
// Config file that determines how the ampersand database is used to display edges and nodes

require_once (__DIR__.'/../../db/Database.php');
require_once (__DIR__.'/../../Generics.php');

$mappingViewConcepts =  array ( 'ProjectMembers'   => array ('Project', 'Person')
							  , 'ProjectTypes' => array ('Project', 'Type')
							  , 'ProjectYear' => array ('Project', 'Year')
                              );

$mappingViewRelation =  array ( 'ProjectMembers'   => array ('pl', 'member')
							  , 'ProjectTypes' => array ('projectType')
							  , 'ProjectYear' => array ('projectYear')
							  );

// View specific info:
if (isset($_GET['view']))
{
    $view = $_GET['view'];
}else{
    $view = 1;
}

if (isset($_GET['focus']) AND !empty($_GET['focus']) AND $_GET['focus'] !== 'undefined')
{
    $_FOCUS = Database::Escape($_GET['focus']);
}else{
    $_FOCUS = false;
}

$_VIEW = $view;

// Default values:
$_defIMG = array();
$_defIMG['ProjectSymbool']		  = array("star.png",50,50);
$_defIMG['PersonSymbool']  		  = array("person.png",32,32);
$_defIMG['TypeSymbool']  		  = array("type.jpg",64,64);
$_defIMG['YearSymbool']  		  = array("plan.png",64,64);


$_defCOL = array(); 
$_defCOL['Green']  = "rgba(0,200,0,0.8)";
$_defCOL['Yellow'] = "rgba(200,200,0,0.8)";
$_defCOL['Red']    = "rgba(200,0,0,0.8)";
$_defCOL['Blue']   = "rgba(0,0,200,0.8)";
$_defCOL['Black']  = "rgba(0,0,0,0.8)";
$_defCOL['White']  = "rgba(255,255,255,0.8)";

// Base ampersand directory, needed for urls that direct to ampersand. Requires trailing /!
$_BASEAMPERSAND = '../';

$defurl =  '?view=ProjectMembers&focus=';
$_defURL = array();
$_defURL['Project'] = $defurl;
$_defURL['Person'] = $defurl;
$_defURL['Type'] = '?view=ProjectTypes&focus=';
$_defURL['Year'] = '?view=ProjectYear&focus=';



if ($view == 'ProjectMembers')
{
    $configData = array();

    $_NIMG = $_defIMG;
    $configData['nodeimage'] = $_NIMG;

    $_SCOL = $_defCOL; // Status colour
    $configData['statuscolour'] = $_SCOL;

    $_LW = array(); // Line width
    $_LW['pl'] = 2;
    $_LW['member'] = 1;

    $configData['linewidth'] = $_LW;

    $_URL = $_defURL; // URL to go to when clicked on a node with a specific NodeType in this view
    $configData['url'] = $_URL;

    $_CONFIG = array();
    $_CONFIG['parsysparams'] = array('repulsion'=>'2600', 'stiffness'=>'512', 'friction'=>'0.5', 'gravity'=>true);
    $configData['config'] = $_CONFIG;

}

if ($view == 'ProjectTypes')
{
    $configData = array();

    $_NIMG = $_defIMG;
    $configData['nodeimage'] = $_NIMG;

    $_SCOL = $_defCOL; // Status colour
    $configData['statuscolour'] = $_SCOL;

    $_LW = array(); // Line width
    $_LW['projectType'] = 2;

    $configData['linewidth'] = $_LW;

    $_URL = $_defURL; // URL to go to when clicked on a node with a specific NodeType in this view
    $configData['url'] = $_URL;

    $_CONFIG = array();
    $_CONFIG['parsysparams'] = array('repulsion'=>'2600', 'stiffness'=>'512', 'friction'=>'0.5', 'gravity'=>true);
    $configData['config'] = $_CONFIG;
}

if ($view == 'ProjectYear')
{
    $configData = array();

    $_NIMG = $_defIMG;
    $configData['nodeimage'] = $_NIMG;

    $_SCOL = $_defCOL; // Status colour
    $configData['statuscolour'] = $_SCOL;

    $_LW = array(); // Line width
    $_LW['projectYear'] = 1;

    $configData['linewidth'] = $_LW;

    $_URL = $_defURL; // URL to go to when clicked on a node with a specific NodeType in this view
    $configData['url'] = $_URL;

    $_CONFIG = array();
    $_CONFIG['parsysparams'] = array('repulsion'=>'2600', 'stiffness'=>'512', 'friction'=>'0.5', 'gravity'=>true);
    $configData['config'] = $_CONFIG;
}

?>
