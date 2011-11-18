<?php
error_reporting(E_ALL^E_NOTICE); 
ini_set("display_errors", 1);

require "Interfaces.php"; // defines $dbName, $isDev, $relationTableInfo and $allInterfaceObjects
require "php/DatabaseUtils.php";
?>

<html>
<head>
<link href="css/Ampersand.css" rel="stylesheet" type="text/css"/>
<link href="css/Custom.css" rel="stylesheet" type="text/css"/>
<link href="css/jquery-ui-1.8.css" rel="stylesheet" type="text/css"/>
<script src="js/jquery-1.5.min.js"></script>
<script src="js/jquery-ui-1.8.min.js"></script>
<script src="js/Ampersand.js"></script>
<script type="text/javascript">

function init() {
  initialize();
}

<?php echo generateInterfaceMap($allInterfaceObjects); ?>

</script>
</head>
<body onload="init()">
<div id="Header"><div id="Logo"></div><div id="Decoration"></div></div>

<?php
echo '<div id="TopLevelInterfaces">';
echo topLevelInterfaceLinks($allInterfaceObjects);
echo '</div>';

if (!isset($_REQUEST['interface']) || !isset($_REQUEST['atom'])) {
  echo '<a href="Installer.php">Reset database</a>';
  echo '<h3>Top-level interfaces</h3>';
  echo topLevelInterfaceLinks($allInterfaceObjects);
} else {
  
  
  $interface=$_REQUEST['interface'];
  $atom=$_REQUEST['atom'];
  
  echo '<div id=AmpersandRoot interface='.showHtmlAttrStr($interface).' atom='.showHtmlAttrStr($atom).
       ' editing=false dev="'.($isDev?'true':'false').'">';

  echo '<div id=DbCommandList></div>';
  echo '<div id=PhpLog></div>';
  echo '<div id=IssueList></div>';
  echo '<button class="Button EditButton" onclick="startEditing()">Edit</button>';
  echo '<button class="Button SaveButton" onclick="commitEditing()">Save</button>';
  echo '<button class="Button CancelButton" onclick="cancelEditing()">Cancel</button>';
  echo generateAtomInterfaces($dbName, $allInterfaceObjects[$interface], $atom, true); 

  echo '</div>';
  echo '<div id=Rollback></div>';
  
} ?>
</body>
</html>