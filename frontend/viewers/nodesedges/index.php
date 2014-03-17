<?php
require_once (__DIR__.'/config.php');

$bg = '';
if (isset($_BGURL)) $bg = '<iframe id="background" src="' . $_BGURL . '"></iframe>';
if (isset($_BGIMG)) $bg = '<div id="background"><img src="' . $_BGIMG . '"></div>';

$view = '';
if (isset($_GET['view'])) $view = $_GET['view'];
$focus = '';
if (isset($_GET['focus'])) $focus = $_GET['focus'];

$table = '';

$xtv = $xsv = $xav = $xscv = $xov = $xnv = '';
if ($view == 'ProjectMembers') $xtv = 'class="selected"';
if ($view == 'ProjectTypes')  $xsv = 'class="selected"';
if ($view == 'ProjectYear') $xav = 'class="selected"';


$table = <<<TBL
<div id="tablediv">
<table>
    <tr><td $xtv><a href="?view=ProjectMembers"><img class="table" src="images/star.png" alt="" title="Project members"/></a></td>
        <td $xsv><a href="?view=ProjectTypes"><img class="table" src="images/type.jpg" alt="" title="Project types"/></a></td></tr>
    <tr><td $xav><a href="?view=ProjectYear"><img class="table" src="images/plan.png" alt="" title="Project years"/></a></td>
        <td $xscv></td></tr>
</table>
</div>
TBL;

?>
<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01//EN"
   "http://www.w3.org/TR/html4/strict.dtd">
<html lang="en">
<head>
        <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
        <link rel="stylesheet" href="style.css" type="text/css" charset="utf-8">
</head>
<body>
<canvas id="viewport" width="800" height="600"></canvas>
<script src="js/jquery.js"></script>
<script src="lib/arbor.js"></script>
<script src="lib/arbor-graphics.js"></script>
<script src="lib/arbor-tween.js"></script>
<script src="lib/creategraph.js"></script>
<script>

$.extend({
    getUrlVars: function( item ) {
        var vars = {};
        var parts = window.location.href.replace(/[?&]+([^=&]+)=([^&]*)/gi, function(m,key,value) { vars[key] = value; });
        return vars[item];
    }
});

$.ajax({
url: "ajax_view.php?view=" + $.getUrlVars('view'),
method: 'POST'
});

</script>
<?php echo $bg;?>
<?php echo $table;?>
<div id="savecoords">
<table>
<tr><td class="store"><img class="icon" src="images/save.png" onClick="javascript:SaveView();" title="Save locations"></td>
<td class="clear"><img class="icon" src="images/trash.png" onClick="javascript:ClearSavedData();" title="Clear locations"></td></tr>
</table>
</div>
</body>
</html>

