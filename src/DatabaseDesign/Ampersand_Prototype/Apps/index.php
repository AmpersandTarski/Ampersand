<?php
if (!isset($_SERVER['AUTH_USER'])|| $_SERVER['AUTH_USER']=='') {
  if (!isset($_SERVER['PHP_AUTH_USER'])) {
    header('WWW-Authenticate: Basic realm="Ampersand - Bedrijfsregels"');
    echo 'Just enter a name without password. Refresh the page to retry...';
    exit;
  } else {
    DEFINE("USER",str_replace(" ","",$_SERVER['PHP_AUTH_USER']));
  }
} else {
  DEFINE("USER", str_replace("\\", "_", $_SERVER['AUTH_USER']));
}

DEFINE("COMPILATIONS_PATH","comp/".USER."/");
@mkdir(COMPILATIONS_PATH);
DEFINE("FILEPATH","comp/".USER."/uploads/");
@mkdir(FILEPATH);

//prevent undefined indexes
if(isset($_REQUEST['output'])) {DEFINE("REQ_OUTPUT",$_REQUEST['output']);} else {DEFINE("REQ_OUTPUT",'');}
if(isset($_REQUEST['operation'])) {DEFINE("REQ_OPERATION",$_REQUEST['operation']);} else {DEFINE("REQ_OPERATION",-1);}
if(isset($_REQUEST['file'])) {DEFINE("REQ_FILE",$_REQUEST['file']);} else {DEFINE("REQ_FILE",'empty.adl');}

$fullfile = REQ_FILE;
if(isset($_POST['adlbestand'])){
        $tmp_name = $_FILES["uploadfile"]["tmp_name"];
        $i=1;
        $name = str_replace(".adl","",$_FILES["uploadfile"]["name"]).'.v'.$i.'.adl';
        while (file_exists(FILEPATH.$name)){
           $j=$i;
           $i++;
           $name = str_replace(".v".$j.".",".v".$i.".",$name);
        }
        move_uploaded_file($tmp_name, FILEPATH.$name);
        $fullfile = FILEPATH.$name;
}
if(isset($_POST['adllaad'])){
        $fullfile = FILEPATH.$_POST['filename'];
}
if(isset($_POST['adltekst']) || REQ_OPERATION==3 || REQ_OPERATION==2){
   $dtstr = '.'.strftime('%Y%m%d%H%M%S').'.';
   $oldfile = basename(REQ_FILE);
   if (isset($_POST['filename'])) {$oldfile = $_POST['filename'];} 
   if (preg_match("/\.\d{14}\./", $oldfile)==0)
       {echo 'x'; $fullfile = FILEPATH.str_replace(".adl","",$oldfile).$dtstr."adl";}
   else
       $fullfile = FILEPATH.preg_replace("/\.\d{14}\./",$dtstr, $oldfile);
}
$file = basename($fullfile);

DEFINE("LOGPATH","comp/".USER."/log/");
@mkdir(LOGPATH);
//$target = escapeshellcmd(COMPILATIONS_PATH.'log/');
//$source = escapeshellcmd(FILEPATH.$file->getId().'.adl');
//if(!is_dir($target)) mkdir($target) or exit('error:could not create directory '.$target);
if(is_file(LOGPATH.'error.txt')) unlink(LOGPATH.'error.txt');
if(is_file(LOGPATH.'verbose.txt')) unlink(LOGPATH.'verbose.txt');
$descriptorspec = array(
      0 => array("pipe", "r"),  // stdin is a pipe that the child will read from
      //1 => array("pipe", "w"),  // stdout is a pipe that the child will write to
      1 => array("file", LOGPATH."verbose.txt", "a"),  // stdout is a pipe that the child will write to
      //2 => array("pipe", "w") // stderr is a pipe that the child will write to
      2 => array("file", LOGPATH."error.txt", "a") // stderr is a pipe that the child will write to
  //    2 => array("file", "/error-output.txt" ,"a") // stderr is a file to write to
      );
//echo getcwd();
include "operations.php";

if (isset($_REQUEST['operation']) || isset($_POST['adltekst']) || isset($_POST['adlbestand']) || isset ($_POST['adllaad'])){
   if (isset($_POST['adllaad'])|| isset($_POST['adlbestand'])) {$operation = 1;}
   elseif (isset($_POST['adltekst'])) {
          $operation = 1;
          file_put_contents($fullfile,$_POST['adltext']);
   } else $operation = $_REQUEST['operation'];

   $process = proc_open($commandstr[$operation], $descriptorspec, $pipes, getcwd());
   if (is_resource($process)) {
        fclose($pipes[0]);
       //fclose($pipes[2]);
       //$perr = stream_get_contents($pipes[2]);
       //$pout = stream_get_contents($pipes[1]);
       //fclose($pipes[1]);
       //fclose($pipes[2]);
       // It is important that you close any pipe before calling
       // proc_close in order to avoid a deadlock
       $return_value = proc_close($process);
   }

   $errorlns = '';
   $verboselns = '';
   if (file_exists(LOGPATH.'error.txt')){
       foreach( file ( escapeshellcmd(LOGPATH.'error.txt')) as $line)
              {$errorlns = $errorlns.'<p>'.$line.'</p>'; }
   }
   if (file_exists(LOGPATH.'verbose.txt'))
       foreach( file ( escapeshellcmd(LOGPATH.'verbose.txt')) as $line)
              {$verboselns = $verboselns.'<p>'.$line.'</p>'; }
} //end running the operation
?>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/strict.dtd">
<HTML>
<HEAD>
<?php
if (isset($operation) && $compileurl[$operation]!='' && $errorlns=='')
   $url = $compileurl[$operation];
if (isset($_REQUEST['adlhuidige']))
   $url = $compileurl[1];
if (isset($_REQUEST['logout']))
   $url = 'logout.htm';
if (isset($url)){
	$x = explode('?',$url);
	if (file_exists($x[0])) echo '<meta HTTP-EQUIV="REFRESH" content="0; url='.$url.'">';
	else $notarget=True;
}

?>
<TITLE>Ampersand</TITLE>
<SCRIPT type="text/javascript" src="overlib421/overlib.js"><!-- overLIB (c) Erik Bosrup --></SCRIPT>
<link rel="stylesheet" type="text/css" href="style.css" />
</HEAD>
<BODY>
  <div id="overDiv" style="position:absolute; visibility:hidden; z-index:1000;"></div><!--needed for overLIB-->

<p><div style="width:100%;background-color:#ffffff;margin:0px;padding:0px;top:0px"> <img style="margin:0px;padding:0px" src="ou.jpg">
</div></p>
<?php
if (isset($notarget)) echo '<H2>Pagina '.$url.' bestaat niet. Waarschuw de systeembeheerder.</H2>';
if (isset($operation)){
   if ($compileurl[$operation]!='' && $errorlns=='')
      echo '<A HREF="'.$compileurl[$operation].'">klik hier om naar de output te gaan.</A>';
   echo '<p>'.$commandstr[$operation].'</p>';
   echo $verboselns;
   echo $errorlns;
}else{
   echo '<FORM name="myForm" action="'.$_SERVER['PHP_SELF'].'" method="POST" enctype="multipart/form-data">';
   echo '<H1>Laad de context in de Atlas...</H1>';
   if (file_exists(COMPILATIONS_PATH.'ctxAtlas.php')){
     echo '<p><input type="submit" name="adlhuidige" value="... die '.USER.' het laatst geladen heeft"/>';
     echo '<a href="javascript:void(0);"';
     echo 'onmouseover="return overlib(\'<p>Let op! Eventuele wijzigingen in het tekstveld gaan verloren!</p>\',WIDTH, 350);"';
     echo 'onmouseout="return nd();">';
     echo '<IMG SRC="warning.png" /></a></p>';
   }

   echo '<p><input type="submit" name="adlbestand" value="... uit bestand op uw computer" />';
   echo '<a href="javascript:void(0);"';
   echo 'onmouseover="return overlib(\'<p>Let op! Eventuele wijzigingen in het tekstveld gaan verloren!</p><p>Let op! Eventuele niet gecommitte wijzigingen in de laatst geladen context in de Atlas gaan verloren!</p>\',WIDTH, 350);"';
   echo 'onmouseout="return nd();">';
   echo '<IMG SRC="warning.png" /></a>';
   echo '<input type="file" name="uploadfile" /></p>';

   if (!isset($_REQUEST['browse'])){     
     echo '<p><input type="submit" name="adltekst" value="... uit onderstaand tekstveld (bewaar)"/>';
     echo '<a href="javascript:void(0);"';
     echo 'onmouseover="return overlib(\'<p>Let op! Eventuele niet gecommitte wijzigingen in de laatst geladen context in de Atlas gaan verloren!</p>\',WIDTH, 350);"';
     echo 'onmouseout="return nd();">';
     echo '<IMG SRC="warning.png" /></a></p>';

     echo '<p><input type="submit" name="adllaad" value="... uit in tekstveld geladen versie"/>';
     echo '<a href="javascript:void(0);"';
     echo 'onmouseover="return overlib(\'<p>Let op! Eventuele wijzigingen in het tekstveld gaan verloren!</p><p>Let op! Eventuele niet gecommitte wijzigingen in de laatst geladen context in de Atlas gaan verloren!</p>\',WIDTH, 350);"';
     echo 'onmouseout="return nd();">';
     echo '<IMG SRC="warning.png" /></a>';
     echo '<a href="javascript:void(0);"';
     echo 'onmouseover="return overlib(\'<p>De laadoptie &quot;uit onderstaand tekstveld (save)&quot; creëert een nieuwe contextversie<p/><p>Als u geen wijzigingen in het tekstveld heeft gemaakt, of deze niet wil opslaan, gebruik dan deze laadoptie</p><p>De contextversie uit serverbestand '.$fullfile.' zal worden geladen.</p>\',WIDTH, 350);"';
     echo 'onmouseout="return nd();">';
     echo '<IMG SRC="info.png" /></a></p>';

     echo '<p>Naam van contextversie in tekstveld ';
     echo '<input type="text" name="filename" value="'.$file.'"/>';
     echo '<a href="javascript:void(0);"';
     echo 'onmouseover="return overlib(\'<p>U mag deze naam wijzigingen.</p><p>Als u laadoptie &quot;... uit onderstaand tekstveld (save)&quot; gebruikt, dan wordt de contextversie in het tekstveld onder deze naam in een serverbestand opgeslagen.</p><p>Een timestamp zal toegevoegd (of ververst) worden.</p>\',WIDTH, 350);"';
     echo 'onmouseout="return nd();">';
     echo '<IMG SRC="info.png" /></a></p>';
     ?>
     <P><textarea name="adltext" cols=100 rows=30><?php
     $i=0;
     foreach( file (escapeshellcmd($fullfile)) as $line){
       $i++;
       $line = (preg_replace('/{-(\d+)-}/','',$line));
       echo '{-'.$i.'-}'.$line;
     }?></textarea></P>
     <?php
   }
   echo '</FORM>';
}
?>
</BODY>
</HTML>

