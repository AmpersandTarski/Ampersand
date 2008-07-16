<?
// htmlFunctions.inc.php
// By Bas Joosten (c) Dec 2005

// contains large chunks of HTML code to improve readability
// may not use any global variable or pass variables by reference:
// put it in interfaceActions if it does
function writeTitle($title){
	?><TITLE><? echo $title; ?></TITLE><?
}
function writeCSS_blue(){
	?>
	<style type="text/css">
	<!--
	td,h3,a,p{
		font-size:12pt;
		color:black;
		font-family: Helvetica;
	}
	th{
		text-align:left;
	}
	.cNotice {
		text-decoration: none;
		color: #3A5B7E;
	}
	h3 {
		margin-top:0;
		margin-bottom:0;
	}
	a {
		text-decoration: underline;
		color: #0A2B5E;
	}
	.clickableRow:hover td, .clickableRow:hover{
		background-color: #CCCCDD;
		border-color: #3A5B7E;
		cursor:pointer;
	}
	.clickableRow td{
		text-decoration:underline;
		color:#0A2B5E;
	}
	.hidden{
		border-style:hidden;
		border-collapse:collapse;
		padding:0;
		margin:0;
		border:0;
	}
	.button{
		color: #0A2B5E;
	}
	-->
	</style>
	<?
}
?>