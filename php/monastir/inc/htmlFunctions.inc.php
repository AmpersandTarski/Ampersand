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
	td,h3,a,p{
		font-size:12pt;
		color:black;
		font-family: Helvetica;
	}
	input.text{
		margin: 4px 4px 4px 0px;
	}
	p,th,td,h4{
		margin-top: 0pt;
		margin-bottom:0pt;
		margin-left: 4px;
		margin-right: 4px;
		padding: 0px;
	}
	h1 { margin-bottom:6pt; }
	p,h4{
		margin-bottom: 0pt;
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
	a.menuItem {
		text-decoration: underline;
		color: #0A2B5E;
		width:100%;
	}
	a.selected{
		text-decoration: underline;
		color: #0A2B5E;
		display:block;
		padding:3pt 0pt 3pt 0pt;
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
	.raised{
	}
	.tail{
		display:block;
		width:100%;
		height:0px;
		margin-top:4pt
	}
	.button{
		color: #0A2B5E;
	}
    html,body{
      margin:0;
      padding:0;
      height:100%;
      border:none
    }
	</style>
	<?
}
?>