$( document ).ready(function() {
	$("tr").hover(
		function(){
			$(this).find(".glyphicon").show();
		},
		function(){
			$(this).find(".glyphicon").hide();
		}
	);
});