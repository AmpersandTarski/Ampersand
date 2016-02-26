AmpersandApp.controller('static_dateTimeController', function ($scope) {
	
	$scope.selected = { value : ''}; // an empty object for temporary storing the input values
	
	$scope.saveDateTimeItem = function(obj, property, patchResource){
		$scope.saveItem(obj, property, patchResource);
	}
	
	$scope.addDateTimeItem = function(obj, property, selected, patchResource){
		if(selected.value != ''){
			$scope.addItem(obj, property, selected, patchResource);
		}else{
			console.log('Empty datetime selected');
		}
	}
	
});