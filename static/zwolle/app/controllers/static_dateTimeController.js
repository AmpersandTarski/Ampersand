AmpersandApp.controller('static_dateTimeController', function ($scope) {
	
	$scope.selected = { value : ''}; // an empty object for temporary storing the input values
	
	$scope.saveDateTimeItem = function(obj, property, resourceId){
		$scope.saveItem(obj, property, resourceId);
	}
	
	$scope.addDateTimeItem = function(obj, property, selected, resourceId){
		if(selected.value != ''){
			$scope.addItem(obj, property, selected, resourceId);
		}else{
			console.log('Empty datetime selected');
		}
	}
	
});