AmpersandApp.controller('static_dateTimeController', function ($scope) {
	
	$scope.selected = { value : ''}; // an empty object for temporary storing the input values
	
	$scope.saveDateTimeItem = function(obj, resourceId){
		$scope.put(resourceId);
	}
	
	$scope.addDateTimeItem = function(obj, property, selected, resourceId){
		if(selected.value != ''){
			if(obj[property] === null) obj[property] = [];
			
			obj[property].push(selected.value);
			selected.value = '';
			$scope.put(resourceId);
		}else{
			console.log('Empty datetime selected');
		}
	}
	
});