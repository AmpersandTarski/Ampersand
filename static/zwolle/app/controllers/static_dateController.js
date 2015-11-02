AmpersandApp.controller('static_dateController', function ($scope) {
	
	function pad(number) {
		var r = String(number);
		if ( r.length === 1 ) {
			r = '0' + r;
		}
		return r;
	};
	
	function modifyToJSON(obj){
		obj.toJSON = function(){
			return this.getUTCFullYear()
		        + '-' + pad(this.getMonth() + 1) // The getMonth() method returns the month in the specified date according to local time, as a zero-based value (where zero indicates the first month of the year).
		        + '-' + pad(this.getDate());
		};
	};
	
	$scope.selected = { value : ''}; // an empty object for temporary storing the input values
	
	$scope.saveDateItem = function(obj, resourceId){
		modifyToJSON(obj);
		$scope.put(resourceId);
	}
			
	$scope.addDateItem = function(obj, property, selected, resourceId){
		if(selected.value != ''){
			if(obj[property] === null) obj[property] = [];
			
			modifyToJSON(selected.value);
			
			obj[property].push(selected.value);
			selected.value = '';
			$scope.put(resourceId);
		}else{
			console.log('Empty date selected');
		}
	}
});