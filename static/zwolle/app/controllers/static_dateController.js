AmpersandApp.controller('static_dateController', function ($scope) {
	
	$scope.isOpen = false;
	
	$scope.openDatepicker = function($event){
		$event.preventDefault();
		$event.stopPropagation();
		$scope.isOpen = true;
	}
	
	function pad(number) {
		var r = String(number);
		if ( r.length === 1 ) {
			r = '0' + r;
		}
		return r;
	};
	
	function modifyToJSON(obj){
		if(obj != null){
			obj.toJSON = function(){
				return this.getUTCFullYear()
			        + '-' + pad(this.getMonth() + 1) // The getMonth() method returns the month in the specified date according to local time, as a zero-based value (where zero indicates the first month of the year).
			        + '-' + pad(this.getDate());
			};
		}
	};
	
	$scope.selected = { value : ''}; // an empty object for temporary storing the input values
	
	$scope.saveDateItem = function(obj, property, patchResource){
		modifyToJSON(obj[property]);
		$scope.saveItem(obj, property, patchResource);
	}
			
	$scope.addDateItem = function(obj, property, selected, patchResource){
		if(selected.value != ''){
			modifyToJSON(selected.value);
			$scope.addItem(obj, property, selected, patchResource);
		}else{
			console.log('Empty date selected');
		}
	}
});