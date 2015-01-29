/*
 * Insert code below in controller if the interface contains an editable relation to the primitive 
 * datatype DATE
 * 
 */

// Function for Datapicker
$scope.datepicker = []; // empty array to administer if datepickers (can be multiple on one page) are open and closed
$scope.openDatepicker = function($event, datepicker) {
	$event.preventDefault();
	$event.stopPropagation();
	
	$scope.datepicker[datepicker] = {'open' : true};
}