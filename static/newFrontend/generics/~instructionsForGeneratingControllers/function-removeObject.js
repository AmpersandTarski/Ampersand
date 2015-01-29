/*
 * Insert code below in controller if the interface has at least 1 editable relation to another concept (i.e. not primitive datatype)
 * 
 */

// RemoveObject function to remove an item (key) from list (obj).
$scope.removeObject = function(obj, key, ResourceId){
	delete obj[key];
	$scope.patch(ResourceId);
}