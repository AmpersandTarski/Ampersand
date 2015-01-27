/*
 * Insert code below in controller if the interface has at least 1 editable relation to another concept (i.e. not primitive datatype)
 * 
 */

// AddObject function to add a new item (val) to a certain property (property) of an object (obj)
// Also needed by addModal function.
$scope.addObject = function(obj, property, selected, ResourceId){
	if(selected.id === undefined || selected.id == ''){
		console.log('selected id is undefined');
	}else{
		if(obj[property] === null) obj[property] = {};
		obj[property][selected.id] = {'id': selected.id};
		selected.id = ''; // reset input field
		$scope.patch(ResourceId);
	}
}