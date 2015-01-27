/*
 * Insert code below in controller if the interface allows to delete the complete atom. Currently not
 * yet specified in ampersand interface, so add this function by default.
 * 
 */

// Delete function to delete a complete Resource
$scope.deleteResource = function (ResourceId){
	if(confirm('Are you sure?')){
		$scope.ResourceList[ResourceId]
			.remove()
			.then(function(data){
				$rootScope.updateNotifications(data.notifications);
				$location.url('/');
			});
	}
}