/*
 * Insert code below in controller if the interface has at least 1 editable relation
 * 
 */

// Patch function to update a Resource
$scope.patch = function(ResourceId){
	$scope.ResourceList[ResourceId]
		.patch()
		.then(function(data) {
			$rootScope.updateNotifications(data.notifications);
			$scope.ResourceList[ResourceId] = Restangular.restangularizeElement('', data.content, url);
		});
}