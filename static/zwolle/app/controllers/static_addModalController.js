AmpersandApp.controller('static_addModalController', function($scope, Restangular, $modalInstance, restUrl) {
	
	$scope.ResourceList = Restangular.all(restUrl).getList().$object;
	
	$scope.select = function(id) {
		console.log('click: ' + id);
		$modalInstance.close(id);
	}
	
	$scope.cancel = function () {
		$modalInstance.dismiss('cancel');
	};
	
});