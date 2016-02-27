AmpersandApp.controller('static_installerController', function ($scope, $rootScope, $routeParams, Restangular, $localStorage) {
	$scope.installing = false;
	$scope.install = function(){
		$scope.installing = true;
		$scope.installed = false;
		Restangular.one('admin/installer').get().then(function(data) {
			$rootScope.updateNotifications(data);
			
			// deactive all roles
			$rootScope.deactivateAllRoles();
			
			$scope.installing = false;
			$scope.installed = true;
		}, function(){
			$scope.installing = false;
			$scope.installed = false;
		});
	}
	
});