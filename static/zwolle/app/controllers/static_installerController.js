AmpersandApp.controller('static_installerController', function ($scope, $rootScope, $routeParams, Restangular, $localStorage, NotificationService) {
	$scope.installing = false;
    $scope.installed = false;
    
	$scope.install = function(defPop){
		$scope.installing = true;
		$scope.installed = false;
		Restangular.one('admin/installer').get({defaultPop : defPop}).then(function(data) {
			NotificationService.updateNotifications(data);
			
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