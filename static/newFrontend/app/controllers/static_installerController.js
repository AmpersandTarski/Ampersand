AmpersandApp.controller('static_installerController', function ($scope, $rootScope, $routeParams, Restangular, $localStorage) {
	$scope.installing = false;
	$scope.install = function(){
		$scope.installing = true;
		Restangular.one('installer').get().then(function(data) {
			$rootScope.updateNotifications(data);
			
			// set roleId back to 0
			$localStorage.roleId = 0;
			
			// refresh session
			$rootScope.session = Restangular.one('session').get().$object;
			
			// refresh interfaces list
			$rootScope.interfaces = Restangular.one('interfaces/all').get().$object;
			
			$scope.installing = false;
		}, function(){
			$scope.installing = false;
		});
	}
	
});