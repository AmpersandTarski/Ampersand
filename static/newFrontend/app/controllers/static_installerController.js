AmpersandApp.controller('static_installerController', ['$scope', '$rootScope', '$routeParams', 'Restangular', function ($scope, $rootScope, $routeParams, Restangular) {
	$scope.installing = false;
	$scope.install = function(){
		$scope.installing = true;
		Restangular.one('installer').get().then(function(data) {
			$rootScope.notifications = data;
			
			// refresh session
			$rootScope.session = Restangular.one('session').get().$object;
			
			// refresh interfaces list
			$rootScope.interfaces = Restangular.one('interfaces/all').get().$object;
			
			$scope.installing = false;
		}, function(){
			$scope.installing = false;
		});
	}
	
}]);