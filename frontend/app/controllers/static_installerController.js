AmpersandApp.controller('static_installerController', ['$scope', '$rootScope', '$routeParams', 'Restangular', function ($scope, $rootScope, $routeParams, Restangular) {
	
	$scope.installer = Restangular.one('installer').get().then(function(data) {
		$rootScope.notifications = data;
		
	});

	
}]);