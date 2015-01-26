AmpersandApp.controller('static_installerController', ['$scope', '$rootScope', '$routeParams', 'Restangular', function ($scope, $rootScope, $routeParams, Restangular) {
	
	$scope.install = function(){
		if(confirm('Are you sure?')){
			Restangular.one('installer').get().then(function(data) {
				$rootScope.notifications = data;
				
				// refresh session
				$rootScope.session = Restangular.one('session').get().$object;
				
				// refresh interfaces list
				$rootScope.interfaces = Restangular.all('interfaces/top').getList().$object;
			});
		}
	}
	
}]);