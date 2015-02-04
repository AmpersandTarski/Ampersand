AmpersandApp.config(function($routeProvider) {
	$routeProvider
		// default start page
		.when('/ext/ExecEngine',
			{
				controller: 'ExecEngineController',
				templateUrl: 'extensions/ExecEngine/ui/views/ExecEngine.html'
			});
});

AmpersandApp.controller('ExecEngineController', function ($scope, $rootScope, Restangular) {
	
	$scope.role = Restangular.one('role/name/ExecEngine').get().$object;
    
});