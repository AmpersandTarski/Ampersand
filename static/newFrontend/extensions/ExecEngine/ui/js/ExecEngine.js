AmpersandApp.config(function($routeProvider) {
	$routeProvider
		// default start page
		.when('/ext/ExecEngine',
			{
				controller: 'ExecEngineController',
				templateUrl: 'extensions/ExecEngine/ui/views/ExecEngine.html'
			});
		.when('/ext/CycleExecEngine',
			{
				controller: 'CycleExecEngineController',
				templateUrl: 'extensions/ExecEngine/ui/views/ExecEngine.html'
			});
});

AmpersandApp.controller('ExecEngineController', function ($scope, $rootScope, Restangular) {
	
	$scope.role = Restangular.one('role/name/ExecEngine').get().$object;
    
});

AmpersandApp.controller('CycleExecEngineController', function ($scope, $rootScope, Restangular) {
	
        console.info('true', 'RieksJ was here');
    };
    
});