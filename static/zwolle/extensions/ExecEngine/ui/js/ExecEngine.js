// Controller for extension app in navigation bar
AmpersandApp.controller('ExecEngineController', function ($scope, $rootScope, $route, ExecEngineRestangular) {	
	
	$scope.run = function (){	
		ExecEngineRestangular.one('run').get()
		.then(
			function(data){ // success
				$rootScope.updateNotifications(data.notifications);
				
			}, function(){ // error
			
			}
		);
	}
});

// Restangular service for the ExecEngine
AmpersandApp.factory('ExecEngineRestangular', function(Restangular) {
  return Restangular.withConfig(function(RestangularConfigurer) {
    RestangularConfigurer.setBaseUrl('extensions/ExecEngine/api');
  });
});