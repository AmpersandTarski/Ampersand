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
AmpersandApp.factory('ExecEngineRestangular', function(Restangular, $sessionStorage) {
  return Restangular.withConfig(function(RestangularConfigurer) {
    RestangularConfigurer.setBaseUrl('extensions/ExecEngine/api');
    
    RestangularConfigurer.addFullRequestInterceptor(function(element, operation, what, url, headers, params, element, httpConfig){
		var roleIds = [];
		angular.forEach($sessionStorage.sessionRoles, function(role) {
			if (role.active == true) {
				roleIds.push(role.id);
			}
		});
		
		params['roleIds[]'] = roleIds; // the '[]' in param 'roleIds[]' is needed by the API to process it as array
		return params;
	});
  });
  
  
});