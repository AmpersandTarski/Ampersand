// Controller for extension app in navigation bar
AmpersandApp.controller('ExecEngineController', function ($scope, $rootScope, $route, Restangular, NotificationService) {	
	
	$scope.run = function (){	
		Restangular.one('execengine/run').get()
		.then(
			function(data){ // success
				NotificationService.updateNotifications(data.notifications);
				
			}, function(){ // error
			
			}
		);
	}
});