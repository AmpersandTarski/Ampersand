// Controller for extension app in navigation bar
angular.module('AmpersandApp').controller('ExecEngineController', function ($scope, $rootScope, $route, Restangular, NotificationService) {	
	
	$scope.run = function (){	
		Restangular.one('execengine/run').get()
		.then(
			function(data){ // success
                data = data.plain();
				NotificationService.updateNotifications(data.notifications);
				
			}, function(){ // error
			
			}
		);
	}
});