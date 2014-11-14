AmpersandApp.controller('NotificationCenterController', ['$scope', '$routeParams', 'NotificationCenterFactory', function ($scope, $routeParams, NotificationCenterFactory) {
	$scope.notifications = NotificationCenterFactory.get({}, function(){
		
	});
	
}]);

AmpersandApp.factory('NotificationCenterFactory', ['$resource', function($resource){
	var url = 'api/v1/notificationcenter/all.json';
	
	return $resource(url, {}, {
		get: {method:'GET'}
	});
}]);