AmpersandApp.controller('NotificationCenterController', ['$scope', '$rootScope', '$routeParams', 'NotificationCenterFactory', function ($scope, $rootScope, $routeParams, NotificationCenterFactory) {
	$rootScope.notifications = NotificationCenterFactory.get({}, function(){
		
	});
	
}]);

AmpersandApp.factory('NotificationCenterFactory', ['$resource', function($resource){
	var url = 'api/v1/notificationcenter/all.json';
	
	return $resource(url, {}, {
		get: {method:'GET'}
	});
}]);

