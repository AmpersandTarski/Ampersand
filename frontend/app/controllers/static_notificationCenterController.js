AmpersandApp.controller('static_notificationCenterController', ['$scope', '$rootScope', '$routeParams', 'Restangular', function ($scope, $rootScope, $routeParams, Restangular) {
	
	$rootScope.notifications = Restangular.one('notifications/all').get().$object;
	
	$scope.closeAlert = function(alerts, index) {
		alerts.splice(index, 1);
	};
	
}]);
