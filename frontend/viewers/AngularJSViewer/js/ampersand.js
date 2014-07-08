var AmpersandApp = angular.module('AmpersandApp', ['ngResource', 'ngRoute']);

AmpersandApp.config(function($routeProvider) {
	$routeProvider
		.when('/',
			{
				controller: 'InterfaceObjectController',
				templateUrl: 'http://localhost/CB/viewers/AngularJSViewer/templates/DefaultInterfaceView.html'
			})
		.otherwise({redirectTo: '/'});
});

AmpersandApp.controller('InterfaceObjectController', ['$scope', 'InterfaceObjectFactory', function ($scope, InterfaceObjectFactory) {
	$scope.interfaceObject = InterfaceObjectFactory.get();
}]);

AmpersandApp.controller('InterfaceObjectContentController', ['$scope', 'InterfaceObjectContentFactory', function ($scope, InterfaceObjectContentFactory) {
	$scope.atom = InterfaceObjectContentFactory.get();
}]);

AmpersandApp.factory('InterfaceObjectFactory', ['$resource', function($resource){
	var url = 'http://localhost/CB/api/v1/interface/' + interface + '.json';
	
	return $resource(url, {}, {
		get: {method:'GET'}
	});
}]);

AmpersandApp.factory('InterfaceObjectContentFactory', ['$resource', function($resource){
	var url = 'http://localhost/CB/api/v1/interface/' + interface + '/atom/' + atom + '.json';
	
	return $resource(url, {}, {
		get: {method:'GET'}
	});
}]);

