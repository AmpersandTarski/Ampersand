var AmpersandApp = angular.module('AmpersandApp', ['ngResource', 'ngRoute']);

AmpersandApp.config(function($routeProvider) {
	$routeProvider
		.when('/',
			{
				controller: 'InterfaceObjectController',
				templateUrl: 'views/DefaultInterfaceView.html'
			})
		.when('/Atom',
			{
				controller: 'InterfaceObjectContentController',
				templateUrl: 'views/Atom.html'
			})
		.when('/Projects',
			{
				controller: 'ProjectsController',
				templateUrl: 'views/Projects.html'
			})
		.otherwise({redirectTo: '/'});
});

AmpersandApp.controller('InterfaceObjectController', ['$scope', 'InterfaceObjectFactory', function ($scope, InterfaceObjectFactory) {
	$scope.interfaceObject = InterfaceObjectFactory.get();
}]);


AmpersandApp.controller('InterfaceObjectContentController', ['$scope', 'InterfaceObjectContentFactory', function ($scope, InterfaceObjectContentFactory) {
	$scope.atom = InterfaceObjectContentFactory.get({atom : atom}, function(){
		// function when GET request is succeeded.
	});
	
	$scope.save = function(){
		$scope.atom.$save({atom : atom});	// .... save data to server
	}
	
}]);

AmpersandApp.controller('ProjectsController', ['$scope', 'InterfaceObjectContentFactory', function ($scope, InterfaceObjectContentFactory) {
	$scope.atom = InterfaceObjectContentFactory.get({atom : atom}, function(){
		// function when GET request is succeeded.
	});
	
	$scope.save = function(){
		$scope.atom.$save({atom : atom});	// .... save data to server
	}
	
}]);

AmpersandApp.factory('InterfaceObjectFactory', ['$resource', function($resource){
	var url = 'api/v1/interface/:interface.json';
	
	return $resource(url, {interface : interface}, {
		get: {method:'GET'}
	});
}]);

AmpersandApp.factory('InterfaceObjectContentFactory', ['$resource', function($resource){
	var url = 'api/v1/interface/:interface/atoms/:atom.json';
	
	return $resource(url, {interface : interface}, {
		get: {method:'GET'},
		save: {method:'POST'}
	});
}]);

