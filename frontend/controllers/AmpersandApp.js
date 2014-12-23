var AmpersandApp = angular.module('AmpersandApp', ['ngResource', 'ngRoute', 'restangular', 'fundoo.services', 'ui.bootstrap']);

AmpersandApp.config(function($routeProvider) {
	$routeProvider
		.when('/',
			{
				controller: '',
				templateUrl: 'views/Home.html'
			})
		.when('/Projects/:atom1',
			{
				controller: 'ProjectsController',
				templateUrl: 'views/interfaces/Projects.html'
			})
		.when('/Project/:atom1',
			{
				controller: 'ProjectController',
				templateUrl: 'views/interfaces/Project.html'
			})
		.when('/Project/:atom1/Person/:atom2',
			{
				controller: 'ProjectController',
				templateUrl: 'views/interfaces/Person.html'
			})
		.otherwise({redirectTo: '/'});
});

AmpersandApp.config(function(RestangularProvider) {
    RestangularProvider.setBaseUrl('/CB/api/v1');
    
    RestangularProvider.addResponseInterceptor(function(data, operation, what, url, response, deferred) {
        return data;
    });
});