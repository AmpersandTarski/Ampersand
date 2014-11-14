var AmpersandApp = angular.module('AmpersandApp', ['ngResource', 'ngRoute']);

AmpersandApp.config(function($routeProvider) {
	$routeProvider
		.when('/',
			{
				controller: '',
				templateUrl: 'views/Home.html'
			})
		.when('/Projects/atom/:atom',
			{
				controller: 'ProjectsController',
				templateUrl: 'views/interfaces/Projects.html'
			})
		.when('/Project/atom/:atom',
			{
				controller: 'ProjectController',
				templateUrl: 'views/interfaces/Project.html'
			})
		.otherwise({redirectTo: '/'});
});
