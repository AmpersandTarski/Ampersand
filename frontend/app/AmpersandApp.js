var AmpersandApp = angular.module('AmpersandApp', ['ngResource', 'ngRoute', 'restangular', 'ui.bootstrap', 'angular.filter']);

AmpersandApp.config(function($routeProvider) {
	$routeProvider
		.when('/',
			{
				controller: '',
				templateUrl: 'app/views/static_home.html'
			})
		.when('/Projects/:atom',
			{
				controller: 'ProjectsController',
				templateUrl: 'app/views/Projects.html'
			})
		.when('/Project/:atom',
			{
				controller: 'ProjectController',
				templateUrl: 'app/views/Project.html'
			})
		.when('/People/:atom',
			{
				controller: 'PeopleController',
				templateUrl: 'app/views/People.html'
			})
		.when('/Person/:atom',
			{
				controller: 'PersonController',
				templateUrl: 'app/views/Person.html'
			})
		.when('/installer',
			{
				controller: 'static_installerController',
				templateUrl: 'app/views/static_installer.html'
			})
		.otherwise({redirectTo: '/'});
});

AmpersandApp.config(function(RestangularProvider) {
    RestangularProvider.setBaseUrl('/CB/api/v1'); // Generate: path to API folder
    
    RestangularProvider.addResponseInterceptor(function(data, operation, what, url, response, deferred) {
        return data;
    });
});

AmpersandApp.directive('myShowonhovertr', function (){
	return {
		link : function(scope, element, attrs) {
			element.hide(); // default hide
			
			element.closest('tr').bind('mouseenter', function() {
				element.show();
			});
			element.closest('tr').bind('mouseleave', function() {
				element.hide();
			});
		}
	}
});