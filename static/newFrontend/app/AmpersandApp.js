var AmpersandApp = angular.module('AmpersandApp', ['ngResource', 'ngRoute', 'restangular', 'ui.bootstrap', 'angular.filter']);

AmpersandApp.config(function($routeProvider) {
	$routeProvider
		// default start page
		.when('/',
			{
				controller: '',
				templateUrl: 'app/views/static_home.html'
			})
		// installer page
		.when('/installer',
			{
				controller: 'static_installerController',
				templateUrl: 'app/views/static_installer.html'
			})
		// atom parameter is optional
		.when('/Projects/:atom?',
			{
				controller: 'ProjectsController',
				templateUrl: 'generics/app/views/Projects.html'
			})
		.when('/People/:atom?',
			{
				controller: 'PeopleController',
				templateUrl: 'generics/app/views/People.html'
			})
		.when('/Project/:atom?',
			{
				controller: 'ProjectController',
				templateUrl: 'generics/app/views/Project.html'
			})
		.when('/Person/:atom?',
			{
				controller: 'PersonController',
				templateUrl: 'generics/app/views/Person.html'
			})
		.otherwise({redirectTo: '/'});
});

AmpersandApp.config(function(RestangularProvider) {
	
    RestangularProvider.setBaseUrl('api/v1'); // Generate: path to API folder
    
    RestangularProvider.addResponseInterceptor(function(data, operation, what, url, response, deferred) {
        return data;
    });
    
    RestangularProvider.setDefaultHeaders({"Content-Type": "application/json"});
    
});

AmpersandApp.run(function(Restangular, $rootScope){
	
	$rootScope.session = Restangular.one('session').get().$object;
	
	Restangular.one('role').get().then(function(data) {
		$rootScope.roleId = data.id; // TODO: samenvoegen met opvragen session hierboven
	});
		
	Restangular.addFullRequestInterceptor(function(element, operation, what, url, headers, params, element, httpConfig){
		params['roleId'] = $rootScope.roleId;
		params['sessionId'] = $rootScope.session.id;
		return params;
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
}).directive('myShowonhovertable', function (){
	return {
		link : function(scope, element, attrs) {
			element.hide(); // default hide
			
			element.closest('table').bind('mouseenter', function() {
				element.show();
			});
			element.closest('table').bind('mouseleave', function() {
				element.hide();
			});
		}
	}
});