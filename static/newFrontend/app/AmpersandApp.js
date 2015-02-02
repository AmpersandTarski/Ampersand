var AmpersandApp = angular.module('AmpersandApp', ['ngResource', 'ngRoute', 'restangular', 'ui.bootstrap', 'angular.filter', 'uiSwitch']);

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
		.when('/Projects/:resourceId?',
			{
				controller: 'ProjectsController',
				templateUrl: 'generics/app/views/Projects.html'
			})
		.when('/People/:resourceId?',
			{
				controller: 'PeopleController',
				templateUrl: 'generics/app/views/People.html'
			})
		.when('/Project/:resourceId?',
			{
				controller: 'ProjectController',
				templateUrl: 'generics/app/views/Project.html'
			})
		.when('/Person/:resourceId?',
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
	
	$rootScope.session = {}; // empty object
	$rootScope.session.id = initSessionId; // initSessionId provided by index.php on startup application // Restangular.one('session').get().$object;
	
	Restangular.restangularizeElement('', $rootScope.session, 'session');
	
	Restangular.one('role').get().then(function(data) {
		$rootScope.roleId = data.id; // TODO: do all initiation in one call (i.e. role, navigationbar, etc)
	});
		
	Restangular.addFullRequestInterceptor(function(element, operation, what, url, headers, params, element, httpConfig){
		params['roleId'] = $rootScope.roleId;
		params['sessionId'] = $rootScope.session.id;
		return params;
	});
	
    Restangular.setErrorInterceptor(function(response, deferred, responseHandler) {
    	
    	console.log(response);
    	$rootScope.notifications.errors[$rootScope.notifications.errors.length] = {'message' : response.status + ' ' + response.data.error.message};	
    	
    	return false; // error handled
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