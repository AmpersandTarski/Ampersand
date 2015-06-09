// when using minified angular modules, use module('myApp', []).controller('MyController', ['myService', function (myService) { ...
var AmpersandApp = angular.module('AmpersandApp', ['ngResource', 'ngRoute', 'restangular', 'ui.bootstrap', 'uiSwitch', 'cgBusy', 'siTable']);

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
		.when('/404',
			{
				templateUrl: 'app/views/static_404.html'
			})
		// here, you can add other stuff (e.g. DndTree)
		//.when('/<interfaceId>/:resourceId?',
 		//	{
		//		controller: 'DndTreeController',
		//		templateUrl: 'extensions/DndTree/ui/views/DndTreeViewer.html'
		//	})
		.otherwise({redirectTo: '/404'});
});

AmpersandApp.config(function(RestangularProvider) {
	
    RestangularProvider.setBaseUrl('api/v1'); // Generate: path to API folder
    
    RestangularProvider.setDefaultHeaders({"Content-Type": "application/json"});
    
});

AmpersandApp.run(function(Restangular, $rootScope){
	
	// Declare $rootScope objects
	$rootScope.session = {};
	$rootScope.notifications = {'errors' : []};
	
	
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
    	
    	$rootScope.notifications.errors.push( {'message' : response.status + ' ' + response.data.error.message} );	
    	
    	return false; // error handled
    });
	
});

AmpersandApp.value('cgBusyDefaults',{
	  message:'Loading...',
	  backdrop: true,
	  //templateUrl: 'my_custom_template.html',
	  //delay: 500, // in ms
	  minDuration: 500, // in ms
	  // wrapperClass: 'my-class my-class2'
	});

AmpersandApp.directive('myShowonhoverRow', function (){
	return {
		link : function(scope, element, attrs) {
			element.hide(); // default hide
			
			element.closest('.row-content').bind('mouseenter', function() {
				element.show();
			});
			element.closest('.row-content').bind('mouseleave', function() {
				element.hide();
			});
		}
	}
}).directive('myShowonhoverBox', function (){
	return {
		link : function(scope, element, attrs) {
			element.hide(); // default hide
			
			element.closest('.box').bind('mouseenter', function() {
				element.show();
			});
			element.closest('.box').bind('mouseleave', function() {
				element.hide();
			});
		}
	}
}).directive('myBluronenter', function() {
    return function(scope, element, attrs) {
        element.bind("keydown keypress", function(event) {
            if(event.which === 13) { // 13 = Carriage return
                event.target.blur();

                event.preventDefault();
            }
        });
    };
});