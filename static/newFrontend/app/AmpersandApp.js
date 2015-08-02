// when using minified angular modules, use module('myApp', []).controller('MyController', ['myService', function (myService) { ...
var AmpersandApp = angular.module('AmpersandApp', ['ngResource', 'ngRoute', 'restangular', 'ui.bootstrap', 'uiSwitch', 'cgBusy', 'siTable', 'ng-code-mirror']);

AmpersandApp.config(function($routeProvider) {
	$routeProvider
		// default start page
		.when('/',
			{	controller: ''
			,	templateUrl: 'app/views/static_home.html'
			,	interfaceLabel: 'Home'
			})
		// installer page
		.when('/installer',
			{	controller: 'static_installerController'
			,	templateUrl: 'app/views/static_installer.html'
			,	interfaceLabel: 'Installer'
			})
		.when('/404',
			{	templateUrl: 'app/views/static_404.html'
			,	interfaceLabel: '404'
			})
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
	
	Restangular.addResponseInterceptor(function(data, operation, what, url, response, deferred){
		if ($.isEmptyObject(data)){
			$rootScope.notifications.errors.push( {'message' : 'Nothing here...'} );
		}
		return data;
	});
	
    Restangular.setErrorInterceptor(function(response, deferred, responseHandler) {
    	
    	$rootScope.notifications.errors.push( {'message' : response.status + ' ' + response.data.error.message} );	
    	
    	return true; // proceed with success or error hooks of promise
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
}).filter('toArray', function() {
	// used from: https://github.com/petebacondarwin/angular-toArrayFilter
	return function (obj, addKey) {
	    if (!obj) return obj;
	    if ($.isArray(obj)) return obj; // obj is already an array
	    if ( addKey === false ) {
	      return Object.keys(obj).map(function(key) {
	        return obj[key];
	      });
	    } else {
	      return Object.keys(obj).map(function (key) {
	        return Object.defineProperty(obj[key], '$key', { enumerable: false, value: key});
	      });
	    }
	  };
}).directive('myNavToInterfaces', function(){
	
	return {
		  restrict		: 'E'
		, scope 		: {ifcs : '=', resource : '=', label : '='} // '=' => two-way bind, '@' => evaluates string (use {{}} in html) 
		, templateUrl	: 'app/views/partials/my_nav_to_interfaces.html'
	};
});