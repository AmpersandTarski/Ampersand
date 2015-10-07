// when using minified angular modules, use module('myApp', []).controller('MyController', ['myService', function (myService) { ...
var AmpersandApp = angular.module('AmpersandApp', ['ngResource', 'ngRoute', 'ngSanitize', 'restangular', 'ui.bootstrap', 'uiSwitch', 'cgBusy', 'siTable', 'ng-code-mirror', 'ngStorage', 'angularFileUpload', 'agGrid']);

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

AmpersandApp.run(function(Restangular, $rootScope, $localStorage, $sessionStorage, $location){
	
	$sessionStorage.session = {'id' : initSessionId}; // initSessionId provided by index.php on startup application
	$rootScope.notifications = {'errors' : []};
	
	if($localStorage.roleId === undefined){
		$localStorage.roleId = 0; // set roleId to zero
	}
		
	Restangular.addFullRequestInterceptor(function(element, operation, what, url, headers, params, element, httpConfig){
		params['roleId'] = $localStorage.roleId;
		params['sessionId'] = $sessionStorage.session.id;
		return params;
	});
	
	Restangular.addResponseInterceptor(function(data, operation, what, url, response, deferred){
		
		return data;
	});
	
    Restangular.setErrorInterceptor(function(response, deferred, responseHandler) {
    	var message = ((response.data || {}).error || {}).message || response.statusText;
    	
    	if(response.status == 401) {
    		$localStorage.roleId = 0;
    		$rootScope.refreshNavBar();
    		$location.path('ext/Login');
    	}
    	
    	$rootScope.addError( response.status + ' ' + message);
    	
    	return true; // proceed with success or error hooks of promise
    });
    
    $rootScope.getCurrentDateTime = function (){
		return (new Date);
	}
	
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
}).directive('myNavToOtherInterfaces', function(){
	
	return {
		  restrict		: 'E'
		, scope 		: {ifcs : '=', resource : '=', label : '='} // '=' => two-way bind, '@' => evaluates string (use {{}} in html) 
		, templateUrl	: 'app/views/partials/my_nav_to_other_interfaces.html'
	};
}).filter('unsafe', function($sce){
	return $sce.trustAsHtml;
});