// when using minified angular modules, use module('myApp', []).controller('MyController', ['myService', function (myService) { ...
var AmpersandApp = angular.module('AmpersandApp', ['ngResource', 'ngRoute', 'ngSanitize', 'restangular', 'ui.bootstrap', 'uiSwitch', 'cgBusy', 'siTable', 'ng-code-mirror', 'ngStorage', 'angularFileUpload', 'agGrid', 'ui.bootstrap.datetimepicker']);

AmpersandApp.config(function($routeProvider) {
	$routeProvider
		// default start page
		.when('/',
			{	controller: ''
			,	templateUrl: 'app/views/static_home.html'
			,	interfaceLabel: 'Home'
			})
		// installer page
		.when('/admin/installer',
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

AmpersandApp.run(function(Restangular, $rootScope, $localStorage, $sessionStorage, $location, $route){
	
	$sessionStorage.session = {'id' : initSessionId}; // initSessionId provided by index.php on startup application
		
	Restangular.addFullRequestInterceptor(function(element, operation, what, url, headers, params, element, httpConfig){
		var roleIds = [];
		angular.forEach($sessionStorage.sessionRoles, function(role) {
			if (role.active == true) {
				roleIds.push(role.id);
			}
		});
		
		params['roleIds[]'] = roleIds; // the '[]' in param 'roleIds[]' is needed by the API to process it as array
		return params;
	});
	
	Restangular.addResponseInterceptor(function(data, operation, what, url, response, deferred){
		
		return data;
	});
	
    Restangular.setErrorInterceptor(function(response, deferred, responseHandler) {
    	
    	// 401: Unauthorized, 440: Login Timeout
    	if(response.status == 401 || response.status == 440) {
    		$rootScope.deactivateAllRoles();
    		$location.path(''); // TODO: redirect to login page (if exists)
    	}
    	
    	if(typeof response.data === 'object'){
    		var message = response.data.msg || response.statusText; // if empty response message, take statusText
    		$rootScope.addError(message, response.status, true);
    		
    		if(response.data.notifications !== undefined) $rootScope.updateNotifications(response.data.notifications); 
    	}else{
    		var message = response.status + ' ' + response.statusText;
    		var details = response.data; // html content is excepted
    		$rootScope.addError(message, response.status, true, details);
    	}
    	
    	return true; // proceed with success or error hooks of promise
    });
    
    $rootScope.getCurrentDateTime = function (){
		return (new Date);
	}
    
    // Add feature to $location.url() function to be able to prevent reloading page (set reload param to false)
    var original = $location.url;
    $location.url = function (url, reload) {
        if (reload === false) {
            var lastRoute = $route.current;
            var un = $rootScope.$on('$locationChangeSuccess', function () {
                $route.current = lastRoute;
                un();
            });
        }
        return original.apply($location, [url]);
    };
    
	
});

AmpersandApp.value('cgBusyDefaults',{
	  message:'Loading...',
	  backdrop: true,
	  //templateUrl: 'my_custom_template.html',
	  //delay: 500, // in ms
	  minDuration: 500, // in ms
	  // wrapperClass: 'my-class my-class2'
	});

AmpersandApp.directive('myShowonhoverBox', function (){
	return {
		link : function(scope, element, attrs) {
			if(!element.closest('.box').hasClass('my-showonhover-box-show')) element.hide(); // default hide
			
			element.closest('.box').bind('mouseenter', function() {
				element.closest('.box').addClass('my-showonhover-box-show');
				element.show();
			});
			element.closest('.box').bind('mouseleave', function() {
				element.closest('.box').removeClass('my-showonhover-box-show');
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