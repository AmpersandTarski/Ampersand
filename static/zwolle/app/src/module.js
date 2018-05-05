// when using minified angular modules, use module('myApp', []).controller('MyController', ['myService', function (myService) { ...
angular.module('AmpersandApp', ['ngResource', 'ngRoute', 'ngSanitize', 'restangular', 'ui.bootstrap', 'uiSwitch', 'cgBusy', 'siTable', 'ngStorage', 'angularFileUpload', 'ui.bootstrap.datetimepicker', 'hc.marked'])
.config(function($routeProvider, $locationProvider) {
    $routeProvider
        // default start page
        .when('/', { 
            controller : '',
            templateUrl : 'app/src/shared/home.html',
            interfaceLabel : 'Home'
            })
        // installer page
        .when('/admin/installer', {
            controller : 'InstallerController',
            templateUrl : 'app/src/admin/installer.html',
            interfaceLabel : 'Installer'
            })
        .when('/404', {
            templateUrl: 'app/src/shared/404.html',
            interfaceLabel: '404'
            })
        .otherwise({redirectTo: '/404'});
    
    $locationProvider.hashPrefix(''); // see: https://stackoverflow.com/questions/41211875/angularjs-1-6-0-latest-now-routes-not-working
}).config(function(RestangularProvider) {
    
    RestangularProvider.setBaseUrl('api/v1'); // Generate: path to API folder
    RestangularProvider.setDefaultHeaders({"Content-Type": "application/json"});
    // RestangularProvider.setPlainByDefault(true); available from Restangular v1.5.3
    
}).run(function(Restangular, $rootScope, $location, $route, NotificationService, RoleService, NavigationBarService){

    Restangular.addFullRequestInterceptor(function(element, operation, what, url, headers, params){
        //params.navIfc = true;
        //params.metaData = true;
        return params;
    });
    
    Restangular.addResponseInterceptor(function(data, operation, what, url, response, deferred){
        if(operation != 'get' && operation != 'getList' && data.sessionRefreshAdvice) NavigationBarService.refreshNavBar();
		if((data || {}).navTo != null) $location.url(data.navTo);
        
        return data;
    });
    
    Restangular.setErrorInterceptor(function(response, deferred, responseHandler) {
        var message;
        var details;
        if(typeof response.data === 'object'){
            if(response.data.error == 404) { // 404: Not found
                NotificationService.addInfo(response.data.msg || 'Resource not found');
            
            } else if(response.status == 401){ // 401: Unauthorized
                if(response.data.data.loginPage) {
                    $location.path(response.data.data.loginPage);
                }
                NotificationService.addInfo(response.data.msg || 'Login required to access this page');
            
            } else {
                message = response.data.msg || response.statusText; // if empty response message, take statusText
                NotificationService.addError(message, response.status, true, response.data.html);
            }
            
            if(response.data.notifications !== undefined) NotificationService.updateNotifications(response.data.notifications); 
        }else{
            message = response.status + ' ' + response.statusText;
            details = response.data; // html content is excepted
            NotificationService.addError(message, response.status, true, details);
        }
        
        return true; // proceed with success or error hooks of promise
    });
    
    $rootScope.getCurrentDateTime = function (){
        return new Date();
    };
    
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
