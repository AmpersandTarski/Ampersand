var app = angular.module('AmpersandApp');
app.requires[app.requires.length] = 'LoginModule'; // add ur.file module to dependencies

var LoginModule = angular.module('LoginModule', ['ngRoute', 'restangular']);

LoginModule.controller('LoginExtLoginController', function($scope, $rootScope, LoginRestangular){
	
	LoginRestangular.one('login').get().then(
		function(data){ // success
			$scope.loginUrl = data.loginUrl;
			$rootScope.updateNotifications(data.notifications);
			
		}, function(){ // error
		
		}
	);
		
	
}).controller('LoginExtLogoutController', function($scope,$http,$sce){
	$http.get('src/loginout.php?action=logout').success(function(data){
		$scope.title = "Logout";
		$scope.html = $sce.trustAsHtml(data);
	});
	
}).controller('SecureController', function($scope,$http,$sce){
	$http.get('src/content.php').success(function(data){
		$scope.title = "Classified page";
		$scope.html = $sce.trustAsHtml(data);
	});
	
});

app.config(function($routeProvider) {
	$routeProvider
		// default start page
		.when('/ext/Login',
			{	controller: 'LoginExtLoginController'
			,	templateUrl: 'extensions/Login/ui/views/Login.html'
			,	interfaceLabel: 'Login'
			})
		.when('/ext/Logout',
			{	controller: 'LoginExtLogoutController'
			,	templateUrl: 'extensions/Login/ui/views/Logout.html'
			,	interfaceLabel: 'Logout'
			});
});

//Restangular service for the ExecEngine
LoginModule.factory('LoginRestangular', function(Restangular) {
  return Restangular.withConfig(function(RestangularConfigurer) {
    RestangularConfigurer.setBaseUrl('extensions/Login/api');
  });
});