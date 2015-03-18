var app = angular.module('AmpersandApp');

AmpersandApp.config(function($routeProvider) {
	$routeProvider
		// default start page
		.when('/ext/Login',
			{
				controller: 'LoginController',
				templateUrl: 'extensions/Login/ui/views/Login.html'
			});
		.when('/ext/Logout',
			{
				controller: 'LogoutController',
				templateUrl: 'extensions/Login/ui/views/Logout.html'
			});
});

AmpersandApp.controller('LoginController', function ($scope, $rootScope, FileUploader) {
	
	// $rootScope, so that all information and uploaded files are kept while browsing in the application
	if (typeof $rootScope.uploader == 'undefined') {

		$rootScope.uploader = new FileUploader({
			 url: 'extensions/ExcelImport/excel_parse.php'
		});
	}
	
	$rootScope.uploader.onSuccessItem = function(fileItem, response, status, headers) {
		$rootScope.notifications = response.notifications;
        console.info('onSuccessItem', fileItem, response, status, headers);
    };
    
});