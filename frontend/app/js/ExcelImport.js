var app = angular.module('AmpersandApp');
app.requires[app.requires.length] = 'angularFileUpload'; // add ur.file module to dependencies

AmpersandApp.config(function($routeProvider) {
	$routeProvider
		// default start page
		.when('/ext/ExcelImport',
			{
				controller: 'ExcelImportController',
				templateUrl: 'app/ext/ExcelImport/views/ExcelImport.html'
			});
});

AmpersandApp.controller('ExcelImportController', function ($scope, $rootScope, FileUploader) {
	
	$scope.uploader = new FileUploader({
		 url: 'extensions/ExcelImport/excel_parse.php'
	});
	
	$scope.uploader.onSuccessItem = function(fileItem, response, status, headers) {
		$rootScope.notifications = response.notifications;
        console.info('onSuccessItem', fileItem, response, status, headers);
    };
    
});