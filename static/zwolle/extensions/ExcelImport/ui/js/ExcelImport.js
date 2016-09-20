var app = angular.module('AmpersandApp');
app.requires[app.requires.length] = 'angularFileUpload'; // add ur.file module to dependencies

app.config(function($routeProvider) {
	$routeProvider
		// default start page
		.when('/ext/ExcelImport',
			{	controller: 'ExcelImportController'
			,	templateUrl: 'extensions/ExcelImport/ui/views/ExcelImport.html'
			,	interfaceLabel: 'Excel import'
			});
}).controller('ExcelImportController', function ($scope, $rootScope, FileUploader) {
	
	// $rootScope, so that all information and uploaded files are kept while browsing in the application
	if (typeof $rootScope.uploader == 'undefined') {

		$rootScope.uploader = new FileUploader({
			 url: 'api/v1/excelimport/import'
		});
	}
	
	$rootScope.uploader.onSuccessItem = function(fileItem, response, status, headers) {
		$rootScope.updateNotifications(response.notifications);
        // console.info('onSuccessItem', fileItem, response, status, headers);
    };
    
    $rootScope.uploader.onErrorItem = function(item, response, status, headers){
        if(typeof response === 'object'){
            var message = response.msg || 'Error while importing';
            $rootScope.addError(message, status, true);
            
            if(response.notifications !== undefined) $rootScope.updateNotifications(response.notifications); 
        }else{
            var message = status + ' Error while importing';
            var details = response; // html content is excepted
            $rootScope.addError(message, status, true, details);
        }
    };
    
});