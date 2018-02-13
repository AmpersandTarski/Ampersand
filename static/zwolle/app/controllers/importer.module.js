var app = angular.module('AmpersandApp');
app.requires[app.requires.length] = 'angularFileUpload'; // add angularFileUpload to dependency list
app.config(function($routeProvider) {
    $routeProvider
        .when('/ext/importer', {
            controller : 'PopulationImportController',
            templateUrl : 'app/views/importer.html',
            interfaceLabel : 'Population importer'
        });
}).service('ImportService', function(FileUploader, NotificationService){
    let uploader = new FileUploader({
        url: 'api/v1/admin/import'
    });

    uploader.onSuccessItem = function(fileItem, response, status, headers) {
        NotificationService.updateNotifications(response.notifications);
    };
    
    uploader.onErrorItem = function(item, response, status, headers){
        let message;
        let details;
        if(typeof response === 'object'){
            message = response.msg || 'Error while importing';
            NotificationService.addError(message, status, true);
            
            if(response.notifications !== undefined) NotificationService.updateNotifications(response.notifications); 
        }else{
            message = status + ' Error while importing';
            details = response; // html content is excepted
            NotificationService.addError(message, status, true, details);
        }
    };
    
    return {uploader : uploader};
}).controller('PopulationImportController', function ($scope, ImportService) {
    uploader = ImportService.uploader;
});