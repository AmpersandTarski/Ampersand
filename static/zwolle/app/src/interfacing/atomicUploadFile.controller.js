angular.module('AmpersandApp')
.controller('AtomicUploadFileController', function($scope, FileUploader, NotificationService){
    
    // File uploader stuff
    $scope.FileUploader = new FileUploader({
        alias : 'file', // fieldname as used in $_FILES['file']
        formData : [],
        removeAfterUpload : true,
        autoUpload : true
    });
    
    $scope.FileUploader.onSuccessItem = function(fileItem, response, status, headers){
        NotificationService.updateNotifications(response.notifications);
        
        // Add response content (newly created FileObject) to ifc list in resource
        fileItem.resource[fileItem.ifc].push(response.content);
    };
    
    $scope.FileUploader.onErrorItem = function(item, response, status, headers){
        NotificationService.addError(response.msg, response.error, true, response.html);
        NotificationService.updateNotifications(response.notifications);
    };
});
