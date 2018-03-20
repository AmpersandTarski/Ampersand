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
        
        newResource = response.content;
        
        // Add new resource to ifc
        if(Array.isArray(fileItem.resource[fileItem.ifc])){ // non-uni = list
            fileItem.resource[fileItem.ifc].splice(-1, 0, newResource);
        }else{ // uni = object
            fileItem.resource[fileItem.ifc] = newResource;
        }
    };
    
    $scope.FileUploader.onErrorItem = function(item, response, status, headers){
        NotificationService.addError(response.msg, response.error, true, response.html);
        NotificationService.updateNotifications(response.notifications);
    };
});
