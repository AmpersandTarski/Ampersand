AmpersandApp.controller('uploadFileController', function($scope, $rootScope, $localStorage, FileUploader){
	  
  // File uploader stuff
  $scope.FileUploader = new FileUploader({
	alias 				: 'file', // fieldname as used in $_FILES['file']
	formData 			: [{'roleId[]' : $rootScope.getActiveRoleIds(), 'requestType' : $rootScope.defaultRequestType}], // the '[]' in param 'roleIds[]' is needed by the API to process it as array
	removeAfterUpload 	: true,
	autoUpload 			: true
  });
  
  $scope.FileUploader.onSuccessItem = function(fileItem, response, status, headers) {
	$scope.updateNotifications(response.notifications);
	
	// Add response content (newly created FileObject) to ifc list in resource
	fileItem.resource[fileItem.ifc].push(response.content);

  };
  
  $scope.FileUploader.onErrorItem = function(item, response, status, headers){
	$rootScope.addError(response.error.message, response.error.code, true);
  };
});