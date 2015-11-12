AmpersandApp.controller('uploadFileController', function($scope, $rootScope, $localStorage, FileUploader){
	  
	  // File uploader stuff
	  $scope.FileUploader = new FileUploader({
		url					: 'api/v1/file',
		alias 				: 'file', // fieldname as used in $_FILES['file']
		formData 			: [{'roleId[]' : $rootScope.getActiveRoleIds()}], // the '[]' in param 'roleIds[]' is needed by the API to process it as array
		removeAfterUpload 	: true,
		autoUpload 			: true
	  });
	  
	  $scope.FileUploader.onSuccessItem = function(fileItem, response, status, headers) {
		$scope.updateNotifications(response.notifications);
		
		selected = { id : response.uploadId};
		
		// Add filename datamodel
		$scope.addObject($scope.val, fileItem.key, selected, fileItem.resourceId);
	  };
	  
	  $scope.FileUploader.onErrorItem = function(item, response, status, headers){
		$scope.notifications.errors.push( {'message' : response.error.code + ' ' + response.error.message} );
	  };
	});