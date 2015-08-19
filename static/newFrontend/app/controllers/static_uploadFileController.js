AmpersandApp.controller('uploadFileController', function($scope, $rootScope, $localStorage, FileUploader){
	  $scope.$storage = $localStorage;
	  
	  // File uploader stuff
	  $scope.FileUploader = new FileUploader({
		url					: 'api/v1/file',
		alias 				: 'file', // fieldname as used in $_FILES['file']
		formData 			: [{'sessionId' : $rootScope.session.id, 'roleId' : $scope.$storage.roleId}],
		removeAfterUpload 	: true,
		autoUpload 			: true
	  });
	  
	  $scope.FileUploader.onSuccessItem = function(fileItem, response, status, headers) {
		$scope.updateNotifications(response.notifications);
		
		selected = { value : response.filename};
		
		// Add filename datamodel
		$scope.addItem($scope.val, fileItem.key, selected, fileItem.resourceId);
	  };
	  
	  $scope.FileUploader.onErrorItem = function(item, response, status, headers){
		$scope.notifications.errors.push( {'message' : response.error.code + ' ' + response.error.message} );
	  };
	});