angular.module('AmpersandApp').controller('BoxController', function($scope, ResourceService){
    
    // Function to create a new resource (does a POST)
    $scope.createResource = ResourceService.createResource; // function(resource, ifc, callingObj, prepend)
    
    // Function to save certain attributes changes of a resource (does a PATCH)
    $scope.save = ResourceService.saveResource; // function(resource)
    
    // Function to cancel unsaved edits (does a GET)
    $scope.cancel = ResourceService.cancelResource; // function(resource)
    
    // Function to remove a resource from an interface (list)
    $scope.remove = ResourceService.removeResource; // function(ifc, resource, patchResource)
    
    // Function to delete a resource
    $scope.delete = ResourceService.deleteResource; // function(ifc, resource)
});