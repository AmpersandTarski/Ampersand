angular.module('AmpersandApp').controller('BoxController', function($scope, ResourceService){
    
    $scope.createResource = ResourceService.createResource; // function(obj, ifc, callingObj, prepend)
});