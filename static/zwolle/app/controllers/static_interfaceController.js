angular.module('AmpersandApp').controller('static_interfaceController', function($scope){
    
    /*
     * An empty object for typeahead functionality.
     * Defined here so it can be reused in an interface
     * Prevents multiple calls for the same resourceType
     */
    $scope.typeahead = {};
}