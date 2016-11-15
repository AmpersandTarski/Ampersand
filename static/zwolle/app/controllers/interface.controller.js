angular.module('AmpersandApp').controller('InterfaceController', function($scope, $location, ResourceService){
    // Function (reference) to save certain attributes changes of a resource (i.e. patches)
    $scope.save = ResourceService.saveResource;
    
    /*
     * An empty object for typeahead functionality.
     * Defined here so it can be reused in an interface
     * Prevents multiple calls for the same resourceType
     */
    $scope.typeahead = {};
    
    // Detects location changes and checks if there are unsaved changes
    $scope.$on("$locationChangeStart", function(event, next, current){
        if(ResourceService.checkRequired()){
            confirmed = confirm("You have unsaved edits. Do you wish to leave?");
            if (event && !confirmed) event.preventDefault();
            else if(event && confirmed) ResourceService.emptyUpdatedResources();
            else console.log('Someting went wrong. Cannot determine action after locationChangeStart');
        }
    });
    
    // Function to change location to create a new resource
    $scope.newResource = function(){
        $location.url('/' + ifcName + '?new');
    };
    
    // Function (reference) to check if there are pending promises for a resource
    $scope.pendingPromises = ResourceService.pendingPromises;
});