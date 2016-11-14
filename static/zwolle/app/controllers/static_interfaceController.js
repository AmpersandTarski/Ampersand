angular.module('AmpersandApp').controller('static_interfaceController', function($scope, $location, ResourceService){
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
    
    $scope.pendingPromises = ResourceService.pendingPromises;
});