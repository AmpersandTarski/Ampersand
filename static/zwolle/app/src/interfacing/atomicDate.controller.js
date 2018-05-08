angular.module('AmpersandApp')
.controller('AtomicDateController', function ($scope, ResourceService) {
    
    $scope.isOpen = false;
    
    // Function is here because ng-model needs to be a Date object.
    // watch listener is initialized by the template
    $scope.watchDateObject = function(resource, ifc){
        $scope.$watch('resource', function(){
            if (!(resource[ifc] instanceof Date)){
                // Only convert to Date object when not NULL, otherwise the 1970-01-01 is created
                if (resource[ifc] !== null) resource[ifc] = new Date(resource[ifc]);
            }
        }, true);
    };

    $scope.openDatepicker = function($event){
        $event.preventDefault();
        $event.stopPropagation();
        $scope.isOpen = true;
    };
    
    // Adds leading 0 if necesarry. Returns 2 digits.
    function pad(number) {
        var r = String(number);
        if ( r.length === 1 ) {
            r = '0' + r;
        }
        return r;
    }
    
    function modifyToJSON(obj){
        if(obj !== null){
            obj.toJSON = function(){
                return this.getUTCFullYear() + 
                    '-' + pad(this.getMonth() + 1) + // The getMonth() method returns the month in the specified date according to local time, as a zero-based value (where zero indicates the first month of the year).
                    '-' + pad(this.getDate());
            };
        }
    }
    
    $scope.selected = { value : ''}; // an empty object for temporary storing the input values
    
    $scope.saveDateItem = function(obj, property, patchResource){
        modifyToJSON(obj[property]);
        ResourceService.saveItem(obj, property, patchResource);
    };
    
    $scope.addDateItem = function(obj, property, selected, patchResource){
        if(selected.value !== ''){
            modifyToJSON(selected.value);
            ResourceService.addItem(obj, property, selected, patchResource);
        }else{
            console.log('Empty date selected');
        }
    };
});
