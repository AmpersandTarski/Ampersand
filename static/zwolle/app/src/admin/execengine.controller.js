// Controller for extension app in navigation bar
angular.module('AmpersandApp')
.controller('ExecEngineController', function ($scope, Restangular, NotificationService) {
    $scope.run = function (){
        Restangular.one('admin/execengine/run').get()
        .then(
            function(data){ // on success
                data = data.plain();
                NotificationService.updateNotifications(data);
            }
        );
    };
});
