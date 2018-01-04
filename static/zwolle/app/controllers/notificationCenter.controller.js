angular.module('AmpersandApp').controller('NotificationCenterController', function ($scope, $rootScope, $route, Restangular, $localStorage, NotificationService) {
    
    $scope.localStorage = $localStorage;
    $scope.notifications = NotificationService.notifications;
    
    $rootScope.checkAllRules = function(){
        Restangular
        .all('admin/ruleengine/evaluate').one('all')
        .get()
        .then(
            function(data){
                data = data.plain();
                NotificationService.addSuccess('Evaluated all rules.');
                NotificationService.updateNotifications(data);
            },function(){
                NotificationService.addError('Something went wrong while evaluating all rules');
            }
        );
    };
    
    // Hide success-, error-, warnings-, info- and invariant violation messages (not signals) upon route change
    $scope.$on("$routeChangeSuccess", function(){
        $scope.notifications.successes = [];
        $scope.notifications.errors = $scope.notifications.errors.filter(function (error){
            if(error.persistent){
                error.persistent = false;
                return true;
            }
            else return false;
        });
        $scope.notifications.warnings = [];
        $scope.notifications.infos = [];
        $scope.notifications.invariants = [];
    });
    
    // Function to close notifications
    $scope.closeAlert = function(alerts, index) {
        alerts.splice(index, 1);
    };
    
});
