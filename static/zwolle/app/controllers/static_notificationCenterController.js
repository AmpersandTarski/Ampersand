AmpersandApp.controller('static_notificationCenterController', function ($scope, $rootScope, $route, Restangular, $localStorage, NotificationService) {
    
    $scope.localStorage = $localStorage;
    $scope.notifications = NotificationService.notifications;
    
    $rootScope.checkAllRules = function(){
        Restangular
        .all('admin/checks/rules/evaluate').one('all')
        .get()
        .then(
            function(data){
                NotificationService.addSuccess('Evaluated all rules.');
                NotificationService.updateNotifications(data);
            },function(){
                NotificationService.addError('Failed to evaluate all rules: ' + reason);
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
