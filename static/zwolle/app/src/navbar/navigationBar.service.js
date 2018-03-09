angular.module('AmpersandApp')
.service('NavigationBarService', function(Restangular, $localStorage, $sessionStorage, NotificationService){
    let navbar = {};
    let defaultSettings = {};

    let service = {
        navbar : navbar,
        defaultSettings : defaultSettings,

        refreshNavBar : function(){
            return Restangular
            .one('app/navbar')
            .get()
            .then(function(data){
                data = data.plain();

                angular.extend(navbar, data);
                $sessionStorage.session = data.session;
                $sessionStorage.sessionRoles = data.sessionRoles;
                $sessionStorage.sessionVars = data.sessionVars;
                angular.extend(defaultSettings, data.defaultSettings);
                
                // Default settings for notificationPrefs
                if($localStorage.notificationPrefs === undefined){
                    service.resetNotificationSettings();
                }
                // Default setting for switchAutoSave
                if($localStorage.switchAutoSave === undefined){
                    service.resetSwitchAutoSave();
                }
                
                // Update notifications
                NotificationService.updateNotifications(data.notifications);
            }, function(error){
                // on error
            });
        },

        resetNotificationSettings : function(){
            $localStorage.notificationPrefs = angular.extend($localStorage.notificationPrefs, service.defaultSettings.notifications);
        },

        resetSwitchAutoSave : function(){
            $localStorage.switchAutoSave = service.defaultSettings.switchAutoSave;
        }
    };
    
    return service;
});
