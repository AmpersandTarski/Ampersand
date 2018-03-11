angular.module('AmpersandApp')
.service('NavigationBarService', function(Restangular, $localStorage, $sessionStorage, NotificationService){
    let defaultSettings = {};
    let navbar = {
        top: [],
        new: [],
        refresh: [],
        role: [],
        ext: []
    };

    let service = {
        navbar : navbar,
        defaultSettings : defaultSettings,

        refreshNavBar : function(){
            return Restangular
            .one('app/navbar')
            .get()
            .then(function(data){
                data = data.plain();

                // Content of navbar
                navbar.top = data.top;
                navbar.new = data.new;
                navbar.refresh = data.refresh;
                navbar.role = data.role;
                navbar.ext = data.ext;

                $sessionStorage.session = data.session;
                $sessionStorage.sessionRoles = data.sessionRoles;
                $sessionStorage.sessionVars = data.sessionVars;
                angular.extend(service.defaultSettings, data.defaultSettings);
                
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
