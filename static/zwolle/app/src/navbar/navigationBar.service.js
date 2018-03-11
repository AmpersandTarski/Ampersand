angular.module('AmpersandApp')
.service('NavigationBarService', function(Restangular, $localStorage, $sessionStorage, NotificationService){
    let navbar = {
        top: [],
        new: [],
        refresh: [],
        role: [],
        ext: []
    };
    let defaultSettings = {
        notifications: {
            showSignals: true,
            showInfos: true,
            showSuccesses: true,
            autoHideSuccesses: true,
            showErrors: true,
            showWarnings: true,
            showInvariants: true
        },
        switchAutoSave: true
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

                // Content for session storage
                $sessionStorage.session = data.session;
                $sessionStorage.sessionRoles = data.sessionRoles;
                $sessionStorage.sessionVars = data.sessionVars;
                
                // Set default settings
                angular.extend(service.defaultSettings.notifications, data.defaultSettings.notifications);
                service.defaultSettings.switchAutoSave = data.defaultSettings.switchAutoSave;
                
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
