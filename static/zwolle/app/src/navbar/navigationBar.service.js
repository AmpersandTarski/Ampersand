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
                
                // Save default settings
                angular.extend(service.defaultSettings.notifications, data.defaultSettings.notifications);
                service.defaultSettings.switchAutoSave = data.defaultSettings.switchAutoSave;
                service.initializeSettings(false);
                
                // Update notifications
                NotificationService.updateNotifications(data.notifications);
            });
        },

        initializeSettings : function(forceSet){
            // null == undefined => true
            angular.forEach(service.defaultSettings.notifications, function(value, index, obj){
                if($localStorage['notify-' + index] == undefined || forceSet) $localStorage['notify-' + index] = value;
            });
            if($localStorage.switchAutoSave == undefined || forceSet) $localStorage.switchAutoSave = service.defaultSettings.switchAutoSave;
        },

        resetSettingsToDefault : function(){
            // all off
            angular.forEach(service.defaultSettings.notifications, function(value, index, obj){
                $localStorage['notify-' + index] = false;
            });
            $localStorage.switchAutoSave = false;
            
            $timeout(function() {
                // Reset to default
                service.initializeSettings(true);
            }, 500);
        }
    };
    
    return service;
});
