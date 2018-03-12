angular.module('AmpersandApp')
.service('NavigationBarService', function(Restangular, $localStorage, $sessionStorage, $timeout, NotificationService){
    let navbar = {
        top: [],
        new: [],
        refresh: [],
        role: [],
        ext: []
    };
    let defaultSettings = {
        notify_showSignals: true,
        notify_showInfos: true,
        notify_showSuccesses: true,
        notify_autoHideSuccesses: true,
        notify_showErrors: true,
        notify_showWarnings: true,
        notify_showInvariants: true,
        autoSave: true
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
                service.defaultSettings = data.defaultSettings;
                service.initializeSettings();
                
                // Update notifications
                NotificationService.updateNotifications(data.notifications);
            });
        },

        initializeSettings : function(){
            let resetRequired = false;

            // Check for undefined settings
            angular.forEach(service.defaultSettings, function(value, index, obj){
                if($localStorage[index] === undefined) {
                    resetRequired = true;
                }
            });

            if(resetRequired) service.resetSettingsToDefault();
        },

        resetSettingsToDefault : function(){
            // all off
            angular.forEach(service.defaultSettings, function(value, index, obj){
                $localStorage[index] = false;
            });
            
            $timeout(function() {
                // Reset to default
                $localStorage.$reset(service.defaultSettings);
            }, 500);
        }
    };
    
    return service;
});
