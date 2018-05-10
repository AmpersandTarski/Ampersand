angular.module('AmpersandApp')
.service('LoginService', function($location, $localStorage){
    let urlLoginPage = null;
    
    let service = {
        setLoginPage : function (url) {
            urlLoginPage = url;
        },

        gotoLoginPage : function () {
            $localStorage.login_urlBeforeLogin = $location.url(); // "/some/path?foo=bar&baz=xoxo"
            if (urlLoginPage) {
                $location.url(urlLoginPage);
            }
        },

        getPageBeforeLogin : function () {
            return $localStorage.login_urlBeforeLogin;
        }
    };
    
    return service;
});