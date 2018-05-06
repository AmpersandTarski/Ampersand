angular.module('AmpersandApp')
.service('LoginService', function($location){
    let urlLoginPage = null;
    let urlBeforeLogin = null;
    
    let service = {
        setLoginPage : function (url) {
            urlLoginPage = url;
        },

        gotoLoginPage : function () {
            urlBeforeLogin = $location.url(); // "/some/path?foo=bar&baz=xoxo"
            if (urlLoginPage) {
                $location.url(urlLoginPage);
            }
        },

        gotoRequestedPageBeforeLogin : function () {
            $location.url(urlBeforeLogin);
        }
    };
    
    return service;
});