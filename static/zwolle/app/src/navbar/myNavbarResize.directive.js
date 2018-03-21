angular.module('AmpersandApp')
.directive('myNavbarResize', function ($window, $timeout, NavigationBarService) {
    return function (scope, element) {
        var w = angular.element($window);
        
        var resizeNavbar = function() {
            $timeout(function(){
                // moving ifc items from dropdown-menu to navbar itself
                while($('#navbar-interfaces').width() < ($('#navbar-wrapper').width() - $('#navbar-options').width()) &&
                        $('#navbar-interfaces-dropdown-menu').children().length > 0){
                    $("#navbar-interfaces-dropdown-menu").children().first().appendTo("#navbar-interfaces");
                }
                
                // moving ifc items from navbar to dropdown-menu
                while($('#navbar-interfaces').width() > ($('#navbar-wrapper').width() - $('#navbar-options').width())){
                    $("#navbar-interfaces").children().last().prependTo("#navbar-interfaces-dropdown-menu");
                    
                    // show/hide dropdown menu for more interfaces (must be inside loop, because it affects the width of the navbar
                    $('#navbar-interfaces-dropdown').toggleClass('hidden', $('#navbar-interfaces-dropdown-menu').children().length <= 0);
                }
                
                // show/hide dropdown menu when possible
                $('#navbar-interfaces-dropdown').toggleClass('hidden', $('#navbar-interfaces-dropdown-menu').children().length <= 0);
            });
        };
        
        // watch navbar
        scope.$watch('NavigationBarService.navbar', function() {
            // small timeout (500ms) for angular to update DOM after navbar data change
            // TODO: instead watch resize of element '#navbar-interfaces' and '#navbar-options'
            $timeout(function(){
                resizeNavbar();
            }, 500);
        });
        
        // when window size gets changed
        w.bind('resize', function () {
            resizeNavbar();
        });
        
        // when page loads
        angular.element(document).ready(function(){
            resizeNavbar();
        });
    };
});
