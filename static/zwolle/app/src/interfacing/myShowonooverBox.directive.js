angular.module('AmpersandApp')
.directive('myShowonhoverBox', function (){
    return {
        link : function(scope, element, attrs) {
            if(!element.closest('.box').hasClass('my-showonhover-box-show')) element.hide(); // default hide
            
            element.closest('.box').bind('mouseenter', function() {
                element.closest('.box').addClass('my-showonhover-box-show');
                element.show();
            });
            element.closest('.box').bind('mouseleave', function() {
                element.closest('.box').removeClass('my-showonhover-box-show');
                element.hide();
            });
        }
    };
});
