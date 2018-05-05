angular.module('AmpersandApp')
.value('cgBusyDefaults',{
    message:'Loading...',
    backdrop: true,
    //templateUrl: 'my_custom_template.html',
    //delay: 500, // in ms
    minDuration: 500, // in ms
    // wrapperClass: 'my-class my-class2'
});
