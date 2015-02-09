AmpersandApp.config(function($routeProvider) {
	$routeProvider
		.when('/Projects/:resourceId?',
			{
				controller: 'ProjectsController',
				templateUrl: 'generics/app/views/Projects.html'
			})
		.when('/People/:resourceId?',
			{
				controller: 'PeopleController',
				templateUrl: 'generics/app/views/People.html'
			})
		.when('/Project/:resourceId?',
			{
				controller: 'ProjectController',
				templateUrl: 'generics/app/views/Project.html'
			})
		.when('/Person/:resourceId?',
			{
				controller: 'PersonController',
				templateUrl: 'generics/app/views/Person.html'
			})
		.otherwise({redirectTo: '/'});
});