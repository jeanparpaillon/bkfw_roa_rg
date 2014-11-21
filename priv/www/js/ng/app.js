/*global angular */
angular
    .module('bkfwApp', [
	'ui.router'
    ])
    .config(function($stateProvider, $urlRouterProvider) {
	$urlRouterProvider.otherwise('/home');

	$stateProvider
	    .state('home', {
		url: '/home',
		templateUrl: 'partial-home.html'
	    })
	    .state('login', {
		url: '/login',
		templateUrl: 'partial-login.html'
	    });
    });
