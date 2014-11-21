/*global angular */
angular
    .module('bkfwApp', [
	'bkfwApp.controllers',
	'ui.router'
    ])
    .config(function($stateProvider, $urlRouterProvider) {
	$urlRouterProvider.otherwise('/home');

	$stateProvider
	    .state('home', {
		url: '/home',
		templateUrl: 'partial-home.html',
		controller: 'homeCtrl'
	    })
	    .state('login', {
		url: '/login',
		templateUrl: 'partial-login.html'
	    });
    });
