/*global angular */

'use strict';

angular.module('bkfwApp', [
	'bkfwApp.services',
	'bkfwApp.controllers',
	'ngResource',
	'ui.router'
])

.config(function($stateProvider, $urlRouterProvider) {

	$urlRouterProvider.otherwise('/dashboard');

	$stateProvider

  .state('dashboard', {
	  url: '/dashboard',
		templateUrl: 'partials/dashboard.html',
	})

  .state('login', {
		url: '/login',
		templateUrl: 'partials/login.html'
	});

});
