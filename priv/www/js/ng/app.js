/*global angular */

'use strict';

angular.module('bkfwApp', [
	'bkfwApp.services',
	'bkfwApp.controllers',
	'bkfwApp.directives',
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
	})

  .state('logout', {
		url: '/logout',
	  controller: ['$state', 'session', function($state, session) {
      if (session.disconnect()) {
        $state.go('dashboard');
      }
    }]
	});

});
