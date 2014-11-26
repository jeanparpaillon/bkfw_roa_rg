/*global angular */

'use strict';

angular.module('bkfwApp', [
	'bkfwApp.services',
	'bkfwApp.controllers',
	'bkfwApp.directives',
	'bkfwApp.utils',
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

  .state('module', {
    url: '/module/:moduleIndex',
    templateUrl: 'partials/module.html',
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
