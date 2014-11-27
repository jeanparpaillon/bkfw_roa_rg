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

.run(['mcu', function(mcu) {

  mcu.refreshList(3000);

}])

.config(function($stateProvider, $urlRouterProvider) {

	$urlRouterProvider.otherwise('/dashboard');

	$stateProvider

  .state('dashboard', {
	  url: '/dashboard',
		templateUrl: 'partials/dashboard.html',
	})

  .state('mcu', {
    url: '/mcu/:mcuIndex',
    controller: 'mcuCtrl as mcu',
    templateUrl: 'partials/mcu.html',
  })

  .state('system', {
    url: '/system',
    controller: 'systemCtrl as system',
    templateUrl: 'partials/system.html',
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
