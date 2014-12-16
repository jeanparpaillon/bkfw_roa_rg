/*global angular */

'use strict';

angular.module('bkfwApp', [
    'bkfwApp.services',
    'bkfwApp.controllers',
    'bkfwApp.directives',
    'bkfwApp.utils',
    'http-auth-interceptor',
    'angularFileUpload',
    'ngResource',
    'ui.router',
    'ws',
    'ngNumeraljs'
])

.run(['$rootScope', '$state', 'AUTH_EVENTS', 'auth', 'mcu', 'alarms', function($rootScope, $state, AUTH_EVENTS, auth, mcu, alarms) {

  var routeBuffer = "";

  // refresh mcu list every 5 seconds
  mcu.refreshList(5);

  $rootScope.$on('$stateChangeStart', function (event, next) {
    var needAuth = next.auth || false;
    if (needAuth && !auth.isAuthenticated()) {
      console.debug("need auth");
      event.preventDefault();
      $rootScope.$broadcast(AUTH_EVENTS.notAuthenticated, next);
    }
  });

  $rootScope.$on(AUTH_EVENTS.notAuthenticated, function(event, next) {
    if (next) {
      routeBuffer = next.name;
    }
  });

  $rootScope.$on(AUTH_EVENTS.loginCancelled, function() {
    routeBuffer = "";
  });

  $rootScope.$on(AUTH_EVENTS.loginSuccess, function() {
    if (routeBuffer)
      $state.go(routeBuffer);
  });

}])

.config(['$stateProvider', '$urlRouterProvider', '$httpProvider', function($stateProvider, $urlRouterProvider, $httpProvider) {

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
    auth: true
  })

  .state('login', {
    url: '/login',
    templateUrl: 'partials/login.html',
  })

  .state('logout', {
    url: '/logout',
    controller: ['$state', 'auth', function($state, auth) {
      auth.disconnect();
    }]
  });

  $httpProvider.interceptors.push('apiErrors');

}]);
