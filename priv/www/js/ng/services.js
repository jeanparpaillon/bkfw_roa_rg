/*global angular */

'use strict';

angular.module('bkfwApp.services', ['base64', 'angular-md5', 'ngStorage'])

.factory('session', ['$http', '$sessionStorage', function($http, $sessionStorage) {

  function Session() {
    this.user = $sessionStorage.user || null;
    this.hash = $sessionStorage.hash || null;
  }

  Session.prototype = {

    get hash() {
      return this._hash;
    },

    set hash(value) {
      this._hash = value;
      if (value !== null) {
        $http.defaults.headers.common.Authorization = "x-basic " + this._hash;
      }
      else {
        delete $http.defaults.headers.common.Authorization;
      }
    },

    create: function(user, hash) {
      this.user = $sessionStorage.user = user;
      this.hash = $sessionStorage.hash = hash;
    },

    destroy: function() {
      this.user = null;
      this.hash = null;
      $sessionStorage.$reset();
    }

  };

  return new Session();

}])

.constant('AUTH_EVENTS', {
  // event:auth-* are used by angular-http-auth
  loginSuccess: 'event:auth-loginConfirmed',
  loginFailed: 'auth-login-failed',
  logoutSuccess: 'auth-logout-success',
  sessionTimeout: 'auth-session-timeout',
  notAuthenticated: 'event:auth-loginRequired',
  notAuthorized: 'event:auth-forbidden'
})

.factory('auth', ['$http', '$rootScope', 'authService', 'AUTH_EVENTS', 'session', '$base64', 'md5', function($http, $rootScope, authService, AUTH_EVENTS, session, $base64, md5) {

  return {

    authenticate: function(user, password) {

      var hash = $base64.encode(user + ':' + md5.createHash(password)),
          req = {
        method: 'GET',
        url: '/api/sys/login',
        headers: {
          'Authorization': "x-basic " + hash
        }
      };

      return $http(req)

      .success(function() {
        session.create(user, hash);
        console.debug('Auth confirmed, proceed..');
        // the login is successfull, fire buffered
        // http requests!
        authService.loginConfirmed(user + "logged in.");
      })

      .error(function() {
        console.debug('Login failed.');
        $rootScope.$broadcast(AUTH_EVENTS.loginFailed, "Wrong username/password.");
      });

    },

    cancelAuthenticate: function() {
      // when the user cancel the authentication
      // process we need to clear buffered requests
      authService.loginCancelled();
    },

    isAuthenticated: function() {
      return !!session.user;
    },

    disconnect: function() {
      session.destroy();
      $rootScope.$broadcast(AUTH_EVENTS.logoutSuccess);
    }
  };

}])

.factory('edfa', ['$resource', function($resource) {

  return {

    info: $resource('/api/edfa'),

    label: {
      curInternalTemp: "Internal Temp",
      powerSupply: "Power Supply",
      vendor: "Vendor",
      moduleType: "Module Type",
      hwVer: "Hardware version",
      hwRev: "Hardware revision",
      swVer: "Software version",
      fwVer: "Firmware version",
      partNum: "Part. Num",
      serialNum: "Serial Number",
      productDate: "Product Date"
    }

  };

}])

.factory('sys', ['$resource', function($resource) {

  return {

    net: $resource('/api/sys/net'),

    community: $resource('/api/sys/community'),

    protocol: $resource('/api/sys/protocol'),

    firmware: $resource('/api/sys/firmware'),

    password: $resource('/api/sys/password')

  };

}])

.factory('mcu', ['$resource', '$timeout', function($resource, $timeout) {

  var refreshId,
      refreshDelay;

  return {

    label: {
      ampConsign: "AMP consign",
      gainConsign: "Gain consign",
      outputPowerConsign: "Output power consign",
      operatingMode: "Operating mode",
      curLaserTemp: "Current laser temp",
      curAmp: "Current AMP",
      curInternalAmp: "Current internal AMP",
      powerInput: "Power input",
      powerOutput: "Power output",
      powerSupply: "Power supply",
      inputLossThreshold: "Input loss threshold",
      outputLossThreshold: "Output loss threshold",
      vendor: "Vendor",
      moduleType: "Module type",
      hwVer: "Hardware version",
      hwRev: "Hardware revision",
      swVer: "Software version",
      fwVer: "Firmware version",
      partNum: "Part. number",
      serialNum: "Serial number",
      productDate: "Product date"
    },

    modeID: {
      1: {name: 'PC', valueType: 'dBm'},
      2: {name: 'GC', valueType: 'dB'},
      3: {name: 'CC', valueType: 'mA'},
      4: {name: 'OFF', valueType: null},
    },

    mode: {
      PC: '1',
      GC: '2',
      CC: '3',
      OFF: '4'
    },

    api: $resource('/api/mcu/:mcuIndex', {mcuIndex: '@index'}),

    list: [],

    refreshList: function(delay) {

      if (delay)
        refreshDelay = delay;
      if (refreshId)
        $timeout.cancel(refreshId);

      var list = this.api.query(angular.bind(this, function() {
        // avoid interface flickering
        this.list = list.sort(function(a, b) {
          return a.index > b.index;
        });
        refreshId = $timeout(angular.bind(this, this.refreshList), refreshDelay);
      }));

    },

  };

}]);
