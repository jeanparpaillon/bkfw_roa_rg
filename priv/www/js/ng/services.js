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

.factory('apiErrorsConfig', function() {
  return {
    intercept: true
  };
})

.factory('apiErrors', ['$q', 'apiErrorsConfig', 'dialogs', function($q, apiErrorsConfig, dialogs) {

  return {

    responseError: function(rejection) {
      // don't handle 401
      if (rejection.status !== 401 && apiErrorsConfig.intercept) {
        var errors = "";
        if (rejection.data) {
          errors = rejection.data.join("<br/>");
        }
        else if (rejection.statusText) {
          errors = rejection.statusText;
        }
        else {
          errors = "Connection lost with the API";
        }

        dialogs.error("An error occured", errors);
      }
      // pass it through the chain
      return $q.reject(rejection);
    }
  };

}])

.factory('auth', ['$http', '$rootScope', 'authService', 'AUTH_EVENTS', 'session', '$base64', 'md5', function($http, $rootScope, authService, AUTH_EVENTS, session, $base64, md5) {

  return {

    authenticate: function(user, password) {

      return $http.post('/api/sys/login', {login: user, password: md5.createHash(password)})

      .success(function() {
        var hash = $base64.encode(user + ':' + md5.createHash(password));
        session.create(user, hash);
        console.debug('Auth confirmed, proceed..');
        // the login is successfull, fire buffered
        // http requests!
        authService.loginConfirmed(user + "logged in.");
      })

      .error(function(errors) {
        $rootScope.$broadcast(AUTH_EVENTS.loginFailed, errors.join('<br/>'));
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

.factory('poll', ['$resource', '$timeout', function($resource, $timeout) {

  var pollers = {};

  function Poller(url, delay, onSuccess, onError) {
    this.started = false;
    this.resource = $resource(url);
    this.delay = delay;
    this.successCallback = onSuccess;
    this.errorCallback = onError;
    this.timeout = null;
  }

  Poller.prototype = {

    get: function() {
      this.resource.query(
        {},
        angular.bind(this, this.successCallback),
        angular.bind(this, this.errorCallback)
      );
    },

    poll: function() {
      // start immediately on the first call
      var delay = this.started ? this.delay : 0;
      this.started = true;
      this.timeout = $timeout(angular.bind(this, this.get), delay)
      .finally(angular.bind(this, this.poll));
    },

    stop: function() {
      this.timeout.cancel();
    }

  };

  return {

      start: function(url, delaySeconds, onSuccess, onError) {
        if (!pollers[url]) {
          pollers[url] = new Poller(url, (delaySeconds * 1000), onSuccess, onError);
          pollers[url].poll();
        }
      },

      stop: function(url) {
        pollers[url].stop();
        delete pollers[url];
      }

  };

}])

.factory('edfa', ['$resource', '$q', '$timeout', 'apiErrorsConfig', function($resource, $q, $timeout, apiErrorsConfig) {

  return {

    info: $resource('/api/edfa'),

    isOnline: function() {
      return this.info.get().$promise;
    },

    checkOnline: function() {
      return $timeout(angular.bind(this, this.isOnline), 2000)

      .then(angular.bind(this, function() {
        return true;
      }))

      .catch(angular.bind(this, this.checkOnline));
    },

    waitUntilOnline: function(initialDelay) {
      // we don't wan't to display API errors now
      apiErrorsConfig.intercept = false;

      return $timeout(angular.bind(this, this.checkOnline), initialDelay)

      .then(function() {
        apiErrorsConfig.intercept = true;
        return true;
      });
    },

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

  };

}])

.factory('mcu', ['$resource', '$timeout', 'poll', function($resource, $timeout, poll) {

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

      poll.start('/api/mcu', delay,
                 angular.bind(this, function(list) {
                  this.list = list.sort(function(a, b) {
                    return a.index > b.index;
                  });
                 }));

      return true;

    }

  };

}]);
