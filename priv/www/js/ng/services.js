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

.factory('alarms', ['$timeout', '$location', '$rootScope', 'ws', function($timeout, $location, $rootScope, ws) {

  function Alarm(data) {
    // data = {"index":12,"name":"pump_temp","msg":"Pump temperature alarm","var": "curLaserTemp"}
    this.data = data;
    this.timeout();
  }

  Alarm.prototype = {
    timeout: function() {
      $timeout(angular.bind(this, function() {
        $rootScope.$broadcast('alarm-expired', this);
      }), 3000);
    }
  };

  var conn = ws.connect({
    url: 'ws://' + $location.host() + ':' + $location.port() + '/api/alarms'
  });

  var alarms = {
    type: {
      pin: {
        msg: "Input power loss",
        field: "powerInput"
      },
      pout: {
        msg: "Output power loss",
        field: "powerOutput"
      },
      pump_temp: {
        msg: "Pump temperature alarm",
        field: "curLaserTemp"
      },
      pump_bias: {
        msg: "Laser current is over 95% of EOL",
        field: "curAmp"
      },
      edfa_temp: {
        msg: "Internal temperature alarm",
        field: "curInternalTemp"
      },
      edfa_psu: {
        msg: "Power Supply alarm",
        field: "powerSupply"
      },
      bref: {
        msg: "Back reflection alarm"
      },
      adi: {
        msg: "Shutdown input is active"
      },
      mute: {
        msg: "Mute input is active"
      }
    },

    list: [],

    forIndex: function(index) {
      var names = [];
      return this.list.filter(function(alarm) {
        if (alarm.data.index == index && names.indexOf(alarm.data.name) === -1) {
          names.push(alarm.data.name);
          return true;
        }
        return false;
      });
    }
  };

  var showAlarms = function() {
    //console.debug(JSON.stringify(alarms.list));
    console.debug(JSON.stringify(alarms.forIndex(0)));
    $timeout(showAlarms, 3000);
  };
  //showAlarms();

  ws.on('message', function(event) {
    var data = JSON.parse(event.data);
    data.msg = alarms.type[data.name].msg;
    alarms.list.push(new Alarm(data));
  });

  $rootScope.$on('alarm-expired', function(event, alarm) {
    var index = alarms.list.findIndex(function(elem) {
      return elem == alarm;
    });
    alarms.list.splice(index, 1);
  });

  return alarms;

}])

.factory('edfa', ['$resource', '$q', '$timeout', 'apiErrorsConfig', 'alarms', function($resource, $q, $timeout, apiErrorsConfig, alarms) {

  var resource = $resource('/api/edfa', {});

  return {

    get: function() {
      return resource.get(function(edfa) {
        edfa.hasAlarmOn = angular.bind(edfa, function(fieldName) {
          return alarms.forIndex(0).filter(function(alarm) {
            return alarm.data.var == fieldName;
          }).length > 0;
        });
        edfa.alarms = angular.bind(edfa, function() {
          return alarms.forIndex(0);
        });
        edfa.hasAlarms = angular.bind(edfa, function() {
          return this.alarms().length > 0;
        });
      });
    },

    isOnline: function() {
      return resource.get().$promise;
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

    targets: $resource('/api/sys/targets')
  };

}])

.factory('mcu', ['$resource', '$http', '$timeout', 'poll', 'alarms', function($resource, $http, $timeout, poll, alarms) {

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
      '1': {name: 'PC', valueType: 'dBm'},
      '2': {name: 'GC', valueType: 'dB'},
      '3': {name: 'CC', valueType: 'mA'},
      '4': {name: 'OFF', valueType: null},
    },

    mode: {
      PC: '1',
      GC: '2',
      CC: '3',
      OFF: '4'
    },

    getControlValueName: function(operatingMode) {
      switch (operatingMode) {
        case this.mode.PC:
          return 'outputPowerConsign';
        case this.mode.GC:
          return 'gainConsign';
        case this.mode.CC:
          return 'ampConsign';
      }
      return null;
    },

    getControlValue: function(mcu, operatingMode) {
      if (operatingMode != this.mode.OFF) {
        return mcu[this.getControlValueName(operatingMode)];
      }
      return null;
    },

    api: $resource('/api/mcu/:mcuIndex', {mcuIndex: '@index'}),

    list: [],

    get: function(index) {
      return this.list.filter(function(mcu) {
        return mcu.index == index;
      })[0] || null;
    },

    save: function(mcu) {
      return $http.post('/api/mcu/' + mcu.index, mcu);
    },

    onList: function(list) {
      this.list = list.sort(function(a, b) {
        return a.index > b.index;
      });
      this.list.map(angular.bind(this, function(mcu) {
        // inject methods to get alarms info for each mcu
        mcu.hasAlarmOn = angular.bind(mcu, function(fieldName) {
          return alarms.forIndex(this.index).filter(function(alarm) {
            return alarm.data.var == fieldName;
          }).length > 0;
        });
        mcu.alarms = angular.bind(mcu, function() {
          return alarms.forIndex(this.index);
        });
        mcu.hasAlarms = angular.bind(mcu, function() {
          return this.alarms().length > 0;
        });
      }));
    },

    refreshList: function(delay) {
      poll.start('/api/mcu', delay, angular.bind(this, this.onList));
      return true;
    }

  };

}]);
