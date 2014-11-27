/*global angular */

'use strict';

angular.module('bkfwApp.services', [])

.factory('session', ['$q', '$timeout', function($q, $timeout) {

  return {
    user: null,

    token: null,

    get connected() {
      return this.token !== null;
    },

    status: {
      DISCONNECTED: 0,
      CONNECTING: 10,
      CONNECTED: 20,
    },

    connect: function(user, password) {
      var deferResult = $q.defer();
      // do auth
      $timeout(angular.bind(this, function() {
        deferResult.notify(this.status.CONNECTING);

        $timeout(angular.bind(this, function() {
          if (password == "admin") {
            deferResult.notify(this.status.CONNECTED);
            this.token = "foo";
            this.user = user;
            deferResult.resolve(true);
          }
          else {
            deferResult.notify(this.status.DISCONNECTED);
            deferResult.reject("Wrong password");
          }
        }), 1000);

      }), 50);
      return deferResult.promise;
    },

    disconnect: function() {
      this.token = null;
      this.user = null;
      return true;
    }
  };

}])

.factory('edfa', ['$resource', function($resource) {

  return $resource('/api/edfa');

}])

.factory('sys', ['$resource', function($resource) {

  return {

    net: $resource('/api/sys/net'),

    community: $resource('/api/sys/community'),

    protocol: $resource('/api/sys/protocol'),

    firmware: $resource('/api/sys/firmware')

  };

}])

.factory('mcu', ['$resource', '$timeout', function($resource, $timeout) {

  var refreshId,
      refreshDelay;

  return {

    modes: {
      1: {name: 'PC', valueType: 'dBm'},
      PC: 1,
      2: {name: 'GC', valueType: 'dB'},
      GC: 2,
      3: {name: 'CC', valueType: 'mA'},
      CC: 3,
      4: {name: 'OFF', valueType: null},
      OFF: 4
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
        this.list = list;
        refreshId = $timeout(angular.bind(this, this.refreshList), refreshDelay);
      }));

    },

  };

}]);
