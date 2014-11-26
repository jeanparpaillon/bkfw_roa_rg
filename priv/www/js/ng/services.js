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

.factory('modules', ['$resource', '$q', function($resource, $q) {

  return {

    list: function() {
      return $q(function(resolve, reject) {

        resolve([
          {
          index: 3,
          mode: "PC",
          curLaserTemp: Math.random() * 60,
          curAmp: 3,
          curInternalTemp: 32,
          powerPd1: 23,
          powerSupply: 13
          },
          {
          index: 4,
          mode: "GC",
          curLaserTemp: 48,
          curAmp: 2.5,
          curInternalTemp: 37.2,
          powerPd1: 27,
          powerSupply: 14.43
    	    },
	        {
          index: 7,
          mode: "GC",
          curLaserTemp: 45,
          curAmp: 2.3,
          curInternalTemp: 34.2,
          powerPd1: 23,
          powerSupply: 12.1
	        }
        ]);

      });
    },

    detail: function(id) {

    }

  };

}]);
