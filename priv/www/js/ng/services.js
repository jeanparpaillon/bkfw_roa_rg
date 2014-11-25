/*global angular */

'use strict';

angular.module('bkfwApp.services', [])

.factory('session', [function() {

  return {
    user: null,
    connected: false
  };

}]);
