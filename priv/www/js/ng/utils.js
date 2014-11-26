'use strict';

angular.module('bkfwApp.utils', [])

.factory('dialogs', function($q) {

  var popups = [];

  function Dialog(params) {
    angular.extend(this, params);
    this.result = $q.defer();
    popups.push(this);
  }

  Dialog.prototype = {

    close: function(value) {
      popups.splice(popups.indexOf(this), 1);
      if (value)
        this.result.resolve(value);
      else
        this.result.reject(value);
    }
  };

  return {

    all: function() {
      return popups;
    },

    confirm: function(title, subtext) {
      return new Dialog({
        type: 'confirm',
        klass: 'info',
        title: title,
        subtext: subtext,
      }).result.promise;
    },

    traceback: function(exception, traceback, call) {
      return new Dialog({
        type: 'traceback',
        klass: 'danger',
        exception: exception,
        traceback: traceback,
        call: call
      }).result.promise;
    },

    close: function() {
      popups[popups.length -1].close(false);
    }
  };
})

.directive('dialogs', function() {

  return {
    restrict: 'E',
    templateUrl: 'partials/dialogs.html',
    controller: ['$scope', 'dialogs', function($scope, dialogs) {
      $scope.dialogs = dialogs;
    }]
  };

});
