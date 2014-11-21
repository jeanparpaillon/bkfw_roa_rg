/*global angular */
angular
    .module('bkfwApp.controllers', [])

    .controller('homeCtrl', function($scope) {
	$scope.title = "Welcome to main!";
    })

    .controller('loginCtrl', function($scope) {
	$scope.title = "You're not logged in !";
    });
