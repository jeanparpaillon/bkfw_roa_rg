/*global angular */
angular
    .module('bkfwApp.controllers', [])

    .controller('homeCtrl', ['$scope', '$resource', function($scope, $resource) {
	//var Edfas = $resource('/api/edfa/:index', {index: '@index'});

	//$scope.edfaList = Edfas.query();
	
	$scope.edfaList = [
	    {
		index: 3,
		mode: "PC",
		curLaserTemp: 43.4,
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
	];
    }])

    .controller('loginCtrl', function($scope) {
	$scope.title = "You're not logged in !";
    });
