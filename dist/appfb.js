'use strict';

var app = firebase.initializeApp(config);
var database = app.database();


//Activity
var ACTIVITIESPATH = "activities"

function addActivity(activity){
  var promise = database
    .ref(ACTIVITIESPATH)
    .push(activity);
  return promise;
}

function updateActivity(activity){
  var id = activity.id;
  var promise = database
    .ref(ACTIVITIESPATH + "/" + id)
    .set(activity);
  return promise;
}

function deleteActivity(activity){
  var id = activity.id;
  var promise = database
    .ref(ACTIVITIESPATH + "/" + id)
    .remove();
  return promise;
}

function activityListener(){
  return database.ref(ACTIVITIESPATH);
}

//ActivityGroup
var ACTIVITYGROUPPATH = "activityGroups"

function addActivityGroup(activityGroup){
  var promise = database
    .ref(ACTIVITYGROUPPATH)
    .push(activityGroup);
  return promise;
}

function updateActivityGroup(activityGroup){
  var id = activityGroup.id;
  var promise = database
    .ref(ACTIVITYGROUPPATH + "/" + id)
    .set(activityGroup);
  return promise;
}

function deleteActivityGroup(activityGroup){
  var id = activityGroup.id;
  var promise = database
    .ref(ACTIVITYGROUPPATH + "/" + id)
    .remove();
  return promise;
}

function activityGroupListener(){
  return database.ref(ACTIVITYGROUPPATH);
}
