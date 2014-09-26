//Provides: unix_gettimeofday
function unix_gettimeofday () {
  return (new Date()).getTime() / 1000;
}

//Provides: unix_time
//Requires: unix_gettimeofday
function unix_time () {
  return Math.floor(unix_gettimeofday ());
}

//Provides: unix_gmtime
function unix_gmtime (t) {
  var d = new Date (t * 1000);
  var januaryfirst = new Date(Date.UTC(d.getUTCFullYear(), 0, 1));
  var doy = Math.floor((d - januaryfirst) / 86400000);
  return {tag:0,0:d.getUTCSeconds(), 1:d.getUTCMinutes(), 2:d.getUTCHours(),
          3:d.getUTCDate(), 4:d.getUTCMonth(), 5:d.getUTCFullYear() - 1900,
          6:d.getUTCDay(), 7:doy,
          8:false | 0 /* for UTC daylight savings time is false */}
}

//Provides: unix_localtime
function unix_localtime (t) {
  var d = new Date (t * 1000);
  var januaryfirst = new Date(d.getFullYear(), 0, 1);
  var doy = Math.floor((d - januaryfirst) / 86400000);
  var jan = new Date(d.getFullYear(), 0, 1);
  var jul = new Date(d.getFullYear(), 6, 1);
  var stdTimezoneOffset = Math.max(jan.getTimezoneOffset(), jul.getTimezoneOffset());
  return {tag:0,0:d.getSeconds(), 1:d.getMinutes(), 2:d.getHours(),
          3:d.getDate(), 4:d.getMonth(), 5:d.getFullYear() - 1900,
          6:d.getDay(), 7:doy,
          8:(d.getTimezoneOffset() < stdTimezoneOffset) | 0 /* daylight savings time  field. */}
}

//Provides: unix_mktime
//Requires: unix_localtime
function unix_mktime(tm){
    var d = new Date(tm[5]+1900,tm[4],tm[3],tm[2],tm[1],tm[0]);
    var t = Math.floor(d.getTime() / 1000);
    var tm2 = unix_localtime(t);
  return {tag:0,0:t,1:tm2};
}
