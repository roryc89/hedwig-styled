// mock expected props in some FE modules
global.requestAnimationFrame = undefined
global.document = {querySelector: function(){}}

exports.quickHash = function(str) {
    var hash = 0;
    if (str.length == 0) {
        return hash;
    }
    for (var i = 0; i < str.length; i++) {
        var char = str.charCodeAt(i);
        hash = ((hash<<5)-hash)+char;
        hash = hash & hash; // Convert to 32bit integer
    }
    return hash < 0 ? hash * (-1) : hash;
}
