// mock expected props in some FE modules
global.requestAnimationFrame = undefined
global.document = {querySelector: function(){}}

exports.neverUse = 'nothing'
