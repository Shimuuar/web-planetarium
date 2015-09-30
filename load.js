// initialize UI etc
$(function(){
    $('#inp-datetime').datetimepicker()
})

// Add handler for numeric input
//  * selector - JQuery selector for 
//  * callback - callback
//  * x0       - initial value
function evts_numeric(selector, callback, minX, maxX, x0) {
    var e    = $(selector);
    var oldX = x0;
    e.val(x0);
    e.change(function(){
	// FIXME: validate better
	var newX = parseFloat(e.val())
	if( isNaN(newX) ||  newX<minX || newX>maxX ) {
	    e.val(oldX);
	} else {
	    oldX = newX;
	    callback(newX);
	}
    })
}



// ================================================================
// Catalog handling
// ================================================================

// Global variable holding part of HD catalog
var catalogHD = {
    ra  : [0.0/0.0],
    dec : [0.0/0.0],
    m   : [0.0/0.0],
}

// Load HD catalog
function load_catalogHD(callback) {
    $.ajax({
	url      : 'data/HD-6m.dat',
	dataType : 'text',
	success  : function(resp) {
	    var lines = resp.match(/[^\r\n]+/g)
	    for(var i = 0; i < lines.length; i++) {
		var row = lines[i];
		var n   = parseInt(row.slice(0,6))
		var ra  = parseFloat(row.slice(18,20)) + parseFloat(row.slice(20,23))/600;
		var dec = parseFloat(row.slice(24,26)) + parseFloat(row.slice(26,28))/60;
		var m   = parseFloat(row.slice(29,34))
		if( row[23]==='-' )
		    dec = -dec;
		catalogHD.ra [n] = ra;
		catalogHD.dec[n] = dec;
		catalogHD.m  [n] = m;
	    }
	    console.log("READY");
	    callback()
	},
    });
}
