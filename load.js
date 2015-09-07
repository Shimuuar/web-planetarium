
var catalogHD = {
    ra : [0.0/0.0],
    dec : [0.0/0.0],
}

// Load HD catalog
function load_catalogHD(callback) {
    $.ajax({
	url      : 'data/catalog.dat',
	dataType : 'text',
	success  : function(resp) {
	    var lines = resp.match(/[^\r\n]+/g)
	    for(var i = 0; i < lines.length; i++) {
		var row = lines[i];
		var ra  = parseFloat(row.slice(18,20)) + parseFloat(row.slice(20,23))/600;
		var dec = parseFloat(row.slice(24,26)) + parseFloat(row.slice(26,28))/60;
		if( row[23]==='-' )
		    dec = -dec;
		catalogHD.ra .push(ra);
		catalogHD.dec.push(dec);
	    }
	    console.log("READY");
	    callback()
	},
    });
}
