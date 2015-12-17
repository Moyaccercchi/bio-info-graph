/*
	*****************************************************
		GML UI - Graph Merging Library User Interface
	*****************************************************
*/

window.GML_UI = {

	init: function() {

		GML.set_to_HTML();

		// every on mouse up should release the verbosity capture
		document.addEventListener('mouseup', function() {
			GML_UI.changeOptions_verbosity_capture = false;
		}, false);

		// default to default options ;)
		this.resetOptions();

		// default to tab 5
		this.showTab(5);
	},



	// stores the visibility of div-out so that we don't reset it when changing tabs
	div_out_visibility: [false, false, false, false, false, false, false],
	xbw_visibility:     [false, false, false, false, false, false, false],

	// how many tabs are there?
	upToTabs: 7,



	/*
		Tab Control
	*/

	unShowAllTabs: function() {
		for (var i = 0; i < this.upToTabs; i++) {
			document.getElementById('tab-btn-' + i).className = 'tabbutton';
			document.getElementById('div-in-' + i).style.display = 'none';
			document.getElementById('div-out-' + i).style.display = 'none';
			var el = document.getElementById('div-xbw-' + i);
			if (el) {
				el.style.display = 'none';
			}
		}
	},

	// the tab that is currently open
	cur_tab: -1,

	showTab: function(nexttab) {

		this.cur_tab = nexttab;

		this.unShowAllTabs();

		document.getElementById('tab-btn-' + nexttab).className = 'tabbutton active';
		document.getElementById('div-in-' + nexttab).style.display = 'block';
		if (this.div_out_visibility[nexttab]) {
			document.getElementById('div-out-' + nexttab).style.display = 'block';
			if (this.xbw_visibility[nexttab] && !GML.hideXBWenvironments) {
				var el = document.getElementById('div-xbw-' + nexttab);
				if (el) {
					el.style.display = 'block';
				}
			}
		}
	},

	activateDivOut: function(i, showXBWenv, showAnchors) {

		GML_UI.cur_tab = i;

		var el = document.getElementById('div-xbw-' + i);
		if (el) {
			if (showXBWenv && !GML.hideXBWenvironments) {
				el.style.display = 'block';
				this.xbw_visibility[i] = true;
			} else {
				el.style.display = 'none';
			}
		}


		this.setJumpDispStyle(i, showAnchors);


		el = document.getElementById('div-out-' + i);
		el.innerHTML = '<div class="working">Working on your request...<br><br>' +
					   'If you see this message for a long time, the page may have crashed.' +
					   'In that case you could try to ' +
					   'refresh the page, reduce the verbosity in the options, ' +
					   'and then restart the computation.</div>';
		el.style.display = 'block';

		this.div_out_visibility[i] = true;

		return el;
	},

	unShowXBWEnv: function(tab) {

		this.xbw_visibility[tab] = false;
		document.getElementById('div-xbw-' + tab).style.display = 'none';
	},

	setJumpDispStyle: function(i, show) {

		var dispstyle = 'none';

		if (show) {
			dispstyle = 'inline';
		}

		var j = 1;
		var el = document.getElementById('a-jump-' + i + '-' + j);

		while (el) {
			el.style.display = dispstyle;
			el = document.getElementById('a-jump-' + i + '-' + j);
			j++;
		}
	},



	/*
		Tab 0 - Generate One BWT (naively)
	*/

	generateNaiveBWT: function() {
		var el = this.activateDivOut(0, false, true);
		el.innerHTML = '<div>' + GML.generate_BWT_naively(
			document.getElementById('in-string-0').value.toUpperCase()) + '</div>';
	},




	/*
		Tab 1 - Merge Two BWTs (naively)
	*/

	generateNaiveBWTs: function() {
		var el = this.activateDivOut(1, false, false);
		el.innerHTML = '<div>' + GML.generate_BWTs_naively(
			document.getElementById('in-string-1-1').value.toUpperCase(),
			document.getElementById('in-string-1-2').value.toUpperCase()) + '</div>';
	},

	mergeNaiveBWTs: function() {
		var el = this.activateDivOut(1, false, true);
		el.innerHTML = '<div>' + GML.merge_BWTs_naively(
			document.getElementById('in-string-1-1').value.toUpperCase(),
			document.getElementById('in-string-1-2').value.toUpperCase()) + '</div>';
	},



	/*
		Tab 2 - Generate One BWT (advanced)
	*/
	generateAdvancedBWT: function() {
		var el = this.activateDivOut(2, true, true);
		el.innerHTML = '<div>' + GML.generate_BWT_advanced(
			document.getElementById('in-string-2').value.toUpperCase()) + '</div>';
	},




	/*
		Tab 3 - Merge Two BWTs (advanced)
	*/

	generateAdvancedBWTs: function() {
		var el = this.activateDivOut(3, false, true);
		el.innerHTML = '<div>' + GML.generate_BWTs_advanced(
			document.getElementById('in-string-3-1').value.toUpperCase(),
			document.getElementById('in-string-3-2').value.toUpperCase()) + '</div>';
	},

	mergeAdvancedBWTs: function() {
		var el = this.activateDivOut(3, true, true);
		el.innerHTML = '<div>' + GML.merge_BWTs_advanced(
			document.getElementById('in-string-3-1').value.toUpperCase(),
			document.getElementById('in-string-3-2').value.toUpperCase()) + '</div>';
	},



	/*
		Tab 5 - Merge Two XBWs
	*/

	mergeGraphXBWs: function() {
		var el = this.activateDivOut(5, true, true);
		el.innerHTML = '<div>' + GML.merge_XBWs(
			document.getElementById('in-string-5-1').value.toUpperCase(),
			document.getElementById('in-string-5-2').value.toUpperCase()) + '</div>';
	},



	/*
		Tab 6 - Fuse Two XBWs
	*/

	fuseGraphXBWs: function() {
		var el = this.activateDivOut(6, true, true);

		// TODO EMRG :: actually implement fuse_XBWs within GML
		el.innerHTML = '<div>' + GML.errorWrap('Sorry, this has not yet been implemented.') + '</div>';

		/*
		el.innerHTML = '<div>' + GML.fuse_XBWs(
			document.getElementById('in-string-6-1').value.toUpperCase(),
			document.getElementById('in-string-6-2').value.toUpperCase()) + '</div>';
		*/
	},



	/*
		Input Enter Key Press
	*/

	// function called on input enter - can be overwritten by the current input field
	// (the function is encoded as array, with the first value representing which library
	// is called - e.g. 'XBW' - and the second one representing the name of the function,
	// with all further values representing extra parameters which are sent to the function
	// in addition to the current tab)
	in_func: [],

	inputEnter: function(e) {

		if (!e) e = window.event;
		var keyCode = e.keyCode || e.which;

		// Enter pressed?
		if (keyCode == '13') {

			switch (GML_UI.in_func[0]) {

				case 'XBW':
					GML.XBWs[GML_UI.cur_tab][GML_UI.in_func[1]](GML_UI.cur_tab);
					break;

				case 'UI':
					GML_UI[GML_UI.in_func[1]]();
					break;

				case 'index':
					switch (GML_UI.in_func[1]) {
						case 'jumpto':
							document.getElementById(GML_UI.in_func[2]).focus();
							break;
					}
					break;
			}
		}
	},



	/*
		Input Randomization
	*/

	randomizeGraphInput: function(input_id) {

		document.getElementById(input_id).value = GML.generateRandomGraphString();
	},



	/*
		Info fields
	*/

	s_naiveInputFormat:
		'Input format:<br>' +
		'<ul>' +
		'<li>In general, all characters are just entered as plain text string, e.g. <code>ACA</code>.</li>' +
		'<li>Lower case characters will automatically be converted to upper case, ' +
		'e.g. <code>aca</code> to <code>ACA</code>.</li>' +
		'<li>To encode a graph, use the bubble notation, ' +
		'e.g. to encode both <code>AAA</code> and <code>ACA</code>, use <code>A(A|C)A</code>.</li>' +
		'<li>Do not add a dollar sign at the end of the input, as it will be added automagically.</li>' +
		'</ul>' +
		'</div>',

	s_advancedInputFormat:
		'Input format:<br>' +
		'<ul>' +
		'<li>In general, all characters of the main path (any one path from # to $) ' +
		'are just entered as plain text string, e.g. <code>ACA</code>.</li>' +
		'<li>Lower case characters will automatically be converted to upper case, ' +
		'e.g. <code>aca</code> to <code>ACA</code>.</li>' +
		'<li>To encode a graph, add a single pipe character after the main path, ' +
		'followed by infoblocks for each path, separated by semicolons, ' +
		'e.g. <code>mainpath|infoblock;infoblock;infoblock</code>.</li>' +
		'<li>Each infoblock contains exactly four parts, separated by commas, ' +
		'e.g. <code>ACA|1,2,3,4;1,2,3,4;1,2,3,4</code>.' +
		'<ol>' +
		'<li>The first part is the identifier of the path, which can contain letters and underscores, ' +
		'as well as numbers in any position but the first. The identifier can also be left empty.</li>' +
		'<li>The second part is the origin of the path, containing the identifier of the path ' +
		'on which this one originates followed by <code>:</code> and the position within that path ' +
		'at which it originates - the identifier of the main path ' +
		'is <code>mp</code>, but in the special case of the main path the identifier and the ' +
		'<code>:</code> can ' +
		'be left out together, e.g. <code>mp:8</code> or just <code>8</code> for ' +
		'position eight on the main path, ' +
		'but <code>path9:8</code> for position eight on a path with the identifier ' +
		'<code>path9</code>.<br>' +
		// TODO :: the following restriction came into place because otherwise
		// self-referential infoblocks might make it really hard to build the path
		// as automaton... however, it would be great if we could drop this restriction.
		'Identifiers need to be defined before they can be used, that is, ' +
		'<code>AC|a,1,G,2;,a:0,C,3</code> is valid, while ' +
		'<code>AC|,a:0,C,3;a,1,G,2</code> is not valid.<br>' +
		'(The counting starts at the specified array offset, ' +
		'so with array offset of 0 the hash tag symbol on the main path would be <code>mp:0</code> ' +
		'and the first alphabetical character on the main path would be <code>mp:1</code>, ' +
		'while the first alphabetical character on a path with the identifier <code>path9</code> ' +
		'would be <code>path9:0</code>.<br>' +
		'Assuming an array offset of 1, the hash tag symbol on the main path would be <code>mp:1</code> ' +
		'while the first alphabetical character on the main path would be <code>mp:2</code>.)' +
		'</li>' +
		'<li>The third part is the content of the path (it can be empty), e.g. <code>TGC</code>.</li>' +
		'<li>The fourth part is the target of the path, specified according to the same ' +
		'format as the origin of the path in the second part.</li>' +
		'</ol>' +
		'</li>' +
		'<li>Overall, a valid graph can look like <code>GACG|p1,1,TGG,3;,p1:0,C,p1:2</code> - this ' +
		'example could in bubble notation be rewritten as <code>G(A|T(G|C)G)CG</code>, assuming an ' +
		'array offset of 0.</li>' +
		'<li>Do neither add a hash tag symbol at the start ' +
		'nor a dollar sign at the end of the input, as they will be added automagically.</li>' +
		'</ul>' +
		'</div>',

	s_input:
		'Use this field to specify the graph ' + GML.DH +
		' for which the BWT will be generated.<br><br>',

	s_input1:
		'Use this field to specify the first graph, ' + GML.DH_1 +
		', which will be merged with ' + GML.DH_2 + '.<br><br>',

	s_input2:
		'Use this field to specify the second graph, ' + GML.DH_2 +
		', which will be merged with ' + GML.DH_1 + '.<br><br>',

	clickOnXBWInfo: function(e, id) {
		
		var el = document.getElementById('xbw-info-box-' + id);

		if (el.style.display == 'block') {
			el.style.display = 'none';
		} else {
			el.style.display = 'block';
		}

		e.stopPropagation();
	},

	generateNaiveBWTIn1Info: function(e) {
		var el = this.activateDivOut(0, false, false);
		
		var sout = '<div>';
		sout += '<u>Na&iuml;ve Graph BWT Generation - Input</u><br><br>';

		sout += this.s_input;
		sout += this.s_naiveInputFormat;

		el.innerHTML = sout;
		e.stopPropagation();
	},

	mergeNaiveBWTsIn1Info: function(e) {
		var el = this.activateDivOut(1, false, false);
		
		var sout = '<div>';
		sout += '<u>Na&iuml;ve Graph BWT Merging - Input 1</u><br><br>';

		sout += this.s_input1;
		sout += this.s_naiveInputFormat;

		el.innerHTML = sout;
		e.stopPropagation();
	},

	mergeNaiveBWTsIn2Info: function(e) {
		var el = this.activateDivOut(1, false, false);
		
		var sout = '<div>';
		sout += '<u>Na&iuml;ve Graph BWT Merging - Input 2</u><br><br>';

		sout += this.s_input2;
		sout += this.s_naiveInputFormat;

		el.innerHTML = sout;
		e.stopPropagation();
	},

	mergeNaiveBWTsInfo: function(e) {
		var el = this.activateDivOut(1, false, false);
		
		var sout = '<div>';
		sout += '<u>Na&iuml;ve Graph BWT Merging</u><br><br>';
		sout += 'Limitations:<br>';
		sout += '<ul>';
		sout += '<li>only works with at most one bubble in ' + GML.H_1 + ' and ' + GML.H_2 +
				' each (with each bubble having two alternatives, each being one character long)</li>';
		sout += '<li>only sorts ' + GML.H_1 + ' and ' + GML.H_2 +
				' by first bubble alternative (so if ' + GML.H_1 + ' is sorted before ' +
				GML.H_2 + ', then ' + GML.H_1 + ' gets ' + GML.DS_1_o +
				', even if the alternative path in ' + GML.H_1 + ' would be sorted after ' +
				GML.H_2 + ' - it would be better to give ' + GML.DS_1_o +
				' up to $<span class="d">4</span> to both alternatives of both strings separately, ' +
				'instead of assigning the same $ to each alternative in the string)</li>';
		sout += '</ul>';
		sout += '</div>';

		el.innerHTML = sout;

		e.stopPropagation();
	},


	generateAdvancedBWTIn1Info: function(e) {
		var el = this.activateDivOut(2, false, false);
		
		var sout = '<div>';
		sout += '<u>Graph BWT Generation - Input</u><br><br>';

		sout += this.s_input;
		sout += this.s_advancedInputFormat;

		el.innerHTML = sout;
		e.stopPropagation();
	},


	mergeAdvancedBWTsIn1Info: function(e) {
		var el = this.activateDivOut(3, false, false);
		
		var sout = '<div>';
		sout += '<u>Graph BWT Merging - Input 1</u><br><br>';

		sout += this.s_input1;
		sout += this.s_advancedInputFormat;

		el.innerHTML = sout;
		e.stopPropagation();
	},

	mergeAdvancedBWTsIn2Info: function(e) {
		var el = this.activateDivOut(3, false, false);
		
		var sout = '<div>';
		sout += '<u>Graph BWT Merging - Input 2</u><br><br>';

		sout += this.s_input2;
		sout += this.s_advancedInputFormat;

		el.innerHTML = sout;
		e.stopPropagation();
	},


	mergeGraphXBWsIn1Info: function(e) {
		var el = this.activateDivOut(5, false, false);
		
		var sout = '<div>';
		sout += '<u>Graph XBW Merging - Input 1</u><br><br>';

		sout += this.s_input1;
		sout += this.s_advancedInputFormat;

		el.innerHTML = sout;
		e.stopPropagation();
	},

	mergeGraphXBWsIn2Info: function(e) {
		var el = this.activateDivOut(5, false, false);
		
		var sout = '<div>';
		sout += '<u>Graph XBW Merging - Input 2</u><br><br>';

		sout += this.s_input2;
		sout += this.s_advancedInputFormat;

		el.innerHTML = sout;
		e.stopPropagation();
	},


	fuseGraphXBWsIn1Info: function(e) {
		var el = this.activateDivOut(6, false, false);
		
		var sout = '<div>';
		sout += '<u>Graph XBW Fusing - Input 1</u><br><br>';

		sout += this.s_input1;
		sout += this.s_advancedInputFormat;

		el.innerHTML = sout;
		e.stopPropagation();
	},

	fuseGraphXBWsIn2Info: function(e) {
		var el = this.activateDivOut(6, false, false);
		
		var sout = '<div>';
		sout += '<u>Graph XBW Fusing - Input 2</u><br><br>';

		sout += this.s_input2;
		sout += this.s_advancedInputFormat;

		el.innerHTML = sout;
		e.stopPropagation();
	},


	hideObject: function(whichOne) {

		var svg_el = document.getElementById('hide-cont-' + whichOne);
		var svg_hide_el = document.getElementById('hide-btn-' + whichOne);

		// get the last element within the hide wrapper which is NOT a <br>,
		// and hide or show it
		for (var i = svg_el.children.length-1; i > -1; i--) {
			if (svg_el.children[i].tagName.toUpperCase() !== 'BR') {
				svg_el = svg_el.children[i];
				break;
			}
		}

		if (svg_el.style.display == 'none') {
			svg_el.style.display = 'block';
			svg_hide_el.innerHTML = 'Hide';
		} else {
			svg_el.style.display = 'none';
			svg_hide_el.innerHTML = 'Show';
		}
	},



	svg_font_latex: true,

	showDropdown: function(whichOne) {

		var bodyRect = document.body.getBoundingClientRect();
		var dropdown = document.getElementById('dropdown');

		var rect = document.getElementById("hide-cont-" + whichOne).getBoundingClientRect();

		dropdown.style.top = (rect.top - bodyRect.top - 20) + 'px';
		dropdown.style.display = 'block';
	},

	hideDropdown: function() {
		document.getElementById('dropdown').style.display = 'none';
	},

	saveSVG: function(whichOne) {

		var dropdown = document.getElementById('dropdown');

		dropdown.innerHTML =
			'<span class="infobtn" onclick="GML_UI.exportSVG(' + whichOne + ')">' +
				'Export SVG code</span>' +
			'<span class="infobtn" onclick="GML_UI.downloadSVG(' + whichOne + ')">' +
				'Download as SVG</span>';

		this.showDropdown(whichOne);
	},

	downloadSVG: function(whichOne) {
		this.downloadOrExportSVG(whichOne, 'application/octet-stream');
	},

	exportSVG: function(whichOne) {
		this.downloadOrExportSVG(whichOne, 'text/plain');
	},

	downloadOrExportSVG: function(whichOne, mime) {

		this.hideDropdown();

		var svg = document.getElementById("hide-cont-" + whichOne).getElementsByTagName('svg')[0];

		var serializer = new XMLSerializer();
		var source = serializer.serializeToString(svg);



		// if we use a LaTeX-compatible font (like Times New Roman), then we want to
		// replace # by bold #, as the regular # in Times New Roman is veeery narrow
		if (this.svg_font_latex) {
			while (source.indexOf('">#</text>') > -1) {
				source = source.replace(
					'">#</text>',
					'"><tspan style="font-weight:bold;">#</tspan></text>'
				);
			}
		}


		// add CSS so that the SVG file can stand on its own

		var sprev = '<style type="text/css">';

		sprev += 'svg * {';
			sprev += 'color:#000;';
			if (this.svg_font_latex) {
				sprev += 'font-family:Times New Roman,serif;';
			} else {
				sprev += 'font-family:Calibri,Candara,Segoe,Segoe UI,Optima,Arial,sans-serif;';
			}
			sprev += 'line-height:22px;';
			sprev += 'font-style:normal;';
			sprev += 'font-weight:500;';
		sprev += '}';

		sprev += 'svg > text, svg > text > tspan {';
			sprev += 'font-size:30px;';
		sprev += '}';

		sprev += 'svg > text.prefix, svg > text.prefix > tspan {';
			sprev += 'font-size:15px;';
		sprev += '}';

		/* subscript */
		sprev += 'svg > text > tspan.d {';
		sprev += 'font-size:20px;';
		sprev += '}';
		sprev += 'svg > text.prefix > tspan.d {';
			sprev += 'font-size:10px;';
		sprev += '}';

		/* superscript */
		sprev += 'svg > text > tspan.u {';
			sprev += 'font-size:20px;';
		sprev += '}';
		sprev += 'svg > text.prefix > tspan.u {';
			sprev += 'font-size:10px;';
		sprev += '}';

		sprev += '</style>';


		source = source.slice(0, source.indexOf('>')+1) + sprev + source.slice(source.indexOf('>')+1);


		source = '<?xml version="1.0" standalone="no"?>\r\n' + source;

		var url = "data:" + mime + ","+encodeURIComponent(source);

		window.open(url, '_blank');
	},

	saveTable: function(whichOne) {

		var dropdown = document.getElementById('dropdown');

		dropdown.innerHTML =
			'<span class="infobtn" onclick="GML_UI.exportHTMLTable(' + whichOne + ')">' +
				'Export HTML code</span>' +
			'<span class="infobtn" onclick="GML_UI.exportLaTeXTable(' + whichOne + ')">' +
				'Export LaTeX code</span>';

		this.showDropdown(whichOne);
	},

	exportHTMLTable: function(whichOne) {

		this.hideDropdown();

		var table = document.getElementById("hide-cont-" + whichOne).getElementsByTagName('table')[0];

		var source = '<table>' + table.innerHTML + '</table>';

		var url = "data:text/plain,"+encodeURIComponent(source);

		window.open(url, '_blank');
	},

	exportLaTeXTable: function(whichOne) {

		this.hideDropdown();

		// get the tbody
		var tbody = document.getElementById("hide-cont-" + whichOne).getElementsByTagName('tbody')[0];

		var source = '';

		source += '\\begin{table}[htb]\n';
		source += '\\centering\n';
		source += '\\caption[GML table]{This is a table from GML.}\n';
		source += '\\begin{tabularx}{1.0\\textwidth}{ ';
		
		// iterate over the cells in the first row and define that many columns in the LaTeX table
		var rowlen = tbody.children[0].children.length;
		for (var i=0; i < rowlen; i++) {
			source += '| c ';
		}
		
		source += '| }\n';
		source += '\\hline\n';

		// iterate over all rows 
		var len = tbody.children.length;
		for (var i=0; i < len; i++) {
			rowlen = tbody.children[i].children.length;

			// iterate over all cells within all rows
			for (var j=0; j < rowlen; j++) {

				if (j === rowlen-1) {
					source += '\\textbf{'
				}

				// append the cell data to the LaTeX table
				source += tbody.children[i].children[j].innerHTML;

				if (j < rowlen-1) {
					source += ' & ';
				} else {
					source += '} \\\\ \\hline \n';
				}
			}
		}

		source += '\\end{tabularx}\n';
		source += '\\label{table:gml_table}\n';
		source += '\\end{table}';

		// replace some HTML commands with some LaTeX commands which do essentially the same =)
		source = source.split('$').join('\\$');
		source = source.split('#').join('\\#');
		source = source.split('\\textbf{<i>i</i>}').join('$\\boldsymbol{i}$');
		source = source.split('\\textbf{<i>M</i>}').join('$\\boldsymbol{M}$');
		source = source.split('\\textbf{<i>F</i>}').join('$\\boldsymbol{F}$');
		source = source.split('<i>').join('$');
		source = source.split('</i>').join('$');
		source = source.split('First&nbsp;Column').join('FC');
		source = source.split('&nbsp;').join('~');

		var url = "data:text/plain,"+encodeURIComponent(source);

		window.open(url, '_blank');
	},



	changeOptions_verbosity_capture: false,
	changeOptions_verbosity_compwidth: 100,

	changeOptions_verbosity_mouse: function(e, down) {
		this.changeOptions_verbosity_capture = down;

		if (down) {
			this.changeOptions_verbosity_move(e);
		}
	},

	changeOptions_verbosity_move: function(e) {
		if (this.changeOptions_verbosity_capture) {
			var rect = document.getElementById('in-options-verbosity').getBoundingClientRect();
			var compwidth = ((100 * (e.clientX - rect.left)) / rect.width);

			this.changeOptions_verbosity_compwidth = compwidth;

			this.changeOptions_verbosity_update();
		}
	},

	changeOptions_verbosity_to_str: [
		'tell me nothing', // 0
		'the results and nothing else please', // 1
		'low', // 2
		'below medium', // 3
		'slightly below medium', // 4
		'medium', // 5
		'slightly above medium', // 6
		'above medium', // 7
		'high', // 8
		'very high', // 9
		'tell me everything', // 10
	],

	changeOptions_verbosity_update: function() {

			if (this.changeOptions_verbosity_compwidth > 99) {
				this.changeOptions_verbosity_compwidth = 100;
			}
			if (this.changeOptions_verbosity_compwidth < 1) {
				this.changeOptions_verbosity_compwidth = 0;
			}

			document.getElementById('in-options-verbosity-inner').style.width =
				this.changeOptions_verbosity_compwidth + '%';

			var verbosity = Math.round((this.changeOptions_verbosity_compwidth + 5) / 10);
			if (verbosity < 1) {
				verbosity = 1;
			}
			if (verbosity > 10) {
				verbosity = 10;
			}

			document.getElementById('verbosity-out').innerHTML =
				this.changeOptions_verbosity_to_str[verbosity];

			GML.verbosity = verbosity;

			this.animateApplyBtn(true);
	},

	changeOptions_checkbox: function(checkbox_id) {

		var el = document.getElementById(checkbox_id);

		if (el.innerHTML == 'X') {
			el.innerHTML = '&nbsp;';
		} else {
			el.innerHTML = 'X';
		}

		this.animateApplyBtn(true);
	},

	changeOptions_show_graph: function() {

		var el = document.getElementById('in-options-show-graph');
		var it = document.getElementById('show-graph-info-text');

		if (el.innerHTML == 'X') {
			el.innerHTML = '&nbsp;';
			it.style.display = 'none';
		} else {
			el.innerHTML = 'X';
			it.style.display = 'inline';
		}

		this.animateApplyBtn(true);
	},

	applyBtncurrentlyanimated: false,
	applyBtncurrentlyintervalID: 0,
	applyBtncurrentlycallint: 0,
	applyBtncurrentlygoingdown: true,

	// if startanimation is set to false, then we stop the animation instead
	animateApplyBtn: function(startanimation) {

		if (this.applyBtncurrentlyanimated !== startanimation) {

			this.applyBtncurrentlyanimated = startanimation;

			this.applyBtncurrentlycallint = 0;

			if (startanimation) {

				this.applyBtncurrentlyintervalID = window.setInterval(
					GML_UI.animateApplyBtnCall, 50);

			} else {

				clearInterval(this.applyBtncurrentlyintervalID);
				this.animateApplyBtnCall();
			}
		}
	},

	animateApplyBtnCall: function() {

		if (GML_UI.applyBtncurrentlygoingdown) {
			GML_UI.applyBtncurrentlycallint -= 5;
		} else {
			GML_UI.applyBtncurrentlycallint += 5;
		}

		if (GML_UI.applyBtncurrentlycallint < -17) {
			GML_UI.applyBtncurrentlygoingdown = false;
			GML_UI.applyBtncurrentlycallint = -17;
		}

		if (GML_UI.applyBtncurrentlycallint > 108) {
			GML_UI.applyBtncurrentlygoingdown = true;
			GML_UI.applyBtncurrentlycallint = 108;
		}

		var c = 238 - GML_UI.applyBtncurrentlycallint;
		document.getElementById('id-apply-btn').style.backgroundColor =
			'rgb(' + c + ',' + c + ',' + c + ')';
	},

	applyOptions: function() {

		GML.ao = parseInt(document.getElementById('in-options-array-offset').value, 10);
		GML.loop_threshold = parseInt(document.getElementById('in-options-loop-threshold').value, 10);

		this.saveOptions();
	},

	resetOptions: function() {

		document.getElementById('in-options-array-offset').value = '0';
		document.getElementById('in-options-loop-threshold').value = '100';

		document.getElementById('in-options-show-xbw-envs').innerHTML = 'X';
		document.getElementById('in-options-show-graph').innerHTML = '&nbsp;';
		document.getElementById('in-options-show-autoi').innerHTML = '&nbsp;';
		document.getElementById('in-options-show-hashtag').innerHTML = 'X';
		document.getElementById('in-options-show-dollarsign').innerHTML = 'X';
		document.getElementById('in-options-always-search-path').innerHTML = '&nbsp;';
		document.getElementById('in-options-find-shortest-path').innerHTML = '&nbsp;';
		document.getElementById('in-options-vis-alternate').innerHTML = 'X';

		document.getElementById('in-options-svg-font-latex').innerHTML = 'X';

		this.changeOptions_verbosity_compwidth = 100;

		this.applyOptions();
	},

	saveOptions: function() {

		for (var i = 0; i < this.upToTabs; i++) {
			this.div_out_visibility[i] = false;
			this.xbw_visibility[i] = false;
			this.setJumpDispStyle(i, false);
		}

		GML.hideXBWenvironments = document.getElementById('in-options-show-xbw-envs').innerHTML != 'X';

		GML.set_to_HTML();

		var dispstyle = 'none';

		if (document.getElementById('in-options-show-graph').innerHTML == 'X') {
			dispstyle = 'inline-block';
		}

		GML.show_auto_i = document.getElementById('in-options-show-autoi').innerHTML == 'X';

		document.getElementById('tab-btn-0').style.display = dispstyle;
		document.getElementById('tab-btn-1').style.display = dispstyle;

		var env_links = document.getElementsByClassName('xbw-env-link');
		var env_disp = 'block';

		if (GML.hideXBWenvironments) {
			env_disp = 'none';
		}

		for (var i=0; i < env_links.length; i++) {
			env_links[i].style.display = env_disp;
		}

		GML_UI.svg_font_latex = document.getElementById('in-options-svg-font-latex').innerHTML == 'X';

		GML.vis_show_hashtag = document.getElementById('in-options-show-hashtag').innerHTML == 'X';
		GML.vis_show_dollarsign = document.getElementById('in-options-show-dollarsign').innerHTML == 'X';
		GML.vis_always_search_path = document.getElementById('in-options-always-search-path').innerHTML == 'X';
		GML.vis_find_shortest_path = document.getElementById('in-options-find-shortest-path').innerHTML == 'X';
		GML.vis_alternate = document.getElementById('in-options-vis-alternate').innerHTML == 'X';

		this.changeOptions_verbosity_update();

		this.animateApplyBtn(false);
	},

};



GML_UI.init();
