<!DOCTYPE html>
<html lang="@constant(lang)">
<head>
	<meta charset="utf-8">
	<title>BioInfo</title>
	<style>
		* {
			color:#000;
			font-family:Calibri,Candara,Segoe,Segoe UI,Optima,Arial,sans-serif; 
			font-size:15px;
			line-height:22px;
			font-style:normal;
			font-weight:500;
			box-sizing:border-box;
			text-align:left;
			text-rendering:optimizeLegibility;
		}

		body {
			padding:16px;
			background:rgb(255, 255, 255) none repeat scroll 0% 0%;
			position:relative;
		}

		i, i > span {
			font-style:italic;
		}

		b {
			font-weight:bold;
		}

		span.creditline {
			position:absolute;
			bottom:8px;
		}

		.absleft {
			left:16px;
		}

		.absright {
			right:16px;
		}

		div.mainbox, div.tabbox > div {
			border:1px solid #000;
			border-radius:16px;
			box-shadow:0px 0px 5px 0px rgba(0, 0, 0, 0.8);
		}

		div.tabbox {
			padding:0px;
			margin-top:-48px;
			margin-bottom:84px;
		}

		div.tabbox > div {
			display:inline-block;
			padding:24px 12px 4px 12px;
			margin-bottom:8px;
			margin-right:8px;
			float:left;
		}

		div.mainbox {
			padding:16px 16px 0px 16px;
			margin-bottom:32px;
		}

		div.mainbox > div, div.mainbox > input, div.mainbox > div.button {
			display:block;
			margin-bottom:16px;
			width:100%;
		}

		div.mainbox > div > input {
			display:block;
			width:100%;
		}

		div.mainbox > div > div.md-2, div.mainbox > div > input.md-2 {
			display:inline-block;
			width:49%;
		}

		div.mainbox > div > div.md-2:last-child, div.mainbox > div > input.md-2:last-child {
			float:right;
		}

		input, div.button {
			border:1px solid #000;
			box-shadow:0px 0px 5px 0px rgba(0, 0, 0, 0.8);
			border-radius:4px;
		}

		input {
			background: #FFF none repeat scroll 0% 0%;
			padding:2px 9px;
		}

		div.button {
			padding:2px 0px;
			text-align:center;
		}

		div.button, div.tabbutton, span.infobtn, div.svg_btn {
			background: #EEE none repeat scroll 0% 0%;
			cursor:pointer;
		}

		div.button:hover, div.tabbutton:hover, span.infobtn:hover, div.svg_btn:hover {
			background: #DDD none repeat scroll 0% 0%;
		}

		div.button:active, div.tabbutton:active, span.infobtn:active, div.svg_btn:active {
			background: #CCC none repeat scroll 0% 0%;
		}

		span.infobtn {
			float:right;
			border:1px solid #000;
			box-shadow:0px 0px 5px 0px rgba(0, 0, 0, 0.8);
			border-radius:4px;
			line-height:20px;
			padding:0px 4px;
			display:block;
		}

		div.input-info-container {
			position:relative;
		}

		div.input-info-container > span.infobtn {
			position:absolute;
			right:6px;
			top:3px;
		}

		div.button > span.infobtn {
			margin-right:5px;
		}

		div.tabbox > div.active {
			padding-top:40px;
			font-weight:700;
		}

		/* subscript */
		span.d {
			font-size:11px;
			top:4px;
			position:relative;
		}

		/* superscript */
		span.u {
			font-size:11px;
			bottom:4px;
			position:relative;
		}

		/* highlight */
		td.h, td.h > span {
			color:#FFF;
			background-color:#F00;
		}

		/* extra highlight */
		td.x, td.x > span {
			color:#FFA;
			background-color:#A0A;
		}

		div.table_box {
			width:100%;
			text-align:center;
		}

		table {
			display:inline-block;
			max-width:100%;
			border-collapse:collapse;
			border: 1px solid rgb(0, 0, 0);
			box-shadow:0px 0px 5px 0px rgba(0, 0, 0, 0.8);
			margin-top:16px;
		}

		/* position texts with indices just like other texts */
		td {
			vertical-align:super;
		}

		tbody.vbars > tr > td, tbody.lastbar > tr > td {
			text-align:center;
		}

		/* vertical bars between all columns */
		tbody.vbars > tr > td {
			border-right:1px solid #000;
		}
		tbody.vbars > tr > td:last-child {
			text-align:left;
		}

		/* vertical bars only before last column */
		tbody.lastbar > tr > td:last-child {
			border-left:1px solid #000;
			text-align:left;
		}

		/* td only in existence for vertical bar */
		td.b {
			padding:1px 1px 0px 0px;
		}

		tr > td:last-child {
			border-right: 0px solid #FFF !important;
		}

		th, td {
			padding:1px 3px 2px 3px;
		}

		tr.barless > td {
			border-left:0px;
			border-right:0px;
		}
		tr.barless > td:first-child {
			border-left:1px solid #000;
		}
		tr.barless > td:nth-last-child(2) {
			border-right:1px solid #000;
		}
		tr.barless > td:last-child {
			border-left:1px solid #000;
			border-right:1px solid #000;
		}



		/* SVG */

		div.svg_container {
			position:relative;
			min-height:10px;
		}

		div.svg_btn {
			position:absolute;
			right:0px;
			top:-10px;
			border:1px solid #000;
			box-shadow:0px 0px 5px 0px rgba(0, 0, 0, 0.8);
			border-radius:4px;
			line-height:20px;
			padding:0px 4px;
		}

		svg {
			width:100%;
			height:300px;
		}

		.svgheight {
			max-height:300px;
		}
		
		svg > text, svg > text > tspan {
			font-size:3px;
		}

		svg > text.prefix, svg > text.prefix > tspan {
			font-size:1.5px;
		}

		/* subscript */
		svg > text > tspan.d {
			font-size:2px;
		}
		svg > text.prefix > tspan.d {
			font-size:1px;
		}

		/* superscript */
		svg > text > tspan.u {
			font-size:2px;
		}
		svg > text.prefix > tspan.u {
			font-size:1px;
		}
	</style>
</head>
<body>

	<script src="create_BWT_merge.js"></script>

	<div class="tabbox">
		<div class="tabbutton" id="tab-btn-0" onclick="showTab(0)" style="display:none">
			Generate Na&iuml;ve Graph BWT
		</div>
		<div class="tabbutton" id="tab-btn-1" onclick="showTab(1)" style="display:none">
			Merge Na&iuml;ve Graph BWTs
		</div>
		<div class="tabbutton" id="tab-btn-2" onclick="showTab(2)">
			Generate Graph BWT
		</div>
		<div class="tabbutton" id="tab-btn-3" onclick="showTab(3)">
			Merge Graph BWTs
		</div>
		<div class="tabbutton" id="tab-btn-5" onclick="showTab(5)">
			Merge Graph XBWs
		</div>
		<div class="tabbutton" id="tab-btn-4" onclick="showTab(4)">
			Options
		</div>
	</div>



	<div id="div-in-0" class="mainbox" style="display:none">
		<div>
			<b>This tab is using the na&iuml;ve graph BWT, which can only handle the very
			simplest graphs and has been discontinued. Please do not expect anything on this
			tab to actually work.</b><br><br>
			Here, you can enter a simple graph in bubble notation. <a id="a-jump-0-1" href="#in-jump-0-1" style="display:none">Go there</a><br>
			It will be converted into a na&iuml;ve graph BWT. <a id="a-jump-0-2" href="#div-out-0" style="display:none">Go there</a>
		</div>
		<div id="in-jump-0-1">
			Please enter the string that you are interested in:
		</div>
		<div class="input-info-container">
			<input id="in-string-0" type="text" value="A(A|C)CAACCC"></input>
			<span class="infobtn" onclick="generateNaiveBWTIn1Info(event)">Info</span>
		</div>
		<div class="button" onclick="generateNaiveBWT()">Generate na&iuml;ve graph BWT</div>
	</div>

	<div id="div-out-0" class="mainbox" style="display:none">
	</div>


	<div id="div-in-1" class="mainbox" style="display:none">
		<div>
			<b>This tab is using the na&iuml;ve graph BWT, which can only handle the very
			simplest graphs and has been discontinued. Please do not expect anything on this
			tab to actually work.</b><br><br>
			Here, you can enter two simple graphs in bubble notation. <a id="a-jump-1-1" href="#in-jump-1-1" style="display:none">Go there</a><br>
			They will both be converted into na&iuml;ve graph BWTs. <a id="a-jump-1-2" href="#div-out-1" style="display:none">Go there</a><br>
			These BWTs will then be merged. <a id="a-jump-1-3" href="#in-jump-1-3" style="display:none">Go there</a>
		</div>
		<div id="in-jump-1-1">
			Please enter the two strings that you are interested in:
		</div>
		<div class="input-info-container">
			<input id="in-string-1-1" type="text" value="A(A|C)CA"></input>
			<span class="infobtn" onclick="mergeNaiveBWTsIn1Info(event)">Info</span>
		</div>
		<div class="input-info-container">
			<input id="in-string-1-2" type="text" value="ACCC"></input>
			<span class="infobtn" onclick="mergeNaiveBWTsIn2Info(event)">Info</span>
		</div>
		<div>
			<div class="button md-2" onclick="generateNaiveBWTs()">Generate na&iuml;ve graph BWTs</div>
			<div class="button md-2" onclick="mergeNaiveBWTs()">Merge na&iuml;ve graph BWTs (see Holt 2014)<span class="infobtn" onclick="mergeNaiveBWTsInfo(event)">Info</span></div>
		</div>
	</div>

	<div id="div-out-1" class="mainbox" style="display:none">
	</div>


	<div id="div-in-2" class="mainbox" style="display:none">
		<div>
			Here, you can enter a graph. <a id="a-jump-2-1" href="#in-jump-2-1" style="display:none">Go there</a><br>
			It will be converted into a prefix-sorted automaton. <a id="a-jump-2-2" href="#div-out-2" style="display:none">Go there</a><br>
			The automaton will be used to initialize an XBW environment, in which path queries and substring searches can be executed. <a id="a-jump-2-3" href="#div-xbw-2" style="display:none">Go there</a>
		</div>
		<div id="in-jump-2-1">
			Please enter the string that you are interested in:
		</div>
		<div class="input-info-container">
			<input id="in-string-2" type="text" value="GACGTACCTG|,2,T,4;,3,,5;,6,,10;,6,,8"></input>
			<span class="infobtn" onclick="generateAdvancedBWTIn1Info(event)">Info</span>
		</div>
		<div class="button" onclick="generateAdvancedBWT()">Generate graph BWT (see Siren 2014)</div>
	</div>

	<div id="div-out-2" class="mainbox" style="display:none">
	</div>

	<div id="div-xbw-2" class="mainbox" style="display:none">
	</div>


	<div id="div-in-3" class="mainbox" style="display:none">
		<div>
			Here, you can enter two graphs. <a id="a-jump-3-1" href="#in-jump-3-1" style="display:none">Go there</a><br>
			They will both be converted into prefix-sorted automata. <a id="a-jump-3-2" href="#div-out-3" style="display:none">Go there</a><br>
			The automata will then be merged. <a id="a-jump-3-3" href="#in-jump-3-3" style="display:none">Go there</a><br>
			The resulting automaton will be used to initialize an XBW environment, in which path queries and substring searches can be executed. <a id="a-jump-3-4" href="#div-xbw-3" style="display:none">Go there</a>
		</div>
		<div id="in-jump-3-1">
			Please enter the two strings that you are interested in:
		</div>
		<div class="input-info-container">
			<input id="in-string-3-1" type="text" value="GACGT|,2,T,4;,3,,5"></input>
			<span class="infobtn" onclick="mergeAdvancedBWTsIn1Info(event)">Info</span>
		</div>
		<div class="input-info-container">
			<input id="in-string-3-2" type="text" value="ACCTG|,1,,5;,1,,3"></input>
			<span class="infobtn" onclick="mergeAdvancedBWTsIn2Info(event)">Info</span>
		</div>
		<div>
			<!-- div class="button" onclick="generateAdvancedBWTs()">Generate graph BWTs (see Siren 2014)</div -->
			<div class="button" onclick="mergeAdvancedBWTs()">Merge graph BWTs (see Holt 2014 and Siren 2014)</div>
		</div>
	</div>

	<div id="div-out-3" class="mainbox" style="display:none">
	</div>

	<div id="div-xbw-3" class="mainbox" style="display:none">
	</div>


	<div id="div-in-4" class="mainbox" style="display:none">
		<div>
			Please enter the two indices that should be used for the two graphs:
		</div>
		<div class="input-info-container">
			<input id="in-options-index-1" type="text" value="0" class="md-2"></input>
			<input id="in-options-index-2" type="text" value="1" class="md-2"></input>
		</div>
		<div onclick="changeOptions_show_graph()" style="cursor:pointer">
			<span id="in-options-show-graph" style="display: inline-block; box-shadow: 0px 0px 5px 0px rgba(0, 0, 0, 0.8); border-radius: 4px; border: 1px solid rgb(0, 0, 0); text-align: center; margin-right: 5px; width: 25px;">&nbsp;</span> Show na&iuml;ve graph functionality
		</div>
		<div>
			The first element of any string and array is obviously <input id="in-options-array-offset" type="text" value="0" style="display: inline-block; width: 10%; margin-right: 5px; margin-left: 5px;"></input>.
		</div>
		<div>
			<div class="button md-2" onclick="applyOptions()">Apply Options</div>
			<div class="button md-2" onclick="resetOptions()">Reset Options</div>
		</div>
	</div>

	<div id="div-out-4" class="mainbox" style="display:none">
	</div>


	<div id="div-in-5" class="mainbox" style="display:none">
		<div>
			Here, you can enter two graphs. <a id="a-jump-5-1" href="#in-jump-5-1" style="display:none">Go there</a><br>
			They will both be converted into prefix-sorted automata. <a id="a-jump-5-2" href="#div-out-5" style="display:none">Go there</a><br>
			The automata will then be used to initialize XBW environments. <a id="a-jump-5-3" href="#in-jump-5-3" style="display:none">Go there</a><br>
			These individual XBW environments will then be merged. <a id="a-jump-5-4" href="#in-jump-5-4" style="display:none">Go there</a>
		</div>
		<div id="in-jump-5-1">
			Please enter the two strings that you are interested in:
		</div>
		<div class="input-info-container">
			<input id="in-string-5-1" type="text" value="GACGT|,2,T,4;,3,,5"></input>
			<span class="infobtn" onclick="mergeGraphXBWsIn1Info(event)">Info</span>
		</div>
		<div class="input-info-container">
			<input id="in-string-5-2" type="text" value="ACCTG|,1,,5;,1,,3"></input>
			<span class="infobtn" onclick="mergeGraphXBWsIn2Info(event)">Info</span>
		</div>
		<div>
			<div class="button" onclick="mergeGraphXBWs()">Merge graph XBWs (see Holt 2014 and Siren 2014)</div>
		</div>
	</div>

	<div id="div-out-5" class="mainbox" style="display:none">
	</div>



	<span class="creditline absleft">
		Version: 0.0.1.2
	</span>
	<span class="creditline absright">
		Moyaccercchi (tws@hi.is), University of Iceland, 1<span class="u">st</span> Sep 2014 - 11<span class="u">th</span> Nov 2015
	</span>
	
	<script>
		c.set_to_HTML();

		// stores the visibility of div-out so that we don't reset it when changing tabs
		div_out_visibility = [false, false, false, false, false, false];

		// how many tabs are there?
		upToTabs = 6;



		/*
			Tab Control
		*/

		function unShowAllTabs() {
			for (var i = 0; i < upToTabs; i++) {
				document.getElementById('tab-btn-' + i).className = 'tabbutton';
				document.getElementById('div-in-' + i).style.display = 'none';
				document.getElementById('div-out-' + i).style.display = 'none';
				var el = document.getElementById('div-xbw-' + i);
				if (el) {
					el.style.display = 'none';
				}
			}
		}

		function showTab(nexttab) {
			unShowAllTabs();

			document.getElementById('tab-btn-' + nexttab).className = 'tabbutton active';
			document.getElementById('div-in-' + nexttab).style.display = 'block';
			if (div_out_visibility[nexttab]) {
				document.getElementById('div-out-' + nexttab).style.display = 'block';
				var el = document.getElementById('div-xbw-' + nexttab);
				if (el) {
					el.style.display = 'block';
				}
			}
		}

		function setJumpDispStyle(i, show) {

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
		}

		function activateDivOut(i, showXBWenv, showAnchors) {

			var el = document.getElementById('div-xbw-' + i);
			if (el) {
				if (showXBWenv) {
					el.style.display = 'block';
				} else {
					el.style.display = 'none';
				}
			}


			setJumpDispStyle(i, showAnchors);


			el = document.getElementById('div-out-' + i);
			el.innerHTML = '<div>... working on your request ...</div>';
			el.style.display = 'block';

			div_out_visibility[i] = true;

			return el;
		}



		/*
			Tab 0 - Generate One BWT (naively)
		*/

		function generateNaiveBWT() {
			var el = activateDivOut(0, false, true);
			el.innerHTML = '<div>' + c.generate_BWT_naively(
				document.getElementById('in-string-0').value.toUpperCase()) + '</div>';
		}




		/*
			Tab 1 - Merge Two BWTs (naively)
		*/

		function generateNaiveBWTs() {
			var el = activateDivOut(1, false, false);
			el.innerHTML = '<div>' + c.generate_BWTs_naively(
				document.getElementById('in-string-1-1').value.toUpperCase(),
				document.getElementById('in-string-1-2').value.toUpperCase()) + '</div>';
		}

		function mergeNaiveBWTs() {
			var el = activateDivOut(1, false, true);
			el.innerHTML = '<div>' + c.merge_BWTs_naively(
				document.getElementById('in-string-1-1').value.toUpperCase(),
				document.getElementById('in-string-1-2').value.toUpperCase()) + '</div>';
		}



		/*
			Tab 2 - Generate One BWT (advanced)
		*/
		function generateAdvancedBWT() {
			var el = activateDivOut(2, true, true);
			el.innerHTML = '<div>' + c.generate_BWT_advanced(
				document.getElementById('in-string-2').value.toUpperCase()) + '</div>';
		}




		/*
			Tab 3 - Merge Two BWTs (advanced)
		*/

		function generateAdvancedBWTs() {
			var el = activateDivOut(3, false, true);
			el.innerHTML = '<div>' + c.generate_BWTs_advanced(
				document.getElementById('in-string-3-1').value.toUpperCase(),
				document.getElementById('in-string-3-2').value.toUpperCase()) + '</div>';
		}

		function mergeAdvancedBWTs() {
			var el = activateDivOut(3, true, true);
			el.innerHTML = '<div>' + c.merge_BWTs_advanced(
				document.getElementById('in-string-3-1').value.toUpperCase(),
				document.getElementById('in-string-3-2').value.toUpperCase()) + '</div>';
		}



		/*
			Tab 5 - Merge Two XBWs
		*/

		function mergeGraphXBWs() {
			var el = activateDivOut(5, false, true);
			el.innerHTML = '<div>' + c.merge_XBWs(
				document.getElementById('in-string-5-1').value.toUpperCase(),
				document.getElementById('in-string-5-2').value.toUpperCase()) + '</div>';
		}



		/*
			Info fields
		*/

		var s_naiveInputFormat =
			'Input format:<br>' +
			'<ul>' +
			'<li>in general, all characters are just entered as plain text string, e.g. "ACA"</li>' +
			'<li>lower case characters will automatically be converted to upper case, ' +
			'e.g. "aca" to "ACA"</li>' +
			'<li>to encode a graph, use the bubble notation, ' +
			'e.g. to encode both "AAA" and "ACA", use "A(A|C)A"</li>' +
			'<li>do not add a dollar sign at the end of the input, as it will be added automagically</li>' +
			'</ul>' +
			'</div>';

		var s_advancedInputFormat =
			'Input format:<br>' +
			'<ul>' +
			'<li>in general, all characters of the main path (from # to $) ' +
			'are just entered as plain text string, e.g. "ACA"</li>' +
			'<li>lower case characters will automatically be converted to upper case, ' +
			'e.g. "aca" to "ACA"</li>' +
			'<li>to encode a graph, add a single pipe character after the main path, ' +
			'followed by infoblocks for each path, separated by semicolons, ' +
			'e.g. "mainpath|infoblock;infoblock;infoblock</li>' +
			'<li>each infoblock contains exactly four parts, separated by commas, ' +
			'e.g. "ACA|1,2,3,4;1,2,3,4;1,2,3,4"' +
			'<ol>' +
			'<li>the first part is the identifier of the path (it can be empty)</li>' +
			'<li>the second part is the origin of the path, containing the identifier of the path on which this one originates followed by ":" and the position in that path on which it originates (counting starts at 1 for the first alphabetical character, as the hash tag symbol in the main path is symbol 0) - the identifier of the main path is "0", but in the special case of the main path the identifer and the ":" can be left out together, e.g. "path9:8" for the eigth position in a path with the identifer "path9"</li>' +
			'<li>the third part is the content of the path (it can be empty), e.g. "TGC"</li>' +
			'<li>the fourth part is the target of the path, containing the identifier of the path on which this one ends followed by ":" and the position in that path on which it ends (counting starts at 1 for the first alphabetical character, as the hash tag symbol in the main path is symbol 0) - the identifier of the main path is "0", but in the special case of the main path the identifer and the ":" can be left out together, e.g. "path9:8" for the eigth position in a path with the identifer "path9"</li>' +
			'</ol>' +
			'</li>' +
			'<li>overall, a valid graph can look like "GACG|p1,1,TGG,3;,p1:1,C,3" - this ' +
			'example could in bubble notation be rewritten as G(A|T(G|C)G)CG</li>' +
			'<li>do neither add a hash tag symbole at the start ' +
			'nor a dollar sign at the end of the input, as they will be added automagically</li>' +
			'</ul>' +
			'</div>';

		var s_input =
			'Use this field to specify the string ' + c.DH +
			' for which the BWT will be generated.<br><br>';

		var s_input1 =
			'Use this field to specify the first string, ' + c.DH_1 +
			', which will be merged with ' + c.DH_2 + '.<br><br>';

		var s_input2 =
			'Use this field to specify the second string, ' + c.DH_2 +
			', which will be merged with ' + c.DH_1 + '.<br><br>';

		function generateNaiveBWTIn1Info(e) {
			var el = activateDivOut(0, false, false);
			
			var sout = '<div>';
			sout += '<u>Na&iuml;ve Graph BWT Generation - Input</u><br><br>';

			sout += s_input;
			sout += s_naiveInputFormat;

			el.innerHTML = sout;
			e.stopPropagation();
		}

		function mergeNaiveBWTsIn1Info(e) {
			var el = activateDivOut(1, false, false);
			
			var sout = '<div>';
			sout += '<u>Na&iuml;ve Graph BWT Merging - Input 1</u><br><br>';

			sout += s_input1;
			sout += s_naiveInputFormat;

			el.innerHTML = sout;
			e.stopPropagation();
		}

		function mergeNaiveBWTsIn2Info(e) {
			var el = activateDivOut(1, false, false);
			
			var sout = '<div>';
			sout += '<u>Na&iuml;ve Graph BWT Merging - Input 2</u><br><br>';

			sout += s_input2;
			sout += s_naiveInputFormat;

			el.innerHTML = sout;
			e.stopPropagation();
		}

		function mergeNaiveBWTsInfo(e) {
			var el = activateDivOut(1, false, false);
			
			var sout = '<div>';
			sout += '<u>Na&iuml;ve Graph BWT Merging</u><br><br>';
			sout += 'Limitations:<br>';
			sout += '<ul>';
			sout += '<li>only works with at most one bubble in ' + c.H_1 + ' and ' + c.H_2 + ' each (with each bubble having two alternatives, each being one character long)</li>';
			sout += '<li>only sorts ' + c.H_1 + ' and ' + c.H_2 + ' by first bubble alternative (so if ' + c.H_1 + ' is sorted before ' + c.H_2 + ', then ' + c.H_1 + ' gets ' + c.DS_1_o + ', even if the alternative path in ' + c.H_1 + ' would be sorted after ' + c.H_2 + ' - it would be better to give ' + c.DS_1_o + ' up to $<span class="d">4</span> to both alternatives of both strings separately, instead of assigning the same $ to each alternative in the string)</li>';
			sout += '</ul>';
			sout += '</div>';

			el.innerHTML = sout;

			e.stopPropagation();
		}


		function generateAdvancedBWTIn1Info(e) {
			var el = activateDivOut(2, false, false);
			
			var sout = '<div>';
			sout += '<u>Graph BWT Generation - Input</u><br><br>';

			sout += s_input;
			sout += s_advancedInputFormat;

			el.innerHTML = sout;
			e.stopPropagation();
		}


		function mergeAdvancedBWTsIn1Info(e) {
			var el = activateDivOut(3, false, false);
			
			var sout = '<div>';
			sout += '<u>Graph BWT Merging - Input 1</u><br><br>';

			sout += s_input1;
			sout += s_advancedInputFormat;

			el.innerHTML = sout;
			e.stopPropagation();
		}

		function mergeAdvancedBWTsIn2Info(e) {
			var el = activateDivOut(3, false, false);
			
			var sout = '<div>';
			sout += '<u>Graph BWT Merging - Input 2</u><br><br>';

			sout += s_input2;
			sout += s_advancedInputFormat;

			el.innerHTML = sout;
			e.stopPropagation();
		}


		function mergeGraphXBWsIn1Info(e) {
			var el = activateDivOut(5, false, false);
			
			var sout = '<div>';
			sout += '<u>Graph XBW Merging - Input 1</u><br><br>';

			sout += s_input1;
			sout += s_advancedInputFormat;

			el.innerHTML = sout;
			e.stopPropagation();
		}

		function mergeGraphXBWsIn2Info(e) {
			var el = activateDivOut(5, false, false);
			
			var sout = '<div>';
			sout += '<u>Graph XBW Merging - Input 2</u><br><br>';

			sout += s_input2;
			sout += s_advancedInputFormat;

			el.innerHTML = sout;
			e.stopPropagation();
		}


		function hideObject(whichOne) {
			
			var svg_el = document.getElementById('hide-cont-' + whichOne);
			var svg_hide_el = document.getElementById('hide-btn-' + whichOne);
			svg_el = svg_el.childNodes[1];

			if (svg_el.style.display == 'none') {
				svg_el.style.display = 'block';
				svg_hide_el.innerHTML = 'Hide';
			} else {
				svg_el.style.display = 'none';
				svg_hide_el.innerHTML = 'Show';
			}
		}



		function changeOptions_show_graph() {

			var el = document.getElementById('in-options-show-graph');

			if (el.innerHTML == 'X') {
				el.innerHTML = '&nbsp;';
			} else {
				el.innerHTML = 'X';
			}
		}

		function applyOptions() {

			c.origin_1 = document.getElementById('in-options-index-1').value;
			c.origin_2 = document.getElementById('in-options-index-2').value;

			c.ao = parseInt(document.getElementById('in-options-array-offset').value, 10);

			saveOptions();
		}

		function resetOptions() {

			document.getElementById('in-options-index-1').value = '0';
			document.getElementById('in-options-index-2').value = '1';

			document.getElementById('in-options-array-offset').value = '0';

			document.getElementById('in-options-show-graph').innerHTML = '&nbsp;';

			applyOptions();
		}

		function saveOptions() {

			for (var i = 0; i < upToTabs; i++) {
				div_out_visibility[i] = false;
				setJumpDispStyle(i, false);
			}

			c.set_to_HTML();

			var dispstyle = 'none';
			
			if (document.getElementById('in-options-show-graph').innerHTML == 'X') {
				dispstyle = 'inline-block';
			}

			document.getElementById('tab-btn-0').style.display = dispstyle;
			document.getElementById('tab-btn-1').style.display = dispstyle;
		};



		// default to tab 5
		showTab(5);
	</script>

</body>
</html>