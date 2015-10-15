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

		div.mainbox > div > div.button {
			display:inline-block;
			width:49%;
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
			padding:2px 5px;
		}

		div.button, div.tabbutton, span.infobtn {
			background: #EEE none repeat scroll 0% 0%;
			cursor:pointer;
		}

		div.button:hover, div.tabbutton:hover, span.infobtn:hover {
			background: #DDD none repeat scroll 0% 0%;
		}

		div.button:active, div.tabbutton:active, span.infobtn:active {
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

		/* SVG */
		svg {
			width:100%;
			height:300px;
		}
		
		svg > text {
			font-size:3px;
		}
	</style>
</head>
<body>

	<script src="create_BWT_merge.js"></script>

	<div class="tabbox">
		<div class="tabbutton" id="tab-btn-0" onclick="showTab(0)">
			Generate One Na&iuml;ve BWT
		</div>
		<div class="tabbutton" id="tab-btn-1" onclick="showTab(1)">
			Merge Two Na&iuml;ve BWTs
		</div>
		<div class="tabbutton" id="tab-btn-2" onclick="showTab(2)">
			Generate One Advanced BWT
		</div>
		<div class="tabbutton" id="tab-btn-3" onclick="showTab(3)">
			Merge Two Advanced BWTs
		</div>
	</div>



	<div id="div-in-0" class="mainbox" style="display:none">
		<div>
			Please enter the string that you are interested in:
		</div>
		<div class="input-info-container">
			<input id="in-string-0" type="text" value="A(A|C)CAACCC"></input>
			<span class="infobtn" onclick="generateNaiveBWTIn1Info(event)">Info</span>
		</div>
		<div class="button" onclick="generateNaiveBWT()">Generate na&iuml;ve BWT</div>
	</div>

	<div id="div-out-0" class="mainbox" style="display:none">
	</div>


	<div id="div-in-1" class="mainbox" style="display:none">
		<div>
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
			<div class="button" onclick="generateNaiveBWTs()">Generate na&iuml;ve BWTs</div>
			<div class="button" style="float:right;" onclick="mergeNaiveBWTs()">Merge na&iuml;ve BWTs (see Holt 2014)<span class="infobtn" onclick="mergeNaiveBWTsInfo(event)">Info</span></div>
		</div>
	</div>

	<div id="div-out-1" class="mainbox" style="display:none">
	</div>


	<div id="div-in-2" class="mainbox" style="display:none">
		<div>
			Please enter the string that you are interested in:
		</div>
		<div class="input-info-container">
			<input id="in-string-2" type="text" value="GACGTACCTG|,2,T,4;,3,,5;,6,,10;,6,,8"></input>
			<span class="infobtn" onclick="generateAdvancedBWTIn1Info(event)">Info</span>
		</div>
		<div class="button" onclick="generateAdvancedBWT()">Generate advanced BWT (see Siren 2014)</div>
	</div>

	<div id="div-out-2" class="mainbox" style="display:none">
	</div>


	<div id="div-in-3" class="mainbox" style="display:none">
		<div>
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
			<div class="button" onclick="generateAdvancedBWTs()">Generate advanced BWTs (see Siren 2014)</div>
			<div class="button" style="float:right;" onclick="mergeAdvancedBWTs()">Merge advanced BWTs (see Holt 2014 and Siren 2014)</div>
		</div>
	</div>

	<div id="div-out-3" class="mainbox" style="display:none">
	</div>



	<span class="creditline absleft">
		Version: 0.0.0.4
	</span>
	<span class="creditline absright">
		Moyaccercchi (tws@hi.is), 2015
	</span>
	
	<script>
		c.set_to_HTML();

		// stores the visibility of div-out so that we don't reset it when changing tabs
		div_out_visibility = [false, false, false, false];



		/*
			Tab Control
		*/

		function unShowAllTabs() {
			for (var i = 0; i < 4; i++) {
				document.getElementById('tab-btn-' + i).className = 'tabbutton';
				document.getElementById('div-in-' + i).style.display = 'none';
				document.getElementById('div-out-' + i).style.display = 'none';
			}
		}

		function showTab(nexttab) {
			unShowAllTabs();

			document.getElementById('tab-btn-' + nexttab).className = 'tabbutton active';
			document.getElementById('div-in-' + nexttab).style.display = 'block';
			if (div_out_visibility[nexttab]) {
				document.getElementById('div-out-' + nexttab).style.display = 'block';
			}
		}

		function activateDivOut(i) {
			var el = document.getElementById('div-out-' + i);
			el.innerHTML = '<div>... working on your request ...</div>';
			el.style.display = 'block';
			div_out_visibility[i] = true;
			return el;
		}



		/*
			Tab 1 - Generate One BWT (naively)
		*/

		function generateNaiveBWT() {
			var el = activateDivOut(0);
			el.innerHTML = '<div>' + c.generate_BWT_naively(
				document.getElementById('in-string-0').value.toUpperCase()) + '</div>';
		}




		/*
			Tab 2 - Merge Two BWTs (naively)
		*/

		function generateNaiveBWTs() {
			var el = activateDivOut(1);
			el.innerHTML = '<div>' + c.generate_BWTs_naively(
				document.getElementById('in-string-1-1').value.toUpperCase(),
				document.getElementById('in-string-1-2').value.toUpperCase()) + '</div>';
		}

		function mergeNaiveBWTs() {
			var el = activateDivOut(1);
			el.innerHTML = '<div>' + c.merge_BWTs_naively(
				document.getElementById('in-string-1-1').value.toUpperCase(),
				document.getElementById('in-string-1-2').value.toUpperCase()) + '</div>';
		}



		/*
			Tab 3 - Generate One BWT (advanced)
		*/
		function generateAdvancedBWT() {
			var el = activateDivOut(2);
			el.innerHTML = '<div>' + c.generate_BWT_advanced(
				document.getElementById('in-string-2').value.toUpperCase()) + '</div>';
		}




		/*
			Tab 4 - Merge Two BWTs (advanced)
		*/

		function generateAdvancedBWTs() {
			var el = activateDivOut(3);
			el.innerHTML = '<div>' + c.generate_BWTs_advanced(
				document.getElementById('in-string-3-1').value.toUpperCase(),
				document.getElementById('in-string-3-2').value.toUpperCase()) + '</div>';
		}

		function mergeAdvancedBWTs() {
			var el = activateDivOut(3);
			el.innerHTML = '<div>' + c.merge_BWTs_advanced(
				document.getElementById('in-string-3-1').value.toUpperCase(),
				document.getElementById('in-string-3-2').value.toUpperCase()) + '</div>';
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
			var el = activateDivOut(0);
			
			var sout = '<div>';
			sout += '<u>Na&iuml;ve BWT Generation - Input</u><br><br>';

			sout += s_input;
			sout += s_naiveInputFormat;

			el.innerHTML = sout;
			e.stopPropagation();
		}

		function mergeNaiveBWTsIn1Info(e) {
			var el = activateDivOut(1);
			
			var sout = '<div>';
			sout += '<u>Na&iuml;ve BWT Merging - Input 1</u><br><br>';

			sout += s_input1;
			sout += s_naiveInputFormat;

			el.innerHTML = sout;
			e.stopPropagation();
		}

		function mergeNaiveBWTsIn2Info(e) {
			var el = activateDivOut(1);
			
			var sout = '<div>';
			sout += '<u>Na&iuml;ve BWT Merging - Input 2</u><br><br>';

			sout += s_input2;
			sout += s_naiveInputFormat;

			el.innerHTML = sout;
			e.stopPropagation();
		}

		function mergeNaiveBWTsInfo(e) {
			var el = activateDivOut(1);
			
			var sout = '<div>';
			sout += '<u>Na&iuml;ve BWT Merging</u><br><br>';
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
			var el = activateDivOut(2);
			
			var sout = '<div>';
			sout += '<u>Advanced BWT Generation - Input</u><br><br>';

			sout += s_input;
			sout += s_advancedInputFormat;

			el.innerHTML = sout;
			e.stopPropagation();
		}


		function mergeAdvancedBWTsIn1Info(e) {
			var el = activateDivOut(3);
			
			var sout = '<div>';
			sout += '<u>Advanced BWT Merging - Input 1</u><br><br>';

			sout += s_input1;
			sout += s_advancedInputFormat;

			el.innerHTML = sout;
			e.stopPropagation();
		}

		function mergeAdvancedBWTsIn2Info(e) {
			var el = activateDivOut(3);
			
			var sout = '<div>';
			sout += '<u>Advanced BWT Merging - Input 2</u><br><br>';

			sout += s_input2;
			sout += s_advancedInputFormat;

			el.innerHTML = sout;
			e.stopPropagation();
		}


		// default to tab 3
		showTab(3);
	</script>

</body>
</html>