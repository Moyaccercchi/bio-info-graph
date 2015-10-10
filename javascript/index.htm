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

		div.mainbox > div, div.mainbox > input, div.mainbox > button {
			display:block;
			margin-bottom:16px;
			width:100%;
		}

		div.mainbox > div > button {
			display:inline;
			width:49%;
		}

		input, button {
			border:1px solid #000;
			box-shadow:0px 0px 5px 0px rgba(0, 0, 0, 0.8);
			border-radius:4px;
		}

		input {
			background: #FFF none repeat scroll 0% 0%;
			padding:2px 9px;
		}

		button {
			padding:2px 5px;
		}

		button, div.button {
			background: #EEE none repeat scroll 0% 0%;
			cursor:pointer;
		}

		button:hover, div.button:hover {
			background: #DDD none repeat scroll 0% 0%;
		}

		button:active, div.button:active {
			background: #CCC none repeat scroll 0% 0%;
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
	</style>
</head>
<body>

	<script src="create_BWT_merge.js"></script>

	<div class="tabbox">
		<div class="button" id="tab-btn-0" onclick="showTab(0)">
			Generate One BWT
		</div>
		<div class="button active" id="tab-btn-1" onclick="showTab(1)">
			Merge Two BWTs
		</div>
	</div>



	<div id="div-in-0" class="mainbox" style="display:none">
		<div>
			Please enter the string that you are interested in:
		</div>
		<input id="in-string-0" type="text" value="A(A|C)CA"></input>
		<div>
			<button type="button" onclick="generateNaiveBWT()">Generate na&iuml;ve BWT</button>
			<button type="button" style="float:right;" onclick="generateAdvancedBWT()">Generate advanced BWT (see Siren 2014)</button>
		</div>
	</div>

	<div id="div-out-0" class="mainbox" style="display:none">
	</div>


	<div id="div-in-1" class="mainbox">
		<div>
			Please enter the two strings that you are interested in:
		</div>
		<input id="in-string-1-1" type="text" value="A(A|C)CA"></input>
		<input id="in-string-1-2" type="text" value="ACCC"></input>
		<div>
			<button type="button" onclick="generateNaiveBWTs()">Generate na&iuml;ve BWTs</button>
			<button type="button" style="float:right;" onclick="mergeNaiveBWTs()">Merge na&iuml;ve BWTs (see Holt 2014)</button>
		</div>
		<div>
			<button type="button" onclick="generateAdvancedBWTs()">Generate advanced BWTs (see Siren 2014)</button>
			<button type="button" style="float:right;" onclick="mergeAdvancedBWTs()">Merge advanced BWTs (see Siren 2014, Holt 2014)</button>
		</div>
	</div>

	<div id="div-out-1" class="mainbox" style="display:none">
	</div>



	<span class="creditline absleft">
		Version: 0.0.0.1
	</span>
	<span class="creditline absright">
		Moyaccercchi (tws@hi.is), 2015
	</span>
	
	<script>
		c.set_to_HTML();

		// stores the visibility of div-out so that we don't reset it when changing tabs
		div_out_visibility = [false, false];



		/*
			Tab Control
		*/

		function unShowAllTabs() {
			for (var i = 0; i < 2; i++) {
				document.getElementById('tab-btn-' + i).className = 'button';
				document.getElementById('div-in-' + i).style.display = 'none';
				document.getElementById('div-out-' + i).style.display = 'none';
			}
		}

		function showTab(nexttab) {
			unShowAllTabs();

			document.getElementById('tab-btn-' + nexttab).className = 'button active';
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
			Tab 1 - Generate One BWT
		*/

		function generateNaiveBWT() {
			var el = activateDivOut(0);
			el.innerHTML = '<div>' + c.generate_BWT_naively(
				document.getElementById('in-string-0').value) + '</div>';
		}

		function generateAdvancedBWT() {
			var el = activateDivOut(0);
			el.innerHTML = '<div>' + c.generate_BWT_advanced(
				document.getElementById('in-string-0').value) + '</div>';
		}




		/*
			Tab 2 - Merge Two BWTs
		*/

		function generateNaiveBWTs() {
			var el = activateDivOut(1);
			el.innerHTML = '<div>' + c.generate_BWTs_naively(
				document.getElementById('in-string-1-1').value,
				document.getElementById('in-string-1-2').value) + '</div>';
		}

		function mergeNaiveBWTs() {
			var el = activateDivOut(1);
			el.innerHTML = '<div>' + c.merge_BWTs_naively(
				document.getElementById('in-string-1-1').value,
				document.getElementById('in-string-1-2').value) + '</div>';
		}

		function generateAdvancedBWTs() {
			alert('Sorry, this has not yet been implemented.');
		}

		function mergeAdvancedBWTs() {
			alert('Sorry, this has not yet been implemented.');
		}
	</script>

</body>
</html>