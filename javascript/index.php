<!DOCTYPE html>
<html lang="@constant(lang)">
<head>
	<meta charset="utf-8">
	<title>BioInfoJS</title>
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

		div.mainbox {
			border:1px solid #000;
			border-radius:16px;
			margin-bottom:32px;
			box-shadow:0px 0px 5px 0px rgba(0, 0, 0, 0.8);
			padding:16px 16px 0px;
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
			background: #EEE none repeat scroll 0% 0%;
			cursor:pointer;
			padding:2px 5px;
		}

		button:hover {
			background: #DDD none repeat scroll 0% 0%;
		}

		button:active {
			background: #CCC none repeat scroll 0% 0%;
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

	<div id="div-in" class="mainbox">
		<div>
			Please enter the two strings that you want to consider:
		</div>
		<input id="in-string-1" type="text" value="A(A|C)CA"></input>
		<input id="in-string-2" type="text" value="ACCC"></input>
		<div>
			<button type="button" onclick="generateNaiveBWTs()">Generate na&iuml;ve BWTs</button>
			<button type="button" style="float:right;" onclick="mergeNaiveBWTs()">Merge na&iuml;ve BWTs (see Holt 2014)</button>
		</div>
		<div>
			<button type="button" onclick="generateAdvancedBWTs()">Generate advanced BWTs (see Siren 2014)</button>
			<button type="button" style="float:right;" onclick="mergeAdvancedBWTs()">Merge advanced BWTs (see Siren 2014, Holt 2014)</button>
		</div>
	</div>
	
	<div id="div-out" class="mainbox" style="display:none">
		
	</div>

	<span class="creditline absleft">
		Version: 0.0.0.1
	</span>
	<span class="creditline absright">
		Moyaccercchi (tws@hi.is), 2015
	</span>
	
	<script>
		c.set_to_HTML();

		function generateNaiveBWTs() {
			var el = document.getElementById('div-out');
			el.innerHTML = '<div>' + c.generate_BWT_naively(
				document.getElementById('in-string-1').value,
				document.getElementById('in-string-2').value) + '</div>';
			el.style.display = 'block';
		}

		function mergeNaiveBWTs() {
			var el = document.getElementById('div-out');
			el.innerHTML = '<div>' + c.merge_BWT_naively(
				document.getElementById('in-string-1').value,
				document.getElementById('in-string-2').value) + '</div>';
			el.style.display = 'block';
		}

		function generateAdvancedBWTs() {
			alert('Sorry, has not yet been implemented.');
		}

		function mergeAdvancedBWTs() {
			alert('Sorry, has not yet been implemented.');
		}
	</script>

</body>
</html>