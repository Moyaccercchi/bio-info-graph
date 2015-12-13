/*
	***********************************
		GML - Graph Merging Library
	***********************************

	Flow through the program:

	The outside world calls "set_to_DaTeX" or "set_to_HTML" (if not, HTML is default.)
	The outside world calls "merge_BWTs_naively".
		It calls "build_BWTs_naively" which generates the BWT if necessary (if parameters since last execution changed.)
			It calls "prepare_BWTs_naively" if necessary.

	Attention!
	Currently, the DaTeX output is NOT FUNCTIONAL!
	Here are the known issues:
	1) DaTeX output uses SVG directly instead of TikZ or a similar TeX-based alternative
	2) # sign in DaTeX may not actually work (does '$ # $' work?)
	3) # sign in DaTeX-mode is not sorted correctly (as '$ # $' internally, it is sorted to front!)
	4) arr_to_highlighted_str() does not highlight anything in DaTeX mode
	5) in a line like "if (ac.indexOf('<') < 0) {" we will never have truth in DaTeX mode, where
	   checking for indexOf '_' might make more sense

	Attention again!
	Currently, the option of setting merge_directly to false is NOT FUNCTIONAL!
	Here are the known issues:
	1) Incorrect pruning at the very end of the merging algorithm (see [NOTE 1])

	And in general:
	In a graph that we consider here (at least in the prefix sorted graph that we consider as
	input for the merging), no node should be connected to another node by more than one edge -
	if there are two edges going out of a node, then they need to connect to DIFFERENT other nodes!
	(Otherwise, e.g. the nextNodes() function would not work... Also, it would be stupid to have
	that kind of situation, but I just thought I might want to let y'all know.)
	TODO :: add a check against this that runs before work is started =) (and print out an error
	message just in case)

	Index of functions in GML:
	set_to_DaTeX: function();
	set_to_HTML: function();
	make_DH: function(o);
	build_BWTs_naively: function(h1, h2);
	prepare_BWTs_naively: function();
	generate_BWT_naively: function(h);
	generate_BWT_advanced: function(h);
	generate_BWTs_advanced: function(h1, h2);
	generate_BWTs_advanced_int: function(h1, h2, generate_F);
	merge_BWTs_advanced: function(h1, h2);
	merge_XBWs: function(h1, h2);
	nextNodes: function(i);
	prevNodes: function(i);
	finalComparison: function(findex);
	sortp12andFindProblems: function();
	fe_findexToTable: function(findex, showBWTandM, show_f, show_i);
	fe_p12ToTableWithHighlights: function(highlight_arr, show_origin, show_f, show_i);
	visualize: function(auto, showPrefixes, highlight_p12, show_vis_hl);
	hideWrap: function(sout, kind);
	errorWrap: function(sout);
	loopOverflowError: function(sout);
	generateRandomGraphString: function();
	stringToGraph: function(str);
	graphToAutomaton: function(graph);
	mergeAutomata: function(auto1, auto2);
	makeAutomatonReverseDeterministic: function(auto, addToSOut);
	makeAutomatonReverseDeterministic_int: function(auto, addToSOut);
	reverseDeterminizer: function(i, newchar, auto, addToSOut);
	mergeNodesInAutomaton: function(auto, node_1, node_2);
	splitNodeInAutomaton: function(auto, node_i);
	isAutomatonReverseDeterministic: function(auto);
	arraysContainSameEls: function(a1, a2);
	arrayContainsOtherArraysEls: function(a1, a2);
	generateFfromPrefixesBWTM: function(prefixes, bwt, m);
	computePrefixes: function(auto);
	workOnAutomatonPrefixes: function(auto, makePrefixSorted, addToSOut);
	workOnAutomatonPrefixes_int: function(auto, makePrefixSorted, addToSOut);
	deep_copy_node: function(node);
	deep_copy_array: function(arr);
	isAutomatonPrefixSorted: function(auto);
	makeAutomatonPrefixSorted: function(auto, addToSOut);
	getFindexFromAutomaton: function(auto);
	getAutomatonFromFindex: function(findex);
	generate_BWTs_naively: function(h1, h2);
	merge_BWTs_naively: function(h1, h2);
	create_cyclic_rotations: function(ha, offset, offletter);
	sort_cyclic_rotations: function(h_cr);
	print_arrofarr: function(haa);
	repjoin: function(len, letter, delimiter);
	reparr: function(len, letter);
	repeatstr: function(len, str);
	add_index_to_col: function(h_col, index);
	add_indices_to_col: function(h_col, indices, h_cols);
	arr_to_highlighted_str: function(arr, hl_pos, extra_hl_arr);
	arr_to_extra_high_str: function(arr, extra_hl_arr);
	arr_to_str_wo_index: function(h_col, delimiter);
	arr_to_str_w_index: function(h_col, delimiter);
	arr_to_arr_w_index: function(h_col);
	sort_indexed_col: function(h_col);
	get_index_from_col: function(h_col);
	get_tabline_from_col_DaTeX: function(h_col);
	get_tabline_from_col_HTML: function(arr, h_col);
	merge_with_interleave: function(h1, h2, h12_itlv);
	get_first_n_from_scr: function(h_scr, n);
	get_last_n_from_scr: function(h_scr, n);
	pos_to_str: function(pos);
	get_pos_from_scr: function(h_scr);
	get_bwt_from_scr: function(h_scr);
	did_itlvs_change: function(itlv1, itlv2);
	expand_pos: function(pos, bwt);
	pos_equals_pos: function(pos1, pos2);
	printKeyValArr: function(keys, values, use_ao);
	count_up_array: function(i);
	makeVisualsNice: function(sout);
	comparePrefixes: function(p1, p2);
	prunePrefixes: function(prefix_arr);
	exchangeInString: function(str, pos1, pos2);
	setInString: function(str, pos, chr);
	make_xbw_environment: function();
	example: function();
	xbw_example: function();

	Abbreviations:
	    BWT .. Burrows-Wheeler Transform
	    FiC .. first column
	      M .. bit-vector M
	      F .. bit-vector F
	 auto i .. index of node in automaton
	 node i .. index of column in node table (table representing automaton 1:1)
	table i .. index of column in flat table

	Specifications:
	An automaton is an array containing objects as nodes and "false" nodes.
	Each object contains:
		the array n (containing the indices of the next nodes)
		the array p (containing the indices of the preceding nodes)
		the string c (the label of the node)
	An object may contain:
		the string f (the prefix of the node)
	The automaton MUST have an object in position 0, which is the entry node
	(e.g. with label '#'.)
	"False" nodes, which are allowed in any position above 0, are array elements
	that are just the boolean false value. Any iteration over the automaton
	nodes should just jump over them. (They usually arise as remnants of deleted
	nodes, when it would be unnecessary to change the indices across the entire
	automaton by actually splicing the automaton array.)
*/

window.GML = {

	loop_threshold: 100, // general overflow protection: whereever it is likely that through
						   // weird input or ridiculous bugs we could run into an infinite loop,
						   // stop when reaching this iteration

	verbosity: 10, // integer, 10 .. tell us about EVERYTHING, 1 .. only give us the results
	hideXBWenvironments: false, // do not show XBW environments - used if we are only interested in the results

	ao: 0, // global array offset - used whenever we show stuff on the GUI / accept data from there
	origins: [0, 1], // which index to show on H_i (and to use for origin i in general)

	last_h1: '', // h1 that was used on last call
	last_h2: '', // h2 that was used on last call
	last_mode: 'none', // 'naive' or 'advanced', init to 'none'
	last_give_out_HTML: -1, // true or false, init as -1

	merge_directly: true, // (considering the graph merging)
						  // true: take out $1 and #1 - they should not actually be in the merged graph,
						  //       and we should jump over them in the individual ones while merging them
						  //       too =)
						  // false: actually insert $1 and #1 nodes in between (currently not supported)

	show_auto_i: false, // true: show auto i above automaton nodes in the visualize function, false: do not

	error_flag: false, // global flag that can be unset before calling functions within GML, and will
					   // be set to true if these functions encounter internal errors

	// A note about DaTeX:
	// In DaTeX, we enclose maths expressions in dollarsigns, and to write an actual dollarsign, we write \S.
	// Just sorting \$ would be a problem as lex(\) > lex(c) for a character c, but sorting $ \$ $ is fine,
	// as that starts with the dollarsign anyway.
	// (In HTML, we just use $ itself without backslash in front of it, so there none of this matters.)

	set_to_DaTeX: function() {

		this.give_out_HTML = false;

		this.origins = [this.ao, this.ao+1];

		this.nl = "\n"; // newline character in code
		this.nlnl = "\n\n"; // newline character in print
		this.nlnlnl = "\n\n\n"; // double newline character in print
		this.DS = "$ \\$ $"; // $
		this.DS_1 = '%'; // $_1 internally
		this.DS_1_o = '$ \\$_' + this.origins[0] + ' $'; // $_1
		this.DS_1_t = '$ \\$_' + this.origins[0] + ' $'; // $_1 in SVG
		this.DS_2_o = '$ \\$_' + this.origins[1] + ' $'; // $_2
		this.DK = '$ # $'; // #
		this.DK_1 = '_'; // #_1 internally
		this.DK_1_o = '$ #_' + this.origins[0] + ' $'; // #_1
		this.DK_1_t = '$ #_' + this.origins[0] + ' $'; // #_1 in SVG
		this.H_1 = 'H_' + this.origins[0]; // string H_1 while in mathmode
		this.H_2 = 'H_' + this.origins[1]; // string H_2 while in mathmode
		this.DH = '$ H $'; // string H
		this.DH_s = '$ H_{';
		this.DH_e = '} $';
		this.DH_1 = this.DH_s + this.origins[0] + this.DH_e; // string H_1
		this.DH_2 = this.DH_s + this.origins[1] + this.DH_e; // string H_2
		this.DM = '$ M $'; // vector M
		this.DF = '$ F $'; // vector F
		this.di = '$ i $'; // integer i

		this.tab = '\\tab'; // start table
		this.tabnl = " \\\\" + this.nl; // newline in table
		this.tabnlnc = ' \\\\'; // newline in table without starting a cell
		this.td = " & "; // table cell divider
		this.endtab = '\\endtab' + this.nlnl; // end table
		this.tabchar = '      '; // a tab (horizontal space)
		var currentdate = new Date();
		this.s_end_document = this.nlnl + // the last line of the document
			"Reykjavík, " + currentdate.getDate() + ". " +
			(currentdate.getMonth() + 1) + ". " + currentdate.getFullYear();
	},

	set_to_HTML: function() {

		this.give_out_HTML = true;

		this.origins = [this.ao, this.ao+1];

		this.nl = '\n'; // newline character in code
		this.nlnl = '<br>\n'; // newline character in print
		this.nlnlnl = '<br><br>\n'; // double newline character in print
		this.DS = '$'; // $
		this.DS_1 = '%'; // $_1 internally
		this.DS_1_o = '$<span class="d">' + this.origins[0] + '</span>'; // $_1
		this.DS_1_t = '<tspan>$</tspan><tspan class="d" dy="0.2">' + this.origins[0] + '</tspan>'; // $_1 in SVG
		this.DS_2_o = '$<span class="d">' + this.origins[1] + '</span>'; // $_2
		this.DK = '^'; // #
		this.DK_1 = '_'; // #_1 internally
		this.DK_1_o = '^<span class="d">' + this.origins[0] + '</span>'; // #_1
		this.DK_1_t = '<tspan>^</tspan><tspan class="d" dy="0.2">' + this.origins[0] + '</tspan>'; // #_1 in SVG
		this.H_1 = 'H<span class="d">' + this.origins[0] + '</span>'; // string H_1 while in mathmode
		this.H_2 = 'H<span class="d">' + this.origins[1] + '</span>'; // string H_2 while in mathmode
		this.DH = '<i>H</i>'; // string H
		this.DH_s = '<i>H<span class="d">';
		this.DH_e = '</span></i>';
		this.DH_1 = this.DH_s + this.origins[0] + this.DH_e; // string H_1
		this.DH_2 = this.DH_s + this.origins[1] + this.DH_e; // string H_2
		this.DM = '<i>M</i>'; // vector M
		this.DF = '<i>F</i>'; // vector F
		this.di = '<i>i</i>'; // integer i

		this.tab = '<div class="table_box"><table>'; // start table
		this.tabnl = '</td></tr>' + this.nl + '<tr><td>'; // newline in table
		this.tabnlnc = '</td></tr>' + this.nl + '<tr>'; // newline in table without starting a cell
		this.td = '</td><td>'; // table cell divider
		this.endtab = '</td></tr></tbody></table></div>' + this.nlnl; // end table
		this.tabchar = '&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;'; // a tab (horizontal space)
		this.s_end_document = ''; // the last line of the document
	},

	// takes in an origin, e.g. 5
	// gives out H_5 styled for display and with the correct offset
	make_DH: function(o) {
		return this.DH_s + (o + this.ao) + this.DH_e;
	},



	/************************************\
		initialization
	\************************************/

	// takes in two strings
	// gives out nothing (but builds the bwts for the two strings separately and merged)
	build_BWTs_naively: function(h1, h2) {

		// Has someone already done our work for us?
		if ((this.last_h1 == h1) && (this.last_h2 == h2) && (this.last_mode == 'naive')) {

			// Yes, someone has! So we don't have to do any work at all. =)

			if (this.last_give_out_HTML !== this.give_out_HTML) {
				// Buuut the output form changed, so we need to work around that anyway...
				this.prepare_BWTs_naively();
			}

			return;
		}



		this.last_h1 = h1;
		this.last_h2 = h2;
		this.last_mode = 'naive';



		// Does h1 or h2 contain a graph? - e.g. A(A|C)CA
		this.h1_graph = h1.indexOf('(') >= 0;
		this.h2_graph = h2.indexOf('(') >= 0;

		var i, h1_A, h1_B, h2_C, h2_D, h1a, h1a_A, h1a_B, h2a, h2a_C, h2a_D, alt_A, alt_B, alt_C, alt_D,
			ha_A, ha_B, ha_C, ha_D;

		if (this.h1_graph) {

			i = h1.indexOf('(');
			alt_A = h1.slice(i + 1, i + 2);
			i = h1.indexOf('|');
			alt_B = h1.slice(i + 1, i + 2);

			h1_A = h1.replace('(', '');
			h1_A = h1_A.slice(0, h1_A.indexOf('|')) + h1_A.slice(h1_A.indexOf(')') + 1);

			h1_B = h1.replace(')', '');
			h1_B = h1_B.slice(0, h1_B.indexOf('(')) + h1_B.slice(h1_B.indexOf('|') + 1);
		
			// create array forms of the input strings
			h1a = h1.split('');
			h1a_A = h1_A.split('');
			h1a_B = h1_B.split('');

		} else {
			// create array forms of the input strings
			h1a = h1.split('');
		}

		if (this.h2_graph) {

			i = h2.indexOf('(');
			alt_C = h2.slice(i + 1, i + 2);
			i = h2.indexOf('|');
			alt_D = h2.slice(i + 1, i + 2);

			h2_C = h2.replace('(', '');
			h2_C = h2_C.slice(0, h2_C.indexOf('|')) + h2_C.slice(h2_C.indexOf(')') + 1);

			h2_D = h2.replace(')', '');
			h2_D = h2_D.slice(0, h2_D.indexOf('(')) + h2_D.slice(h2_D.indexOf('|') + 1);
		
			// create array forms of the input strings
			h2a = h2.split('');
			h2a_C = h2_C.split('');
			h2a_D = h2_D.split('');

		} else {
			// create array forms of the input strings
			h2a = h2.split('');
		}

		var h1_comp = h1;
		if (this.h1_graph) {
			h1_comp = h1_A;
		}
		var h2_comp = h2;
		if (this.h2_graph) {
			h2_comp = h2_C;
		}
		this.reorder1and2 = h2_comp < h1_comp;

		if (this.reorder1and2) {
			this.DS_1 = this.DS_2_o;
			this.DS_2 = this.DS_1_o;
		} else {
			this.DS_1 = this.DS_1_o;
			this.DS_2 = this.DS_2_o;
		}

		if (this.h1_graph && this.h2_graph) {
			ha_A = h1a_A.concat([this.DS_1]).concat(h2a_C).concat([this.DS_2]);
			ha_B = h1a_B.concat([this.DS_1]).concat(h2a_C).concat([this.DS_2]);
			ha_C = h1a_A.concat([this.DS_1]).concat(h2a_D).concat([this.DS_2]);
			ha_D = h1a_B.concat([this.DS_1]).concat(h2a_D).concat([this.DS_2]);
		} else if (this.h1_graph) {
			ha_A = h1a_A.concat([this.DS_1]).concat(h2a).concat([this.DS_2]);
			ha_B = h1a_B.concat([this.DS_1]).concat(h2a).concat([this.DS_2]);
		} else if (this.h2_graph) {
			ha_C = h1a.concat([this.DS_1]).concat(h2a_C).concat([this.DS_2]);
			ha_D = h1a.concat([this.DS_1]).concat(h2a_D).concat([this.DS_2]);
		}

		// create full array based on both strings
		var ha = h1a.concat([this.DS_1]).concat(h2a).concat([this.DS_2]);

		// append delimiter character to input arrays
		h1a[h1a.length] = this.DS;
		h2a[h2a.length] = this.DS;

		var h_cr_A, h_cr_B, h_cr_C, h_cr_D, h1_cr_A, h1_cr_B, h2_cr_C, h2_cr_D, h1_len;

		if (this.h1_graph && this.h2_graph) {

			// append delimiter character to input arrays
			h1a_A[h1a_A.length] = this.DS;
			h1a_B[h1a_B.length] = this.DS;
			h2a_C[h2a_C.length] = this.DS;
			h2a_D[h2a_D.length] = this.DS;

			// generate cyclic rotations
			h_cr_A = this.create_cyclic_rotations(ha_A, 0, alt_A);
			h_cr_B = this.create_cyclic_rotations(ha_B, 0, alt_B);
			h_cr_C = this.create_cyclic_rotations(ha_C, 0, alt_C);
			h_cr_D = this.create_cyclic_rotations(ha_D, 0, alt_D);
			this.h_cr = h_cr_A.concat(h_cr_B).concat(h_cr_C).concat(h_cr_D);
			h1_cr_A = this.create_cyclic_rotations(h1a_A, 0, alt_A);
			h1_cr_B = this.create_cyclic_rotations(h1a_B, 0, alt_B);
			this.h1_cr = h1_cr_A.concat(h1_cr_B);

			// we are here implicitly assuming that h1a_A has the same length as h1a_B!
			h1_len = h1a_A.length;

			// generate cyclic rotations
			h2_cr_C = this.create_cyclic_rotations(h2a_C, h1_len, alt_C);
			h2_cr_D = this.create_cyclic_rotations(h2a_D, h1_len, alt_D);
			this.h2_cr = h2_cr_C.concat(h2_cr_D);

		} else if (this.h1_graph) {

			// append delimiter character to input arrays
			h1a_A[h1a_A.length] = this.DS;
			h1a_B[h1a_B.length] = this.DS;

			// generate cyclic rotations
			h_cr_A = this.create_cyclic_rotations(ha_A, 0, alt_A);
			h_cr_B = this.create_cyclic_rotations(ha_B, 0, alt_B);
			this.h_cr = h_cr_A.concat(h_cr_B);
			h1_cr_A = this.create_cyclic_rotations(h1a_A, 0, alt_A);
			h1_cr_B = this.create_cyclic_rotations(h1a_B, 0, alt_B);
			this.h1_cr = h1_cr_A.concat(h1_cr_B);

			// we are here implicitly assuming that h1a_A has the same length as h1a_B!
			h1_len = h1a_A.length;

			this.h2_cr = this.create_cyclic_rotations(h2a, h1_len, '');

		} else if (this.h2_graph) {

			this.h1_cr = this.create_cyclic_rotations(h1a, 0, '');

			h1_len = h1a.length;

			// append delimiter character to input arrays
			h2a_C[h2a_C.length] = this.DS;
			h2a_D[h2a_D.length] = this.DS;

			// generate cyclic rotations
			h_cr_C = this.create_cyclic_rotations(ha_C, 0, alt_C);
			h_cr_D = this.create_cyclic_rotations(ha_D, 0, alt_D);
			this.h_cr = h_cr_C.concat(h_cr_D);
			h2_cr_C = this.create_cyclic_rotations(h2a_C, h1_len, alt_C);
			h2_cr_D = this.create_cyclic_rotations(h2a_D, h1_len, alt_D);
			this.h2_cr = h2_cr_C.concat(h2_cr_D);

		} else {

			// generate cyclic rotations
			this.h_cr = this.create_cyclic_rotations(ha, 0, '');
			this.h1_cr = this.create_cyclic_rotations(h1a, 0, '');
			h1_len = h1a.length;
			this.h2_cr = this.create_cyclic_rotations(h2a, h1_len, '');
		}

		// create strings based on arrays with delimiters
		this.h = ha.join('');
		this.h1 = h1a.join('');
		this.h2 = h2a.join('');

		// sort cyclic rotations
		this.h_scr = this.sort_cyclic_rotations(this.h_cr);
		this.h1_scr = this.sort_cyclic_rotations(this.h1_cr);
		this.h2_scr = this.sort_cyclic_rotations(this.h2_cr);

		// get the positions
		this.h_pos = this.get_pos_from_scr(this.h_scr);
		this.h1_pos = this.get_pos_from_scr(this.h1_scr);
		this.h2_pos = this.get_pos_from_scr(this.h2_scr);

		// get the BWT
		this.h_bwt = this.get_bwt_from_scr(this.h_scr);
		this.h1_bwt = this.get_bwt_from_scr(this.h1_scr);
		this.h2_bwt = this.get_bwt_from_scr(this.h2_scr);



		// recalculate the output strings
		this.prepare_BWTs_naively();
	},



	// takes in nothing (but assumes that build_BWTs_naively() was executed just before)
	// gives out nothing (but initializes several strings for the output)
	prepare_BWTs_naively: function() {

		// a table head that we will print over and over again...
		this.s_h_table_head = this.tab;
		if (this.give_out_HTML) {
			this.s_h_table_head += '<tbody class="lastbar"><tr><td>';
		} else {
			this.s_h_table_head += "{" + this.repjoin(this.h_pos.length, 'c', ' ') + " | l}" + this.nl;
		}

		// ... and a full table that we will also print several times =)
		this.s_h_table = this.s_h_table_head;
		this.s_h_table += this.h_pos.join(this.td) + this.td + 'Position' + this.tabnl;
		this.s_h_table += this.h_bwt.join(this.td) + this.td + 'BWT' + this.nl;
		this.s_h_table += this.endtab;
	},



	/************************************\
		text output
	\************************************/

	// takes in an unterminated string
	// gives out a section in DaTeX or HTML about the generation of its BWT
	generate_BWT_naively: function(h) {

		// Does h1 or h2 contain a graph? - e.g. A(A|C)CA
		var h_graph = h.indexOf('(') >= 0;

		var ha, ha_A, ha_B, alt_A, alt_B, h_len, h_cr, i;

		if (h_graph) {

			i = h.indexOf('(');
			alt_A = h.slice(i + 1, i + 2);
			i = h.indexOf('|');
			alt_B = h.slice(i + 1, i + 2);

			var h_A = h.replace('(', '');
			h_A = h_A.slice(0, h_A.indexOf('|')) + h_A.slice(h_A.indexOf(')') + 1);

			var h_B = h.replace(')', '');
			h_B = h_B.slice(0, h_B.indexOf('(')) + h_B.slice(h_B.indexOf('|') + 1);
		
			// create array forms of the input string
			ha = h.split('');
			ha_A = h_A.split('');
			ha_B = h_B.split('');

		} else {

			// create array form of the input string
			ha = h.split('');
		}

		// append delimiter character to input arrays
		ha[ha.length] = this.DS;

		if (h_graph) {

			// append delimiter character to input arrays
			ha_A[ha_A.length] = this.DS;
			ha_B[ha_B.length] = this.DS;

			// generate cyclic rotations
			var h_cr_A = this.create_cyclic_rotations(ha_A, 0, alt_A);
			var h_cr_B = this.create_cyclic_rotations(ha_B, 0, alt_B);
			h_cr = h_cr_A.concat(h_cr_B);

			// we are here implicitly assuming that h1a_A has the same length as h1a_B!
			h_len = ha_A.length;

		} else {

			// generate cyclic rotations
			h_cr = this.create_cyclic_rotations(ha, 0, '');
			h_len = ha.length;
		}

		// create strings based on arrays with delimiters
		var h = ha.join('');

		// sort cyclic rotations
		var h_scr = this.sort_cyclic_rotations(h_cr);

		// get the positions
		var h_pos = this.get_pos_from_scr(h_scr);

		// get the BWT
		var h_bwt = this.get_bwt_from_scr(h_scr);



		// a table head that we will print over and over again...
		var s_h_table_head = this.tab;
		if (this.give_out_HTML) {
			s_h_table_head += '<tbody class="lastbar"><tr><td>';
		} else {
			s_h_table_head += "{" + this.repjoin(h_pos.length, 'c', ' ') + " | l}" + this.nl;
		}

		// ... and a full table that we will also print several times =)
		var s_h_table = s_h_table_head;
		s_h_table += h_pos.join(this.td) + this.td + 'Position' + this.tabnl;
		s_h_table += h_bwt.join(this.td) + this.td + 'BWT' + this.nl;
		s_h_table += this.endtab;



		var sout = '';
		
		if(!this.give_out_HTML) {
			sout += " Graph Alignment - BWT Generation" + this.nl;
			sout += "¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯" + this.nl;
		}

		sout += "We are looking at " + this.nlnl;
		
		if(this.give_out_HTML) {
			sout += this.nlnl + this.DH + ' = ' + h + this.nlnlnl;
		} else {
			sout += '$$ H = "' + h + '" $$' + this.nlnl;
		}



		// BWT and pos for H

		sout += "To generate the full BWT of " + this.DH + ", ";
		sout += "we first create its cyclic rotations:" + this.nlnl;

		sout += this.print_arrofarr(h_cr).join(this.nlnl);

		sout += this.nlnlnl + "All of the cyclic rotations sorted together:" + this.nlnl;
		
		sout += this.print_arrofarr(h_scr).join(this.nlnl);

		sout += this.nlnlnl + "So overall we get the following positions ";
		sout += "and BWT for " + this.DH + ":" + this.nlnl;

		sout += s_h_table;



		sout += this.s_end_document;

		return sout;
	},



	// takes in an unterminated string
	// gives out a section in DaTeX or HTML about the generation of its BWT
	generate_BWT_advanced: function(h) {

		this.error_flag = false;

		// generate the graph
		var graph = this.stringToGraph(h);

		if (this.error_flag) {
			return this.errorWrap('An infoblock in the input is wrongly formatted.');
		}

		// generate the automaton
		var auto = this.graphToAutomaton(graph);



		var sout = '';
		
		if(!this.give_out_HTML) {
			sout += " Graph Alignment - BWT Generation" + this.nl;
			sout += "¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯" + this.nl;
		}

		if (this.verbosity > 1) {

			sout += "We are looking at" + this.nlnl;

			sout += this.visualize(auto, false);
		}



		if (this.verbosity > 2) {

			sout += "We first need to convert this into a reverse deterministic automaton." + this.nlnl;
		}

		var isRevDet = this.isAutomatonReverseDeterministic(auto);
		var abortDueToRevDet = false;

		if (isRevDet) {

			if (this.verbosity > 1) {

				sout += "Luckily, the given automaton is already reverse deterministic." + this.nlnl;
			}
		
		} else {

			if (this.verbosity > 5) {

				sout += "To achieve a reverse deterministic automaton, " +
						"we consider the string alignment that a problematic region in the " +
						"automaton represents and slide the predecessor characters as far to the right " +
						"as possible (effectively sliding the gaps to the left.)" + this.nl;
				sout += "We then reconstruct the automaton, and it will be guaranteed to be reverse " +
						"deterministic (see Siren 2014.)" + this.nlnl;
				sout += 'As a safeguard against malformed input we also merge nodes that have the ' +
						'same labels and lead into the same nodes.' + this.nlnl;
			}

			this.sout = '';
			auto = this.makeAutomatonReverseDeterministic(auto, true);

			if (this.verbosity > 5) {
				sout += this.nlnl + this.sout;
			}

			isRevDet = this.isAutomatonReverseDeterministic(auto);

			abortDueToRevDet = !isRevDet;


			if (this.verbosity > 3) {
				if (abortDueToRevDet) {
					sout += "Ooops! We do not have a reverse deterministic automaton:" + this.nlnl;
				} else {
					sout += "We now have the following reverse deterministic automaton:" + this.nlnl;
				}

				sout += this.visualize(auto, false);
			}
		}


		if (abortDueToRevDet) {

			sout += this.errorWrap('As we failed to generate a reverse deterministic automaton, ' +
								   'we stop here.');

		} else {

			if (this.verbosity > 3) {

				sout += "We now need to convert this reverse deterministic automaton " +
						"into a prefix-sorted automaton." + this.nlnl;
			}

			auto = this.computePrefixes(auto);

			if (this.verbosity > 5) {

				sout += "Let's have a look at the prefixes:" + this.nlnl;

				sout += this.visualize(auto, true);
			}

			var isPrefSort = this.isAutomatonPrefixSorted(auto);

			if (isPrefSort) {

				if (this.verbosity > 3) {
					sout += "Luckily, the given automaton is already prefix sorted." + this.nlnl;
				}
			
			} else {

				this.sout = '';
				auto = this.makeAutomatonPrefixSorted(auto, true);

				if (this.verbosity > 3) {
					sout += "To achieve a prefix sorted automaton, " + 
							"we split all nodes that prohibit this automaton from being prefix sorted " +
							"into several parts and repeat this process until no more nodes have " +
							"problematic prefixes (as indicated by the " + this.prefixErrorChar + 
							" signs within the shown prefixes.)" +
							this.nlnlnl;

					sout += this.sout;
				}

				isPrefSort = this.isAutomatonPrefixSorted(auto);

				if (this.verbosity > 3) {
					if (isPrefSort) {
						sout += "We now have the following prefix-sorted automaton:" + this.nlnl;
					} else {
						sout += "Ooops! We do not have a prefix-sorted automaton:" + this.nlnl;
					}

					sout += this.visualize(auto, true);
				}
			}



			// BWT and pos for H

			if (this.verbosity > 3) {

				sout += "We can now generate the BWT of the graph, ";
				sout += "together with the vector " + this.DM + "." + this.nlnl;

			} else {

				sout += 'We get:' + this.nlnl;
			}

			var findex = this.getFindexFromAutomaton(auto);

			if (this.verbosity > 6) {

				sout += "To do so, we first sort the prefixes alphabetically ";
				sout += "(being assured that each prefix is unique and therefore the sorting will not ";
				sout += "be ambiguous).";

				sout += this.fe_findexToTable(findex, false);

				sout += "We can now add the labels of the preceding nodes as BWT and the ";
				sout += "encoded out-degree of each node as vector " + this.DM + ":" + this.nlnl;
			}

			if (this.verbosity > 2) {

				sout += this.fe_findexToTable(findex, true);
			}

			if (this.verbosity > 3) {

				var auto = this.getAutomatonFromFindex(findex);

				sout += 'Converting this table back into an automaton (just to make sure ' +
						'that everything worked out correctly) yields the following result:' + this.nlnl;

				sout += this.visualize(auto, true);
			}

			if (this.verbosity > 2) {

				sout += 'We now want to do a bit of work on the table, so we add the ' + this.DF + ' vector:';
			}

			findex[3] = this.generateFfromPrefixesBWTM(findex[0], findex[1], findex[2]);

			sout += this.fe_findexToTable(findex, true, true, true);



			if (!GML.hideXBWenvironments) {
				// initialize XBW environment
				GML.XBW = this.make_xbw_environment();
				GML.XBW.init(findex);
				GML.XBW.generateHTML(2);
			}
		}



		sout += this.s_end_document;

		// replace '^' with '#' before printout
		sout = sout.replace(/\^/g, '#');

		return sout;
	},



	// takes in two graph strings
	// gives out a string containing info about the BWT generation for both
	generate_BWTs_advanced: function(h1, h2) {

		var sout = this.generate_BWTs_advanced_int(h1, h2, false)[0];

		sout += this.s_end_document;

		// replace '^' with '#' before printout
		sout = sout.replace(/\^/g, '#');

		return sout;
	},



	// takes in two graph strings
	// gives out a string containing info about the BWT generation for both,
	//   as well as the findexes for the merged H, for H_1 and for H_2
	generate_BWTs_advanced_int: function(h1, h2, generate_F) {

		this.error_flag = false;

		// generate the graph
		var graph1 = this.stringToGraph(h1);
		var graph2 = this.stringToGraph(h2);

		if (this.error_flag) {
			return this.errorWrap('An infoblock in the input is wrongly formatted.');
		}

		// generate the automaton
		var auto1 = this.graphToAutomaton(graph1);
		var auto2 = this.graphToAutomaton(graph2);

		// merge the automata
		var auto = this.mergeAutomata(auto1, auto2);



		var sout = '';
		
		if(!this.give_out_HTML) {
			sout += " Graph Alignment - BWT Generation" + this.nl;
			sout += "¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯" + this.nl;
		}

		if (this.verbosity > 1) {

			sout += "We are looking at " + this.DH_1 + ":" + this.nlnl;

			sout += this.visualize(auto1, false);

			sout += "and " + this.DH_2 + ":" + this.nlnl;

			sout += this.visualize(auto2, false);

			sout += "We are also interested in the intended merging result " + this.DH + ":" + this.nlnl;

			sout += this.visualize(auto, false);
		}



		var isRevDet = this.isAutomatonReverseDeterministic(auto);
		var isRevDet1 = this.isAutomatonReverseDeterministic(auto1);
		var isRevDet2 = this.isAutomatonReverseDeterministic(auto2);

		var failat;

		if (!isRevDet) {
			auto = this.makeAutomatonReverseDeterministic(auto, false);
			isRevDet = this.isAutomatonReverseDeterministic(auto);
			if (!isRevDet) {
				failat = this.DH;
				this.error_flag = true;
			}
		}

		if (!isRevDet1) {
			auto1 = this.makeAutomatonReverseDeterministic(auto1, false);
			isRevDet1 = this.isAutomatonReverseDeterministic(auto1);
			if (!isRevDet1) {
				failat = this.DH_1;
				this.error_flag = true;
			}
		}

		if (!isRevDet2) {
			auto2 = this.makeAutomatonReverseDeterministic(auto2, false);
			isRevDet2 = this.isAutomatonReverseDeterministic(auto2);
			if (!isRevDet2) {
				failat = this.DH_2;
				this.error_flag = true;
			}
		}


		if (this.error_flag) {

			sout += this.errorWrap('As we failed to generate a reverse deterministic automaton for ' +
								   failat + ', we stop here.');
		} else {

			if (this.verbosity > 2) {
				sout += "We need to convert these into reverse deterministic and then " +
						"into prefix-sorted automata." + this.nlnl;
			}

			auto = this.computePrefixes(auto);
			auto1 = this.computePrefixes(auto1);
			auto2 = this.computePrefixes(auto2);

			var isPrefSort = this.isAutomatonPrefixSorted(auto);
			var isPrefSort1 = this.isAutomatonPrefixSorted(auto1);
			var isPrefSort2 = this.isAutomatonPrefixSorted(auto2);

			if (!isPrefSort) {
				auto = this.makeAutomatonPrefixSorted(auto, false);
				isPrefSort = this.isAutomatonPrefixSorted(auto);
				if (!isPrefSort) {
					sout += "Ooops! We do not have a prefix-sorted automaton for " + this.DH + "!" + this.nlnl;
				}
			}

			if (!isPrefSort1) {
				auto1 = this.makeAutomatonPrefixSorted(auto1, false);
				isPrefSort1 = this.isAutomatonPrefixSorted(auto1);
				if (!isPrefSort1) {
					sout += "Ooops! We do not have a prefix-sorted automaton for " + this.DH_1 + "!" + this.nlnl;
				}
			}

			if (!isPrefSort2) {
				auto2 = this.makeAutomatonPrefixSorted(auto2, false);
				isPrefSort2 = this.isAutomatonPrefixSorted(auto2);
				if (!isPrefSort2) {
					sout += "Ooops! We do not have a prefix-sorted automaton for " + this.DH_2 + "!" + this.nlnl;
				}
			}

			if (this.verbosity > 1) {
				sout += "This gives us for " + this.DH_1 + ":" + this.nlnl;
				sout += this.visualize(auto1, true);
				sout += "and for " + this.DH_2 + ":" + this.nl;
				sout += this.visualize(auto2, true);
				sout += "as well as for " + this.DH + ":" + this.nl;
				sout += this.visualize(auto, true);
			}



			// BWT and pos for H

			if (this.verbosity > 2) {
				sout += "We can now generate the BWT of the graph, ";
				sout += "together with the ";
				if (generate_F) {
					sout += "vectors " + this.DM + ' and ' + this.DF;
				} else {
					sout += "vector " + this.DM;
				}
				sout += "." + this.nlnl;
			}

			var findex1 = this.getFindexFromAutomaton(auto1);
			if (generate_F) {
				findex1[3] = this.generateFfromPrefixesBWTM(findex1[0], findex1[1], findex1[2]);
			}

			if (this.verbosity > 1) {
				sout += "For " + this.DH_1 + " we get:" + this.nlnl;
				sout += this.fe_findexToTable(findex1, true, generate_F);
			}

			var findex2 = this.getFindexFromAutomaton(auto2);
			if (generate_F) {
				findex2[3] = this.generateFfromPrefixesBWTM(findex2[0], findex2[1], findex2[2]);
			}

			if (this.verbosity > 1) {
				sout += "And for " + this.DH_2 + " we have:" + this.nlnl;
				sout += this.fe_findexToTable(findex2, true, generate_F);
			}

			var findex = this.getFindexFromAutomaton(auto);
			if (generate_F) {
				findex[3] = this.generateFfromPrefixesBWTM(findex[0], findex[1], findex[2]);
			}

			if (this.verbosity > 1) {
				sout += "As well as for " + this.DH + ":" + this.nlnl;
				sout += this.fe_findexToTable(findex, true, generate_F);
			}
		}

		return [sout, findex, findex1, findex2];
	},



	// takes in two graph strings
	// gives out a string contain info about the BWT merging for both
	merge_BWTs_advanced: function(h1, h2) {

		var ret = this.generate_BWTs_advanced_int(h1, h2, false);

		var sout = ret[0];

		if (!this.error_flag) {

			var unsuccessful = false;
			var findex = ret[1];
			var findex1 = ret[2];
			var findex2 = ret[3];

			var bwt1 = findex1[1];
			var bwt2 = findex2[1];
			var m1 = findex1[2];
			var m2 = findex2[2];


			sout += '<span id="in-jump-3-3"></span>';

			if (this.verbosity > 1) {
				sout += 'We now want to find this prefix array, BWT and ' +
						this.DM + ' vector just based on the ' +
						'data we have for ' + this.DH_1 + ' and ' + this.DH_2 + '.' + this.nlnlnl;
			}

			if (this.verbosity > 3) {
				sout += 'The very first thing that we need to figure out is which letter is the last of ' +
						this.DH_1 + ' and which letter is the first of ' + this.DH_2 + ', as this information ' +
						'will be useful later on, and it is much easier to figure it out once and remember it.' +
						this.nlnl;
			}

			// find last letter of H_1
			// (implicitly assuming that there is exactly one edge
			// out of #_1, the first node of H_2 - that is, when we
			// go through the BWT and find any entry for #_1, then
			// the first letter of the corresponding prefix will be
			// the label of the node following #_1 in H_2, no matter
			// what)
			this.lastH1Letter = '';
			for (i=0; i < findex1[0].length; i++) {
				if (findex1[0][i][0] == this.DS) {
					this.lastH1Letter = bwt1[i];
					if (this.verbosity > 3) {
						sout += 'Looking at the prefixes of ' + this.DH_1 + ', we can see that the prefix ' +
								this.DS + ' has the BWT entry ' + this.lastH1Letter +
								' associated with it, so the last letter of ' + this.DH_1 +
								' is ' + this.lastH1Letter + '.' + this.nlnl;
					}
					break;
				}
			}

			// find first letter of H_2
			// (implicitly assuming that there is exactly one edge
			// out of #_1, the first node of H_2 - that is, when we
			// go through the BWT and find any entry for #_1, then
			// the first letter of the corresponding prefix will be
			// the label of the node following #_1 in H_2, no matter
			// what)
			this.firstH2Letter = '';
			for (i=0; i < bwt2.length; i++) {
				if (bwt2[i] == this.DK) {
					this.firstH2Letter = findex2[0][i][0][0];
					if (this.verbosity > 3) {
						sout += 'Looking at the BWT of ' + this.DH_2 + ', we can see that ' +
								this.DK + ' in the BWT has the prefix ' + findex2[0][i][0] +
								' associated with it, so the first letter of ' + this.DH_2 +
								' is ' + this.firstH2Letter + '.' + this.nlnlnl;
					}
					break;
				}
			}



			if (this.verbosity > 2) {
				sout += 'We are now ready to look at all the prefixes, while keeping track of where ' +
						'they are coming from:' + this.nlnl;
			}


			var p1 = this.add_index_to_col(findex1[0], this.origins[0]);
			var p2 = this.add_index_to_col(findex2[0], this.origins[1]);
			this.p12 = p1.concat(p2); // prefixes
			this.p12_itlv = this.get_index_from_col(this.p12); // orig / interleave vector


			this.s_table_head = this.tab;
			if (this.give_out_HTML) {
				this.s_table_head += '<tbody class="vbars"><tr><td>';
			} else {
				this.s_table_head += "{" + this.repjoin(this.p12.length, 'c', ' | ') + " | l}" + this.nl;
			}


			if (this.verbosity > 2) {
				var stab = this.s_table_head;
				stab += this.arr_to_str_wo_index(this.p12, this.td) + this.td + 'Prefix' + this.tabnl;
				stab += this.p12_itlv.join(this.td) + this.td + 'Origin' + this.nl;
				stab += this.endtab;
				sout += this.hideWrap(stab, 'Table');
			}


			var i;
			for (i=0; i < p1.length; i++) {
				p1[i][0] = p1[i][0].replace(this.DS, this.DS_1_o);
				bwt1[i] = bwt1[i].replace(this.DS, this.DS_1_o);
			}
			for (i=0; i < p2.length; i++) {
				p2[i][0] = p2[i][0].replace(this.DK, this.DK_1_o);
				bwt2[i] = bwt2[i].replace(this.DK, this.DK_1_o);
			}
			this.p12 = p1.concat(p2);

			if (this.verbosity > 4) {
				sout += 'We actually want to more closely keep track of the ' + this.DK +
						' and ' + this.DS + ' characters in use.' + this.nlnl +
						'That is, from now on we will let ' + this.DH_1 + ' start with ' +
						this.DK + ' and end with ' + this.DS_1_o + ', while ' + this.DH_2 +
						' will start with ' + this.DK_1_o + ' and end with ' + this.DS + ':' +
						this.nlnl;

				var stab = this.s_table_head;
				stab += this.arr_to_str_wo_index(this.p12, this.td) + this.td + 'Prefix' + this.tabnl;
				stab += this.p12_itlv.join(this.td) + this.td + 'Origin' + this.nl;
				stab += this.endtab;
				sout += this.hideWrap(stab, 'Table');
			}


			for (i=0; i < this.p12.length; i++) {
				if (this.p12[i][0].indexOf(this.DS_1_o) > -1) {
					this.p12[i][0] += this.DK_1_o + this.firstH2Letter;
				}
			}

			if (this.verbosity > 2) {
				sout += 'It is important to now append ' + this.DK_1_o + ' and the first letter of ' +
						this.DH_2 + ' (which is ' + this.firstH2Letter + ') to all prefixes that end on ' +
						this.DS_1_o + ', as ' +
						this.DS_1_o + ' is not actually in use in the final merged BWT.' + this.nlnl +
						'(Which means that we are implicitly removing ' + this.DS_1_o + ' here, which ' +
						'did correspond to a real node when looking at ' + this.DH_1 + ' in isolation.)' +
						this.nlnl + 'Naturally, we first of all need to find the first node of ' + this.DH_2 +
						'.' + this.nlnlnl +
						'Appending the first letter of ' + this.DH_2 + '(which is ' + 
						this.firstH2Letter + ') as well as ' + this.DK_1_o +
						' to all prefixes ending with ' +
						this.DS_1_o + ' gives us:' + this.nlnl;

				var stab = this.s_table_head;
				stab += this.arr_to_str_wo_index(this.p12, this.td) + this.td + 'Prefix' + this.tabnl;
				stab += this.p12_itlv.join(this.td) + this.td + 'Origin' + this.nl;
				stab += this.endtab;
				sout += this.hideWrap(stab, 'Table');
			}


			// add flags to the prefixes for future highlighting
			for (i=0; i < this.p12.length; i++) {
				this.p12[i] = [
					this.p12[i][0], // prefix - string
					this.p12[i][1], // origin - 1 or 2
					false,     // problem here - true or false
				];
			}

			var thereAreProblems = this.sortp12andFindProblems();
			this.p12_itlv = this.get_index_from_col(this.p12);
			this.bwt = this.merge_with_interleave(bwt1, bwt2, this.p12_itlv);
			this.m = this.merge_with_interleave(m1, m2, this.p12_itlv);

			if (this.verbosity > 1) {
				sout += 'We now need to sort these prefixes together. If we are really lucky, ' +
						'all the prefixes are different, we can sort them together, and be done.' +
						this.nlnl +
						'If this does not work in a certain position, then this position will be ' +
						'highlighted.';

				var stab = this.s_table_head;
				if (this.give_out_HTML) {
					stab = stab.slice(0, -4);
				}
				stab += this.arr_to_highlighted_str(this.p12, 2) + this.td + 'Prefix' + this.tabnl;
				stab += this.p12_itlv.join(this.td) + this.td + 'Origin' + this.nl;
				stab += this.endtab;
				sout += this.hideWrap(stab, 'Table');
			}


			if (this.verbosity > 4) {
				sout += 'We should also take a look at the BWT and ' + this.DM + ' vector associated ' +
						'with this preliminary ordering.' + this.nlnl +
						'Oh, and from now on, we will replace "' +
						this.DS_1_o + this.DK_1_o + '" with "&#62;", just to indicate that we are ' +
						'switching from ' + this.DH_1 + ' to ' + this.DH_2 + ', but without having ' +
						'to write so much all the time.' + this.nlnl +
						'Finally, we will also rewrite ' + this.DS_1_o + this.DK_1_o + this.firstH2Letter +
						' as just ' + this.DS_1_o + ' as this node is indeed very special: ' +
						'We keep it in the table because it will be used whenever we come off ' + this.DH_1 +
						', but it will not actually be considered (and will indeed be dropped in the very end)' +
						' as it is equivalent to the node ' + this.DK_1_o + ' (which will also be dropped) ' +
						'and the node with prefix ' + this.firstH2Letter + ' and origin 2, which in fact will ' +
						'go on to represent the other two nodes once they are dropped in the end.' + this.nlnl;

				sout += this.fe_p12ToTableWithHighlights([], true);
			}


			for (i=0; i < p1.length; i++) {
				bwt1[i] = bwt1[i].replace(this.DS_1_o, this.DS);
			}
			for (i=0; i < p2.length; i++) {
				bwt2[i] = bwt2[i].replace(this.DS, this.DS_1_o);
			}
			this.bwt = this.merge_with_interleave(bwt1, bwt2, this.p12_itlv);

			if (this.verbosity > 4) {
				sout += 'However, we need to exchange ' + this.DS + ' and ' + this.DS_1_o +
						' in the BWT.' + this.nlnl + 'The reason for this is that the predecessor ' +
						'overflow now occurs across both ' + this.DH_1 + ' and ' + this.DH_2 +
						', instead of separately for both of them:' + this.nlnl;

				sout += this.fe_p12ToTableWithHighlights([], true);
			}



			if (this.verbosity > 9) {
				sout += 'We now think a little bit about how we can navigate through this table, ' +
						'which we will have to do a lot during the following steps.' + this.nlnl +
						'Every entry in the table corresponds to one node. ' +
						'So even though we ' + "don't" + ' explicitly have the two original graphs ' +
						'(or the fully merged graph), we can still meaningfully think about nodes, ' +
						'and doing so will make our life much easier.' + this.nlnl +
						'In particular, instead of the more traditional mappings ' +
						'(last-to-first and vice versa) we can simply consider the functions ' +
						'nextNodes(' + this.di + ') and prevNodes(' + this.di + '). ' +
						'We define both of them to be functions that take in an integer ' +
						'(the position of the node within the table; that is, the number of the column), ' +
						'and give out an array of integers (the positions of nodes.)' + this.nlnl +
						'So, if we call nextNodes(10), ' +
						'then the result will be an array containing all indicies of nodes ' +
						'which have in-edges coming from node 10.' + this.nlnl +
						'Similarly, if we call prevNodes(10), ' +
						'the result will be an array containing all indicies of nodes ' +
						'which have out-edges going to node 10.' + this.nlnl +
						'In practice, we can do this by just looking at the information ' +
						'given by the prefixes, origin, BWT and ' + this.DM + ' vector.' + this.nlnlnl +

						'So to compute prevNodes(' + this.di + '), we look at BWT(' + this.di + '), ' +
						'and then find any prefix starting with that letter ' +
						'(if BWT(' + this.di + ') contains several letters, ' +
						'we consider each of them separately one after the other.)' + this.nlnl +
						'Of all these prefixes, ' + 
						'we then only consider the ones having the same origin as node ' + this.di + '.' + this.nlnl +
						'We finally jump over as many prefixes as there are nodes before ' + this.di + ' ' + 
						'that contain the same letter in their BWT values, ' +
						'counting each prefix by its ' + this.DM + ' value.' + this.nlnl +
						'E.g. if we have the following table ' + 
						'(just looking at the table and ignoring where it might come from)' + this.nlnl;

				var ti = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
				var tpref = ['AA', 'AA', 'AB', 'AB', 'AC', 'BA', 'BA', 'BB', 'BB', 'BC'];
				var torig = [1, 2, 1, 2, 1, 1, 2, 1, 2, 1];
				var tbwt = ['B', 'A', 'B', 'B', 'B', 'A', 'C', 'D|A', 'A|E', 'A'];
				var tm = ['10', '1', '1', '1', '10', '10', '1', '10', '10', '1'];

				var stab = this.s_table_head;
				stab += ti.join(this.td) + this.td + this.di + this.tabnl;
				stab += tpref.join(this.td) + this.td + 'Prefix' + this.tabnl;
				stab += torig.join(this.td) + this.td + 'Origin' + this.tabnl;
				stab += tbwt.join(this.td) + this.td + 'BWT' + this.tabnl;
				stab += tm.join(this.td) + this.td + this.DM + this.nl;
				stab += this.endtab;

				sout += this.hideWrap(stab, 'Table');



				sout += 'and we are looking for prevNodes(3), ' +
						'then we can consider all prefixes starting with B - ' +
						'that is, 6 through 10.' + this.nlnl +
						'Of these, we drop 7 and 9, as their origin is not 1.' + this.nlnl +
						'The remaining nodes are 6, 8 and 10, which we will now multiply according to ' +
						'their ' + this.DM + ' values (as we are interested in how many edges are going ' +
						'out of them.) This gives us the list 6, 6, 8, 8, 10, as both 6 and 8 have ' +
						this.DM + ' value 10, and 10 has ' + this.DM + ' value 1.' + this.nlnl +
						'We then count: Before node 3, we have node 1 which has B in its BWT and origin 1. ' +
						'We also have node 2, but that one has a different origin, so is ignored.' + this.nlnl +
						'So of the list 6, 6, 8, 8, 10, we jump over first one, ' +
						'leaving us with the second one, which is node 6. ' +
						'So we return the array [6]. ' +
						'(We ignore the nodes following 6 and only return the first ' +
						'one that was not jumped over.)' + this.nlnl +
						'Notice that the returned array of prevNodes(' + this.di + ') ' +
						'will always contain exactly as many entries ' +
						'as the BWT of node ' + this.di + ' contains letters.' + this.nlnlnl +

						'To compute nextNodes(3) within the same table, we look at the first ' +
						'letter of its prefix AB. That first letter is A.' + this.nlnl +
						'We now need to find all nodes whose BWTs contain the letter A - these ' +
						'are 2, 6, 8, 9 and 10.' + this.nlnl +
						'Of these, the ones with the same origin are 6, 8 and 10, as 2 and 9 have origin 2.' +
						this.nlnl +
						'We then count: Before node 3, we have node 1 which starts with A in its prefix ' +
						'and has origin 1. ' +
						'It has multiplicity (' + this.DM + ' value) 10, counting for 2. ' +
						'We also have node 2 which has a prefix starting with A, ' +
						'but that one has a different origin, so is ignored.' + this.nlnl +
						'So of the remaining nodes 6, 8 and 10, we jump over first 2 (both for node 1), ' +
						'leaving us with node 10, so we return the array [10]. ' +
						'(If node 3 had had a different ' + this.DM + ' value, we would ' +
						'have returned that many nodes - e.g., for value 10, we would have returned ' +
						'not just [10], but [10, 11], assuming that node 11 would have been another ' +
						'remaining node.)' +
						this.nlnlnl;
			}

			if (this.verbosity > 1) {
				sout += 'We are now ready to work on the highlighted problems.' + this.nlnl;
			}



			if (thereAreProblems) {

				// prevent endlessly hanging script if something doesn't work out and
				// we accidentally produced and infinite loop

				var patience = this.ao;

				while (thereAreProblems) {

					// initialize to an arbitrary big number, which is bigger than
					// the length of any prefix in the graph (and no prefix should
					// need to be longer than the amount of nodes, but just to be
					// sure, let's multiply by 100)
					var shortestRedLength = 100 * this.p12.length;
					var rep = this.DS_1_o + this.DK_1_o;

					for (i=0; i < this.p12.length; i++) {
						if (this.p12[i][2] && (this.p12[i][0].replace(rep, '').length < shortestRedLength)) {
							firstRedi = i;
							shortestRedLength = this.p12[i][0].replace(rep, '').length;
						}
					}

					var firstRedPrefix = this.p12[firstRedi][0];
					var curLetter = firstRedPrefix[firstRedPrefix.length-1];

					var shide = '<div>';


					// 1 - look at last letter of first red prefix
					if (this.verbosity > 5) {
						shide += "We want to consider any one of the shortest red prefixes." + this.nlnl;
					}

					if ((patience === 0) && (this.verbosity > 7)) {
						shide += "(Let's arbitrarily pick the first one of the shortest red prefixes, " +
								"where " + this.DS_1_o + this.DK_1_o + " is not counted. " +
								"Oh, and it is important that we focus on the shortest prefixes, " +
								"as we could otherwise get into a situation in which we split " +
								"a node, but have the node following it still unsplit in the table, " +
								"which would put the table in an unsafe / inconsistent state unless " +
								"we included the same BWT letter several times in that other node and " +
								"always kept track of that until the split of that node happened, which " +
								"seems like a lot of work.)" + this.nlnl;
					}

					if (this.verbosity > 5) {
						shide += "The prefix is " + firstRedPrefix + " and we jump through the table " +
								"all the way until we reach its last letter - that is, " + curLetter +
								'.' + this.nlnl;

						shide += this.fe_p12ToTableWithHighlights([[firstRedi]], true);
					}


					// 2 - check BWT containing the letter with origin being the same the one of the letter
					if (this.verbosity > 5) {
						shide += "We now look through the BWT that has the same origin and " +
								"search for the letters one after the other, " +
								"using the nextNodes(i) function." +
								this.nlnl;
					}

					var next_nodes = [firstRedi];
					for (i=0; i < firstRedPrefix.length; i++) {
						// TODO :: make this more professional
						//         (currently, we are converting an array to string and back to array
						//         to do a deep copy rather than a pointer copy - there must a cleaner
						//         way ^^)
						var next_s = '';
						for (var k=0; k < next_nodes.length; k++) {
							next_s += this.nextNodes(next_nodes[k]).join(',') + ',';
						}
						next_nodes = next_s.slice(0, -1).split(',');
						for (k=0; k < next_nodes.length; k++) {
							next_nodes[k] = parseInt(next_nodes[k], 10);
						}
						if (this.verbosity > 6) {
							shide += this.fe_p12ToTableWithHighlights([[], [], next_nodes], true);
						}
					}


					// 3 - look at corresponding prefixes, and append them to the letter
					if (this.verbosity > 5) {
						shide += 'We now take the corresponding prefixes.' + this.nlnl;

						shide += this.fe_p12ToTableWithHighlights([next_nodes], true);
					}

					var replacement_prefixes = [];

					for (i=0; i < next_nodes.length; i++) {
						var pref = firstRedPrefix + this.p12[next_nodes[i]][0];
						replacement_prefixes.push(pref);
					}

					if (this.verbosity > 5) {
						shide += 'These prefixes get added to the original prefix ' + firstRedPrefix +
								' that we were looking at, producing the following new prefixes:' + this.nlnl;

						shide += replacement_prefixes.join(this.nlnl);
						shide += this.nlnlnl;
					}

					// 4 - insert these new prefixes instead of the original column
					if (this.verbosity > 5) {
						shide += 'We insert these new prefixes ' +
								'instead of the original column.' + this.nlnl;
					}

					// calculate prevNodes only if necessary - but do so before actually doing any inserting,
					// so that we do not calculate them when the table is not in a safe state
					var prev_nodes = [];
					if (replacement_prefixes.length > 1) {
						prev_nodes = this.prevNodes(firstRedi);
					}

					this.p12[firstRedi][0] = replacement_prefixes[0];
					this.m[firstRedi] = '1';
					ins_arr = [firstRedi];
					
					var Madd = '';

					for (i=1; i < replacement_prefixes.length; i++) {
						this.p12.splice(firstRedi+i, 0, [replacement_prefixes[i], this.p12[firstRedi][1], false]);
						this.p12_itlv.splice(firstRedi+i, 0, this.p12_itlv[firstRedi]);
						this.bwt.splice(firstRedi+i, 0, this.bwt[firstRedi]);

						// set M of the inserted column to 1, as each of these nodes now has exactly one
						// edge leaving it (which is precisely why we are doing the splitting in the first
						// place - and we are splitting precisely once for each outedge, so we have exactly
						// one outedge for each node now!)
						this.m.splice(firstRedi+i, 0, '1');

						ins_arr.push(firstRedi+i);
						Madd += '0';
					}

					if (this.verbosity > 5) {
						shide += this.fe_p12ToTableWithHighlights([ins_arr, ins_arr, ins_arr, ins_arr], true);

						shide += 'We now need to consider the ' + this.DM + ' vector, for which we have set ' +
								'the correct value for the new columns (taking the one from their origin), ' +
								'but however have not yet reset the values of the preceding nodes. In this case, ';
					}

					if (replacement_prefixes.length > 1) {

						if (this.verbosity > 5) {
							shide += 'we replaced 1 column with ' + replacement_prefixes.length +
									' columns, so we inserted ' + (replacement_prefixes.length - 1) + ' columns.' +
									this.nlnl;

							shide += 'This means that we have to increase the ' + this.DM +
									' values of the preceding nodes by that same amount, ' +
									'as these nodes now have more outgoing edges. ' +
									'To find these preceding nodes, we used the prevNodes(i) ' + 
									'function on the table before inserting the new columns, ' +
									'so as to not use it while the table is in an unsafe or ' +
									'inconsistent state.' + this.nlnl;
						}

						var mrep_arr = [];

						for (i=0; i < prev_nodes.length; i++) {

							var pnode = prev_nodes[i];

							// ignore the freshly inserted columns
							if (pnode > firstRedi) {
								pnode += replacement_prefixes.length - 1;
							}

							mrep_arr.push(pnode);

							// add replacement_prefixes.length-1 zeroes to their M vectors
							// (thereby adding as many outgoing edges as columns were inserted)
							this.m[pnode] += Madd;
						}

						if (this.verbosity > 5) {
							shide += this.fe_p12ToTableWithHighlights([ins_arr,[],[],mrep_arr], true);
						}
					} else {
						if (this.verbosity > 5) {
							shide += 'we replaced one column with another one column, so we did not insert ' +
									'any new columns at all, and no action is required.' + this.nlnlnl;
						}
					}


					// 5&6 - resort prefixes and recheck problems
					if (this.verbosity > 5) {
						shide += 'We can now sort the prefixes again alphabetically and ' +
								'check for further problems.' + this.nlnl;
					}

					// keep track of the BWT and M vector while sorting
					// (we cannot just merge afterwards with the interleave vector,
					// as we have inserted columns and therefore one original
					// column can lead to several merged columns)
					for (i=0; i < this.p12.length; i++) {
						this.p12[i][3] = this.bwt[i];
						this.p12[i][4] = this.m[i];
					}

					thereAreProblems = this.sortp12andFindProblems();

					this.p12_itlv = this.get_index_from_col(this.p12);
					this.bwt = this.get_first_n_from_scr(this.p12, 3);
					this.m = this.get_first_n_from_scr(this.p12, 4);

					if (this.verbosity > 5) {
						shide += this.fe_p12ToTableWithHighlights([], true);
					}


					if (this.verbosity > 5) {
						sout += this.hideWrap(shide + '</div>', 'Step ' + patience) + this.nlnl;
					}

					// 7 - repeat approach until no more problems (or run out of patience)
					patience++;

					if (patience > this.loop_threshold) {
						return this.loopOverflowError(sout);
					}
				}

				if (thereAreProblems) {
					sout += 'Sadly, the merging was not successful.' + this.nlnl;
					unsuccessful = true;
				} else {
					if (this.verbosity > 1) {
						sout += 'We have now achieved the fully merged BWT.' + this.nlnl;
					}
				}
			} else {
				if (this.verbosity > 1) {
					sout += 'We are lucky, as there are no problems, and we have therefore already ' +
							'found the fully merged BWT.' + this.nlnl;
				}
			}



			if (!unsuccessful) {
				if (this.merge_directly) {
					// get M-value of "#_1" node
					var Madd = '';
					for (i=0; i < this.p12.length; i++) {
						if (this.p12[i][0] === this.DK_1_o) {
							Madd = this.m[i].slice(1);
						}
					}

					for (i=0; i < this.p12.length; i++) {
						// take out the "$_1" and "#_1" nodes
						if ((this.p12[i][0].indexOf(this.DS_1_o) === 0) ||
							(this.p12[i][0].indexOf(this.DK_1_o) === 0)) {
							this.p12.splice(i, 1);
							this.p12_itlv.splice(i, 1);
							this.bwt.splice(i, 1);
							this.m.splice(i, 1);
						}

						// could be broken because of splicing ;)
						if (i < this.p12.length) {
							// if there is some M-value of "#_1" node to add...
							if (Madd !== '') {
								// ... then add M-value of "#_1" node to M-value of last node of H_1
								if (this.p12[i][0].indexOf(this.DS_1_o) === 1) {
									this.m[i] += Madd;
								}
							}

							// replace a "...$_1#_1..." caption with a "......" caption
							this.p12[i][0] = this.p12[i][0].replace(this.DS_1_o+this.DK_1_o, '');

							// replace #_1 in BWT with last node of H_1
							if (this.bwt[i] === this.DK_1_o) {
								this.bwt[i] = this.lastH1Letter;
							}
						}
					}

					if (this.verbosity > 4) {
						sout += 'We can take out the helper nodes ' + this.DS_1_o + ' and ' + this.DK_1_o +
								' (adding the ' + this.DM + ' value of ' + this.DK_1_o + ' to the last node of ' +
								this.DH_1 + '), as well as replacing ' + this.DK_1_o + ' in the BWT with ' +
								this.lastH1Letter +
								', the last letter of ' + this.DH_1 + ':' + this.nlnl;

						sout += this.fe_p12ToTableWithHighlights([], true);
					}
				}



				// [NOTE 1] ::
				//     if this.merge_directly is NOT true, then here we can get into trouble with
				//     ...$_1#_1..., as e.g. TG vs. T$_1#_1A would be pruned to TG vs. T$ instead of
				//     the correct TG vs. TA - for now we will just discontinue support for merge_directly
				//     being false ;)

				for (i=1; i < this.p12.length-1; i++) {
					while ((this.p12[i][0].slice(0, this.p12[i][0].length-1).slice(0, this.p12[i-1][0].length) !==
						    this.p12[i-1][0].slice(0, this.p12[i][0].length-1)) &&
						   (this.p12[i][0].slice(0, this.p12[i][0].length-1).slice(0, this.p12[i+1][0].length) !==
						    this.p12[i+1][0].slice(0, this.p12[i][0].length-1)) &&
						   (this.p12[i][0] != '')) {
						this.p12[i][0] = this.p12[i][0].slice(0, this.p12[i][0].length-1);
					}
				}

				if (this.verbosity > 3) {
					sout += 'As we are done with the prefix-doubling, we can now prune the prefixes back down ' +
							'to the shortest possible lengths that still leave the prefixes unique:' + this.nlnl;

					sout += this.fe_p12ToTableWithHighlights([], true);
				}
			}

			if (this.verbosity > 1) {
				sout += 'Finally, we take out the origin row, as it is not needed anymore.' + this.nl +
						'We however do add another row, namely the ' + this.DF + ' bit vector.' + this.nlnl;
			} else {
				sout += 'We get:' + this.nlnl;
			}

			this.f = this.generateFfromPrefixesBWTM(this.p12, this.bwt, this.m);

			var p12_generated = [];
			for (var i=0; i < this.p12.length; i++) {
				p12_generated.push(this.p12[i][0]);
			}
			var findex_generated = [p12_generated, this.bwt, this.m, this.f];

			sout += this.fe_findexToTable(findex_generated, true, true);



			sout += 'To make the comparison simpler, here are the BWT, ' + this.DM + ' and ' +
					this.DF + ' vector ' +
					'again, which we obtained from the merged graph directly:' + this.nlnl;

			findex[3] = this.generateFfromPrefixesBWTM(findex[0], findex[1], findex[2]);

			sout += this.fe_findexToTable(findex, true, true);



			if (this.finalComparison(findex)) {
				sout += 'We can see that the table found through merging the BWTs and the ' +
						'table found through merging the graphs and then building one BWT ' +
						'are exactly the same! Jippey! =)' + this.nlnl;
			} else {
				sout += 'We can see that the table found through merging the BWTs and the ' +
						'table found through merging the graphs and then building one BWT ' +
						'are not the same... Sad face. =(' + this.nlnl;
			}



			if (!GML.hideXBWenvironments) {
				// initialize XBW environment
				GML.XBW = this.make_xbw_environment();
				GML.XBW.init(findex);
				GML.XBW.generateHTML(3);
			}
		}



		sout += this.s_end_document;

		// replace '^' with '#' before printout
		sout = sout.replace(/\^/g, '#');

		return sout;
	},



	// takes in two graph strings
	// gives out a string contain info about the XBW merging for both
	merge_XBWs: function(h1, h2) {

		var ret = this.generate_BWTs_advanced_int(h1, h2, true);

		var sout = ret[0];

		if (!this.error_flag) {

			var unsuccessful = false;

			var findex = ret[1];
			var findex1 = ret[2];
			var findex2 = ret[3];

			var bwt1 = findex1[1];
			var bwt2 = findex2[1];
			var m1 = findex1[2];
			var m2 = findex2[2];


			sout += '<span id="in-jump-5-3"></span>';

			if (this.verbosity > 1) {
				sout += 'We now want to find the XBW data of ' + this.DH +
						' just based on the ' +
						'XBW data we have for ' + this.DH_1 + ' and ' + this.DH_2 + '.' + this.nlnl;
			}

			if (this.verbosity > 2) {
				sout += 'We need to flatten the BWTs and drop the prefixes.' +
						this.nlnl;
			}



			// initialize XBW environments

			var xbw1  = this.make_xbw_environment();
			var xbw2  = this.make_xbw_environment();
			var xbw12 = this.make_xbw_environment();

			var xbw = this.make_xbw_environment();

			xbw1.init(findex1);
			xbw2.init(findex2);

			xbw12.initAsMergeHost();
			xbw12.addSubXBW(xbw1);
			xbw12.addSubXBW(xbw2);

			xbw.init(findex);



			if (this.verbosity > 3) {
				sout += "For " + this.DH_1 + " we get:" + this.nlnl;

				shide = '<div class="table_box">' + xbw1.generateTable() + '</div>';
				sout += this.hideWrap(shide, 'Table') + this.nlnl;

				sout += "And for " + this.DH_2 + " we have:" + this.nlnl;

				shide = '<div class="table_box">' + xbw2.generateTable() + '</div>';
				sout += this.hideWrap(shide, 'Table') + this.nlnl;

				sout += "As well as for " + this.DH + ":" + this.nlnl;

				shide = '<div class="table_box">' + xbw.generateTable() + '</div>';
				sout += this.hideWrap(shide, 'Table') + this.nlnl;
			}



			// start splitting

			sout += '<span id="in-jump-5-4"></span>';

			if (this.verbosity > 1) {
				sout += 'We now need to split nodes in order to ensure that the generated ' +
						'structure will be prefix-sorted.' + this.nlnl;
			}

			if (this.verbosity > 2) {
				sout += 'For that, we go pretend to merge ' + 
						'by comparing the prefixes one by one, and if we cannot make a decision, ' +
						'we split the node and remember that we will have to do the entire process ' +
						'another time.' + this.nlnl;
			}

			var round = this.ao;
			var doAnotherRound = true;

			this.error_flag = false;

			while (doAnotherRound && !this.error_flag) {

				doAnotherRound = false;

				xbw12.startNewSplitRound();

				if (this.verbosity > 4) {
					sround = '<div>' + this.nlnl + 'We start round ' + round + ':' + this.nlnl;

					shide = '<div class="table_box">' + xbw12.generateSubTables(true) + '</div>';
					sround += this.hideWrap(shide, 'Tables') + this.nlnl;
				}

				var patience = this.ao;

				while (xbw12.notFullyMerged() && !this.error_flag) {

					var splitnodes = [];

					var sstep = '<div>' + xbw12.checkIfSplitOneMore(splitnodes);

					if (splitnodes.length > 0) {
						sstep += xbw12.splitOneMore(splitnodes);
						doAnotherRound = true;
					}

					if (this.verbosity > 8) {
						if (!this.error_flag) {
							sstep += this.nlnl + 'We now have:' + this.nlnl;

							shide = '<div class="table_box">' + xbw12.generateSubTables(true) + '</div>';
							sstep += this.hideWrap(shide, 'Tables') + this.nlnl;
						}

						sstep += '</div>';

						sround += this.hideWrap(sstep, 'Step ' + patience) + this.nlnl;
					}

					patience++;

					if (patience > this.loop_threshold) {
						return this.loopOverflowError(sout);
					}
				}

				if (this.verbosity > 4) {
					sround += '</div>';
					
					sout += this.hideWrap(sround, 'Round ' + round) + this.nlnl;
				}

				round++;

				if (round > this.loop_threshold) {
					return this.loopOverflowError(sout);
				}
			}

			// end splitting


			if (this.error_flag) {

				document.getElementById('div-xbw-5').style.display = 'none';
				if (sout.indexOf(' class="error"') < 0) {
					sout += this.errorWrap('An error occurred!');
				}
				sout = sout.replace(/\^/g, '#');
				return sout;
			}



			sout += '<span id="in-jump-5-5"></span>';

			if (this.verbosity > 1) {
				sout += 'We can now merge ' + this.DH_1 + ' and ' +
						this.DH_2 + ' just based on their XBW data: BWT, ' + this.DM + ', ' + this.DF +
						' and <i>C</i>.' + this.nlnl;
			}

			if (this.verbosity > 2) {
				sout += this.nlnl;
				sout += 'To do so, we add nodes from the ' + this.DH_2 + ' table on the right ' +
						'one by one to the ' + this.DH_1 + ' table on the left, keeping track of ' +
						'our position within each table. We never need to go left in either table, ' +
						'as we know that both are already sorted within themselves; so if we get an ' +
						'entry from table ' + this.DH_2 + ' into ' + this.DH_1 + ' at position 9, ' +
						'then we know that the next entry from table ' + this.DH_2 + ' must be put ' +
						'into ' + this.DH_1 + ' at position 9 or greater, not lower than 9.' +
						this.nlnl;
			}

			if (this.verbosity > 1) {
				shide = '<div class="table_box">' + xbw12.generateSubTables() + '</div>';
				sout += this.hideWrap(shide, 'Tables') + this.nlnl;
			}

			var patience = this.ao;

			xbw12.startNewSplitRound();

			while (xbw12.notFullyMerged()) {

				if (this.verbosity > 5) {

					var sstep = '<div>' + xbw12.mergeOneMore(true);

					if (this.verbosity > 9) {
						sstep += 'We now have:' + this.nlnl;

						shide = '<div class="table_box">' + xbw12.generateSubTables() + '</div>';
						sstep += this.hideWrap(shide, 'Tables') + this.nlnl;
					}

					sstep += '</div>';

					sout += this.hideWrap(sstep, 'Step ' + patience) + this.nlnl;

				} else {

					xbw12.mergeOneMore(false);
				}

				patience++;

				if (patience > this.loop_threshold) {
					return this.loopOverflowError(sout);
				}
			}

			if (this.verbosity > 1) {
				sout += 'The main part of the merging algorithm has now finished, but we should still ' +
						'join the ' + this.DS_1_o + ' and ' + this.DK_1_o + ' nodes.';

				shide = '<div class="table_box">' + xbw12.generateTable() + '</div>';
				sout += this.hideWrap(shide, 'Table') + this.nlnl;
			}

			xbw12.finalizeMerge();

			// TODO :: we could here also create finalizeMerge functionality for xbw1 and xbw2,
			// to restore them to their previous form, if we were inclined to do so... however,
			// why would we?
			// (If that is ever necessary: set multiOrigin = false in xbw1, and replace the
			// #_1 and $_1 back to their original forms.)

			if (this.verbosity > 1) {
				sout += 'Having joined the ' + this.DS_1_o + ' and ' + this.DK_1_o + ' nodes, ' +
						'the merging has now been completed:';
			} else {
				sout += 'The merging result is:';
			}

			shide = '<div class="table_box">' + xbw12.generateTable() + '</div>';
			sout += this.hideWrap(shide, 'Table') + this.nlnl;

			sout += 'To simplify the comparison, here is the table that we wanted to achieve:';

			shide = '<div class="table_box">' + xbw.generateTable() + '</div>';
			sout += this.hideWrap(shide, 'Table') + this.nlnl;

			if (xbw.equals(xbw12)) {
				sout += 'We can see that these are exactly the same, and we are happy!' + this.nlnl;
			} else {
				sout += 'We can see that these are different from each other, ' +
						'so something somewhere went wrong...' + this.nlnl;
			}



			// TODO :: analyze and use these... and then take them out =)

			console.log('xbw findex:');
			console.log(xbw._publishFindex());

			console.log('xbw12 findex:');
			console.log(xbw12._publishFindex());



			if (!GML.hideXBWenvironments) {
				// start up control center for merged XBW
				GML.XBW = xbw12;
				GML.XBW.init(xbw12._publishFindex());
				GML.XBW.generateHTML(5);
			}
		}



		sout += this.s_end_document;

		// replace '^' with '#' before printout
		sout = sout.replace(/\^/g, '#');

		return sout;
	},



	// takes in the integer position of a node in our table
	// gives out an array containing the positions of all nodes which are directly following that one
	nextNodes: function(i) {

		// look at first letter of the prefix
		var firstPrefLetter = this.p12[i][0][0];

		// the origin of our node, which we will expect all other nodes to have too
		var curOrigin = this.p12_itlv[i];

		// keep track of how many nodes we need to jump over
		var jumpOver = 0;

		// count how many nodes to jump over (all nodes before i that have the same origin
		// and the same first letter in their prefix count according to their M value)
		for (var k=0; k < i; k++) {
			if ((this.p12[k][0][0] === firstPrefLetter) && (this.p12_itlv[k] === curOrigin)) {
				jumpOver += this.m[k].length;
			}
		}

		// the amount of nodes that we want to add to the result is equal to the M value
		// of the considered node, i (as we are looking at the next nodes, going out from
		// i, and the M value of node i tells us how many outgoing edges there are)
		var addToResult = this.m[i].length;

		var retNodes = [];

		// find nodes / columns whose BWTs contain this letter, and whose origin is the same,
		// and jump over as many as necessary
		for (var k=0; k < this.p12.length; k++) {
			if ((this.p12_itlv[k] === curOrigin) && (this.bwt[k].indexOf(firstPrefLetter) > -1)) {
				// still jumping...
				if (jumpOver > 0) {
					jumpOver--;
				} else {
					addToResult--;
					retNodes.push(k);
					// did we add all we want?
					if (addToResult < 1) {
						return retNodes; // we are already done here =)
					}
				}
			}
		}

		// we didn't get as many as we wanted, but we should return the ones we have anyway
		// TODO :: maybe create an error message here? (at least some console-error stuff?)
		return retNodes;
	},



	// takes in the integer position of a node in our table
	// gives out an array containing the positions of all nodes which are directly preceding that one
	prevNodes: function(i) {

		var bwtLetters = this.bwt[i].split('|');
		var curOrigin = this.p12_itlv[i];

		var retNodes = [];

		for (var n=0; n < bwtLetters.length; n++) {

			var curBWTLetter = bwtLetters[n];
			var jumpOver = 0;

			for (var k=0; k < i; k++) {
				if ((this.p12_itlv[k] === curOrigin) && (this.bwt[k].indexOf(curBWTLetter) > -1)) {
					jumpOver++;
				}
			}

			// if the BWT letter is #_1 (or $_1, but that doesn't really occur), then we
			// need to check more fanciful, as a simple check against p12[][][0] is not enough
			// anymore
			if ((curBWTLetter === this.DK_1_o) || (curBWTLetter === this.DS_1_o)) {
				// find any prefix starting with the BWT letter and having the same origin as our node
				for (var k=0; k < this.p12.length; k++) {
					if ((this.p12[k][0].indexOf(curBWTLetter) === 0) && (this.p12_itlv[k] === curOrigin)) {
						jumpOver -= this.m[k].length;
						if (jumpOver < 0) {
							retNodes.push(k);
							break;
						}
					}
				}
			} else {
				// find any prefix starting with the BWT letter and having the same origin as our node
				for (var k=0; k < this.p12.length; k++) {
					if ((this.p12[k][0][0] === curBWTLetter) && (this.p12_itlv[k] === curOrigin)) {
						jumpOver -= this.m[k].length;
						if (jumpOver < 0) {
							retNodes.push(k);
							break;
						}
					}
				}
			}
		}

		return retNodes;
	},



	// takes in the integer position of a node in our table, assuming that the table is fully
	//   merged and the origin has been dropped
	// gives out an array containing the positions of all nodes which are directly preceding that one
	prevNodes_wo_origin: function(i) {

		var bwtLetters = this.bwt[i].split('|');

		var retNodes = [];

		for (var n=0; n < bwtLetters.length; n++) {

			var curBWTLetter = bwtLetters[n];
			var jumpOver = 0;

			for (var k=0; k < i; k++) {
				if (this.bwt[k].indexOf(curBWTLetter) > -1) {
					jumpOver++;
				}
			}

			// find any prefix starting with the BWT letter
			for (var k=0; k < this.p12.length; k++) {
				if (this.p12[k][0][0] === curBWTLetter) {
					jumpOver -= this.m[k].length;
					if (jumpOver < 0) {
						retNodes.push(k);
						break;
					}
				}
			}
		}

		return retNodes;
	},



	// takes in an findex (and assumes "this" has p12, BWT and M)
	// gives out true if the findex contains the same data as "this", or
	//   false if there are differences
	finalComparison: function(findex) {

		// check p12 vs. findex[0]
		for (var i=0; i < findex[0].length; i++) {
			if (this.p12[i][0] !== findex[0][i]) {
				return false;
			}
		}

		// check BWT vs. findex[1]
		for (var i=0; i < findex[1].length; i++) {
			if (this.bwt[i] !== findex[1][i]) {
				return false;
			}
		}

		// check M vs. findex[2]
		for (var i=0; i < findex[2].length; i++) {
			if (this.m[i] !== findex[2][i]) {
				return false;
			}
		}

		return true;
	},



	// takes in nothing (but assumes "this" has a p12 array)
	// gives out a boolean telling us whether or not there were problems,
	//   and internally sorts the this.p12 array and highlights its problems
	sortp12andFindProblems: function() {

		// reset all to false
		for (i=0; i < this.p12.length; i++) {
			this.p12[i][2] = false;
		}

		// do the sorting!
		this.p12.sort(function(a, b) {

			var af = a[0]; // a full
			var bf = b[0]; // b full

			if (bf === undefined) {
				bf = '';
			}

			// take out '...$_1#_1...' - we need to keep them in in general
			// to know that we are switching from H_1 to H_2, but we do not
			// actually want to have them within the comparison
			// BUT do leave them in if they are on position 0 - that is,
			// "$_1" and "#_1" should be left intact =) (that's why we have
			// the indexOf-comparisons all over the place here)
			if (af.indexOf(GML.DS_1_o+GML.DK_1_o) > 0) {
				af = af.replace(GML.DS_1_o+GML.DK_1_o, '');
			}
			if (af.indexOf(GML.DS_1_o) > 0) {
				af = af.replace(GML.DS_1_o, '');
			}
			if (bf.indexOf(GML.DS_1_o+GML.DK_1_o) > 0) {
				bf = bf.replace(GML.DS_1_o+GML.DK_1_o, '');
			}
			if (bf.indexOf(GML.DS_1_o) > 0) {
				bf = bf.replace(GML.DS_1_o, '');
			}

			var ac = af;   // a cropped
			var bc = bf;   // b cropped

			// slice / crop most prefixes, so when we compare 'C' and 'CC',
			// actually compare 'C' and 'C' (and throw error),
			// BUT do not slice if there is '<' present, as that is
			// only true for '#_1' and '$_1', and they are never problematic
			// (as we cut out $_1 and #_1 from later positions within the string)

			if (ac.indexOf('<') < 0) {
				ac = ac.slice(0, bc.length);
			}
			if (bc.indexOf('<') < 0) {
				bc = bc.slice(0, ac.length);
			}

			// the cropped ones are the same...
			if (ac == bc) {
				// ... which means that we have a problem! ...
				a[2] = true;
				b[2] = true;

				// ... but let's compare the non-cropped ones anyway,
				// so that the shortest ones get sorted first
				if (af > bf) {
					return 1;
				} else {
					if (af < bf) {
						return -1;
					} else {
						return 0;
					}
				}
			}

			// here, actually all is good - we can compare and find a clear winner
			// even within the cropped ones!
			if (ac > bc) {
				return 1;
			}
			return -1;
		});

		// go through the sorting results and decide where we need to highlight additional
		// problems...
		// (this could not be done directly during sorting, as some comparisons work out fine,
		// and only later are understood to be problematic; e.g. having C, CC and CT - if we
		// first compare CC and CT, both are deemed fine and sorted, and then sort C into them,
		// then C and CC are marked, but CT is not touched again and cannot be marked directly
		// during the sorting!)

		var lastProblem = '%';
		var lastProblemLen = 0;

		// are there actually any problems?
		var thereAreProblems = false;

		// go through each position (going forwards is important here - we are
		// making use of the fact that the first definitely flagged mention of
		// a problem is always the shortest string, and therefore sorted first)
		for (i=0; i < this.p12.length; i++) {
			// if this prefix starts like the last problem did (e.g. 'CT' starts like 'C'),
			// then this here is also a problem
			if (this.p12[i][0].slice(0, lastProblemLen) === lastProblem) {
				this.p12[i][2] = true;
			} else {
				// otherwise, if we currently have a problem...
				if (this.p12[i][2]) {
					// ... then we want this to be known as the new problem
					lastProblem = this.p12[i][0];
					lastProblemLen = lastProblem.length;
					thereAreProblems = true;
				} else {
					// if we don't even have a problem, then let's just default to no problem, all good
					lastProblem = '%';
					lastProblemLen = 0;
				}
			}
		}

		return thereAreProblems;
	},



	/*
		fe (frontend) functions - helping building the output faster
	*/

	// takes in an findex (an array consisting of prefixes, BWT and M) and a boolean parameter
	// gives out a table either with just the prefixes, or all of findex, depending of the parameter
	fe_findexToTable: function(findex, showBWTandM, show_f, show_i) {

		// prefixes .. findex[0]
		// BWT      .. findex[1]
		// M        .. findex[2]
		// F        .. findex[3]

		var sout = '';

		sout += this.tab;
		if (this.give_out_HTML) {
			sout += '<tbody class="vbars"><tr><td>';
		} else {
			sout += "{" + this.repjoin(BWT.length, 'c', ' | ') + " | l}" + this.nl;
		}
		if (show_i) {
			for (var i=0; i < findex[0].length; i++) {
				sout += i + this.td;
			}
			sout += this.di + this.tabnl;
		}

		if (showBWTandM) {
			sout += findex[1].join(this.td) + this.td + 'BWT' + this.tabnl;
		}

		sout += findex[0].join(this.td) + this.td + 'Prefix';

		if (showBWTandM) {
			sout += this.tabnl;
			sout += findex[2].join(this.td) + this.td + this.DM;
		}

		if (show_f) {
			sout += this.tabnl;
			sout += findex[3].join(this.td) + this.td + this.DF;
		}

		sout += this.nl;
		sout += this.endtab;

		return this.hideWrap(sout, 'Table');
	},



	// takes in a highlight array and a boolean parameter (and assumes a p12 array, p12
	//   interleave vector, BWT and M vector on "this")
	//   (highlight_arr contains the coordinates of cells in the table that should be specially
	//   highlighted, so e.g. highlight_arr = [[3,4,5], [0,2], [1,5]] to highlight cells
	//   3, 4 and 5 in row 0 (prefix), 0 and 2 in row 1 (origin) and 1 and 5 in row 2 (BWT))
	// gives out a string containing a table with all the information and with the extra
	//   highlighting
	fe_p12ToTableWithHighlights: function(highlight_arr, show_origin, show_f, show_i) {

		var spref = this.arr_to_highlighted_str(this.p12, 2, highlight_arr[0]);

		while (spref.indexOf(this.lastH1Letter+this.DS_1_o+this.DK_1_o) > -1) {
			spref = spref.replace(this.lastH1Letter+this.DS_1_o+this.DK_1_o, this.lastH1Letter+'&#62;');
		}
		while (spref.indexOf(this.DS_1_o+this.DK_1_o+this.firstH2Letter) > -1) {
			spref = spref.replace(this.DS_1_o+this.DK_1_o+this.firstH2Letter, this.DS_1_o);
		}

		var stab = this.s_table_head;
		if (this.give_out_HTML) {
			stab = stab.slice(0, -4);
		}
		if (show_i) {
			if (this.give_out_HTML) {
				stab += '<td>';
			}
			for (var i=0; i < this.p12.length; i++) {
				stab += i + this.td;
			}
			stab += this.di + this.tabnlnc;
		}
		stab += spref + this.td + 'Prefix' + this.tabnlnc;
		if (show_origin) {
			stab += this.arr_to_extra_high_str(this.p12_itlv, highlight_arr[1]) + this.td + 'Origin' + this.tabnlnc;
		}
		stab += this.arr_to_extra_high_str(this.bwt, highlight_arr[2]) + this.td + 'BWT' + this.tabnlnc;
		stab += this.arr_to_extra_high_str(this.m, highlight_arr[3]) + this.td + this.DM;
		if (show_f) {
			stab += this.tabnlnc;
			stab += this.arr_to_extra_high_str(this.f, highlight_arr[4]) + this.td + this.DF;
		}
		stab += this.nl + this.endtab;

		return this.hideWrap(stab, 'Table');
	},



	// basically, we start with a table i (id of table column) that we want to highlight
	// which is stored in the array of arrays vis_highlight_nodes,
	// use vis_tableToP12 to convert it to a p12 i (id of node in graph-like table),
	// use vis_p12ToAuto to convert it to an auto i (id of node in automaton),
	// and finally put the outcoming value into a new array of nodes within the automaton
	// that are actually highlighted
	// (all of this happens in the visualization function IF show_vis_hl is set to true)
	vis_highlight_nodes: [],
	vis_tableToP12: [],
	vis_p12ToAuto: [],



	// takes in an automaton, a boolean parameter (whether to show the prefixes
	//   or not) and optionally an array (containing all prefixes for which the
	//   nodes with these prefixes should be highlighted) and optionally the
	//   boolean parameter show_vis_hl (default: false), which tells us whether
	//   the nodes in GML.vis_highlight_nodes should be highlighted too
	// gives out a string containing a graph visualization of the input
	visualize: function(auto, showPrefixes, highlight_p12, show_vis_hl) {

		var default_color = '#000';
		var default_bg_color = '#FFF';
		var extra_color = '#C00';
		var extra_bg_color = '#FCF';
		var high_color = '#A0A';
		var high_bg_color = '#FFA';

		var extra_high_nodes = [];
		var extra_high_edges = [];
 
		if (show_vis_hl && (this.vis_highlight_nodes.length > 0) && (this.vis_highlight_nodes[0].length > 0)) {
			
			for (var j=0; j < this.vis_highlight_nodes.length; j++) {
				
				// convert the first node using tableToP12 from table i to p12 i to auto i
				extra_high_nodes.push(
					this.vis_p12ToAuto[
						this.vis_tableToP12[
							this.vis_highlight_nodes[j][0]
						]
					]
				);
				
				// convert all other nodes just from p12 i to auto i
				for (var i=1; i < this.vis_highlight_nodes[j].length; i++) {
					extra_high_nodes.push(
						this.vis_p12ToAuto[
							this.vis_highlight_nodes[j][i]
						]
					);
					extra_high_edges.push(extra_high_nodes[extra_high_nodes.length-2] + '_' + extra_high_nodes[extra_high_nodes.length-1]);
				}
			}
		}

		if (highlight_p12 === undefined) {
			highlight_p12 = [];
		}

		// TODO :: make this work for DaTeX output as well (e.g. with TikZ)
		var sout = '';

		var markerArrow = 'markerArrow' + (this.hide_counter + 1);

		sout += '<defs>';
		sout += '<marker id="' + markerArrow + '" markerWidth="13" markerHeight="13" refX="4" refY="7" orient="auto">';
		sout += '<path d="M2,7 L2,9.5 L5,7 L2,4.5 L2,7" style="fill:' + default_color + ';" />';
		sout += '</marker>';
		sout += '<marker id="' + markerArrow + '_extra" markerWidth="13" markerHeight="13" refX="4" refY="7" orient="auto">';
		sout += '<path d="M2,7 L2,9.5 L5,7 L2,4.5 L2,7" style="fill:' + extra_color + ';" />';
		sout += '</marker>';
		sout += '</defs>';

		var positions = [];

		// keep track of paths we have not visualized yet
		// (for each path, the location of the first node in auto is saved)
		var more_paths = [];
		var done_paths = [];

		// find main row by following from # always the first alternative, until we arrive at $
		// alternatively, we could also go until we reached a node with no outgoing nodes,
		// but sometimes some funny bunnies actually put an edge from $ to # to confuse us,
		// so we'll not do that ;)
		var mainrow = [0];
		var i = 0;
		while ((auto[i].c !== this.DS) && (auto[i].c !== this.DS_1)) {
			i = auto[i].n[0];
			mainrow.push(i);
		}
		var hlen = mainrow.length;

		var xoff, xoffnext;

		// iterate over main row
		for (var j = 0; j < hlen; j++) {
			var i = mainrow[j];
			xoff = 50 + (100 * j);
			xoffnext = 50 + (100 * (j+1));
			positions[i] = xoff;

			var highcolor = default_color;
			var bgcolor = default_bg_color;
			if (extra_high_nodes.indexOf(i) >= 0) {
				highcolor = extra_color;
				bgcolor = extra_bg_color;
			}
			if (highlight_p12.indexOf(auto[i].f) >= 0) {
				highcolor = high_color;
				bgcolor = high_bg_color;
			}

			sout += '<circle cx="' + xoff + '" cy="100" r="20" style="fill:' + highcolor + '" />';
			sout += '<circle cx="' + xoff + '" cy="100" r="18" style="fill:' + bgcolor + '" />';
			sout += '<text x="' + xoff + '" y="110" text-anchor="middle" style="fill:' + highcolor + ';">' +
					auto[i].c + '</text>';

			if (showPrefixes || this.show_auto_i) {
				sout += '<text class="prefix" x="' + xoff +
						'" y="78" text-anchor="middle" style="fill:' + highcolor + '">';
				if (showPrefixes) {
					sout += auto[i].f;
					if (this.show_auto_i) {
						sout += ', ';
					}
				}
				if (this.show_auto_i) {
					sout += (i + this.ao);
				}
				sout += '</text>';
			}

			var strokecolor = default_color;
			var marker_kind = '';
			if (extra_high_edges.indexOf(i + '_' + mainrow[j+1]) >= 0) {
				strokecolor = extra_color;
				marker_kind = '_extra';
			}

			if (auto[i].c !== this.DS) {
				sout += '<path d="M' + (xoff + 25) + ',100 L' + (xoffnext - 25) + ',100" ';
				sout += 'style="stroke: ' + strokecolor + '; stroke-width: 2.25px; fill: none; marker-end: url(#' + markerArrow + marker_kind + ');" ';
				sout += '/>';
			}

			for (var k = 1; k < auto[i].n.length; k++) {
				var addPath = auto[i].n[k];
				if ((done_paths.indexOf(addPath) < 0) && (more_paths.indexOf(addPath) < 0)) {
					more_paths.push(addPath);
				}
			}
		}


		// alternate yoff between 52.5 and 47.5 (around 50), so that adjacent paths
		// get put on opposite sides of the main path
		// TODO :: improve this with an actual space-aware visualization solution
		var yoff = 125; // offset for path starts and ends - small distance from origin
		var yoffl = 170;  // offset for nodes - large distance from origin
		var yoffdl = 200; // offset for Bezier curve control points - doubly large distance from origin


		while (more_paths.length > 0) {

			var curNode_i = more_paths[0];
			var curNode = auto[curNode_i];
			done_paths.push(curNode_i);
			more_paths.splice(0, 1);
			var j_init = 1;
			if (mainrow.indexOf(curNode_i) < 0) {
				j_init = 0;
			}

			for (j=j_init; j < curNode.p.length; j++) {

				var from = curNode.p[j];
				var to = curNode_i;

				// if we are on the mainrow, and we get an incoming edge not from the
				// mainrow, then we ignore it (as the node outside of the mainrow
				// will draw its own outgoing edge)
				if ((j_init == 1) && (mainrow.indexOf(from) < 0)) {
					continue;
				}

				for (var k=1; k < auto[from].n.length; k++) {
					var addPath = auto[from].n[k];
					if ((done_paths.indexOf(addPath) < 0) && (more_paths.indexOf(addPath) < 0)) {
						more_paths.push(addPath);
					}
				}

				var xoff_start, xoff_end, xoff_mid;

				if (positions[to] === undefined) {

					// add (at least one) new node

					var path = [curNode_i];
					var nextNode_i = curNode.n[0];

					while (positions[nextNode_i] === undefined) {
						path.push(nextNode_i);
						for (var k=1; k < auto[nextNode_i].n.length; k++) {
							var addPath = auto[nextNode_i].n[k];
							if ((done_paths.indexOf(addPath) < 0) && (more_paths.indexOf(addPath) < 0)) {
								more_paths.push(addPath);
							}
						}
						nextNode_i = auto[nextNode_i].n[0];
					}

					var plen = path.length;
					xoff_start = positions[auto[path[0]].p[0]];
					xoff_end = positions[auto[path[plen-1]].n[0]];
					xoff_mid = (xoff_end + xoff_start) / 2;
					var xoff_width = xoff_end - xoff_start;

					for (var i = 0; i < plen; i++) {
						xoff = xoff_mid+(xoff_width*(0.5+i - (plen / 2))/plen);
						xoffnext = xoff_mid+(xoff_width*(1.5+i - (plen / 2))/plen);
						positions[path[i]] = xoff;

						var highcolor = default_color;
						var bgcolor = default_bg_color;
						if (extra_high_nodes.indexOf(path[i]) >= 0) {
							highcolor = extra_color;
							bgcolor = extra_bg_color;
						}
						if (highlight_p12.indexOf(auto[path[i]].f) >= 0) {
							highcolor = high_color;
							bgcolor = high_bg_color;
						}

						sout += '<circle cx="' + xoff + '" cy="' + yoffl + '" r="20" style="fill:' + highcolor + '" />';
						sout += '<circle cx="' + xoff + '" cy="' + yoffl + '" r="18" style="fill:' + bgcolor + '" />';
						sout += '<text x="' + xoff + '" y="' + (yoffl+10) +
								'" text-anchor="middle" style="fill:' + highcolor + ';">' +
								auto[path[i]].c + '</text>';

						if (showPrefixes || this.show_auto_i) {
							sout += '<text class="prefix" x="' + xoff + '" y="' + (yoffl-22) +
									'" text-anchor="middle" style="fill:' + highcolor + '">';
							if (showPrefixes) {
								sout += auto[path[i]].f;
								if (this.show_auto_i) {
									sout += ', ';
								}
							}
							if (this.show_auto_i) {
								sout += (path[i] + this.ao);
							}
							sout += '</text>';
						}

						if (i < 1) {
							var strokecolor = default_color;
							var marker_kind = '';
							if (extra_high_edges.indexOf(auto[path[0]].p[0] + '_' + path[i]) >= 0) {
								strokecolor = extra_color;
								marker_kind = '_extra';
							}

							sout += '<path d="M' + (xoff_start + 3) + ',' + yoff + ' Q' + (xoff_start + 10) + ',' + yoffl + ' ' + (xoff - 25) + ',' + yoffl + '" ';
							sout += 'style="stroke: ' + strokecolor + '; stroke-width: 2.25px; fill: none; marker-end: url(#' + markerArrow + marker_kind + ');" ';
							sout += '/>';
						}

						if (i < plen-1) {
							var strokecolor = default_color;
							var marker_kind = '';
							if (extra_high_edges.indexOf(path[i] + '_' + path[i+1]) >= 0) {
								strokecolor = extra_color;
								marker_kind = '_extra';
							}

							sout += '<path d="M' + (xoff + 25) + ',' + yoffl + ' L' + (xoffnext - 25) + ',' + yoffl + '" ';
							sout += 'style="stroke: ' + strokecolor + '; stroke-width: 2.25px; fill: none; marker-end: url(#' + markerArrow + marker_kind + ');" ';
							sout += '/>';
						} else {
							var strokecolor = default_color;
							var marker_kind = '';
							if (extra_high_edges.indexOf(path[i] + '_' + auto[path[plen-1]].n[0]) >= 0) {
								strokecolor = extra_color;
								marker_kind = '_extra';
							}

							sout += '<path d="M' + (xoff + 25) + ',' + yoffl + ' Q' + (xoff_end - 10) + ',' + yoffl + ' ' + (xoff_end - 3) + ',' + yoff + '" ';
							sout += 'style="stroke: ' + strokecolor + '; stroke-width: 2.25px; fill: none; marker-end: url(#' + markerArrow + marker_kind + ');" ';
							sout += '/>';
						}
					}

				} else {

					// just add an edge, but do not add a new node

					xoff_start = positions[from];
					xoff_end = positions[to];
					xoff_mid = (xoff_end + xoff_start) / 2;

					var strokecolor = default_color;
					var marker_kind = '';
					if (extra_high_edges.indexOf(from + '_' + to) >= 0) {
						strokecolor = extra_color;
						marker_kind = '_extra';
					}

					sout += '<path d="M' + (xoff_start + 10) + ',' + yoff + ' Q' + xoff_mid + ',' + yoffdl + ' ' + (xoff_end - 10) + ',' + yoff + '" ';
					sout += 'style="stroke: ' + strokecolor + '; stroke-width: 2.25px; fill: none; marker-end: url(#' + markerArrow + marker_kind + ');" ';
					sout += '/>';
				}

				// alternate!
				if (showPrefixes || this.show_auto_i) {
					yoff = 190 - yoff;
				} else {
					yoff = 200 - yoff;
				}
				yoffl = 200 - yoffl;
				yoffdl = 200 - yoffdl;
			}
		}


		var vByoff = '0';
		if (showPrefixes || this.show_auto_i) {
			vByoff = '-5';
		}

		var sprev = '<div';
		sprev += ' style="overflow-x:auto;text-align:center;"';
		sprev += '>';

		sprev += '<svg ';
		sprev += 'style="width:' + (mainrow.length * 100) + 'px;height:200px" ';
		sprev += 'xmlns="http:/' + '/www.w3.org/2000/svg" version="1.1"' +
				 'viewBox="0 ' + vByoff;
		sprev += ' ' + (mainrow.length * 100) + ' 200" preserveAspectRatio="xMidYMid slice">';

		sprev += '<rect x="0" y="' + vByoff + '" width="' + (mainrow.length * 100) + '" ' +
				 'height="200" style="fill:#FFF" />';

		sout = sprev + sout;

		sout += '</svg>';

		sout += '</div>';


		while (sout.indexOf(this.DS_1_o) > -1) {
			sout = sout.replace(this.DS_1_o, this.DS_1_t);
		}
		while (sout.indexOf(this.DK_1_o) > -1) {
			sout = sout.replace(this.DK_1_o, this.DK_1_t);
		}

		sout = '<div class="svg_btn" style="right:95px" ' +
			   'onclick="GML_UI.saveSVG(' + (this.hide_counter + 1) + ')">' +
			   	'<span>Save</span></div>' +
			   	sout;

		sout = this.hideWrap(sout, 'Graph');

		sout += this.nlnl;

		return sout;
	},



	hide_counter: 0, // keeps track of how many hideable objects we are showing on the page



	// takes in a string containing HTML and a string kind
	// gives out the first string wrapped in a hide-box labelled with the kind string
	hideWrap: function(sout, kind) {

		this.hide_counter++;

		return '<div id="hide-cont-' + this.hide_counter + '" class="svg_container">' +
			   '<div class="svg_btn" onclick="GML_UI.hideObject(' + this.hide_counter + ')">' +
			   '<span id="hide-btn-' + this.hide_counter + '">Hide</span> ' + kind + '</div>' +
			   sout +
			   '</div>';
	},



	// takes in a string containing HTML that wants to be shown as an error message
	// gives out the string formatted as error message and hides the XBW environment
	//   from the current tab
	errorWrap: function(sout) {

		if (GML_UI) {
			GML_UI.unShowXBWEnv(GML_UI.cur_tab);
		}

		return '<div class="error">' + sout + '</div>';
	},



	// takes in a string containing HTML
	// gives out the finished string, having an overflow error message appended to it,
	//   and hides the XBW environment from the current tab
	loopOverflowError: function(sout) {

		sout += this.errorWrap(
				'The computation has been stopped to prevent an infinite loop.<br>' +
				'You could retry it with a higher loop threshold - its current value is ' +
				this.loop_threshold + '.'
			);

		sout = sout.replace(/\^/g, '#');

		return sout;
	},



	// takes in nothing
	// gives out a string containing a pseudo-random graph string
	//   (the graph string is actually not very random at all; instead
	//   it will only contain very basic graphs)
	generateRandomGraphString: function() {

		var gstr = '';

		// get a mainrow between 5 and 25 characters in length
		var mainrow_length = 5 + Math.floor(Math.random() * 20);

		var alphabet = ['A', 'C', 'G', 'T'];

		// add that many random characters
		for (var i=0; i < mainrow_length; i++) {
			gstr += alphabet[Math.floor(Math.random() * 4)];
		}

		// add a random about of infostrings, if any
		var delimiter = '|';

		// 80% chance to add at least one infoblock
		var threshold = 0.8;

		while (Math.random() < threshold) {

			// delimiter to previous infoblock (or to main row)
			gstr += delimiter;
			delimiter = ';';

			// 50% chance to add another infoblock
			threshold = 0.5;

			// own designation
			gstr += ',';

			// origin
			var orig = 1 + Math.floor(Math.random() * mainrow_length);
			gstr += orig + ',';

			// characters along path
			var path_length = Math.floor(Math.random() * 5);
			for (var i=0; i < path_length; i++) {
				gstr += alphabet[Math.floor(Math.random() * 4)];
			}
			gstr += ',';

			// target - recalculated until it is not before origin, as we do not handle loops well
			var targ = -1;
			while (targ < orig) {
				targ = 1 + Math.floor(Math.random() * mainrow_length);
			}
			gstr += targ;
		}

		return gstr;
	},



	// takes in a string representing a graph, e.g. TGA|1,T,2;1,3
	// gives out an array containing the main row and an array containing the infoblocks
	stringToGraph: function(str) {

		var ssplit = str.split('|');

		// insert hashtag character at the beginning of input string
		// (actually, we are using '^' internally instead of '#', as it has lexicographic
		// value above all alphabetical characters)
		str = '^' + ssplit[0];

		// graph info appended to h, e.g. in TGA|1,T,2;1,3 the "|1,T,2;1,3" is the graph info
		var infoblocks = [];

		// we here check for ssplit[1] !== '' to allow for someone writing "TGA|", with pipe
		// but without following it up with infoblocks (according to the standard, a pipe
		// should ONLY be given if infoblocks are following... but humans are squishy and
		// we should trust no one)

		if ((ssplit.length > 1) && (ssplit[1] !== '')) {
			var possible_infoblocks = ssplit[1].split(';');

			// we here check for possible_infoblocks[i] !== '' and in fact go through
			// the whole trouble of using possible_infoblocks instead of assinging to
			// infoblocks directly to allow for someone writing "TGA|1,T,2;", with
			// semicolon in the end without following it up with another infoblock
			// (according to the standard, a semicomma should ONLY separate infoblocks,
			// but not be appended at the end... but humans are squishy and we should
			// trust no one)

			for (var i=0; i < possible_infoblocks.length; i++) {
				if (possible_infoblocks[i] !== '') {
					var b = possible_infoblocks[i].split(',');
					if (b.length !== 4) {
						this.error_flag = true;
					}
					b[1] -= this.ao;
					b[3] -= this.ao;
					infoblocks.push(b.join(','));
				}
			}
		}

		// create array form of the input string
		var mainrow = str.split('');

		// append delimiter character to input array
		mainrow[mainrow.length] = this.DS;

		return [mainrow, infoblocks];
	},



	// takes in an array of characters (the main row) and an array of the
	//   corresponding graph information (the infoblocks), in the form of
	//   an array containing the first array and the second array
	// gives out an automaton (an array of objects which correspond to nodes,
	//   with each node containing is caption c, an array of integers representing
	//   the in-nodes p and an array of integers representing the out-nodes n)
	graphToAutomaton: function(graph) {

		// array of nodes
		var auto = [];

		var ha = graph[0];
		var h_graph = graph[1];

		// first node: hashtag
		auto.push({c: '^', p: [], n: [1]});

		for (var i = 1; i < ha.length - 1; i++) {
			// add a node for each element of the main row
			auto.push({
				c: ha[i], // caption
				p: [i-1], // previous nodes
				n: [i+1], // next nodes
			});
		}

		// last node: dollarsign
		auto.push({c: this.DS, p: [ha.length-2], n: []});

		// add paths of the graph
		for (var i = 0; i < h_graph.length; i++) {
			var path = h_graph[i].split(',');

			// TODO :: make this work for named paths too
			// (currently, the name in p0 and before : in p1 and p3 is just ignored
			// and everything is applied to the main row)

			var p1 = path[1];
			var p2 = path[2];
			var p3 = path[3];

			// if no path identifier is specified for the origin, use the main path
			if (p1.indexOf(':') < 0) {
				p1 = 'mp:' + p1;
			}
			p1 = p1.split(':');
			var p11 = parseInt(p1[1], 10);

			// if no path identifier is specified for the target, use the main path
			if (p3.indexOf(':') < 0) {
				p3 = 'mp:' + p3;
			}
			p3 = p3.split(':');
			var p31 = parseInt(p3[1], 10);

			if (path[2].length === 0) {
				auto[p11].n.push(p31);
				auto[p31].p.push(p11);
			} else {
				var firstNew = auto.length;

				for (var j = 0; j < p2.length; j++) {
					auto.push({
						c: p2[j], // caption
						p: [auto.length-1], // previous nodes
						n: [auto.length+1], // next nodes
					});
				}

				var lastNew = auto.length - 1;

				// update prev and new info for first and last node of path
				auto[firstNew].p = [p11];
				auto[lastNew].n = [p31];

				// update prev and new info for nodes on main row
				auto[p11].n.push(firstNew);
				auto[p31].p.push(lastNew);
			}
		}

		return auto;
	},



	// takes in two automata
	// gives out the automaton formed by merging the one automaton with the other one
	mergeAutomata: function(auto1, auto2) {

		var auto = [];

		var outOf1 = 0; // position of $_1 in auto

		var offset = auto1.length; // offset added to all positions within auto2
		var del_i = -1; // keep track of the i of the deleted node

		// start building auto as copy of auto1
		for (var i=0; i < auto1.length; i++) {
			if (auto1[i]) {
				var newNode = this.deep_copy_node(auto1[i]);
				if (newNode.c == this.DS) {
					if (this.merge_directly) {

						del_i = i;
						offset -= 2;

						// we are here assuming that only one edge goes into $ in H_1
						outOf1 = newNode.p[0];

						continue;
					}
					outOf1 = i;
				}
				auto.push(newNode);
			}
		}

		// update counts of other nodes due to deletion of $ at the end of H_1
		if (this.merge_directly) {
			for (i=0; i < auto.length; i++) {
				if (auto[i]) {
					for (var j=0; j < auto[i].p.length; j++) {
						if (auto[i].p[j] >= del_i) {
							auto[i].p[j] -= 1;
						}
					}
					for (var j=0; j < auto[i].n.length; j++) {
						if (auto[i].n[j] >= del_i) {
							auto[i].n[j] -= 1;
						}
					}
				}
			}
		}

		var into2 = auto.length;  // position of #_1 in auto
		var i_start = 0;

		if (this.merge_directly) {
			
			i_start = 1;

			// we are here assuming that only one edge goes out of # in H_2
			into2 = auto2[0].n[0] + offset;
		}

		// add all nodes from auto2
		for (var i=i_start; i < auto2.length; i++) {
			if (auto2[i]) {
				var newNode = this.deep_copy_node(auto2[i]);
				for (var k=0; k < newNode.p.length; k++) {
					newNode.p[k] += offset;
				}
				for (var k=0; k < newNode.n.length; k++) {
					newNode.n[k] += offset;
				}
				auto.push(newNode);
			}
		}

		// fuse them together

		// set outOf1.n = [into2] and into2.p = [outOf1]
		auto[outOf1].n = [into2];
		auto[into2].p = [outOf1];

		if (!this.merge_directly) {
			auto[outOf1].c = this.DS_1_o;
			auto[into2].c = this.DK_1_o;
		}

		return auto;
	},



	// takes in an automaton and a boolean parameter stating whether we should be verbose or not
	// gives out a reverse deterministic automaton realizing the same language
	makeAutomatonReverseDeterministic: function(auto, addToSOut) {

		/*
		We here take in an automaton that realizes an alignment graph
		and want to make it reverse deterministic.

		Consider the input
		GAGGTA|,2,T,4;,3,,5

		Here we have two 'G' nodes leading both to the same 'T' node; as alignment, this is:

		GAGGTA
		GAG-TA
		GATGTA

		To make this reverse deterministic, we move the predecessors of the 'T' node as far to
		the right as possible:

		GAGGTA
		GA-GTA
		GATGTA

		So that we achieve the following path
		GAGGTA|,2,T,4;,2,,4
		*/


		// TODO :: Right now, we here just do a very simplified version:
		//         We check to find something like
		//           A B B C, graph from 2 to 4
		//         and then we convert it into something like
		//           A B B C, graph from 1 to 3
		//         Even this simplified version may be WRONG / BUGGY in some cases,
		//         let along not working / not being enough for many many cases.
		//         => improve that!

		while (this.makeAutomatonReverseDeterministic_int(auto, addToSOut)) {
			if (addToSOut) {
				this.sout += this.visualize(auto);
			}
		}

		return auto;
	},



	// takes in an automaton
	// gives out true if more work could help to make it reverse deterministic,
	//   or false otherwise
	makeAutomatonReverseDeterministic_int: function(auto, addToSOut) {

		for (var i=0; i < auto.length; i++) {
			if (auto[i]) {
				var prev = auto[i].p;
				var plen = prev.length;

				// if the node has less than 2 predecessors, all is fine anyway, so
				// only continue if there are 2 or more predecessors
				if (plen >= 2) {
					var prevchars = [];
					for (var j=0; j < plen; j++) {
						var newchar = auto[prev[j]].c;
						if (prevchars.indexOf(newchar) < 0) {
							// all is good, the new character has not been seen before
							prevchars.push(newchar);
						} else {
							// the character has been seen before, check if we have a
							// special case and can do something about it
							
							// (only return true to continue if this here is successful,
							// otherwise return false anyway to not hang in an infinite loop)
							return this.reverseDeterminizer(i, newchar, auto, addToSOut);
						}
					}
				}
			}
		}

		return false;
	},



	// takes in a position i, a label newchar and an automaton auto
	// gives out true if it manages to improve reverse determinicism,
	//   false otherwise
	reverseDeterminizer: function(i, newchar, auto, addToSOut) {

		var nodesWithSameLabel = [];
		var cur_p = auto[i].p;

		// find all nodes for which newchar is .c
		for (var k=0; k < cur_p.length; k++) {
			if (auto[cur_p[k]].c == newchar) {
				nodesWithSameLabel.push(cur_p[k]);
			}
		}



		// PLAN 1 :: make reverse deterministic by merging two nodes with same prev or next nodes
		//
		//           For this to work, we wish to have:
		//           1.   two nodes A and B
		//           2.   that have the same label
		//           3.1. and both lead to exactly the same nodes OR
		//           3.2. both are preceded by exactly the same nodes OR
		//           3.3. all the nodes that A leads to, B also leads to
		//                all the nodes that A is preceded by, B is also preceded by
		//
		//           If all this is fulfilled for any two nodes that
		//           we can find, then we can merge them.
		//
		// A note about 3.2: if both are preceded bz exactly the same nodes then we CAN
		// merge them, but we do not usually want to do that for any arbitrary nodes (it
		// will actually be quite detrimental to what we want to achieve, and we will go
		// on and on splitting and merging in circles forever - usually); however, we are
		// here not considering arbitrary nodes. Instead, we consider only nodes that lead
		// into the same node and have the same label. (In no other cases is this whole
		// function even called.)
		// Therefore, the merging of the two nodes IN THESE CIRCUMSTANCES is actually
		// rather helpful, and we will not resort to splitting them again just a second
		// later. =)

		// 1. check that there actually are at least two nodes that we are working on ;)
		if (nodesWithSameLabel.length > 1) {

			// 2. is trivially fulfilled as we are given only nodes with the same label,
			//    so we do not have to worry about checking it

			for (var k=0; k < nodesWithSameLabel.length; k++) {
				for (var j=0; j < nodesWithSameLabel.length; j++) {
					if (k !== j) {
						var n1i = nodesWithSameLabel[k];
						var n2i = nodesWithSameLabel[j];
						var n1 = auto[n1i];
						var n2 = auto[n2i];

						// do they both have exactly the same next nodes?
						if (this.arraysContainSameEls(n1.n, n2.n)) {

							this.mergeNodesInAutomaton(auto, n1i, n2i);
							return true;
						}

						// do they both have exactly the same prev nodes?
						if (this.arraysContainSameEls(n1.p, n2.p)) {

							this.mergeNodesInAutomaton(auto, n1i, n2i);
							return true;
						}

						// is n1 contained in n2?
						if ((n1.n.length <= n2.n.length) && (n1.p.length <= n2.p.length) &&
							this.arrayContainsOtherArraysEls(n2.n, n1.n) &&
							this.arrayContainsOtherArraysEls(n2.p, n1.p)) {

							this.mergeNodesInAutomaton(auto, n1i, n2i);
							return true;
						}

						// is n2 contained in n1?
						if ((n1.n.length >= n2.n.length) && (n1.p.length >= n2.p.length) &&
							this.arrayContainsOtherArraysEls(n1.n, n2.n) &&
							this.arrayContainsOtherArraysEls(n1.p, n2.p)) {

							this.mergeNodesInAutomaton(auto, n1i, n2i);
							return true;
						}
					}
				}
			}
		}



		// PLAN 2 :: make reverse deterministic by moving an edge
		//
		//           For this to work, we wish to have:
		//           1. two nodes B and C that have the same label,
		//           2. and both have an out-edge leading to the same node D,
		//           3. and one is the predecessor of the other
		//
		//           If all this is fulfilled, we can take out the edge
		//           from B to D and replace it with an edge from A, the
		//           predecessor of B, to C.

		// check if these are exactly two nodes
		if (nodesWithSameLabel.length == 2) {

			// check if either of these nodes is predecessor of each other
			var nodeL = null;
			var nodeR = null;
			var nodesP = [];
			if (auto[nodesWithSameLabel[0]].n.indexOf(nodesWithSameLabel[1]) > -1) {
				nodeL = nodesWithSameLabel[0];
				nodeR = nodesWithSameLabel[1];
			}
			if (auto[nodesWithSameLabel[1]].n.indexOf(nodesWithSameLabel[0]) > -1) {
				nodeL = nodesWithSameLabel[1];
				nodeR = nodesWithSameLabel[0];
			}

			if (nodeL) {

				nodesP = auto[nodeL].p;

				// move path to the left, where we have
				// nodesP => nodeL => nodeR => i
				// originally, we have a path from nodeL to i on top of that,
				// and we want to move it so that it now points from nodesP to nodeR

				if (addToSOut) {
					this.sout += 'We take the path from ' + auto[nodeL].c + ' around ' + auto[nodeR].c +
								 ' to ' + auto[i].c + ' and move it to the left, ' + 
								 'thereby moving the gap to the left:' + this.nl;
				}

				// remove i from nodeL.n
				auto[nodeL].n.splice(auto[nodeL].n.indexOf(i), 1);

				// remove nodeL from i.p
				auto[i].p.splice(auto[i].p.indexOf(nodeL), 1);

				for (var i=0; i < nodesP.length; i++) {
					// add nodeR to nodeP.n
					auto[nodesP[i]].n.push(nodeR);

					// add nodeP to nodeR.p
					auto[nodeR].p.push(nodesP[i]);
				}

				return true;
			}
		}



		// PLAN 3 :: split nodes to enable merging in a later step
		//
		//           We have now gotten quite desparate, as we cannot find
		//           a way to improve our situation through merging.
		//           We shall instead split nodes as wisely as we can to
		//           make it possible to merge again in the next step,
		//           and achieve a positive outcome.
		//
		//           For this to work, we wish to have:
		//           1. a node that has several outgoing edges
		//
		//           If this is fulfilled, we can split the node.
		//           (As with point 3.2 in PLAN 1, we can also here see
		//           that just splitting ANY random node would most likely
		//           not be particularly helpful. However, splitting one
		//           of the nodes that are passed into this function when
		//           it is called is actually quite beneficial, as we know
		//           that these nodes need to be worked on somehow, and if
		//           merging cannot achieve anything anymore, and edge
		//           moving cannot achieve anything anymore, then we need
		//           to resort to the last thing possible: node splitting!)

		for (var i=0; i < nodesWithSameLabel.length; i++) {
			if (auto[nodesWithSameLabel[i]].n.length > 1) {

				this.splitNodeInAutomaton(auto, nodesWithSameLabel[i]);
				return true;
			}
		}



		return false;
	},



	// takes in an automaton and two indices for nodes
	// gives out nothing, but merges the two nodes within the automaton
	mergeNodesInAutomaton: function(auto, node_1, node_2) {

		// we figure out which of the two nodes is higher
		var n_high = node_1;
		var n_low = node_2;
		if (n_low > n_high) {
			n_high = node_2;
			n_low = node_1;
		}

		// we merge the HIGHER node into the LOWER node,
		// so that the lower one persists - we do this
		// because an automaton can have "false" nodes
		// in any position EXCEPT for position 0, so
		// by keeping the lower node, we automatically
		// keep node 0 without having to worry about it
		var mergeThisNode = auto[n_high];
		var intoThisNode = auto[n_low];

		// put prevNodes from mergeThisNode intoThisNode
		for (var k=0; k < mergeThisNode.p.length; k++) {

			var updateNode = auto[mergeThisNode.p[k]];

			// does the merge target already have this preceding node?
			if (intoThisNode.p.indexOf(mergeThisNode.p[k]) < 0) {

				// insert new preceding node into merge target
				intoThisNode.p.push(mergeThisNode.p[k]);

				// update the preceding node itself to now point to the merge target node
				for (var j=0; j < updateNode.n.length; j++) {
					if (updateNode.n[j] == n_high) {
						updateNode.n[j] = n_low;
						break;
					}
				}
			} else {
				// update the preceding node itself to drop the pointer to the merge origin node
				// (as it already points to the merge target node, there is no reason for replacing,
				// instead we really just need to delete)
				for (var j=0; j < updateNode.n.length; j++) {
					if (updateNode.n[j] == n_high) {
						// delete the out-edge at position j
						updateNode.n.splice(j, 1);
						break;
					}
				}
			}
		}

		// put nextNodes from mergeThisNode intoThisNode
		for (var k=0; k < mergeThisNode.n.length; k++) {

			var updateNode = auto[mergeThisNode.n[k]];

			// does the merge target already have this following node?
			if (intoThisNode.n.indexOf(mergeThisNode.n[k]) < 0) {

				// insert new following node into merge target
				intoThisNode.n.push(mergeThisNode.n[k]);

				// update the following node itself to now point to the merge target node
				for (var j=0; j < updateNode.p.length; j++) {
					if (updateNode.p[j] == n_high) {
						updateNode.p[j] = n_low;
						break;
					}
				}
			} else {
				// update the following node itself to drop the pointer to the merge origin node
				// (as it already points to the merge target node, there is no reason for replacing,
				// instead we really just need to delete)
				for (var j=0; j < updateNode.p.length; j++) {
					if (updateNode.p[j] == n_high) {
						// delete the in-edge at position j
						updateNode.p.splice(j, 1);
						break;
					}
				}
			}
		}

		// We could now do lots and lots of work to
		// delete mergeThisNode by slicing the array that auto is,
		// and by updating all indices throughout auto.
		// However, this may not actually be necessary - instead,
		// we could just replace the node with nothingness and
		// trust that no one will ever access it, as an automaton
		// is not used like an array ("give me nodes 5 through 9"),
		// but usually more like a graph ("give me all the nodes
		// preceding node 5.")

		auto[n_high] = false;
	},



	// takes in an automaton and an index of a node
	// gives out nothing, but splits the node within the automaton,
	//   that is, replaces the node with as many nodes as it has outedges,
	//   with each replacement having exactly one of the outedges,
	//   and all the replacements having the same inedges as the original node
	splitNodeInAutomaton: function(auto, node_i) {

		var node = auto[node_i];

		var listOfNextNodes = node.n;
		var orig_p = node.p;
		var orig_c = node.c;

		// update the first node - here we only need to set the nextNodes from all down to this one
		node.n = [node.n[0]];

		// for all other nodes...
		for (var i=1; i < listOfNextNodes.length; i++) {

			var this_i = auto.length;

			// ... we push them into the automaton, ...
			auto.push({
				p: this.deep_copy_array(orig_p),
				c: orig_c,
				n: [listOfNextNodes[i]]
			});

			// ... we update the preceding nodes (by adding) ...
			for (var k=0; k < orig_p.length; k++) {
				auto[orig_p[k]].n.push(this_i);
			}

			// ... and we update the following node (by updating)
			var nextNode_p = auto[listOfNextNodes[i]].p;
			for (var k=0; k < nextNode_p.length; k++) {
				if (nextNode_p[k] == node_i) {
					nextNode_p[k] = this_i;
					break;
				}
			}
		}
	},



	// takes in an automaton
	// gives out true if the automaton is reverse deterministic, false otherwise
	isAutomatonReverseDeterministic: function(auto) {

		for (var i=0; i < auto.length; i++) {
			if (auto[i]) {
				var prev = auto[i].p;
				var plen = prev.length;

				// we want to check if some nodes have several predecessors that have
				// the same label - so if the node has 0 or 1 predecessors, then we
				// do not need to check anything ^^
				if (plen > 1) {
					var prevchars = [];
					for (var j=0; j < plen; j++) {
						var newchar = auto[prev[j]].c;
						if (prevchars.indexOf(newchar) < 0) {
							// all is good, the new character has not been seen before
							prevchars.push(newchar);
						} else {
							// oh noooo, all is ruined! - the character has been seen before
							return false;
						}
					}
				}
			}
		}

		return true;
	},



	// takes in two arrays a1 and a2
	// gives out true if a1 and a2 contain the same elements, false otherwise
	arraysContainSameEls: function(a1, a2) {

		return this.arrayContainsOtherArraysEls(a1, a2) && this.arrayContainsOtherArraysEls(a2, a1);
	},



	// takes in two arrays a1 and a2
	// gives out true if all elements of a2 are contained in a1
	arrayContainsOtherArraysEls: function(a1, a2) {

		// go through all elements of a2
		for (var i=0; i < a2.length; i++) {

			// is this element of a2 contained in a1?
			if (a1.indexOf(a2[i]) < 0) {

				// ... no, it is not .__.
				return false;
			}
		}

		// none are missing, so all are there, so we return true =)
		return true;
	},



	// takes in a prefix array, a BWT array and an M bit vector array
	// gives out the F bit vector array generated from these
	generateFfromPrefixesBWTM: function(prefixes, bwt, m) {

		// TODO :: this is quite experimental for now, as I don't actually know
		// if this is how Siren2014 defines the F bit vector... we'll see

		var f = [];

		// PUSH
		var orig_p12 = this.p12;
		this.p12 = prefixes;
		var orig_bwt = this.bwt;
		this.bwt = bwt;
		var orig_m = this.m;
		this.m = m;

		// generate F
		for (var i=0; i < prefixes.length; i++) {

			var newf = '1';
			var len = this.prevNodes_wo_origin(i).length;
			for (var j=1; j < len; j++) {
				newf += '0';
			}

			f.push(newf);
		}

		// POP
		this.p12 = orig_p12;
		this.bwt = orig_bwt;
		this.m = orig_m;

		return f;
	},



	// takes in an automaton
	// gives out the automaton with prefixes calculated for each node
	computePrefixes: function(auto) {
		return this.workOnAutomatonPrefixes(auto, false, false);
	},



	// one character that indicates a problem in a prefix
	// needs to be different from any input characters, from ^ and from $
	prefixErrorChar: '!',



	// takes in an automaton and a boolean parameter as well as a boolean parameter
	//   stating whether we should be verbose or not
	// gives out the automaton with prefixes calculated for each node,
	//   and converted into a prefix sorted automaton if makePrefixSorted is true
	workOnAutomatonPrefixes: function(auto, makePrefixSorted, addToSOut) {

		// while more work is required, do more work
		while (this.workOnAutomatonPrefixes_int(auto, makePrefixSorted, addToSOut)) {
			if (addToSOut) {
				this.sout += this.visualize(this.computePrefixes(auto), true);
			}
		}

		return auto;
	},


	// takes in an automaton and a boolean parameter
	// gives out true if more work on the automaton is required,
	//   or false if the automaton has been successfully equipped with
	//   prefixes / has been converted to a prefix sorted one (depending
	//   on the makePrefixSorted parameter)
	workOnAutomatonPrefixes_int: function(auto, makePrefixSorted, addToSOut) {

		// start by setting all prefixes just to the labels themselves
		for (var i=0; i < auto.length; i++) {
			if (auto[i]) {
				auto[i].f = auto[i].c;
			}
		}

		var changed_something = true;

		// perform several rounds
		// each time: go over all nodes, check if other nodes have the same prefix,
		// if so: elongate prefixes and do another round,
		// otherwise: no further rounds are needed
		while (changed_something) {
			changed_something = false;

			// always get a new length, as the length changes ;)
			var alen = auto.length;

			for (var i=0; i < alen; i++) {

				if (auto[i]) {
					var cur_auto_f = auto[i].f;

					// if we are already in a nonsensical position, then going further down the
					// rabbit hole is not going to achieve anything ;)
					if (cur_auto_f[cur_auto_f.length - 1] !== this.prefixErrorChar) {

						var same_as = [i];

						for (var j=0; j < alen; j++) {
							if (i !== j) {
								if (cur_auto_f == auto[j].f) {
									same_as.push(j);
									changed_something = 1;
								}
							}
						}

						// alright, so we found other nodes with the same prefix...
						if (same_as.length > 1) {
							// we know that auto[i].f == auto[j].f for all j in same_as,
							// so the rec_depth is the same for all, so we can compute it
							// outside the for loop
							var rec_depth = auto[i].f.length - 1;
							for (var j=0; j < same_as.length; j++) {

								// ... so now for each node, we follow along the graph
								// until rec_depth is reached, and append that node's label
								// (and if that is impossible as the graph splits into several
								// nodes with different labels, then we append an exclamation
								// point, indicating that this is why the automaton is currently
								// NOT prefix sorted!)

								// we actually keep track of several nodes, as a path could split
								// and still be acceptable, e.g.
								//     C -> G
								//   /
								// A            <= here, the prefix of A can be ACG without problem
								//   \
								//     C -> G

								var curNodes = [same_as[j]];
								// for each letter K in the prefix...
								for (var k=0; k < rec_depth; k++) {
									var nextNodes = [];
									// ... we look at each node L on the current node list ...
									for (var l=0; l < curNodes.length; l++) {
										var curNode = auto[curNodes[l]];
										// ... and look at each node M following that node L
										for (var m=0; m < curNode.n.length; m++) {
											nextNodes.push(curNode.n[m]);
										}
									}
									curNodes = nextNodes;
								}

								var nextNode_confusing = false;

								var firstNodeLabel = auto[auto[curNodes[0]].n[0]].c;

								// check if any nodes on the list have any successors that have labels
								// different than firstNodeLabel
								for (var l=0; l < curNodes.length; l++) {
									var curNode = auto[curNodes[l]];
									for (var m=0; m < curNode.n.length; m++) {
										if (firstNodeLabel !== auto[curNode.n[m]].c) {
											nextNode_confusing = true;
											break;
										}
									}
									if (nextNode_confusing) {
										break;
									}
								}

								if (nextNode_confusing) {
									if (makePrefixSorted) {
										
										// we need to actually make it prefix sorted, so we need
										// to split the current node into n.length nodes

										// we actually need deep copies, so that we can later
										// do work on this without getting REALLY confusing problems
										var orig_node = this.deep_copy_node(auto[same_as[j]]);

										// update the current node: leave p and c, but only keep the
										// first of the out-nodes and add their label to the prefix
										auto[same_as[j]] = {
											p: orig_node.p, // prev
											c: orig_node.c, // caption
											n: [orig_node.n[0]], // next
											f: orig_node.f + auto[orig_node.n[0]].c, // prefix
										};

										if (addToSOut) {
											this.sout += 'We split the node with label ' + orig_node.c +
														' and prefix ' + orig_node.f + this.prefixErrorChar +
														' into ' + orig_node.n.length + ' nodes:' + this.nl;
										}

										// add new nodes to the automaton
										for (var k=1; k < orig_node.n.length; k++) {
											orig_node = this.deep_copy_node(orig_node);
											// add the new node as outnode to its predecessor
											for (var l=0; l < orig_node.p.length; l++) {
												auto[orig_node.p[l]].n.push(auto.length);
											}
											// update the outgoing node and set its predecessor
											// to this new node (we update instead of add, as the
											// node is being split and the other node is not a
											// predecessor anymore)
											for (var l=0; l < auto[orig_node.n[k]].p.length; l++) {
												if (auto[orig_node.n[k]].p[l] == same_as[j]) {
													auto[orig_node.n[k]].p[l] = auto.length;
												}
											}
											auto.push({
												p: orig_node.p, // prev
												c: orig_node.c, // caption
												n: [orig_node.n[k]], // next
												f: orig_node.f + auto[orig_node.n[k]].c, // prefix
											});
										}

										// ask the calling function to recompute everything - as adding a
										// node is messy, and it is much simpler to just recaluclate all
										// prefixes than to keep track of which prefixes where need to be
										// updated how
										return true;

									} else {
										// don't worry about anything, just indicate that an error
										// occurred
										auto[same_as[j]].f += this.prefixErrorChar;
									}
								} else {
									// don't worry about anything - as actually nothing is wrong
									auto[same_as[j]].f += auto[auto[curNodes[0]].n[0]].c;
								}
							}
						}
					}
				}
			}
		}

		return false;
	},



	// takes in a node
	// gives out a deep copy of the exact same node, with no pointers attaching it to the old one
	deep_copy_node: function(node) {

		var onode = {
			p: [], // prev
			c: node.c, // caption
			n: [], // next
			f: node.f // prefix
		};

		// deep copy prev
		for (var i=0; i < node.p.length; i++) {
			onode.p.push(node.p[i]);
		}

		// deep copy next
		for (var i=0; i < node.n.length; i++) {
			onode.n.push(node.n[i]);
		}

		return onode;
	},



	// takes in an array
	// gives out an exact copy of the array, instead of just a pointer to it
	//   (this only copies the outer layer - if there are pointers within it,
	//   they will be kept as pointers)
	deep_copy_array: function(arr) {

		var oarr = [];

		for (var i=0; i < arr.length; i++) {
			oarr.push(arr[i]);
		}

		return oarr;
	},



	// takes in an automaton with prefixes
	// gives out true if it is prefix sorted or false otherwise
	isAutomatonPrefixSorted: function(auto) {

		for (var i=0; i < auto.length; i++) {
			if (auto[i]) {
				var cur_auto_f = auto[i].f;

				// aha! there is at least one node that does not have a useful prefix!
				if (cur_auto_f[cur_auto_f.length - 1] == this.prefixErrorChar) {
					return false;
				}
			}
		}

		return true;
	},



	// takes in an automaton with prefixes and a boolean parameter stating whether we
	//   should be verbose or not
	// gives out a prefix sorted automaton recognizing the same language
	makeAutomatonPrefixSorted: function(auto, addToSOut) {
		return this.workOnAutomatonPrefixes(auto, true, addToSOut);
	},



	// takes in an automaton
	// gives out the BWT and M vector
	getFindexFromAutomaton: function(auto) {

		var prefixes = [];

		for (var i=0; i < auto.length; i++) {
			if (auto[i]) {
				prefixes.push([auto[i].f, i]);
			}
		}

		prefixes.sort();

		var BWT = [];
		var M = [];

		for (var i=0; i < prefixes.length; i++) {

			var BWTstr = '';
			var p = auto[prefixes[i][1]].p;
			var klen = p.length;
			for (var k=0; k < klen; k++) {
				// we do not need to worry about characters appearing multiple times,
				// as reverse determinism assures us that each one will only appear once
				BWTstr += auto[p[k]].c + '|';
			}
			if (BWTstr === '') {
				BWTstr = this.DS;
			} else {
				BWTstr = BWTstr.slice(0, BWTstr.length - 1);
			}
			BWT.push(BWTstr);

			var Mstr = '1';
			var klen = auto[prefixes[i][1]].n.length;
			for (var k=1; k < klen; k++) {
				Mstr += '0';
			}
			M.push(Mstr);

			prefixes[i] = prefixes[i][0];
		}

		return [prefixes, BWT, M];
	},



	// takes in an findex with BWT and M vector
	// gives out the automaton
	getAutomatonFromFindex: function(findex) {

		var i;
		var auto = [];

		// initiate the environment (overwriting this.p12, this.p12_itlv, this.bwt and this.m)
		this.p12 = [];
		this.p12_itlv = this.reparr(this.p12.length, this.origins[1]);
		for (i=0; i < findex[0].length; i++) {
			this.p12.push([findex[0][i], this.origins[1]]);
		}

		this.bwt = findex[1];
		this.m = findex[2];

		// find the first node
		var firstNodei;

		for (i=0; i < this.p12.length; i++) {
			if (this.p12[i][0] === this.DK) {
				firstNodei = i;
				break;
			}
		}

		var p12ToAuto = [];
		p12ToAuto[firstNodei] = 0;
		var nextAutoNode = 0;
		var edgesDone = [];

		auto[0] = {
			p: [], // prev
			c: this.p12[firstNodei][0][0], // caption
			n: [], // next
			f: this.p12[firstNodei][0], // prefix
		};

		// advance using the nextNodes function
		var next_nodes = this.nextNodes(firstNodei);
		var further_edges = [];
		for (i=0; i < next_nodes.length; i++) {
			further_edges.push(firstNodei + '>' + next_nodes[i]);
			edgesDone.push(firstNodei + '>' + next_nodes[i]);
		}

		while (further_edges.length > 0) {

			var new_further_edges = [];

			for (i=0; i < further_edges.length; i++) {

				// current node (the node we are adding)
				var c_node_i = further_edges[i].slice(further_edges[i].indexOf('>')+1);
				var c_node_ai = p12ToAuto[c_node_i]; // current node index in auto

				// previous node (the node from which the edge leads to the current node)
				var p_node_i = further_edges[i].slice(0, further_edges[i].indexOf('>'));
				var p_node_ai = p12ToAuto[p_node_i]; // previous node index in auto

				// if the current node has not been added to the automaton at all yet,
				// add it now
				if (c_node_ai === undefined) {
					nextAutoNode++;
					c_node_ai = nextAutoNode;
					p12ToAuto[c_node_i] = c_node_ai;
					auto.push({
						p: [], // prev
						c: this.p12[c_node_i][0][0], // caption
						n: [], // next
						f: this.p12[c_node_i][0], // prefix
					});
				}

				// add current edge to automaton
				auto[c_node_ai].p.push(p_node_ai);
				auto[p_node_ai].n.push(c_node_ai);

				// add all out-edges of the current node to the next round
				next_nodes = this.nextNodes(c_node_i);
				for (var j=0; j < next_nodes.length; j++) {
					new_further_edges.push(c_node_i + '>' + next_nodes[j]);
				}
			}

			further_edges = [];

			for (i=0; i < new_further_edges.length; i++) {
				if (edgesDone.indexOf(new_further_edges[i]) < 0) {
					// store this edge as one that we want to do in the next step
					further_edges.push(new_further_edges[i]);

					// store the information that this edge has been done
					// or will have been done soon
					edgesDone.push(new_further_edges[i]);
				}
			}
		}

		// take out the next from '$'
		auto[auto[0].p[0]].n = [];
		// and take out the prev from '#'
		auto[0].p = [];

		// This here is not strictly necessary for a valid automaton,
		// but it being done is an assumption within the visualization
		// function: basically, we assume that on the main row, .n[0]
		// contains the next node on the main row (which is trivially
		// true, as that is how we define the main row ^^), AND that
		// on the main row, .p[0] contains the previous node on the
		// main row - which however would not necessarily be true at
		// this point here!
		// To circumvent doing A LOT of calculations within the visualization
		// function over and over again, we just swap entries within .p
		// over here until .p[0] contains the previous node on the main
		// row (we now that the previous one is SOMEWHERE in .p anyway,
		// but without this piece of code it might be in .p[1] or .p[2] etc.)
		
		var i = 0;

		// go through the mainrow by going from # always via the first alternative
		// until we find a node that has no outgoing nodes (which will be $ / $_1,
		// and here we know that no funny bunnies put an edge from $ to #, as we
		// just built all the edges ourselves ^^)
		while (auto[i].n.length > 0) {
			
			var next = auto[i].n[0];

			// now replace auto[next].p = [..., i, ...] with newp = [i, ..., ...],
			// so that i is always in the beginning of auto[next].p
			var newp = [i];
			for (var j=0; j < auto[next].p.length; j++) {
				if (auto[next].p[j] != i) {
					newp.push(auto[next].p[j]);
				}
			}
			auto[next].p = newp;

			i = next;
		}

		// just used for later visualization purposes
		GML.vis_p12ToAuto = p12ToAuto;

		return auto;
	},



	// takes in two unterminated strings
	// gives out a section in DaTeX or HTML about the generation of their BWTs
	generate_BWTs_naively: function(h1, h2) {

		this.build_BWTs_naively(h1, h2);



		var sout = '';
		
		if(!this.give_out_HTML) {
			sout += " Graph Alignment - BWT Generation" + this.nl;
			sout += "¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯" + this.nl;
		}

		sout += "We are looking at " + this.h1 + " and " + this.h2 + "." + this.nlnl;
		
		sout += "That is, we have" + this.nlnl;
		
		if(this.give_out_HTML) {
			sout += this.nlnl + this.DH + ' = ' + this.h + this.tabchar+this.tabchar + this.DH_1 + ' = ' + this.h1 +
					this.tabchar+this.tabchar + this.DH_2 + ' = ' + this.h2 + this.nlnlnl;
		} else {
			sout += '$$ H = "' + this.h + '"' + this.tabchar+this.tabchar + this.H_1 + ' = "' + this.h1 +
					'"' + this.tabchar+this.tabchar + this.H_2 + ' = "' + this.h2 + '" $$' + this.nlnl;
		}



		// BWT and pos for H

		sout += "To generate the full BWT of " + this.DH + ", ";
		sout += "we first create its cyclic rotations:" + this.nlnl;

		sout += this.print_arrofarr(this.h_cr).join(this.nlnl);

		sout += this.nlnlnl + "All of the cyclic rotations sorted together:" + this.nlnl;
		
		sout += this.print_arrofarr(this.h_scr).join(this.nlnl);

		sout += this.nlnlnl + "So overall we get the following positions ";
		sout += "and BWT for " + this.DH + ":" + this.nlnl;

		sout += this.s_h_table;



		// BWT and pos for H_1 and H_2

		sout += "We now generate the BWTs for " + this.DH_1 +
				" and " + this.DH_2 + "." + this.nlnl;

		sout += "We again first write down the cyclic rotations for " + this.DH_1 + ":" + this.nlnl;

		sout += this.print_arrofarr(this.h1_cr).join(this.nlnl);

		sout += this.nlnlnl + "And for " + this.DH_2 + ":" + this.nlnl;

		sout += this.print_arrofarr(this.h2_cr).join(this.nlnl);

		sout += this.nlnlnl + "We now have all of the cyclic rotations sorted together for " +
				this.DH_1 + ":" + this.nlnl;

		sout += this.print_arrofarr(this.h1_scr).join(this.nlnl);

		sout += this.nlnlnl + "And for " + this.DH_2 + ":" + this.nlnl;

		sout += this.print_arrofarr(this.h2_scr).join(this.nlnl);

		sout += this.nlnlnl + "Which gives us the following positions ";
		sout += "and BWT:" + this.nlnl;

		sout += this.tab;
		if (this.give_out_HTML) {
			sout += '<thead><tr>';
			sout += "<th>For " + this.DH_1 + ":</th><th>" + this.tabchar + "</th><th>For " + this.DH_2 + ":</th>" + this.nl;
			sout += '</tr></thead><tbody><tr><td>';
		} else {
			sout += "{l l l}" + this.nl;
			sout += "For " + this.DH_1 + ": & & For " + this.DH_2 + ":" + this.tabnl;
		}

		sout += this.h1_pos.join(', ') + this.td+this.td + this.h2_pos.join(', ') + this.tabnl;
		sout += this.h1_bwt.join(', ') + this.td+this.td + this.h2_bwt.join(', ') + this.nl;
		sout += this.endtab;

		sout += "The full BWT for " + this.DH + " was:" + this.nlnl;

		sout += this.s_h_table;



		sout += this.s_end_document;

		return sout;
	},



	// takes in two unterminated strings
	// gives out a section in DaTeX or HTML about their merging behavior
	merge_BWTs_naively: function(h1, h2) {

		this.build_BWTs_naively(h1, h2);



		var sout = '';
		
		if(!this.give_out_HTML) {
			sout += " Graph Alignment - BWT Merging" + this.nl;
			sout += "¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯" + this.nl;
		}

		sout += "We are merging " + this.h1 + " and " + this.h2 + "." + this.nlnl;

		if (this.reorder1and2) {
			sout += "We see that " + this.h1 + " > " + this.h2;
		} else {
			sout += "We see that " + this.h1 + " < " + this.h2;
		}
		sout += ", so we concatenate them as " + h1 + this.DS_1 + h2 + this.DS_2 + this.nl;
		sout += "(with " + this.DS_1 + " before " + this.DS_2 + ".)" + this.nlnl;
		
		sout += "That is, we have" + this.nlnl;
		
		if(this.give_out_HTML) {
			sout += this.nlnl + this.DH + ' = ' + this.h + this.tabchar+this.tabchar + this.DH_1 + ' = ' + this.h1 +
					this.tabchar+this.tabchar + this.DH_2 + ' = ' + this.h2 + this.nlnlnl;
		} else {
			sout += '$$ H = "' + this.h + '"' + this.tabchar+this.tabchar + this.H_1 + ' = "' + this.h1 +
					'"' + this.tabchar+this.tabchar + this.H_2 + ' = "' + this.h2 + '" $$' + this.nlnl;
		}



		// BWT and pos for H

		sout += "To find the intended merging result, ";
		sout += "we first create the cyclic rotations of " + this.DH + ":" + this.nlnl;

		sout += this.print_arrofarr(this.h_cr).join(this.nlnl);

		sout += this.nlnlnl + "All of the cyclic rotations sorted together:" + this.nlnl;
		
		sout += this.print_arrofarr(this.h_scr).join(this.nlnl);

		sout += this.nlnlnl + "So overall we want to find the following positions ";
		sout += "and BWT through merging " + this.DH_1 + " and " + this.DH_2 + ":" + this.nlnl;

		sout += this.s_h_table;



		// BWT and pos for H_1 and H_2

		sout += "To achieve this with merging, we generate the BWTs for " + this.DH_1 +
				" and " + this.DH_2 + "." + this.nlnl;

		sout += "We again first write down the cyclic rotations for " + this.DH_1 + ":" + this.nlnl;

		sout += this.print_arrofarr(this.h1_cr).join(this.nlnl);

		sout += this.nlnlnl + "And for " + this.DH_2 + ":" + this.nlnl;

		sout += this.print_arrofarr(this.h2_cr).join(this.nlnl);

		sout += this.nlnlnl + "We now have all of the cyclic rotations sorted together for " +
				this.DH_1 + ":" + this.nlnl;

		sout += this.print_arrofarr(this.h1_scr).join(this.nlnl);

		sout += this.nlnlnl + "And for " + this.DH_2 + ":" + this.nlnl;

		sout += this.print_arrofarr(this.h2_scr).join(this.nlnl);

		sout += this.nlnlnl + "Which gives us the following positions ";
		sout += "and BWT:" + this.nlnl;

		sout += this.tab;
		if (this.give_out_HTML) {
			sout += '<thead><tr>';
			sout += "<th>For " + this.DH_1 + ":</th><th>" + this.tabchar + "</th><th>For " + this.DH_2 + ":</th>" + this.nl;
			sout += '</tr></thead><tbody><tr><td>';
		} else {
			sout += "{l l l}" + this.nl;
			sout += "For " + this.DH_1 + ": & & For " + this.DH_2 + ":" + this.tabnl;
		}

		sout += this.h1_pos.join(', ') + this.td+this.td + this.h2_pos.join(', ') + this.tabnl;
		sout += this.h1_bwt.join(', ') + this.td+this.td + this.h2_bwt.join(', ') + this.nl;
		sout += this.endtab;

		sout += "Again, what we want to generate from this is" + this.nlnl;

		sout += this.s_h_table;


		// round 1

		sout += '<span id="in-jump-1-3">To</span> ';
		sout += "do so, we basically need to sort the two sorted cyclic ";
		sout += "rotation lists into one big sorted cyclic rotation list. ";
		sout += "However, if we just do that naively, then we are doing just ";
		sout += "as much work as we would have by starting with the full " + this.DH + " in ";
		sout += "the beginning!" + this.nlnl;

		sout += "Instead, we can (and should!) take advantage of the fact that ";
		sout += "the two cyclic rotation lists of " + this.DH_1 + " and " + this.DH_2 + " have ";
		sout += "already been sorted." + this.nlnl;

		sout += "One method to achieve this sorting, according to Holt2014, is as follows:" + this.nlnl;

		sout += "We create an interleave vector which is " + this.origins[0] + " in each position ";
		sout += "in which we choose the next element from " + this.DH_1 + " and ";
		sout += "which is " + this.origins[1] + " in each position in which we choose the ";
		sout += "next element from " + this.DH_2;

		if ((this.origins[0] == '0') && (this.origins[1] == '1')) {
			sout += '. ';
		} else {
			sout += " (originally it's 0 and 1, but the ";
			sout += "difference is purely notational.) ";
		}

		sout += "The first interleave vector that we create here simply corresponds ";
		sout += "to fully writing out the information for " + this.DH_1 + ", followed by ";
		sout += "fully writing out the information for " + this.DH_2 + ":" + this.nlnl;

		sout += this.tab;
		if (this.give_out_HTML) {
			sout += '<tbody class="vbars"><tr><td>';
		} else {
			sout += "{" + this.repjoin(this.h1_pos.length, 'c', ' | ') + " | ";
			sout += this.repjoin(this.h2_pos.length, 'c', ' | ') + " | l}" + this.nl;
		}
		sout += this.h1_pos.join(this.td) + this.td + this.h2_pos.join(this.td) + this.td + "Position" + this.tabnl;
		sout += this.h1_bwt.join(this.td) + this.td + this.h2_bwt.join(this.td) + this.td + "BWT" + this.tabnl;
		sout += this.repjoin(this.h1_pos.length, this.origins[0], this.td) + this.td;
		sout += this.repjoin(this.h2_pos.length, this.origins[1], this.td) + this.td + "Interleave" + this.nl;
		sout += this.endtab;

		sout += "The method that we will be using for the next steps is ";
		sout += "to use the first column (sorted alphabetically) ";
		sout += "instead of focusing on the last column (the BWT):" + this.nlnl;

		var h1_col1 = this.add_index_to_col(this.get_first_n_from_scr(this.h1_scr, 0), this.origins[0]);
		var h2_col1 = this.add_index_to_col(this.get_first_n_from_scr(this.h2_scr, 0), this.origins[1]);
		var h12_cols = this.sort_indexed_col(h1_col1.concat(h2_col1));
		var h12_itlv = this.get_index_from_col(h12_cols);
		var itlv_changed =
			this.repjoin(this.h1_pos.length, this.origins[0], '') +
			this.repjoin(this.h2_pos.length, this.origins[1], '') !==
			h12_itlv.join('');

		var nth;
		if (this.give_out_HTML) {
			nth = '1<span class="u">st</span>';
		} else {
			nth = '$ 1^"st" $';
		}

		sout += this.tab;
		if (this.give_out_HTML) {
			sout += '<tbody class="vbars"><tr><td>';
		} else {
			sout += "{" + this.repjoin(h1_col1.length, 'c', ' | ') + " | ";
			sout += this.repjoin(h2_col1.length, 'c', ' | ') + " | l}" + this.nl;
		}
		sout += this.arr_to_str_wo_index(h1_col1, this.td) + this.td +
				this.arr_to_str_wo_index(h2_col1, this.td) + this.td + nth + ' column' + this.nl;
		sout += this.endtab;

		sout += "Then to append an index to each letter that tells us its origin:" + this.nlnl;

		sout += this.tab;
		if (this.give_out_HTML) {
			sout += '<tbody class="vbars"><tr><td>';
		} else {
			sout += "{" + this.repjoin(h1_col1.length, 'c', ' | ') + " | ";
			sout += this.repjoin(h2_col1.length, 'c', ' | ') + " | l}" + this.nl;
		}
		sout += this.arr_to_str_w_index(h1_col1, this.td) + this.td + this.arr_to_str_w_index(h2_col1, this.td) +
				this.td + nth + ' column with indices' + this.nl;
		sout += this.endtab;

		sout += "And to then sort the entire line alphabetically ";
		sout += "(and to keep track of the character boundaries we introduce extra vertical lines):" + this.nlnl;

		sout += this.tab;
		if (this.give_out_HTML) {
			sout += '<tbody class="vbars"><tr><td>';
			sout += this.get_tabline_from_col_HTML(this.arr_to_arr_w_index(h12_cols), h12_cols);
		} else {
			sout += "{" + this.get_tabline_from_col_DaTeX(h12_cols) + " | l}" + this.nl;
			sout += this.arr_to_str_w_index(h12_cols, this.td) + this.td;
		}
		sout += 'Sorted ' + nth + ' column' + this.nl;
		sout += this.endtab;

		sout += "To finally arrive at the interleave vector by just looking at the indices:" + this.nlnl;

		sout += this.tab;
		if (this.give_out_HTML) {
			sout += '<tbody class="vbars"><tr><td>';
			sout += this.get_tabline_from_col_HTML(this.arr_to_arr_w_index(h12_cols), h12_cols);
			sout += 'Sorted ' + nth + ' column' + this.tabnl;
			sout += this.get_tabline_from_col_HTML(h12_itlv, h12_cols) + "New Interleave" + this.nl;
		} else {
			sout += "{" + this.get_tabline_from_col_DaTeX(h12_cols) + " | l}" + this.nl;
			sout += this.arr_to_str_w_index(h12_cols, this.td) + this.td;
			sout += 'Sorted ' + nth + ' column' + this.tabnl;
			sout += h12_itlv.join(this.td) + this.td + "New Interleave" + this.nl;
		}
		sout += this.endtab;

		sout += "Using this interleave vector to resort the position and BWT that we had before, we get:" + this.nlnl;

		var h12_pos = this.merge_with_interleave(this.h1_pos, this.h2_pos, h12_itlv);
		var h12_bwt = this.merge_with_interleave(this.h1_bwt, this.h2_bwt, h12_itlv);

		sout += this.tab;
		if (this.give_out_HTML) {
			sout += '<tbody class="vbars"><tr><td>';
		} else {
			sout += "{" + this.repjoin(h1_col1.length, 'c', ' | ') + " | ";
			sout += this.repjoin(h2_col1.length, 'c', ' | ') + " | l}" + this.nl;
		}
		sout += this.h1_pos.join(this.td) + this.td + this.h2_pos.join(this.td) + this.td + "Old Position" + this.tabnl;
		sout += this.h1_bwt.join(this.td) + this.td + this.h2_bwt.join(this.td) + this.td + "Old BWT" + this.tabnl;
		sout += this.repjoin(this.h1_pos.length, this.origins[0], this.td) + this.td;
		sout += this.repjoin(this.h2_pos.length, this.origins[1], this.td) + this.td + "Old Interleave" + this.nl;
		sout += this.endtab;

		sout += this.tab;
		if (this.give_out_HTML) {
			sout += '<tbody class="vbars"><tr><td>';
			sout += this.get_tabline_from_col_HTML(h12_pos, h12_cols) + "New Position" + this.tabnl;
			sout += this.get_tabline_from_col_HTML(h12_bwt, h12_cols) + "New BWT" + this.tabnl;
			sout += this.get_tabline_from_col_HTML(h12_itlv, h12_cols) + "New Interleave" + this.nl;
		} else {
			sout += "{" + this.get_tabline_from_col_DaTeX(h12_cols) + " | l}" + this.nl;
			sout += h12_pos.join(this.td) + this.td + "New Position" + this.tabnl;
			sout += h12_bwt.join(this.td) + this.td + "New BWT" + this.tabnl;
			sout += h12_itlv.join(this.td) + this.td + "New Interleave" + this.nl;
		}
		sout += this.endtab;

		sout += "We have now achieved" + this.nlnl;

		sout += this.s_h_table_head;
		sout += h12_pos.join(this.td) + this.td + 'Position' + this.tabnl;
		sout += h12_bwt.join(this.td) + this.td + 'BWT' + this.nl;
		sout += this.endtab;

		var expanded = this.expand_pos(h12_pos, h12_bwt);
		if (expanded) {
			sout += "When expanding the merged BWT and its positions, we get" + this.nlnl;

			var h12_pos_ex = expanded[0];
			var h12_bwt_ex = expanded[1];

			sout += this.s_h_table_head;
			sout += h12_pos_ex.join(this.td) + this.td + 'Position' + this.tabnl;
			sout += h12_bwt_ex.join(this.td) + this.td + 'BWT' + this.nl;
			sout += this.endtab;
		}

		if (itlv_changed) {
			sout += "However, what we actually want is" + this.nlnl;
		} else {
			sout += "What we actually want is" + this.nlnl;
		}
		sout += this.s_h_table;

		sout += "In general, we can only be sure that we can stop when the interleave vector ";
		sout += "did not change between runs ";
		if (!itlv_changed) {
			sout += "(as it did now) ";
		}
		sout += "or when we see that each letter is in its own group, ";
		sout += "as then no further changes can occur." + this.nlnlnl;



		// rounds 2 and further

		var h12_itlv_new;
		var n = 1;

		while (itlv_changed) {

			n += 1;

			sout += "The previous interleave vector and the new interleave vector are not the same, ";
			sout += "so we need to carry on for another step ";
			sout += "by first selecting the next column." + this.nl;
			sout += "We previously had column " + (n-1) + " (in step " + (n-1) + "), ";
			sout += "so in this step we consider column " + n + ". ";
			sout += "That is, each entry from the new BWT is replaced with the entry from ";
			sout += "column " + n + " that belongs to that position in the BWT." + this.nl;

			sout += "We here keep track of the overall position, as the letter following the first “A” is ";
			sout += "not necessarily the same as the letter following the second “A”, etc." + this.nlnl;

			sout += "To do so, let's have a quick look at the sorted cyclic rotations:" + this.nlnl;

			sout += this.tab;
			if (this.give_out_HTML) {
				sout += '<thead><tr>';
				sout += "<th>For " + this.DH_1 + ":</th><th>" + this.tabchar + "</th><th>For " + this.DH_2 + ":</th>" + this.nl;
				sout += '</tr></thead><tbody><tr><td>';
			} else {
				sout += "{l l l}" + this.nl;
				sout += "For " + this.DH_1 + ": & & For " + this.DH_2 + ":" + this.tabnl;
			}

			var h1_cr_as = this.print_arrofarr(this.h1_cr);
			var h2_cr_as = this.print_arrofarr(this.h2_cr);
			var len1 = h1_cr_as.length;
			var len2 = h2_cr_as.length;
			var len = Math.max(len1, len2);
			for (var i = 0; i < len; i++) {
				if (i < len1) {
					sout += h1_cr_as[i];
				}
				sout += this.td+this.td;
				if (i < len2) {
					sout += h2_cr_as[i];
				}
				sout += this.tabnl;
			}
			sout += this.endtab;

			if (this.give_out_HTML) {
				nth = n + '<span class="u">th</span>';
				switch (n) {
					case 1:
						nth = '1<span class="u">st</span>';
						break;
					case 2:
						nth = '2<span class="u">nd</span>';
						break;
					case 3:
						nth = '3<span class="u">rd</span>';
						break;
				}
			} else {
				nth = n + '^"th"';
				switch (n) {
					case 1:
						nth = '1^"st"';
						break;
					case 2:
						nth = '2^"nd"';
						break;
					case 3:
						nth = '3^"rd"';
						break;
				}
				nth = "$ " + nth + " $";
			}

			sout += "We find the " + nth + " column always as the letter following the ";
			sout += "indicated BWT letter, that is, position ";
			sout += "(althewhile keeping track of the character groups that ";
			sout += "had been formed in the previous step, here represented with double lines):" + this.nlnl;

			var h12_col = this.add_indices_to_col(
								this.merge_with_interleave(
									this.get_first_n_from_scr(this.h1_scr, n - 1),
									this.get_first_n_from_scr(this.h2_scr, n - 1),
									h12_itlv),
								h12_itlv,
								h12_cols);

			sout += this.tab;
			if (this.give_out_HTML) {
				sout += '<tbody class="vbars"><tr><td>';
				sout += this.get_tabline_from_col_HTML(h12_bwt, h12_cols) + "BWT" + this.tabnl;
				sout += this.get_tabline_from_col_HTML(h12_pos, h12_cols) + "Position" + this.tabnl;
				sout += this.get_tabline_from_col_HTML(h12_itlv, h12_cols) + "Interleave" + this.tabnl;
				sout += this.get_tabline_from_col_HTML(this.get_first_n_from_scr(h12_col, 0), h12_cols);
				sout += nth + " column " + this.nl;
			} else {
				sout += "{" + this.get_tabline_from_col_DaTeX(h12_cols) + " | l}" + this.nl;
				sout += h12_bwt.join(this.td) + this.td + "BWT" + this.tabnl;
				sout += h12_pos.join(this.td) + this.td + "Position" + this.tabnl;
				sout += h12_itlv.join(this.td) + this.td + "Interleave" + this.tabnl;
				sout += this.get_first_n_from_scr(h12_col, 0).join(this.td) + this.td;
				sout += nth + " column " + this.nl;
			}
			sout += this.endtab;

			sout += "We now add the old interleave vector as indices:" + this.nlnl;

			sout += this.tab;
			if (this.give_out_HTML) {
				sout += '<tbody class="vbars"><tr><td>';
				sout += this.get_tabline_from_col_HTML(h12_itlv, h12_cols) + 'Old Interleave' + this.tabnl;
				sout += this.get_tabline_from_col_HTML(this.arr_to_arr_w_index(h12_col), h12_cols);
				sout += nth + " column" + this.nl;
			} else {
				sout += "{" + this.get_tabline_from_col_DaTeX(h12_cols) + " | l}" + this.nl;
				sout += h12_itlv.join(this.td) + this.td + 'Old Interleave' + this.tabnl;
				sout += this.arr_to_str_w_index(h12_col, this.td) + this.td + nth + " column" + this.nl;
			}
			sout += this.endtab;

			sout += "We now sort alphabetically WITHIN the character groups from the previous step:" + this.nlnl;

			h12_cols = this.sort_indexed_col(h12_col);
			h12_itlv_new = this.get_index_from_col(h12_cols);
			itlv_changed = this.did_itlvs_change(h12_itlv, h12_itlv_new);
			h12_itlv = h12_itlv_new;
			h12_pos = this.merge_with_interleave(this.h1_pos, this.h2_pos, h12_itlv);
			h12_bwt = this.merge_with_interleave(this.h1_bwt, this.h2_bwt, h12_itlv);

			sout += this.tab;
			if (this.give_out_HTML) {
				sout += '<tbody class="vbars"><tr><td>';
				sout += this.get_tabline_from_col_HTML(this.arr_to_arr_w_index(h12_cols), h12_cols);
			} else {
				sout += "{" + this.get_tabline_from_col_DaTeX(h12_cols) + " | l}" + this.nl;
				sout += this.arr_to_str_w_index(h12_cols, this.td) + this.td;
			}
			sout += "Sorted " + nth + " column" + this.nl;
			sout += this.endtab;

			sout += "We arrive at the following new interleave vector, ";
			if (itlv_changed) {
				sout += "which is different from the old one - meaning that this was ";
				sout += "not the last step:" + this.nlnl;
			} else {
				sout += "which is the same as the old one - meaning that this was ";
				sout += "the last step:" + this.nlnl;
			}

			sout += this.tab;
			if (this.give_out_HTML) {
				sout += '<tbody class="vbars"><tr><td>';
				sout += this.get_tabline_from_col_HTML(this.arr_to_arr_w_index(h12_cols), h12_cols);
				sout += "Sorted " + nth + " column" + this.tabnl;
				sout += this.get_tabline_from_col_HTML(h12_itlv, h12_cols) + 'New Interleave' + this.nl;
			} else {
				sout += "{" + this.get_tabline_from_col_DaTeX(h12_cols) + " | l}" + this.nl;
				sout += this.arr_to_str_w_index(h12_cols, this.td) + this.td + "Sorted " + nth + " column" + this.tabnl;
				sout += h12_itlv.join(this.td) + this.td + 'New Interleave' + this.nl;
			}
			sout += this.endtab;

			sout += "We can now look at the BWT and the positions according to this interleave vector:" + this.nlnl;

			sout += this.tab;
			if (this.give_out_HTML) {
				sout += '<tbody class="vbars"><tr><td>';
				sout += this.get_tabline_from_col_HTML(h12_pos, h12_cols) + "New Position" + this.tabnl;
				sout += this.get_tabline_from_col_HTML(h12_bwt, h12_cols) + "New BWT" + this.tabnl;
				sout += this.get_tabline_from_col_HTML(h12_itlv, h12_cols) + 'New Interleave' + this.nl;
			} else {
				sout += "{" + this.get_tabline_from_col_DaTeX(h12_cols) + " | l}" + this.nl;
				sout += h12_pos.join(this.td) + this.td + "New Position" + this.tabnl;
				sout += h12_bwt.join(this.td) + this.td + "New BWT" + this.tabnl;
				sout += h12_itlv.join(this.td) + this.td + 'New Interleave' + this.nl;
			}
			sout += this.endtab;

			sout += "We have now achieved" + this.nlnl;

			sout += this.s_h_table_head;
			sout += h12_pos.join(this.td) + this.td + 'Position' + this.tabnl;
			sout += h12_bwt.join(this.td) + this.td + 'BWT' + this.nl;
			sout += this.endtab;

			expanded = this.expand_pos(h12_pos, h12_bwt);
			if (expanded) {
				sout += "When expanding the merged BWT and its positions, we get" + this.nlnl;

				h12_pos_ex = expanded[0];
				h12_bwt_ex = expanded[1];

				sout += this.s_h_table_head;
				sout += h12_pos_ex.join(this.td) + this.td + 'Position' + this.tabnl;
				sout += h12_bwt_ex.join(this.td) + this.td + 'BWT' + this.nl;
				sout += this.endtab;
			}

			if (itlv_changed) {
				sout += "However, what we actually want is" + this.nlnl;
			} else {
				sout += "What we actually want is" + this.nlnl;
			}
			sout += this.s_h_table;
		}

		if (expanded) {
			h12_pos = h12_pos_ex;
		}

		if (this.pos_equals_pos(h12_pos, this.h_pos)) {
			sout += "We can see that we found exactly what we wanted to achieve, ";
			sout += "and therefore we are happy.";
		} else {
			sout += "So if all worked out right we should have found the same ";
			sout += "as what we wanted to achieve." + this.nl;
			sout += "But they are not actually the same which makes us sad.";
		}



		sout += this.s_end_document;

		return sout;
	},



	// takes in an array of letters, an offset integer and an annotational
	//   letter
	// gives out an array containing each cyclic rotation of the array
	//   (with the last column containing the position plus offset in an
	//   array together with the annotational letter,
	//   the second-to-last column containing whitespace,
	//   and the third-to-last column containing the BWT)
	create_cyclic_rotations: function(ha, offset, offletter) {

		var aout = [];

		for (var i = 0; i < ha.length; i++) {

			// take the part of ha including and after i,
			// append the part before i,
			// append whitespace,
			// append i as position
			aout.push(ha.slice(i).concat(ha.slice(0, i)).concat([this.tabchar, [i+offset, offletter]]));
		}

		return aout;
	},



	// takes in an array of cyclic rotations
	// gives out the sorted cyclic rotations as array
	sort_cyclic_rotations: function(h_cr) {

		var aout = [];

		for (var i = 0; i < h_cr.length; i++) {
			aout.push(h_cr[i]);
		}

		aout = aout.sort();

		return aout;
	},



	// takes in an array of arrays
	// gives out an array that contains strings which are the merged arrays in the original array
	print_arrofarr: function(haa) {

		var aout = [];

		for (var i = 0; i < haa.length; i++) {
			var len = haa[i].length - 1;
			var oldpos = haa[i][len];
			haa[i][len] = this.pos_to_str(haa[i][len]);
			
			aout.push(haa[i].join(''));

			haa[i][len] = oldpos;
		}

		return aout;
	},



	// takes in the length of an array arr, a replacement letter and a delimiter
	// gives out arr.join(delimiter) after replacing every element of arr with the replacement letter
	repjoin: function(len, letter, delimiter) {

		len--;

		if (len < 0) {
			return '';
		}

		var sout = letter;

		for (var i = 0; i < len; i++) {
			sout += delimiter + letter;
		}

		return sout;
	},



	// takes in the length of an array arr and a replacement letter
	// gives out an array of the given length with each element being the letter
	reparr: function(len, letter) {

		var aout = [];

		for (var i = 0; i < len; i++) {
			aout.push(letter);
		}

		return aout;
	},



	// takes in a length and a string
	// gives out that same string repeated as often as the length indicates
	repeatstr: function(len, str) {

		var sout = '';

		for (var i=0; i < len; i++) {
			sout += str;
		}

		return sout;
	},



	// takes in an array that contains just a column and an index character
	// gives out an array that contains the original column in each zeroeth element,
	//   the index character in each first element and an integer denoting how many
	//   vertical bars follow that character (set to zero) in each second element
	add_index_to_col: function(h_col, index) {

		var aout = [];

		for (var i = 0; i < h_col.length; i++) {
			aout.push([h_col[i], index, 0]);
		}

		return aout;
	},



	// takes in an array that contains just a column, an array containing index
	//   characters of the same length and an array containing a column with vertical bars
	// gives out an array that contains the original column in each zeroeth element,
	//   the index characters in each first element and an integer denoting how many
	//   vertical bars follow that character (set to the value of the third input) in
	//   each second element
	add_indices_to_col: function(h_col, indices, h_cols) {

		var aout = [];

		for (var i = 0; i < h_col.length; i++) {
			aout.push([h_col[i], indices[i], h_cols[i][2]]);
		}

		return aout;
	},



	// takes in an array of arrays, a position integer and an extra highlight array
	// gives out a string representing each first element joined on the standard
	//   delimiter with each element being highlighted that has a truthy value in
	//   hl_pos, and with each entry of extra_hl_arr being extra highlighted
	arr_to_highlighted_str: function(arr, hl_pos, extra_hl_arr) {

		var sout = '';
		var delimiter;

		// also allow this to be undefined
		if (!extra_hl_arr) {
			extra_hl_arr = [];
		}

		if (this.give_out_HTML) {
			delimiter = '</td><td>';
			for (var i=0; i < arr.length; i++) {
				if (extra_hl_arr.indexOf(i) >= 0) {
					sout += '</td><td class="x">' + arr[i][0];
				} else {
					if (arr[i][hl_pos]) {
						sout += '</td><td class="h">' + arr[i][0];
					} else {
						sout += delimiter + arr[i][0];
					}
				}
			}
			// take out the first delimiter end
			sout = sout.slice(5);
		} else {
			delimiter = ' & ';
			for (var i=0; i < arr.length; i++) {
				sout += delimiter + arr[i][0];
			}
			// take out the first delimiter
			sout = sout.slice(delimiter.length);
		}

		return sout;
	},



	// takes in an array and an extra highlight array
	// gives out a string representing each element joined on the standard
	//   delimiter with each entry of hl_arr being highlighted and each
	//   entry of extra_hl_arr being extra highlighted
	arr_to_extra_high_str: function(arr, hl_arr, extra_hl_arr) {

		var sout = '';
		var delimiter;

		// also allow this to be undefined
		if (!hl_arr) {
			hl_arr = [];
		}
		if (!extra_hl_arr) {
			extra_hl_arr = [];
		}

		if (this.give_out_HTML) {
			delimiter = '</td><td>';
			for (var i=0; i < arr.length; i++) {
				if (extra_hl_arr.indexOf(i) >= 0) {
					sout += '</td><td class="h">' + arr[i];
				} else {
					if (hl_arr.indexOf(i) >= 0) {
						sout += '</td><td class="x">' + arr[i];
					} else {
						sout += delimiter + arr[i];
					}
				}
			}
			// take out the first delimiter end
			sout = sout.slice(5);
		} else {
			delimiter = ' & ';
			for (var i=0; i < arr.length; i++) {
				sout += delimiter + arr[i];
			}
			// take out the first delimiter
			sout = sout.slice(delimiter.length);
		}

		return sout;
	},



	// takes in an array containing a column with indexes
	// gives out a string representing the column joined on the delimiter without showing the indexes
	arr_to_str_wo_index: function(h_col, delimiter) {

		return this.get_first_n_from_scr(h_col, 0).join(delimiter);
	},



	// takes in an array containing a column with indexes
	// gives out a string representing the column joined on the delimiter while showing the indexes
	arr_to_str_w_index: function(h_col, delimiter) {

		return this.arr_to_arr_w_index(h_col).join(delimiter);
	},



	// takes in an array containing a column with indexes
	// gives out an array representing the column while showing the indexes
	arr_to_arr_w_index: function(h_col) {

		var aout = [];
		var len = h_col.length;

		if (this.give_out_HTML) {
			for (var i = 0; i < len; i++) {
				aout.push(h_col[i][0] + '<span class="d">' + h_col[i][1] + '</span>');
			}
		} else {
			for (var i = 0; i < len; i++) {
				if (h_col[i][0][0] == '$') {
					aout.push('$ \\$_' + h_col[i][1] + ' $');
				} else {
					aout.push('$ "' + h_col[i][0] + '"_' + h_col[i][1] + ' $');
				}
			}
		}

		return aout;
	},



	// takes in a column array with indices
	// gives out the sorted column array (sorted mainly by letters, ties broken by indices)
	sort_indexed_col: function(h_col) {

		var colout = [[]];
		var colout_cur = 0;
		var orig_vbars = [];

		// store the original vertical bar count
		for (var i = 0; i < h_col.length; i++) {
			orig_vbars.push(h_col[i][2]);
		}

		// create array of arrays, with each internal array being a group
		for (var i = 0; i < h_col.length; i++) {
			colout[colout_cur].push(h_col[i]);
			if (h_col[i][2] > 0) {
				colout.push([]);
				colout_cur += 1;
			}
		}

		// do the sorting within each group
		for (var i = 0; i < colout.length; i++) {

			// do the sorting
			colout[i].sort(function(a, b) {

				if (a[0] == b[0]) {
					// break ties by indices
					if (a[1] == b[1]) {
						return 0;
					} else if(a[1] > b[1]) {
						return 1;
					}
					return -1;
				}

				// sort by letters
				if (a[0] > b[0]) {
					return 1;
				}
				return -1;
			});
		}

		// merge all the groups
		var cout = [];

		for (var i = 0; i < colout.length; i++) {
			cout = cout.concat(colout[i]);
		}

		// restore the original vertical bar count
		for (var i = 0; i < cout.length; i++) {
			cout[i][2] = orig_vbars[i];
		}

		// update the vertical bar count
		for (var i = 0; i < cout.length-1; i++) {
			if ((cout[i][0] !== cout[i+1][0]) || (cout[i][2] > 0)) {
				cout[i][2] += 1;
			}
		}

		return cout;
	},



	// takes in a column array
	// gives out an array containing just the indices
	get_index_from_col: function(h_col) {

		return this.get_first_n_from_scr(h_col, 1);
	},



	// takes in a column array
	// gives out the appropriate tabline for DaTeX
	get_tabline_from_col_DaTeX: function(h_col) {

		var len = h_col.length;

		if (len < 1) {
			return '';
		}

		var sout = 'c ';

		for (var i = 0; i < len-1; i++) {
			for (var j = 0; j < h_col[i][2]; j++) {
				sout += '|';
			}
			sout += '|';
			sout += ' c ';
		}

		return sout;
	},



	// takes in any array and a column array
	// gives out the array interwoven with the tds necessary to produce
	//   the correct about of vertical bars for HTML
	get_tabline_from_col_HTML: function(arr, h_col) {

		var len = h_col.length;

		if (len < 1) {
			return '';
		}

		var sout = arr[0];

		for (var i = 1; i < len; i++) {
			for (var j = 0; j < h_col[i-1][2]; j++) {
				sout += '</td><td class="b">';
			}
			sout += '</td><td>';
			sout += arr[i];
		}

		return sout + '</td><td>';
	},



	// takes in two arrays h1 and h2 and an interleave vector
	// gives out the merged arrays as one array according to the interleave vector
	merge_with_interleave: function(h1, h2, h12_itlv) {

		var aout = [];
		var i1 = 0;
		var i2 = 0;

		for (var i = 0; i < h12_itlv.length; i++) {
			if (h12_itlv[i] == this.origins[0]) {
				aout.push(h1[i1]);
				i1++;
			} else {
				aout.push(h2[i2]);
				i2++;
			}
		}

		return aout;
	},



	// takes in a sorted cyclic rotation array and an integer n
	// gives out an array containing the elements of the nth column
	get_first_n_from_scr: function(h_scr, n) {

		var aout = [];

		for (var i = 0; i < h_scr.length; i++) {
			aout.push(h_scr[i][n]);
		}

		return aout;
	},



	// takes in a sorted cyclic rotation array and an integer n
	// gives out an array containing the elements of the last - nth column
	get_last_n_from_scr: function(h_scr, n) {

		var aout = [];

		n++;

		for (var i = 0; i < h_scr.length; i++) {
			aout.push(h_scr[i][h_scr[i].length - n]);
		}

		return aout;
	},



	// takes in a position array (containing an integer and an identifier)
	// gives out a string that is the integer with the identifier as index
	//   in DaTeX
	pos_to_str: function(pos) {

		if (pos[1] === '') {
			// instead of `$ "1"_"" $`, just use `1`
			return pos[0];
		}

		if (this.give_out_HTML) {
			// generate `1<span class="d">2</span>`
			return pos[0] + '<span class="d">' + pos[1] + '</span>';
		} else {
			// generate `$ "1"_"2" $`
			return '$ "' + pos[0] + '"_"' + pos[1] + '" $';
		}
	},



	// takes in a sorted cyclic rotation array
	// gives out an array containing the positions (the last column), in the format
	//   $ "1"_"2" $ where 1 is the first element of the last column and 2 is the second
	get_pos_from_scr: function(h_scr) {

		var aout = this.get_last_n_from_scr(h_scr, 0);

		for (var i = 0; i < aout.length; i++) {
			aout[i] = this.pos_to_str(aout[i]);
		}

		return aout;
	},



	// takes in a sorted cyclic rotation array
	// gives out an array containing the BWT (the third-last column)
	get_bwt_from_scr: function(h_scr) {

		return this.get_last_n_from_scr(h_scr, 2);
	},



	// takes in two interleave vectors
	// gives out true if they are different (so if they changed),
	//   and false if they are the same
	did_itlvs_change: function(itlv1, itlv2) {

		for (var i = 0; i < itlv1.length; i++) {
			if (itlv1[i] !== itlv2[i]) {
				return true;
			}
		}

		return false;
	},



	// takes in a pos array that contains both graph-y and flat entries,
	//   e.g. ['$ "9"_"A" $', '$ "9"_"C" $', 10], and a bwt array
	// gives out the expanded pos and bwt arrays, in which every flat entry
	//   has been replaced by several graph-y entries,
	//   e.g. ['$ "9"_"A" $', '$ "9"_"C" $', '$ "10"_"A" $', '$ "10"_"C" $'],
	//   OR gives back false in case of no expanding happening at all!
	expand_pos: function(pos, bwt) {

		// collect all of the alternatives on which we want to expand
		var expand_on = [];

		if (this.give_out_HTML) {
			for (var i = 0; i < pos.length; i++) {
				if (pos[i][pos[i].length-1] == '>') {
					var ind = pos[i].slice(pos[i].indexOf('<span class="d">')+16, -7);
					if (expand_on.indexOf(ind) < 0) {
						expand_on.push(ind);
					}
				}
			}
		} else {
			for (var i = 0; i < pos.length; i++) {
				if (pos[i][0] == '$') {
					var ind = pos[i].slice(pos[i].indexOf('_')+2, -3);
					if (expand_on.indexOf(ind) < 0) {
						expand_on.push(ind);
					}
				}
			}
		}

		// The array expand_on contains nothing?
		// Well, that means that we have nothing to do here, as the input is
		// entirely flat...
		if (expand_on.length < 1) {
			return false;
		}

		// in a second pass, actually expand the pos array into a new one
		var opos = [];
		var obwt = [];

		if (this.give_out_HTML) {
			for (var i = 0; i < pos.length; i++) {
				if (pos[i][pos[i].length-1] == '>') {
					opos.push(pos[i]);
					obwt.push(bwt[i]);
				} else {
					for (var j = 0; j < expand_on.length; j++) {
						opos.push(pos[i] + '<span class="d">' + expand_on[j] + '</span>');
						obwt.push(bwt[i]);
					}
				}
			}
		} else {
			for (var i = 0; i < pos.length; i++) {
				if (pos[i][0] == '$') {
					opos.push(pos[i]);
					obwt.push(bwt[i]);
				} else {
					for (var j = 0; j < expand_on.length; j++) {
						opos.push('$ "' + pos[i] + '"_"' + expand_on[j] + '" $');
						obwt.push(bwt[i]);
					}
				}
			}
		}

		return [opos, obwt];
	},



	// takes in two pos arrays
	// gives out true if they have the same contents and false if not
	pos_equals_pos: function(pos1, pos2) {

		var len = pos1.length;

		if (len != pos2.length) {
			return false;
		}

		for (var i = 0; i < len; i++) {

			if (pos1[i] != pos2[i]) {
				return false;
			}
		}

		return true;
	},



	// takes in two arrays, one containing keys and the other one
	//   containing values for the keys (e.g. ['A', 'B'] and ['A'=>0, 'B'=>2]),
	//   and the boolean parameter use_ao (optional, default: false),
	//   which if true leads to the array offset being added to all values
	// gives out a php-like string representation of the values
	printKeyValArr: function(keys, values, use_ao) {

		var sout = '[';

		for (var i=0; i < keys.length; i++) {
			sout += keys[i] + ' => ';
			if (use_ao) {
				sout += (values[keys[i]] + GML.ao);
			} else {
				sout += values[keys[i]];
			}
			sout += ', ';
		}

		sout = sout.slice(0, -2);

		sout += ']';

		return sout;
	},



	// takes in an integer i
	// gives out an array [0, 1, 2, ..., i-1], plus the general array offset in every element
	count_up_array: function(i) {
		aout = [];
		var ao = this.ao;
		for (var j=0; j < i; j++) {
			aout.push(j + ao);
		}
		return aout;
	},



	// takes in an HTML text
	// gives out the same HTML text, but internal representations replaced with
	//   actual visual representations
	makeVisualsNice: function(sout) {

		// replace the internal representation of '#_1' with the actual visual representation
		while (sout.indexOf(GML.DK_1) > -1) {
			sout = sout.replace(GML.DK_1, GML.DK_1_o);
		}

		// replace the internal representation of '$_1' with the actual visual representation
		while (sout.indexOf(GML.DS_1) > -1) {
			sout = sout.replace(GML.DS_1, GML.DS_1_o);
		}

		// replace '^' with '#' before printout
		sout = sout.replace(/\^/g, '#');

		return sout;
	},


	// takes in two prefix strings p1 and p2
	// gives out 1 if p1 > p2, 0 if p1 == p2, -1 if p1 < p2
	//   keeps track of taking out $_0#_0 before doing comparisons!
	comparePrefixes: function(p1, p2) {

		var S0K0 = this.DS_1 + this.DK_1;

		var p1i = p1.indexOf(S0K0);

		if (p1i >= 0) {
			p1 = p1.slice(0, p1i) + p1.slice(p1i+2);
		}

		var p2i = p2.indexOf(S0K0);

		if (p2i >= 0) {
			p2 = p2.slice(0, p2i) + p2.slice(p2i+2);
		}

		if (p1 > p2) {
			return 1;
		}
		if (p1 === p2) {
			return 0;
		}
		return -1;
	},



	// takes in an array of prefixes
	// gives out the same array of prefixes, but with each prefix being cut as short as acceptable
	prunePrefixes: function(prefix_arr) {

		for (var i=0; i < prefix_arr.length; i++) {

			var keepuntil = -1;

			if (i > 0) {
				var len = Math.min(prefix_arr[i].length, prefix_arr[i-1].length);

				for (var k=0; k < len; k++) {
					if (prefix_arr[i][k] !== prefix_arr[i-1][k]) {
						keepuntil = k;
						break;
					}
				}
			}


			if (i < prefix_arr.length - 1) {
				var len = Math.min(prefix_arr[i].length, prefix_arr[i+1].length);

				for (var k=0; k < len; k++) {
					if (prefix_arr[i][k] !== prefix_arr[i+1][k]) {
						if (keepuntil < k) {
							keepuntil = k;
							break;
						}
					}
				}
			}

			if (keepuntil >= 0) {
				prefix_arr[i] = prefix_arr[i].slice(0, keepuntil+1);
			}
		}

		return prefix_arr;
	},



	// takes in a string and two positions
	// gives out the same string, but with the characters in the two positions exchanged
	exchangeInString: function(str, pos1, pos2) {

		var char1 = str[pos1];
		var char2 = str[pos2];

		str = str.slice(0, pos1) + char2 + str.slice(pos1+1);
		str = str.slice(0, pos2) + char1 + str.slice(pos2+1);

		return str;
	},



	// takes in a string, an integer position and a character
	// gives out the same string, with with the character in position pos replaced by char
	setInString: function(str, pos, chr) {
		return str.slice(0, pos) + chr + str.slice(pos+1);
	},



	// takes nothing in
	// gives out an example run
	example: function() {
		return this.merge_BWTs_naively('A(A|C)CA', 'CAAA');
	},

};



GML.set_to_HTML();
