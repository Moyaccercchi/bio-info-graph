/*
	*****************************************************************************
		GML XBW - Graph Merging Library - Extended Burrows-Wheeler Sublibrary
	*****************************************************************************

	Note: Functions within the XBW Environment that are starting with an underscore
		  are used internally. They are not part of the interface to the outside
		  world and should (usually) not need to be called from the outside.

	On indexing methods: We starting writing this source code without thinking about
						 indexing methods much; after some time, we came up with the
						 difference between node indexing (as in node tables) and
						 table indexing (as in the actual flat XBW table we are
						 looking at).
						 However, in the thesis we actually more properly define
						 absolute indexing, FC-indexing and BWT-indexing.
						 To understand older comments in the node i / table i system,
						 it can be helpful to think about node indexing as absolute
						 indexing, while table indexing can refer to both FC-indexing
						 and BWT-indexing.
*/

// use as:
// var xbw = GML.make_xbw_environment();
// xbw.initFromFindex(findex);
GML.make_xbw_environment = function() {

	// navigation through XBW

	// the BWT (as string, so ['A', 'A|C', 'C'] is here 'AACC')
	var BWT = '';

	// the M bit-vector (as string)
	var M = '';

	// the F bit-vector (as string)
	var F = '';

	// the C array, containing the amount of characters before each key in the first column
	var C = [];

	// the first column / sorted BWT (as string)
	// can be used to convert an index into the table to a character:
	// c = char[i]
	var char = '';

	// the alphabet (set of characters) we are using;
	// can be used to convert an index into the alphabet to a character:
	// c = alph[i]
	var alph = [];

	// the positions of the characters within the alphabet;
	// can be used to convert a character to an index:
	// i = ord[c]
	var ord = [];

	// 1 .. this is a regular XBW environment (can be used as sub XBW)
	// 2 .. this is a host XBW environment for merging several sub XBWs
	// 3 .. this is a host XBW environment for fusing several sub XBWs
	var role = 1;

	// array of XBWs stored within this XBW (that is, an XBW can consist of several
	// XBWs inside, and when functions are called from the outside, then they can
	// be related through to the inside)
	var subXBWs = [];

	// if this here is a sub XBW, then prevXBW points to the XBW before this one and next XBW points
	// to the one after this one (for now, we will assume that all merged / fused XBWs are orderly,
	// one after the other in here, as this is how it will most likely be in practice anyway)
	var prevXBW;
	var nextXBW;

	var multi_cur_fic = []; // first column and M
	var multi_cur_bwt = []; // BWT and F
	var multi_cur_fic_int = []; // like multi_cur_fic, but including the aftersort
	var multi_cur_bwt_int = []; // like multi_cur_bwt, but including the aftersort

	// keep track from where the last node came to highlight it visually
	var last_high_fic = []; // the first two contain the last high node
	var last_high_bwt = []; // for each subXBW
	var last_high_fic_arr = []; // the second two contain all characters
	var last_high_bwt_arr = []; // leading up to the very latest high node

	var aftersort = [];



	// the following entries are not explicitly used in the XBW environment,
	// but are here to help us illustrate the interior processes to the user

	// the automaton
	var auto = [];

	// prefixes of the automaton
	var prefixes = [];


	// assumes that BWT, M and F are set
	// calculates FiC (as "char"), the C and ord arrays, and the alphabet alph
	function recalculate(updateChar) {

		/*
		C[0, s°+1] is an array such that C[c] is the total number of characters
		in the BWT that are contained in the set {$, 1, 2, ..., c-1},
		with C[0] = C[$] = 0, C[1] = 1, C[2] = 1 + amount of character 1, ... and C[s°+1] = n
		*/

		var characters = [];

		var i = BWT.length;
		while (i--) {
			characters.push(BWT[i]);
		}

		characters.sort();

		if (updateChar) {
			char = characters.join('');
		}

		C = [];
		alph = [];
		ord = [];
		var lastChar = '';
		var add = 0;
		var prev = 0;

		for (i=0; i < characters.length; i++) {
			if (lastChar !== characters[i]) {
				lastChar = characters[i];
				ord[lastChar] = alph.length;
				alph.push(lastChar);
				C[lastChar] = prev + add;
				prev += add;
				add = 0;
			}
			add++;
		}
	}

	/*
	rank(BWT[i], BWT, i) is the lexicographic rank of suffix T[SA[i], n] among suffixes
	preceded by character BWT[i]

	rank in python:
		# only used by advanced BWT (see Siren2014, but with or without pigeonhole algorithm)
		# rank(c, BWT, i) is the number of occurrences of character c in prefix BWT[1, i]
		def rank(c, arr, i):
			return arr.count(c, 0, i)
	*/
	function rank(c, arr, i) {

		if (role === 3) {
			return [subXBWs[c[1]]._rank(c[0], arr, i), c[1]];
		}

		switch (arr) {
			case 'M':
				arr = M;
				break;
			case 'F':
				arr = F;
				break;
			case 'BWT':
				arr = BWT;
				break;
			case 'FC':
			case 'FIC':
			case 'CHAR':
				arr = char;
				break;
		}

		// offset adjustment
		i++;

		var counter = 0;

		for (var j=0; j < i; j++) {
			if (arr[j] === c) {
				counter++;
			}
		}

		// offset adjustment
		counter--;

		return counter;
	}

	/*
	select in python:
		# select(c, BWT, j) is the position in BWT at which the character c occurs for the jth time
		def select(c, arr, j):
			i = 0
			k = 0
			len_arr = len(arr)
			while (k < j) and (i < len_arr):
				if (arr[i] == c):
					k += 1
				i += 1
			return i
	*/
	function select(c, arr, j) {

		if (role === 3) {
			return [subXBWs[c[1]]._select(c[0], arr, j), c[1]];
		}

		switch (arr) {
			case 'M':
				arr = M;
				break;
			case 'F':
				arr = F;
				break;
			case 'BWT':
				arr = BWT;
				break;
			case 'FC':
			case 'FIC':
			case 'CHAR':
				arr = char;
				break;
		}

		// offset adjustment
		j++;

		var i = 0;
		var k = 0;
		var len_arr = arr.length;

		while ((k < j) && (i < len_arr)) {
			if (arr[i] === c) {
				k++;
			}
			i++;
		}

		// adjustment (if some j left over, add it here, so that we can call select(+1)-1)
		// without this adjustment, e.g. the following would fail:
		// find('G$') on 'GACGTACCTG|,2,T,4;,3,,5;,6,,10;,6,,8'
		if (k < j) {
			i += j - k - 1;
		}

		// offset adjustment
		i--;

		return i;
	}

	// spep is [sp, ep]
	function lf(spep, c, do_select, do_rank) {

		if (role === 3) {
			// we are the host environment

			var j = spep[1];
			spep = spep[0];

			spep = subXBWs[j]._lf(spep, c, do_select, do_rank);

			/*
			if spep is # (considering which indexing method we use!) {
				j--
				set spep to $
			}
			*/

			if ((c === GML.DK) && (spep.length > 0)) {
				j--;
				// just [0, 0], even if we are not in absolute indexing, as there is only one edge
				// into the end node $ in the previous table
				spep = [0, 0];
			}

			return [spep, j];
		}

		var sp = spep[0];
		var ep = spep[1];

		if (do_select) {
			// convert absolute indexing to BWT-indexing
			sp = select('1', F, sp);
			ep = select('1', F, ep+1) - 1;
		}

		// perform last-to-first mapping while jumping from BWT-indexing to FC-indexing
		sp = C[c] + rank(c, BWT, sp-1) + 1;
		ep = C[c] + rank(c, BWT, ep);

		// if ep < sp, then we cannot return anything
		// (this here was to be done before the following
		// rank-rank, as that might bring ep = sp when
		// ep < sp before!)
		if (ep < sp) {
			return [];
		}

		if (do_rank) {
			// convert FC-indexing to absolute indexing
			sp = rank('1', M, sp);
			ep = rank('1', M, ep);
		}

		return [sp, ep];
	}

	// psi finds the succeeding nodes of a given node
	// i is usually in absolute indexing, and the output is usually in absolute indexing
	// take_edge_no is the index of the edge that we want to follow out of node i; so if node i has
	//   three outgoing edges, then the result for take_edge_no=0 is the node we reach when following
	//   the first edge, the result for take_edge_no=1 is the node we reach when following the second
	//   edge, and the result for take_edge_no=2 is the node we reach when following the third edge
	// if do_select is false, then the input is in FC indexing
	// if do_rank is false, then the output is in BWT indexing
	function psi(i, take_edge_no, do_select, do_rank) {

		if (role === 3) {
			// we are the host environment

			var j = i[1];
			i = i[0];

			i = subXBWs[j]._psi(i, take_edge_no, do_select, do_rank);

			/*
			if i is $ (considering which indexing method we use!) {
				j++
				set i to #
			}
			*/

			if (do_rank) {
				// we use absolute indexing
				if (i === 0) {
					j++;
					i = nextXBW._publishBWTlenWithF();
				}
			} else {
				// we use BWT-indexing
				// this rank operation is useless, as we require the start node
				// to have only one incoming edge anyway
				// if (rank('1', F, i) === 0) {
				if (i === 0) {
					j++;
					// just the last column, even if we are not in absolute indexing,
					// as there is only one edge out of the start node # in the next table
					i = nextXBW._publishBWTlen() - 1;
				}
			}

			return [i, j];
		}

		if (do_select) {
			// convert absolute indexing to FC-indexing
			i = select('1', M, i);
		}

		i += take_edge_no;

		var c = char[i];

		// get the next node within the table,
		// while jumping from FC-indexing to BWT-indexing
		i = select(c, BWT, i - C[c]);

		if (do_rank) {
			// convert BWT-indexing to absolute indexing
			i = rank('1', F, i);
		}

		return i;
	}

	function find(P) {

		var spep = [0, BWT.length - 1];
		var i = P.length;

		// just for visualization
		GML.vis_highlight_nodes = [];

		while (i--) {
			spep = lf(spep, P[i], true, i > 0);

			if ((spep.length < 1) || (spep[1] < spep[0])) {
				return [];
			}
		}

		// just for visualization
		var vis_int = [];
		for (var j=spep[0]; j < spep[1]+1; j++) {
			// why can we not just call this._publishPrefix? (are we calling this function
			// from contexts in which "this" would not point towards the current XBW environment?)
			GML.XBWs[GML_UI.cur_tab]._publishPrefix(j, false, P.length);
			for (var k=0; k < GML.vis_highlight_nodes.length; k++) {
				vis_int.push(GML.vis_highlight_nodes[k]);
			}
		}
		GML.vis_highlight_nodes = vis_int;

		return spep;
	}

	return {

		// basic initialization, called by all other init methods
		_init: function() {
			aftersort = [];
			subXBWs = [];
			role = 1;

			BWT = '';
			M = '';
			F = '';

			C = [];
			alph = [];
			ord = [];
		},


		// initialize the XBW as regular table with the data from the findex
		//   (the findex should be an array containing a BWT array in position 1,
		//   an M bit vector array in position 2, and an F bit vector array in position 3;
		//   the p12 / prefix array which could be in position 0 is ignored, as the
		//   prefixes are generated on the fly when the graph is generated)
		//   If findex is undefined, then the init function simply clears the XBW data.

		initFromFindex: function(findex) {

			this.initAsRegularTable();

			// create an automaton and prefixes that will be used later for visualization
			auto = GML.getAutomatonFromFindex(findex);

			// if something went wrong during merging (especially if our core assumptions
			// were not adhered to), then we cannot actually compute the prefixes, so we
			// need to check in advance, and if something goes wrong, then we need to abort
			GML.error_flag = false;
			GML.checkAutomatonIsValid(auto);
			if (GML.error_flag) {
				return;
			}

			// console.log('XBW computePrefixes');
			auto = GML.computePrefixes(auto);

			prefixes = [];
			for (var i=0; i < auto.length; i++) {
				if (auto[i]) {
					prefixes.push(auto[i].f);
				} else {
					prefixes.push('');
				}
			}
			prefixes.sort();


			var pBWT = findex[1];
			BWT = '';
			for (var i=0; i < pBWT.length; i++) {
				if (pBWT[i].length > 1) {
					var BWTarr = pBWT[i].split('|');
					for (var j=0; j < BWTarr.length; j++) {
						BWT += BWTarr[j];
					}
				} else {
					BWT += pBWT[i];
				}
			}

			M = findex[2].join('');

			F = findex[3].join('');


			var tableToP12 = [];
			var cur_ttop12 = -1;
			for (var i=0; i < M.length; i++) {
				if (M[i] == '1') {
					cur_ttop12++;
				}
				tableToP12.push(cur_ttop12);
			}
			GML.vis_tableToP12 = tableToP12;

			recalculate(true);
		},

		initFromFFXData: function(ffxdata) {

			this.initAsRegularTable();

			BWT = ffxdata.BWT;
			M = ffxdata.M;
			F = ffxdata.F;

			alph = GML.readKeys(ffxdata.ord, true);
			ord = GML.readKeyValArr(ffxdata.ord, true, true);
			C = GML.readKeyValArr(ffxdata.C, true, true);

			char = ''; // FC
			var k=-1;
			for (var j=0; j < alph.length; j++) {
				while (k < C[alph[j]]) {
					char += alph[j];
					k++;
				}
			}
		},

		initFromFFX: function(ffx) {

			this.initAsFuseHost();

			var cur_xbw;

			// append one comment line to the end, as we read out each datablock when
			// encountering a comment, and would otherwise not read out the last one
			ffx.push('>');

			for (var j=0; j < ffx.length; j++) {
				switch (ffx[j][0]) {
					case '>':
						if (cur_xbw && cur_xbw.BWT && cur_xbw.M && cur_xbw.F && cur_xbw.ord && cur_xbw.C) {
							var new_xbw = GML.make_xbw_environment();
							new_xbw.initFromFFXData(cur_xbw);
							this.addSubXBW(new_xbw);
						}
						cur_xbw = {};
						break;
					case 'B':
						cur_xbw.BWT = ffx[j].slice(2);
						break;
					case 'M':
						cur_xbw.M = ffx[j].slice(2);
						break;
					case 'F':
						cur_xbw.F = ffx[j].slice(2);
						break;
					case 'O':
						cur_xbw.ord = ffx[j].slice(2);
						break;
					case 'C':
						cur_xbw.C = ffx[j].slice(2);
						break;
				}
			}
		},


		initAsRegularTable: function() {

			this._init();

		},

		initAsMergeHost: function() {

			this._init();

			multi_cur_fic = [];
			multi_cur_bwt = [];
			multi_cur_fic_int = [];
			multi_cur_bwt_int = [];

			role = 2;
		},

		initAsFuseHost: function() {

			this._init();

			multi_cur_fic = [];
			multi_cur_bwt = [];
			multi_cur_fic_int = [];
			multi_cur_bwt_int = [];

			role = 3;
		},



		addSubXBW: function(newXBW) {

			newXBW._setPrevXBW(undefined);
			newXBW._setNextXBW(undefined);

			if (subXBWs.length > 0) {
				subXBWs[subXBWs.length-1]._setNextXBW(newXBW);
				newXBW._setPrevXBW(subXBWs[subXBWs.length-1]);

				switch (role) {
					case 2:
						// if we are merging, then we replace $ with $_0 and # with #_0
						// - but if we are fusing, this is not necessary
						subXBWs[subXBWs.length-1]._replaceSpecialChar(GML.DS, GML.DS_1);
						newXBW._replaceSpecialChar(GML.DK, GML.DK_1);
						break;
					case 3:
						// if we are fusing, we check explicitly that the end node
						// of the current last table has only one incoming edge and
						// that the start node of the new table has only one outgoing edge

						if (subXBWs[subXBWs.length-1]._hasMoreThanOneEndNodeEdge()) {
							GML.error_flag = true;
							GML.error_text = 'Invalid input: A fused end node has more than one incoming edge.';
							GML.error_kind = 'note';
							break;
						}
						if (newXBW._hasMoreThanOneStartNodeEdge()) {
							GML.error_flag = true;
							GML.error_text = 'Invalid input: A fused start node has more than one outgoing edge.';
							GML.error_kind = 'note';
							break;
						}
				}
			}

			multi_cur_fic.push(0);
			multi_cur_bwt.push(0);
			multi_cur_fic_int.push(0);
			multi_cur_bwt_int.push(0);

			subXBWs.push(newXBW);
		},

		clearSubXBWs: function() {
			subXBWs = [];
		},

		_setPrevXBW: function(pXBW) {
			prevXBW = pXBW;
		},

		_setNextXBW: function(nXBW) {
			nextXBW = nXBW;
		},

		// allow the outside world to access the psi and lf functions,
		// such that the host structure in a fused graph can call them
		_psi: function(i, take_edge_no, do_select, do_rank) {
			return psi(i, take_edge_no, do_select, do_rank);
		},
		_lf: function(spep, c, do_select, do_rank) {
			return lf(spep, c, do_select, do_rank);
		},
		_rank: function(c, arr, i) {
			return rank(c, arr, i);
		},
		_select: function(c, arr, j) {
			return select(c, arr, j);
		},

		// returns true if this XBW's end node's indegree is above 1
		_hasMoreThanOneEndNodeEdge: function() {
			// the end node has label $, and its predecessors are found at the start of the BWT row,
			// so if the F row starts on 11..., then we have a BWT row of xy... with x being the
			// predecessor label of $, but y being unrelated,
			// so we have only one ingcoming edge into $, and so we return false;
			// but if the F row starts on 10..., then we have a BWT row of xx... with x being the
			// predecessor label of $,
			// so we have multiple incoming edges into $, and so we return true
			return F[1] === '0';
		},

		// returns true if this XBW's start node's outdegree is above 1
		_hasMoreThanOneStartNodeEdge: function() {
			// the start node has label #, and is found at the end of the FC row,
			// so if the M row ends on ...1, then we have an FC row of ..x# with x not being a #,
			// so we have only one outgoing edge from #, and so we return false;
			// but if the M row ends on ...1...0, then we have an FC row of ...#...#,
			// so we have multiple outgoing edges from #, and so we return true
			return M[M.length-1] === '0';
		},

		// returns true if the data stored within this XBW environment is graphless,
		// that is, sequential
		graphless_data: function() {

			return M.indexOf('0') < 0;
		},

		generate_aftersort_array: function() {

			// [TODO P3 FLAT]
			// We here replaced the original source code with a new, more robust version which
			// is sorting alllll the prefixes across alllll their length - this NEEDS to
			// be done in a less resource hungry way, or else none of this is actually useful!
			// The reason why we went with this instead of the code after the return is GGAAGG and GG

			var S0K0 = GML.DS_1 + GML.DK_1;

			var pos = -1;
			var prefixes = [];

			for (var i=0; i < BWT.length; i++) {
				// we jump over M[i] = 0, because the prefix locations are based on FiC, and M[i] = 0
				// means that this is just another out-edge of the same FiC-node
				while (M[i] === '0') {
					i++;
				}
				if (i >= BWT.length) {
					break;
				}
				pos++;

				// TODO :: we here have the length undefined and therefore get looong prefixes...
				// but that surely is not actually necessary? (it is just a bit difficult to know
				// how long the prefix needs to be to be able to compare it... but meeeps *g*)
				prefixes.push([this._publishPrefix(i, true, undefined, true).replace(S0K0, ''), pos]);
			}

			prefixes = prefixes.sort();

			aftersort = [];
			for (var i=0; i < prefixes.length; i++) {
				if (i !== prefixes[i][1]) {
					aftersort[i] = prefixes[i][1];
				}
			}

			return;








			/*
			var cols_to_be_sorted = [];
			var prefs_to_be_sorted = [];
			var cols_newly_sorted = [];
			var pos = -1;

			for (var i=0; i < BWT.length; i++) {
				// we jump over M[i] = 0, because the prefix locations are based on FiC, and M[i] = 0
				// means that this is just another out-edge of the same FiC-node
				while (M[i] === '0') {
					i++;
				}
				if (i >= BWT.length) {
					break;
				}
				pos++;

				// TODO :: we here have the length undefined and therefore get looong prefixes...
				// but that surely is not actually necessary? (it is just a bit difficult to know
				// how long the prefix needs to be to be able to compare it... but meeeps *g*)
				cur_pref = this._publishPrefix(i, true, undefined, true);
				console.log(cur_pref);

				// EMRGEMRG :: we here need not check for S0K0 in cur_pref, right?
				// I mean, it is ALWAYS there, the question is just how early...
				// (so is this just a NOT-IN-POS-0 check? and would it be sad to also
				// allow it being in pos 0?)
				if (cur_pref.indexOf(S0K0) > 0) {
					var new_pref = cur_pref.replace(S0K0, '');
					var j = 0;
					while (prefs_to_be_sorted[j] && (new_pref > prefs_to_be_sorted[j])) {
						j++;
					}
					cols_to_be_sorted.splice(j, 0, pos);
					prefs_to_be_sorted.splice(j, 0, new_pref);
				}
				while (prefs_to_be_sorted.length > 0) {
					if (cur_pref.replace(S0K0, '') > prefs_to_be_sorted[0]) {
						// put the column from cols_to_be_sorted[0] into position i-1
						// (it would theoretically be position i, but we have to say -1
						// becauwse we delete the column first and then all indices after
						// it get shifted down by 1)
						if (cols_to_be_sorted[0] !== pos-1) {
							cols_newly_sorted.push([cols_to_be_sorted[0], pos-1]);
						}
						cols_to_be_sorted.splice(0, 1);
						prefs_to_be_sorted.splice(0, 1);
					} else {
						break;
					}
				}
			}

			aftersort = [];

			for (var i=0; i < cols_newly_sorted.length; i++) {
				for (var j=cols_newly_sorted[i][0]; j < cols_newly_sorted[i][1]; j++) {
					if (aftersort[j+1] === undefined) {
						aftersort[j] = j+1;
					} else {
						aftersort[j] = aftersort[j+1];
					}
				}
				aftersort[cols_newly_sorted[i][1]] = cols_newly_sorted[i][0];
			}

			// with TATA|,2,A,4 vs. C we get aftersort = [2,3], [1,3], [5,6]


			console.log('aftersort:');
			console.log(aftersort);
			*/
		},

		_publishFindex: function() {

			var findex = [];


			var p12 = [];

			for (var i=0; i < char.length; i++) {
				if (M[i] == '1') {
					p12.push(this._publishPrefix(i));
				}
			}


			var pBWT = [];

			for (var i=0; i < BWT.length; i++) {
				if (F[i] == '1') {
					pBWT.push(BWT[i]);
				} else {
					pBWT[pBWT.length-1] += '|' + BWT[i];
				}
			}


			var pM = [];

			for (var i=0; i < M.length; i++) {
				if (M[i] == '1') {
					pM.push('1');
				} else {
					pM[pM.length-1] += '0';
				}
			}


			var pF = [];

			for (var i=0; i < F.length; i++) {
				if (F[i] == '1') {
					pF.push('1');
				} else {
					pF[pF.length-1] += '0';
				}
			}


			p12 = GML.prunePrefixes(p12);


			findex = [p12, pBWT, pM, pF];

			return findex;
		},



		startNewSplitRound: function() {

			for (var i=0; i < multi_cur_fic.length; i++) {
				multi_cur_fic[i] = 0;
				multi_cur_fic_int[i] = 0;
			}
			for (var i=0; i < multi_cur_bwt.length; i++) {
				multi_cur_bwt[i] = 0;
				multi_cur_bwt_int[i] = 0;
			}
		},

		_replaceSpecialChar: function(oldspecchar, newspecchar) {

			while (BWT.indexOf(oldspecchar) > -1) {
				BWT = BWT.replace(oldspecchar, newspecchar);
			}

			while (char.indexOf(oldspecchar) > -1) {
				char = char.replace(oldspecchar, newspecchar);
			}

			for (var i=0; i < alph.length; i++) {
				if (alph[i] === oldspecchar) {
					alph[i] = newspecchar;
				}
			}

			C[newspecchar] = C[oldspecchar];
			ord[newspecchar] = ord[oldspecchar];
		},

		finalizeMerge: function(verbose) {

			sout = '';
			role = 1;
			subXBWs = [];

			var loc, i;
			var DS_1 = GML.DS_1;
			var DK_1 = GML.DK_1;
			var DK = GML.DK;

			var crossstart = select(DK_1, char, 0);
			var old_M_slice = M.slice(crossstart);

			for (i=0; i < char.length; i++) {
				if (char[i] === DK_1) {
					M = GML.setInString(M, i, '1');
				}
			}

			if (verbose) {
				sout += 'We get ' +
						'<code>crossstart = ' + crossstart + '</code>, ' + 
						'<code>old_M_slice = "' + old_M_slice + '"</code> ' 
						'and set some M values to 1:' + GML.nlnl;
				sout += GML.hideWrap(
					'<div class="table_box">' + this.generateSubTables() + '</div>',
					'Table') + GML.nlnl;
			}

			// We need to exchange
			//   FiC in position where FiC == #_0
			// and
			//   FiC in position where FiC == BWT in position where FiC == $_0
			// (in GACGT|,2,T,4;,3,,5 vs. ACCTG|,1,,5;,1,,3,
			// we here exchange three #_0 for one T in FiC, where this T is the first T in FiC)
			
			var pos1, pos2;
			for (i=0; i < BWT.length; i++) {
				if (char[i] === DS_1) {
					pos1 = i;
					break;
				}
			}

			pos1 = rank('1', M, pos1);
			pos1 = select('1', F, pos1);

			var BWTatpos = BWT[pos1];

			// we find out the how manieth A, C, G or T this one is
			loc = rank(BWTatpos, BWT, pos1);

			// we find out where in the first column this is
			loc = select(BWTatpos, char, loc);

			// now we need to cram every FiC = #_0 into here, and push this to the very end
			var crossstart = select(DK_1, char, 0);

			if (verbose) {
				sout += 'We get ' +
						'<code>pos1 = ' + pos1 + '</code>, ' + 
						'<code>loc = ' + loc + '</code>, ' + 
						'<code>BWTatpos = "' + BWTatpos + '"</code> and ' + 
						'update the first column by slicing around magically:' + GML.nlnl +
						char.slice(0, loc) + GML.nlnl +
						GML.repeatstr(char.slice(crossstart).length, char[loc]) + GML.nlnl +
						char.slice(loc+1, crossstart) + GML.nlnl +
						DK_1 + ':' + GML.nlnl;
			}

			char = char.slice(0, loc) +
				   GML.repeatstr(char.slice(crossstart).length, char[loc]) +
				   char.slice(loc+1, crossstart) +
				   DK_1;

			if (verbose) {
				sout += GML.hideWrap(
					'<div class="table_box">' + this.generateSubTables() + '</div>',
					'Table') + GML.nlnl;
			}

			// We now exchange
			//   BWT in position where FiC == $_0
			// and
			//   BWT in position where BWT == #_0

			var firstHash0Replacement;
			var hash0replacement_len = 0;

			for (i=BWT.length-1; i>-1; i--) {
				if (BWT[i] === DK_1) {
					firstHash0Replacement = i;
					hash0replacement_len++;
					BWT = GML.setInString(BWT, i, BWTatpos);
					// we do NOT break as we can have several #_0 nodes,
					// e.g. in GACGT|,2,T,4;,3,,5 vs. ACCTG|,1,,5;,1,,3
				}
			}

			BWT = GML.setInString(BWT, pos1, DK_1);

			firstHash0Replacement = rank(BWTatpos, BWT, firstHash0Replacement);
			firstHash0Replacement = select(BWTatpos, char, firstHash0Replacement);


			if (verbose) {
				sout += 'We slice around the <i>M</i> value magically, into the parts' + GML.nlnl +
						M.slice(0, firstHash0Replacement) + GML.nlnl +
						GML.repeatstr(old_M_slice.length, '1') + GML.nlnl +
						M.slice(firstHash0Replacement+1, crossstart) + GML.nlnl +
						M[firstHash0Replacement] + ':' + GML.nlnl;
			}

			M = M.slice(0, firstHash0Replacement) +
				GML.repeatstr(old_M_slice.length, '1') +
				M.slice(firstHash0Replacement+1, crossstart) +
				M[firstHash0Replacement];

			if (verbose) {
				sout += GML.hideWrap(
					'<div class="table_box">' + this.generateSubTables() + '</div>',
					'Table') + GML.nlnl;
			}


			for (i=1; i < hash0replacement_len; i++) {
				M = GML.setInString(M, firstHash0Replacement+i, '0');
			}

			if (verbose) {
				sout += 'We do some hash replacement nonsense in <i>M</i>, ' +
						'with <code>firstHash0Replacement = ' + firstHash0Replacement + '</code> and ' +
						'and <code>hash0replacement_len = ' + hash0replacement_len + '</code>:' + GML.nlnl;
				sout += GML.hideWrap(
					'<div class="table_box">' + this.generateSubTables() + '</div>',
					'Table') + GML.nlnl;
			}

			// cut out $_0 from BWT and F
			for (i=0; i < BWT.length; i++) {
				if (BWT[i] === DS_1) {
					loc = i;
					break;
				}
			}
			BWT  =  BWT.slice(0, loc) +  BWT.slice(loc+1);
			F    =    F.slice(0, loc) +    F.slice(loc+1);

			if (verbose) {
				sout += 'We cut out ' + GML.DS_1_o + ' from BWT and <i>F</i>:' + GML.nlnl;
				sout += GML.hideWrap(
					'<div class="table_box">' + this.generateSubTables() + '</div>',
					'Table') + GML.nlnl;
			}

			// cut out $_0 from FiC and M
			for (i=0; i < char.length; i++) {
				if (char[i] === DS_1) {
					loc = i;
					break;
				}
			}
			char = char.slice(0, loc) + char.slice(loc+1);
			M    =    M.slice(0, loc) +    M.slice(loc+1);

			if (verbose) {
				sout += 'We cut out ' + GML.DS_1_o + ' from first column and <i>M</i>:' + GML.nlnl;
				sout += GML.hideWrap(
					'<div class="table_box">' + this.generateSubTables() + '</div>',
					'Table') + GML.nlnl;
			}

			// cut out #_0 from BWT and F
			for (i=0; i < BWT.length; i++) {
				if (BWT[i] === DK_1) {
					loc = i;
					break;
				}
			}
			BWT  =  BWT.slice(0, loc) +  BWT.slice(loc+1);
			F    =    F.slice(0, loc) +    F.slice(loc+1);

			if (verbose) {
				sout += 'We cut out ' + GML.DK_1_o + ' from BWT and <i>F</i>:' + GML.nlnl;
				sout += GML.hideWrap(
					'<div class="table_box">' + this.generateSubTables() + '</div>',
					'Table') + GML.nlnl;
			}

			// cut out #_0 from FiC and M
			for (i=0; i < char.length; i++) {
				if (char[i] === DK_1) {
					loc = i;
					break;
				}
			}
			char = char.slice(0, loc) + char.slice(loc+1);
			M    =    M.slice(0, loc) +    M.slice(loc+1);

			if (verbose) {
				sout += 'We cut out ' + GML.DK_1_o + ' from first column and <i>M</i>:' + GML.nlnl;
				sout += GML.hideWrap(
					'<div class="table_box">' + this.generateSubTables() + '</div>',
					'Table') + GML.nlnl;
			}

			recalculate(true);

			if (verbose) {
				sout += 'We now recalculate the first column, ' +
						'and the internal <i>C</i> and ord arrays:' + GML.nlnl;
			}

			return sout;
		},

		// gives back true if more merging needs to be done, and false if
		// a whole round of merging has been finished
		notFullyMerged: function() {

			for (var i=0; i < subXBWs.length; i++) {
				if (multi_cur_bwt[i] <= subXBWs[i]._publishBWTlenWithF()) {
					return true;
				}
			}

			return false;
		},

		// makes certain that the next call to notFullyMerged() results in false
		forceFullyMerged: function() {

			for (var i=0; i < subXBWs.length; i++) {
				multi_cur_bwt[i] = subXBWs[i]._publishBWTlenWithF()+1;
			}
		},

		_publishBWTlen: function() {
			return BWT.length;
		},

		// TODO EMRG :: store this, instead of recalculating again and again =)
		_publishBWTlenWithF: function() {
			return rank('1', F, BWT.length-1);
		},

		// pref_cur_i .. current prefix i in FC-indexing
		// use_all_out_edges .. boolean [optional, default: true]
		//           (if true, the starting node is checked for more outgoing edges
		//           and if there are several different ones, a ! report is issued;
		//           if false, the edge out of the starting node is determined by the
		//           flat table pref_cur_i, and only all following nodes are handled
		//           as previously described)
		// length .. maximum length of prefix reported [optional]
		//           (if given, result can be shorter, but not longer)
		// spillover .. boolean [optional, default: false]
		//           (if true, when a $_0 is encountered, increase length by 2 and
		//           and #_0 and first node from H_2;
		//           if false, encountering $_0 ends the prefix creation just like $)
		// give_the_split_node .. object [optional], containing o = 1 or o = 2, the origin
		//                        see [NOTE 2] to learn more about it
		_publishPrefix: function(pref_cur_i, use_all_out_edges, length, spillover, give_the_split_node) {

			if (use_all_out_edges === undefined) {
				use_all_out_edges = true;
			}

			var pref = '';

			GML.vis_highlight_nodes = [[]];

			if (length === undefined) {
				length = GML.loop_threshold;
			}


			var pref_cur_is = [pref_cur_i];
			var pref_cur_is_store;


			// [NOTE 2] ::
			//     If give_the_split_node is undefined, then don't care about which
			//     node should be split upon encountering a !. (E.g. if someone just
			//     wants to hear about the prefix, but doesn't want to work on the
			//     data structure, then give_the_split_node can be undefined.)
			//
			//     If give_the_split_node is actually given, then the caller DOES
			//     want to know about the split node - that is, a node that could
			//     be split to help in case of a ! being encountered (this is not
			//     to say that such a node will always be returned - consider just
			//     a plain string of data, no graphyness: there, we would never
			//     encounter a ! while building prefixes, but also we could never
			//     return a useful splitting node, as for a node to be that, it
			//     needs to have several outgoing edges that it can be split into.)
			//
			//     Previously, we assumed that we have just one node that needs to
			//     be split (which is why we took [0] of all pref_cur_is), but if
			//     there are several nodes, then only one will be returned in that case:
			//
			//     A > C > D < this will be split on C
			//           > E < this will be split on C
			//       > C > D < this will be ignored
			//           > E < this will be ignored
			//
			//     Indeed, if the one that needs to be split is not the first one, then it
			//     all completely breaks down:
			//
			//     A > C > D < this will be split on C, but it CANNOT be, as C only has one out-edge
			//       > C > D < this will be ignored
			//           > E < this will be ignored
			//
			//     What we need instead is to return the first one with a problem...
			//     (Or ideally return several ones and split all of them, but that would be
			//     hard as during the splitting process the indices change...)
			//     So, yes: what we do here is return always the FIRST one that has a problem,
			//     so in the first example return like now [0], but when coming around
			//     again the next time this will look like the second example, and we
			//     will return [1].
			//
			//
			//     In addition to everything that has just been said, we ALSO
			//     need to consider the following:
			//     when we get something like
			//
			//     A > C > D
			//       > C > E
			//
			//     then we should not actually split on either C, but instead we then need
			//     to split on the A that is before it all...
			//     so basically we need to bubble up by first going down within the Cs (which
			//     is what the beginning of this note is about),
			//     to find a C that can be split (that has M (or F?) above 1), and if we cannot
			//     find somesuch, we have to bubble forward, and down, and forward, and down,
			//     and so on, until we FINALLY find something that can be split!
			//
			//     => So, yes, to do all of this magic, we use pref_cur_is_store and give back
			//        our results via give_the_split_node.

			if (give_the_split_node) {
				pref_cur_is_store = [];

				// initialize to i = -1, "no node found"
				give_the_split_node.i = -1;
			}

			for (var i=0; i<length; i++) {

				if (give_the_split_node) {
					pref_cur_is_store.push([]);
				}

				for (var j=0; j<pref_cur_is.length; j++) {
					GML.vis_highlight_nodes[j].push(pref_cur_is[j]);
				}

				// ignore this all if we are in the first iteration
				// (we do not convert node i to table i, as table i is given in,
				// and we do not abandon all hope if the current node has more
				// than one outgoing edge, as a direct table i has been provided
				// which tells us about the exact edge we want to take - without
				// that (that is, at i > 0) we would have to select one at random,
				// and that is a real reason for despair!)
				if ((i > 0) || use_all_out_edges) {
					// here, getting the length before going into the loop is actually
					// important, as we do not want to loop through the elements that
					// are added inside (as they have already been converted)
					var len = pref_cur_is.length;
					for (var j=0; j<len; j++) {
						if (i > 0) {
							// convert node i to table i, that is, absolute indexing to FC-indexing
							pref_cur_is[j] = select('1', M, pref_cur_is[j]);
						} else {
							// if use_all_out_edges, we need to go to the left as long as
							// we have '0' in M to get to the bottom of this all ;)
							while (M[pref_cur_is[j]] == '0') {
								pref_cur_is[j]--;
							}
						}

						var pcis_len, pcis_len_m;
						if (give_the_split_node) {
							pcis_len = pref_cur_is_store.length-1;

							// the i tells us which node we are talking about
							// the m tells us whether it can be split (m > 1) or not (m == 1)
							pref_cur_is_store[pcis_len].push({i: pref_cur_is[j], m: 1});
							pcis_len_m = pref_cur_is_store[pcis_len].length-1;
						}

						for (var k = 1; M[pref_cur_is[j]+k] == '0'; k++) {
							pref_cur_is.push(pref_cur_is[j]+k);

							if (give_the_split_node) {
								pref_cur_is_store[pcis_len][pcis_len_m].m++;

								// we say m = 1 here as this node is already represented
								// in the previous line, getting an m++ there, and there
								// is no point in splitting the same node twice
								pref_cur_is_store[pcis_len].push({i: pref_cur_is[j]+k, m: 1});

								// we here can actually keep track of where the pref_cur_is_store
								// entries come from, and basically clone their history...
								// however, that data would not really be used right now anyway
								/*
								for (var l=0; l < pcis_len; l++) {
									pref_cur_is_store[l].push({
										i: pref_cur_is_store[l][j].i,
										m: pref_cur_is_store[l][j].m
									});
								}
								*/
							}

							var new_vis_high = [];
							for (var l=0; l<GML.vis_highlight_nodes[0].length; l++) {
								new_vis_high.push(GML.vis_highlight_nodes[0][l]);
							}
							GML.vis_highlight_nodes.push(new_vis_high);
						}
					}
				}

				for (var j=0; j<pref_cur_is.length; j++) {
					// We just follow the first outgoing edge - which would seem completely
					// stupid usually, but we also deactivate the select, as we are already
					// in FC-indexing, and the offset of the outgoing edge IS ALREADY INCLUDED
					// in the index we give as first parameter!
					// We also deactivate the ranking, so that we effectively give in an
					// FC-indexed value and get out a BWT-indexed value.
					pref_cur_is[j] = psi(pref_cur_is[j], 0, false, false);
				}

				var char_to_add_now = '.';
				var not_all_chars_are_the_same = false;

				if (BWT[pref_cur_is[0]]) {
					char_to_add_now = BWT[pref_cur_is[0]];
				}

				for (var j=0; j<pref_cur_is.length; j++) {
					if (BWT[pref_cur_is[j]]) {
						if (char_to_add_now != BWT[pref_cur_is[j]]) {
							not_all_chars_are_the_same = true;
						}
					} else {
						if (char_to_add_now != '.') {
							not_all_chars_are_the_same = true;
						}
					}
				}

				if (not_all_chars_are_the_same) {

					// add error char '!' to the prefix
					pref += GML.prefixErrorChar;

					// remove last highlighted node
					for (var j=0; j < GML.vis_highlight_nodes.length; j++) {
						GML.vis_highlight_nodes[j].splice(GML.vis_highlight_nodes[j].length-1, 1);
					}

					// abandon all hope / leave function
					break;

				} else {

					pref += char_to_add_now;

					if (spillover) {

						// We only spillover if we are in a position after the first,
						// so A$_0 => A$_0#_0C, but $_0 => $_0
						// The reason for this is that the prefix starting with $_0 is
						// a very special case - it in fact does not really exist (and will
						// be taken out later on), and we would try to compare prefixes across
						// ALL of H_2, as its prefix is essentially the same as that of the
						// first node in H_2, so that they would add more and more characters
						// and never reach a difference.
						// We could instead also change the comparison routine, but this here
						// wants to be done anyway for nicer display, so we may as well leave
						// it out of the comparison routine.

						if ((pref.length > 1) && (char_to_add_now == GML.DS_1)) {

							// find #_0 node in H_2 - which is necessarily the very last node, lucky us!
							var pref_cur_i = nextXBW._publishBWTlen() - 1;
							if (give_the_split_node) {
								give_the_split_node.o++;
							}
							var pref_in_h2 = nextXBW._publishPrefix(pref_cur_i, true, length + 2 - pref.length, false, give_the_split_node);

							// if no split node was found on the other one...
							if (give_the_split_node && (give_the_split_node.i < 0)) {
								// ... find one here, maybe? =)
								this._setSplitNodeBasedOnStore(give_the_split_node, pref_cur_is_store, 0);
							}

							return pref + pref_in_h2;
						}
					}
				}

				if ((char_to_add_now == GML.DS) || (char_to_add_now == GML.DS_1)) {
					break;
				}

				// convert from BWT-indexing to absolute indexing
				for (var j=0; j<pref_cur_is.length; j++) {
					pref_cur_is[j] = rank('1', F, pref_cur_is[j]);
				}
			}

			if (give_the_split_node) {
				// have offset -1, as it does not help splitting the very last node, as that
				// one is the one where the ! occurred - instead, we want to split a node
				// before it (or even earlier)
				// e.g. here with offset 0 we would get the D, which does not help, instead
				// we need to split on the C =)
				// 
				// A > C > D > F			(prefix: AC!, split C to get ACD and ACE)
				//           > G
				//       > E
				this._setSplitNodeBasedOnStore(give_the_split_node, pref_cur_is_store, -1);
			}

			return pref;
		},

		_setSplitNodeBasedOnStore: function(give_the_split_node, pref_cur_is_store, offset) {

			var i = pref_cur_is_store.length + offset;
			while (i > 0) {
				i--;
				for (var j = 0; j < pref_cur_is_store[i].length; j++) {
					if (pref_cur_is_store[i][j].m > 1) {
						give_the_split_node.i = pref_cur_is_store[i][j].i;
						return;
					}
				}
			}
		},

		_publishAllNodes: function() {

			return [BWT, char, M, F];
		},

		// TODO :: add different runmodes (this function is called twice, but once almost all
		// its return values are discarded - how sad!)
		_constructAllPrefixes: function() {

			var sout = '';
/*
sout += '<br>' +
		'BEFORE:' + '<br>' +
		'multi_cur_bwt[0]: ' + multi_cur_bwt[0] + '<br>' +
		'multi_cur_bwt_int[0]: ' + multi_cur_bwt_int[0] + '<br>' +
		'multi_cur_fic[0]: ' + multi_cur_fic[0] + '<br>' +
		'multi_cur_fic_int[0]: ' + multi_cur_fic_int[0] + '<br>' +
		'multi_cur_bwt[1]: ' + multi_cur_bwt[1] + '<br>' +
		'multi_cur_bwt_int[1]: ' + multi_cur_bwt_int[1] + '<br>' +
		'multi_cur_fic[1]: ' + multi_cur_fic[1] + '<br>' +
		'multi_cur_fic_int[1]: ' + multi_cur_fic_int[1] + '<br>';
*/
			var takeNodeFrom;
			var split_nodes = [];
			for (var i=0; i < subXBWs.length; i++) {
				split_nodes.push({sn_i: -1});
				last_high_fic[i] = -1;
				last_high_bwt[i] = -1;
			}

			var pEC = GML.prefixErrorChar;



			// construct prefixes

			// TODO NOW :: keep previous results in memory (so if we generated the
			//             prefix for this here in the previous step, then we do not
			//             need to create it again)

			var prefixes = ['', '']; // TODO :: the prefixes here are always just two... never more or less
									 // all lines that need to be changed regarding this have
									 // been highlighted as TE1TE

			if (multi_cur_bwt[0] > subXBWs[0]._publishBWTlenWithF()) { // TE1TE - here we say if H_1 is bad, use H_2, and vice versa - but for three and more this is much more complicated, we need to instead think of a pool of candidates
				return [1, sout, prefixes, split_nodes]; // TE1TE
			} // TE1TE

			if (multi_cur_bwt[1] > subXBWs[1]._publishBWTlenWithF()) { // TE1TE
				return [0, sout, prefixes, split_nodes]; // TE1TE
			} // TE1TE

			var patience = 1;

			// we here do not need to explicitly check for reaching $ or $_1,
			// as they are both different, and therefore we would necessarily
			// reach a difference between the prefixes, which we ARE already
			// checking for anyway =)
			while ((GML.comparePrefixes(prefixes[0], prefixes[1]) === 0) && // TE1TE
					(prefixes[0][prefixes[0].length-1] != pEC) && // TE1TE
					(prefixes[1][prefixes[1].length-1] != pEC)) { // TE1TE

				// TODO NOW :: build prefixes char-by-char and keep last pref in memory
				//             instead of in every cycle building again from the very
				//             beginning
				var gtsn = {};
				for (var j=0; j < subXBWs.length; j++) {
					gtsn.o = j;
					prefixes[j] = subXBWs[j]._publishPrefix(
						multi_cur_fic_int[j], true, patience, true, gtsn);
					split_nodes[j] = {sn_i: gtsn.i, o: gtsn.o};
				}

				patience++;

				if (patience > GML.loop_threshold) {
					return [0, GML.loopOverflowError(sout), prefixes, split_nodes];
				}
			}

			sout += '<span class="col-6">The prefix of ' + GML.DH_1 + '[' + multi_cur_fic_int[0] + '] is ' +
					prefixes[0] + '.</span>'; // TE1TE

			sout += 'The prefix of ' + GML.DH_2 + '[' + multi_cur_fic_int[1] + '] is ' +
					prefixes[1] + '.' + GML.nlnl; // TE1TE

			var comp = GML.comparePrefixes(prefixes[0], prefixes[1]); // TE1TE

			// TE1TE for the next 9 lines
			if (comp > 0) {
				takeNodeFrom = 1;
			}
			if (comp < 0) {
				takeNodeFrom = 0;
			}
			if (comp == 0) {
				takeNodeFrom = -1;
			}

/*
sout += '<br>' +
		'AFTER:' + '<br>' +
		'multi_cur_bwt[0]: ' + multi_cur_bwt[0] + '<br>' +
		'multi_cur_bwt_int[0]: ' + multi_cur_bwt_int[0] + '<br>' +
		'multi_cur_fic[0]: ' + multi_cur_fic[0] + '<br>' +
		'multi_cur_fic_int[0]: ' + multi_cur_fic_int[0] + '<br>' +
		'multi_cur_bwt[1]: ' + multi_cur_bwt[1] + '<br>' +
		'multi_cur_bwt_int[1]: ' + multi_cur_bwt_int[1] + '<br>' +
		'multi_cur_fic[1]: ' + multi_cur_fic[1] + '<br>' +
		'multi_cur_fic_int[1]: ' + multi_cur_fic_int[1] + '<br>';
*/

			return [takeNodeFrom, sout, prefixes, split_nodes];
		},

		// verbose .. [boolean] (default: false)
		//            if true, give out fancy explanatory HTML,
		//            if false, just do the work and do not give out anything
		mergeOneMore: function(verbose) {

			var p = this._constructAllPrefixes();

			var takeNodeFrom = p[0];
			var fic_o, fic_i, bwt_o, bwt_i;


			if (takeNodeFrom < 0) {
				// TODO :: throw an error
			}


			last_high_fic[takeNodeFrom] = multi_cur_fic_int[takeNodeFrom];
			last_high_bwt[takeNodeFrom] = multi_cur_bwt_int[takeNodeFrom];

			// insert node from the sub XBW at multi_cur into the merged XBW
			fic_i = multi_cur_fic_int[takeNodeFrom];
			fic_o = takeNodeFrom;
			bwt_i = multi_cur_bwt_int[takeNodeFrom];
			bwt_o = takeNodeFrom;

			multi_cur_fic[takeNodeFrom]++;
			multi_cur_bwt[takeNodeFrom]++;

			this._addNodeBasedOnTwo(fic_o, fic_i, bwt_o, bwt_i);



			// [OVERRIDE 3]
			multi_cur_fic_int[takeNodeFrom] = subXBWs[takeNodeFrom]._doAftersortFiC(multi_cur_fic[takeNodeFrom]);
			multi_cur_bwt_int[takeNodeFrom] = subXBWs[takeNodeFrom]._doAftersortBWT(multi_cur_bwt[takeNodeFrom]);

			if (verbose) {

				var sout = '';
				var DH = GML.make_DH(takeNodeFrom);

				sout += 'That is, we take the first column and <i>M</i> from node ' +
						(fic_i + GML.ao) + ' from ' + DH +
						' and insert it into the merged table.' + GML.nlnl;

				sout += 'We also take the BWT and <i>F</i> from node ' +
						(bwt_i + GML.ao) + ' from ' + DH +
						' and insert it into the merged table.' + GML.nlnlnl;

				var sstep = 'We add the following red cells to the merged table:' + GML.nlnl;

				var shide = '<div class="table_box">' + this.generateSubTables() + '</div>';
				sstep += GML.hideWrap(shide, 'Tables') + GML.nlnl;

				for (var i=0; i < subXBWs.length; i++) {
					last_high_bwt[i] = -1;
					last_high_fic[i] = -1;
				}

				last_high_bwt_arr = [];
				last_high_fic_arr = [];

				sout = GML.makeVisualsNice(p[1]) + GML.nlnl + sstep + GML.makeVisualsNice(sout);

				return sout;
			}
		},

		_doAftersortBWT: function(i) {

			var ret = aftersort[i];
			if (ret === undefined) {
				ret = i;
			}

			ret = select('1', F, ret);

			return ret;
		},

		_doAftersortFiC: function(i) {

			var ret = aftersort[i];
			if (ret === undefined) {
				ret = i;
			}

			ret = select('1', M, ret);

			return ret;
		},
		checkIfSplitOneMore: function(split_nodes_out) {

			var p = this._constructAllPrefixes();

			var takeNodeFrom = p[0];
			var sout = p[1];
			var prefixes = p[2];

			// split_node_1 and split_node_2 are basically multi_cur_1_fic
			// and multi_cur_2_fic, however with one important difference:
			// the multi_cur things are always the beginning, e.g. in AC!,
			// they point to the A; but what we want to be splitting is not
			// the A - we want to be splitting the C, the last letter before
			// the !; so that is where split_node_1 and _2 are pointing to =)
			// Also, split_node_1 and _2 are not just integers, but are objects,
			// each containing sn_i (for the position) and o (for the origin.)
			var split_nodes_in = p[3];

			var pEC = GML.prefixErrorChar;

			if (GML.verbosity > 1) {
				sout = GML.makeVisualsNice(sout);
			}



			// Check if one of the prefixes contains a '!' at the end - if so,
			// earmark it being split.
			// Btw., splitnodes is an array of nodes that are supposed to be split,
			// each node having an origin o (1 or 2) and an sn_i (split node i.)
			// We can split at most 4 different nodes (fic and bwt for both H_1 and H_2,
			// if these are ALL ending in ! and bwt and fic are in different locations.)
			// However, to make it simpler, we will just report one for now.
			// TODO :: think about reporting several ones at once and handling them all!

			for (var i=0; i < subXBWs.length; i++) {
				if (prefixes[i][prefixes[i].length-1] === pEC) {
					split_nodes_out.push(split_nodes_in[i]);
					return sout;
				}
			}


			// do the regular update if nothing changed
			if (split_nodes_out.length < 1) {
				// TODO :: throw an error if takeNodeFrom > -1 here is not true
				// (basically, takeNodeFrom == -1 means that no origin was chosen,
				// as both prefixes are the same... which should not happen)
				if (takeNodeFrom > -1) {
					multi_cur_fic[takeNodeFrom]++;
					multi_cur_bwt[takeNodeFrom]++;

					multi_cur_fic_int[takeNodeFrom] = subXBWs[takeNodeFrom]._doAftersortFiC(multi_cur_fic[takeNodeFrom]);
					multi_cur_bwt_int[takeNodeFrom] = subXBWs[takeNodeFrom]._doAftersortBWT(multi_cur_bwt[takeNodeFrom]);
				}
			}

			return sout;
		},

		// H is 1 or 2, depending on whether we are splitting a node in H_1 or H_2
		// sn_i is the splitting node i as flat table i
		splitOneMore: function(nodes) {

			var sout = '';

			// it would be great to here split all the nodes that are given,
			// but it is much simpler to only split the first node that is given,
			// and therefore we just go for nodes[0] instead of a nodes-for-loop
			// (after splitting one node the index of another node might be
			// screwed up... and yes, even in an adjacent XBW, through spillovers)

			if (nodes.length > 0) {

				// console.log('splitOneMore on nodes:');
				// console.log(nodes);

				sout += 'We observe a ' + GML.prefixErrorChar + ' in the prefix, ' +
						'and therefore have to split a node in ' +
						GML.make_DH(nodes[0].o) + '.<br>';


				// The indicated node is the default one (that is, no node for
				// splitting could be found?) => Error!
				if (nodes[0].sn_i < 0) {
					GML.error_flag = true;
					return sout + '<div class="error">The splitting of the node failed! (No node for splitting found.)</div>';
				}


				var oxbw = subXBWs[nodes[0].o];

				var prevlen = oxbw._publishBWTlen();

				oxbw._splitNode(nodes[0].sn_i);

				if (GML.error_flag) {
					return sout + '<div class="note">Invalid input: To compute this, nodes would need to be split across graphs.</div>';
				}

				var newlen = oxbw._publishBWTlen();


				// After splitting, the amount of nodes has not increased? => Error!
				if (prevlen >= newlen) {
					GML.error_flag = true;
					return sout + '<div class="error">The splitting of the node failed! (Splitting did not result in more nodes.)</div>';
				}
			}

			return sout;
		},

		// _addNodeBasedOnTwo is called inside H12 - that is, inside the merged XBW.
		// We call it from H1, where we have "this" and "otherXBW" pointing towards
		// H1 and H2, respectively.
		// To be able to use "this" and "otherXBW" onside H12 as well, we need to
		// therefore supply them here explicitly.
		_addNodeBasedOnTwo: function(fic_o, fic_i, bwt_o, bwt_i) {

			var i;

			last_high_fic_arr = [];
			last_high_bwt_arr = [];

			var node_fic = subXBWs[fic_o]._publishAllNodes();
			last_high_fic_arr.push(char.length);
			char += node_fic[1][fic_i];
			M    += node_fic[2][fic_i];
			i = 1;
			while (node_fic[2][fic_i + i] === '0') {
				last_high_fic_arr.push(char.length);
				char += node_fic[1][fic_i + i];
				M    += node_fic[2][fic_i + i];
//				multi_cur_fic[fic_o]++;
				i++;
			}

			var node_bwt = subXBWs[bwt_o]._publishAllNodes();
			last_high_bwt_arr.push(BWT.length);
			BWT  += node_bwt[0][bwt_i];
			F    += node_bwt[3][bwt_i];
			i = 1;
			while (node_bwt[3][bwt_i + i] === '0') {
				last_high_bwt_arr.push(BWT.length);
				BWT  += node_bwt[0][bwt_i + i];
				F    += node_bwt[3][bwt_i + i];
//				multi_cur_bwt[bwt_o]++;
				i++;
			}

			// TODO :: do not recalculate everything here,
			// but instead just update what needs to be updated!
			// (right now, we do not do any calculations on the mergedXBW
			// anyway, so we could even not call this at all, but later on
			// we probably need to because of merging graphy stuff and so
			// on...) <- TODO NOW :: check if this is actually necessary
			recalculate(false);
		},

		equals: function(otherXBW, give_highlight_arr) {

			return otherXBW._areWeTheSame(BWT, char, M, F, alph, ord, C, give_highlight_arr);
		},

		_areWeTheSame: function(pBWT, pchar, pM, pF, palph, pord, pC, give_highlight_arr) {

			var highlight_arr = [];

			if (give_highlight_arr) {
				
				highlight_arr.push([]);

				if (BWT !== pBWT) {
					var arr = [];
					for (var i = 0; i < Math.max(BWT.length, pBWT.length); i++) {
						if (BWT[i] !== pBWT[i]) {
							arr.push(i);
						}
					}
					highlight_arr.push(arr);
				} else {
					highlight_arr.push([]);
				}

				if (char !== pchar) {
					var arr = [];
					for (var i = 0; i < Math.max(char.length, pchar.length); i++) {
						if (char[i] !== pchar[i]) {
							arr.push(i);
						}
					}
					highlight_arr.push(arr);
				} else {
					highlight_arr.push([]);
				}

				if (M !== pM) {
					var arr = [];
					for (var i = 0; i < Math.max(M.length, pM.length); i++) {
						if (M[i] !== pM[i]) {
							arr.push(i);
						}
					}
					highlight_arr.push(arr);
				} else {
					highlight_arr.push([]);
				}

				if (F !== pF) {
					var arr = [];
					for (var i = 0; i < Math.max(F.length, pF.length); i++) {
						if (F[i] !== pF[i]) {
							arr.push(i);
						}
					}
					highlight_arr.push(arr);
				} else {
					highlight_arr.push([]);
				}
			}

			if (BWT !== pBWT) {
				return [false, highlight_arr];
			}

			if (char !== pchar) {
				return [false, highlight_arr];
			}

			if (M !== pM) {
				return [false, highlight_arr];
			}

			if (F !== pF) {
				return [false, highlight_arr];
			}

			var len = Math.max(alph.length, palph.length);

			for (var i=0; i < len; i++) {
				if (alph[i] !== palph[i]) {
					return [false, highlight_arr];
				}
			}

			for (var i=0; i < len; i++) {
				if (ord[alph[i]] !== pord[palph[i]]) {
					return [false, highlight_arr];
				}
			}

			for (var i=0; i < len; i++) {
				if (C[alph[i]] !== pC[palph[i]]) {
					return [false, highlight_arr];
				}
			}

			return [true, highlight_arr];
		},


		// split the node in flat table sn_i
		_splitNode: function(sn_i) {

// () console.log('_splitNode:');

// EMRG aftersort
// console.log('aftersort before:');
// console.log(GML.deep_copy_array(aftersort));

			// Thinking about the node table, here is what we want to do:
			// 1. Take the node and copy it for each 0 in its M node.
			//    (Here we basically copy the entire node as it is: BWT, FiC, M, F.)
			// 2. Set the M value of the original and all copies to 1.
			// 3. Add as many zeroes to the M of the preceding node as nodes were copied.
			// 4. Add the first letter of the next node prefixes to the new prefixes.
			//    (4 is not necessary when looking at the flat table)
			// = this here assumes that:
			//   (a) the node has several outgoing edges (which we will all split up)
			//       > if not, then we cannot split anything without actually changing
			//         the underlying language that our automaton realizes
			//   (b) the node only has one incoming edge
			//       > if not, then we have to think even more about what to do with
			//         the several preceding nodes; MAYBE this works in that case,
			//         but likely not

			// However, we here have a flat table, not a node table,
			// so we need to instead go through the following steps:
			// 1. Just like before, take the node and copy it for each 0 in its M node.
			//    (We also add ones to the F vector; we do not need to copy FiC, as it is
			//    just regenerated afterwards, and M will be looked at separately anyway.)
			// 2. Just like before, set the M value of the original and all copies to 1.
			// 3. Add as many zeroes to the M of the preceding node as nodes were copied.
			//    (Basically, first of all find the preceding node, and then just insert
			//    the zeroes - do not replace / overwrite, just insert.)

			// For an example, consider "ACTG|,2,,4" which is transformed into "ACTG|,1,C,4"
			// when we split on node 2 (flat table i 2, counting arrays from 0)


			// 1. Take the node and copy it for each 0 in its M node.

			var Mloc1 = sn_i;

			// this line makes CACT and GCGTACG|,4,,7;,1,C,5 work and brings us down from 3 fails to 2 fails
			sn_i = rank('1', M, sn_i);

			// get location of this node's M
			var Mloc = sn_i;
			var Floc1 = select('1', F, sn_i);

			// get number within this node's M
			var Mloc2 = select('1', M, Mloc+1);
			var Mnum = Mloc2 - Mloc1;

			// if the first character of the node to the split is #_0,
			// then the input is most likely malformed
			if (char[Mloc1] === GML.DK_1) {
				GML.error_flag = true;
				return;
			}

			// we used to have sn_i here instead of Floc1 - but using Floc1 actually makes TC|,1,C,2 work and does not make all else fail =)
			var prevNodes = [lf([Floc1, Floc1], BWT[Floc1], false, false)[0]];

			// We check if the previous node actually is more than one node by checking the F to the right
			// (Basically, we landed on F = 1; now if F on the right is also 1, then that is a new node,
			// different from what we are considering.
			// But if F = 0 on the right, then that is one more edge coming into this node here, meaning
			// there is one more preceding. And so on we go, until we have F = 1.)

			var i=1;
			while (F[Floc1+i] === '0') {
				prevNodes.push(lf([Floc1+i, Floc1+i], BWT[Floc1+i], false, false)[0]);
				i++;
			}

			var foff = -Mnum;

			//for (var j=prevNodes.length-1; j > -1; j--) { // this makes CACT and GCGTACG|,5,,7;,1,C,5 fail
			for (var j=0; j<prevNodes.length; j++) { // this makes CACTC and GGGCAGTACGTGG|,9,,11;,7,CGCG,8 fail

				foff += Mnum;

				// insert into BWT and F at the same time
				var newBWT = BWT.slice(0, Floc1+1);
				var newF = F.slice(0, Floc1+1);
// () console.log('newBWT start: ' + newBWT);
// () console.log('  newF start: ' + newF);
				// see [_splitNode with 2 in and 3 out] for the reason why these funky formulas are necessary
				// (in a nutshell, when we split a node with 2 ins and 3 outs, then we want to splice the
				// prevNode results into each other)
				for (var i=1; i < Mnum; i++) {
// () console.log('newBWT for this run: ' + BWT[Floc1+foff]);
// () console.log('  newF for this run: ' + F[Floc1+foff]);
					newBWT += BWT[Floc1+foff];
					newF += F[Floc1+foff];

// () console.log('newBWT for prev run: ' + BWT.slice(Floc1+(i*j), Floc1+(i*j)+j).split('').reverse().join(''));
// () console.log('  newF for prev run: ' + F.slice(Floc1+(i*j), Floc1+(i*j)+j).split('').reverse().join(''));

					// we here reverse because looking at
					// TACGAAACGACAGTTCCTTATA|,1,A,4;,1,TTA,17;,18,A,21;,15,GAG,17 and ACT
					// we figure out that we actually need to get the column with the 1 in its F into the very last position
					// TODO :: this works for splitting nodes with an indegree of up to 3, but does it work for more?
					newBWT += BWT.slice(Floc1+(i*j), Floc1+(i*j)+j).split('').reverse().join('');
					newF += F.slice(Floc1+(i*j), Floc1+(i*j)+j).split('').reverse().join('');

/*
console.log('newBWT for prev run: ' + BWT.slice(Floc1+(i*j), Floc1+(i*j)+j));

					newBWT += BWT.slice(Floc1+(i*j), Floc1+(i*j)+j);

// overwrite for _splitNode with 3 in and 2 out
if (j === prevNodes.length-1) {
	var extraF = F.slice(Floc1+(i*j), Floc1+(i*j)+j);
	var eflen = extraF.length;
	if (eflen > 0) {
		extraF = '';
		for (var ef=0; ef < eflen-1; ef++) {
			extraF += '0';
		}
		extraF += '1';
	}
console.log('  newF for prev run override: ' + extraF);
	newF += extraF;
} else {
console.log('  newF for prev run: ' + F.slice(Floc1+(i*j), Floc1+(i*j)+j));
	newF += F.slice(Floc1+(i*j), Floc1+(i*j)+j);
}
//					newF += F.slice(Floc1+(i*j), Floc1+(i*j)+j);
*/
				}
// () console.log('newBWT end: ' + BWT.slice(Floc1+1+((Mnum-1)*j)));
// () console.log('  newF end: ' + F.slice(Floc1+1+((Mnum-1)*j)));
				newBWT += BWT.slice(Floc1+1+((Mnum-1)*j));
				newF += F.slice(Floc1+1+((Mnum-1)*j));


				for (var k=j+1; k < prevNodes.length; k++) {
					if (prevNodes[k] > prevNodes[j]) {
						prevNodes[k] += Mnum - 1;
					}
				}

				// 2. Set the M value of the original and all copies to 1.
				//    (We are not inserting / deleting here, just setting values to 1 that were 0,
				//    so we can do this BEFORE step 3, in which we modify M again, based on the
				//    locations of ones and zeroes BEFORE step 2.)
				//    => of course, we only do this the very first time around, as it only needs
				//       to be done once (otherwise, GCTAGTTACAACT and GCAAGCGAA|,3,TCG,4;,5,T,8 fails)

				var newM;
				if (j === 0) {
					newM = M.slice(0,Mloc1);
					for (var i=Mloc1; i < Mloc2; i++) {
						newM += '1';
					}
					newM += M.slice(Mloc2);



// EMRG try to update the aftersort array
// => this seems to have no effect either way...

/**/
for (var afs=0; afs < aftersort.length; afs++) {
	if ((aftersort[afs] !== undefined) && (aftersort[afs] > Mloc)) {
		aftersort[afs] += Mnum-1;
	}
}
for (var afs=1; afs < Mnum; afs++) {
	var am = aftersort[Mloc];
	if (am !== undefined) {
		am += afs;
	}
	aftersort.splice(Mloc+afs-1, 0, am);
}
/**/


				} else {
					newM = M;
				}

/**/

				// 3. Add as many zeroes to the M of the preceding node as nodes were copied.

/*
// emrg: actually check for Mnum
if (newM[prevNodes[j]] === '0') {
	console.log('blubb');
	var newMn = newM.slice(0, prevNodes[j]+1);
	for (var i=1; i < Mnum; i++) {
		newMn += '1';
	}
	newMn += newM.slice(prevNodes[j]+1);

} else {
*/
				var newMn = newM.slice(0, prevNodes[j]+1);
				for (var i=1; i < Mnum; i++) {
					newMn += '0';
				}
				newMn += newM.slice(prevNodes[j]+1);

/*
				var newMn = M.slice(0, prevNodes[j]+1);
				for (var i=1; i < Mnum; i++) {
					newMn += '0';
				}
				newMn += M.slice(prevNodes[j]+1);

				var newM = newMn.slice(0,Mloc1);
				for (var i=Mloc1; i < Mloc2; i++) {
					newM += '1';
				}
				newM += newMn.slice(Mloc2);
				newMn = newM;
/**/

				// actually update BWT, M and F explicitly

				BWT = newBWT;
				M = newMn;
				F = newF;
/*}*/

			}

			// TODO :: apply directly to char, ord etc., so that no recalculation is necessary
			recalculate(true);

// EMRG aftersort
// console.log('aftersort after:');
// console.log(GML.deep_copy_array(aftersort));

		},



		// quick analysis via the command line
		// (most of this functionality is also provided by the generateHTML GUI)

		analyze: function() {
			var ostr = 'i   : ';
			for (var i=0; i < BWT.length; i++) {
				if (i % 10 == 0) {
					ostr += Math.round(i / 10) % 10;
				} else {
					ostr += ' ';
				}
			}
			console.log(ostr);
			ostr = '      ';
			for (var i=0; i < BWT.length; i++) {
				ostr += i % 10;
			}
			console.log(ostr);
			console.log('char: ' + char);
			console.log('BWT : ' + BWT);
			console.log('M   : ' + M);
			console.log('F   : ' + F);
			console.log('C   : ' + GML.printKeyValArr(alph, C, true));
			console.log('alph: {' + alph.join(', ') + '}');
			console.log('ord : ' + GML.printKeyValArr(alph, ord, true));
			console.log(' ');

			var flag = 0;
			if (M.length !== F.length) {
				console.log('[ERROR] length mismatch between M and F');
				flag = 1;
			}
			var foundLess = false;
			for (var i=1; i < C.length; i++) {
				if (C[i-1] > C[i]) {
					foundLess = true;
					break;
				}
			}
			if (foundLess) {
				console.log('[ERROR] C not monotonically increasing');
				flag = 1;
			}
			if (flag === 0) {
				console.log('(no errors detected)');
			}
		},



		// HTML functions that are not part of the core XBW environment, but just used
		// for I/O with the user.

		generateHTML: function() {

			var tab = GML_UI.cur_tab;

			if (GML.hideXBWenvironments) {
				document.getElementById('div-xbw-' + tab).style.display = 'none';
				return;
			}

			document.getElementById('div-xbw-' + tab).style.display = 'block';

			var sout = '';

			sout += '<div>';
			
			if (GML.error_flag) {

				sout += '<div class="error">Failure: The XBW environment cannot be initialized.</div>';
				sout += '</div>';

				document.getElementById('div-xbw-' + tab).innerHTML =
					sout;

				return;
			}

			if (role === 3) {
				// we are a fused XBW host environment

				sout += '<u>Fused XBW Environment</u>';

				sout += '<br><br>';

				sout += 'This XBW environment contains ' + subXBWs.length + ' flat tables.';

				if (GML.verbosity > 4) {
					sout += '<br>';
				} else {
					sout += ' ';
				}
				sout += 'They are:<br>';

				var shide = '<div class="table_box" id="div-xbw-' + tab + '-env-table">';

				for (var sx = 0; sx < subXBWs.length; sx++) {
					shide += '<div style="display:inline-block;margin:0px 10px;" id="div-xbw-' + tab + '-env-table-' + sx + '">';
					shide += '</div>';
				}

				shide += '</div>';

				sout += GML.hideWrap(shide, 'Tables');

				if (GML.verbosity > 7) {
					sout += 'To better keep track of what is happening, we also have a look at the corresponding fused graphs:';
				}

				sout += '<br>';

				sout += '<div style="overflow-x:auto;overflow-y:hidden;white-space:nowrap;min-height:50px;text-align:center;">';

				for (var sx = 0; sx < subXBWs.length; sx++) {
					sout += '<div style="display:inline-block;margin:15px 5px -25px ';
					if (sx > 0) {
						sout += '5';
					}
					sout += '5px;min-width:130px;vertical-align:top;" id="div-xbw-' + tab + '-env-graph-' + sx + '">';
					sout += '</div>';
				}

				sout += '</div>';

				var together_margin = '20px';
				if (GML.verbosity > 7) {
					sout += 'We can also look at the overall graph represented by these fused graphs:';
					together_margin = '0px';
				}

				sout += '<div id="div-xbw-' + tab  + '-env-graph-together" style="margin-top:' + together_margin + ';">' +
						'</div>';

				if (GML.verbosity > 9) {
					sout += '<br>';
					sout += "Let's use the XBW to search for some string using <code>find()</code>, or go for the navigation functions directly:";
				}

				sout += '</div>';

				// for two entries in a row, use width 38% and width 9%
				// for three entries in a row, use width 21% and width 9%

				sout += '<div>' +
							'<input id="in-string-' + tab + '-xbw-find" class="up" onkeypress="GML_UI.in_func = [' + "'XBW', 'findHTML'" + ']; GML_UI.inputEnter(event);" type="text" value="AC" style="display: inline-block; width: 21%;"></input>' +
							'<div class="button" onclick="GML.XBWs[GML_UI.cur_tab].findHTML()" style="background-color:#A55;width:9%; margin-left:2%;display:inline-block;">find()</div>' +
							'<input id="in-string-' + tab + '-xbw-pref" class="up" onkeypress="GML_UI.in_func = [' + "'XBW', 'prefHTML'" + ']; GML_UI.inputEnter(event);" type="text" value="(2;0)" style="display: inline-block; width: 21%; margin-left:2%;"></input>' +
							'<div class="button" onclick="GML.XBWs[GML_UI.cur_tab].prefHTML()" style="background-color:#A55;width:9%; margin-left:2%;display:inline-block;">prefix()</div>' +
							'<div class="button" onclick="GML.XBWs[GML_UI.cur_tab].splitNodeHTML()" style="float:right; width:9%; margin-left:2%;">split node()</div>' +
							'<input id="in-string-' + tab + '-xbw-split-node" class="up" onkeypress="GML_UI.in_func = [' + "'XBW', 'splitNodeHTML'" + ']; GML_UI.inputEnter(event);" type="text" value="(1;0)" style="float:right; display: inline-block; width: 21%;"></input>' +
						'</div>';

				sout += '<div>' +
							'<div class="input-info-container" style="display: inline-block; width: 21%;">' +
								'<input id="in-string-' + tab + '-xbw-lf" class="up" onkeypress="GML_UI.in_func = [' + "'XBW', 'lfHTML'" + ']; GML_UI.inputEnter(event);" type="text" value="(7;0),10,A" style="width:100%"></input>' +
								'<span class="infobtn" onclick="GML_UI.clickOnXBWInfo(event, 1)">Info</span>' +
							'</div>' +
							'<div class="button" onclick="GML.XBWs[GML_UI.cur_tab].lfHTML()" style="background-color:#A55;width:9%; margin-left:2%;display:inline-block;">LF()</div>' +
							'<div class="input-info-container" style="display: inline-block; width: 21%; margin-left:2%;">' +
								'<input id="in-string-' + tab + '-xbw-psi" class="up" onkeypress="GML_UI.in_func = [' + "'XBW', 'psiHTML'" + ']; GML_UI.inputEnter(event);" type="text" value="(1;0),0" style="width:100%"></input>' +
								'<span class="infobtn" onclick="GML_UI.clickOnXBWInfo(event, 2)">Info</span>' +
							'</div>' +
							'<div class="button" onclick="GML.XBWs[GML_UI.cur_tab].psiHTML()" style="background-color:#A55;width:9%; margin-left:2%;display:inline-block;">&#936;()</div>' +
							'<div class="button" onclick="GML.XBWs[GML_UI.cur_tab].saveAsGML()" style="float:right; width:15%; margin-left:2%;">Save as GML file</div>' +
							'<div class="button" onclick="GML.XBWs[GML_UI.cur_tab].exportAsGML()" style="float:right; width:15%; margin-left:2%;">Export as GML file</div>' +
						'</div>';

				sout += '<div>' +
							'<input id="in-string-' + tab + '-xbw-select" class="up" onkeypress="GML_UI.in_func = [' + "'XBW', 'selectHTML'" + ']; GML_UI.inputEnter(event);" type="text" value="(1;0),M,4" style="display: inline-block; width: 21%;"></input>' +
							'<div class="button" onclick="GML.XBWs[GML_UI.cur_tab].selectHTML()" style="width:9%; margin-left:2%;display:inline-block;">select()</div>' +
							'<input id="in-string-' + tab + '-xbw-rank" class="up" onkeypress="GML_UI.in_func = [' + "'XBW', 'rankHTML'" + ']; GML_UI.inputEnter(event);" type="text" value="(1;0),F,4" style="display: inline-block; width: 21%; margin-left:2%;"></input>' +
							'<div class="button" onclick="GML.XBWs[GML_UI.cur_tab].rankHTML()" style="width:9%; margin-left:2%; display:inline-block;">rank()</div>' +
							'<div class="button" onclick="GML.XBWs[GML_UI.cur_tab].saveAsFFX()" style="float:right; width:15%; margin-left:2%;">Save as FFX file</div>' +
							'<div class="button" onclick="GML.XBWs[GML_UI.cur_tab].exportAsFFX()" style="float:right; width:15%; margin-left:2%;">Export as FFX file</div>' +
						'</div>';

			} else {
				// we are a regular XBW environment

				sout += '<u>XBW Environment</u>';

				sout += '<br><br>';

				if (GML.verbosity > 7) {
					sout += 'To start the XBW environment, we first of all flatten the BWT (replacing any ' +
							'entries with several options by as many single-optioned entries) and add the ' +
							'first column (the alphabetically sorted BWT.)<br>' +
							'We will also have a look at the ' + GML.DM + ' and ' + GML.DF + ' vectors.' +
							'<br>';
				}

				var alph_and_C_str = this.get_alph_and_C_str();

				sout += 'The alphabet that we are considering is <code>&#931; = ' + alph_and_C_str[0] +
						'</code>, ' +
						'the ord array is <code>' + GML.printKeyValArr(alph, ord, true) + '</code> ' +
						'and the <i>C</i> array is <code>' + alph_and_C_str[1] + '</code> ' +
						'.<br><br>';

				var shide = '<div class="table_box" id="div-xbw-' + tab + '-env-table">' +
							'</div>';

				sout += GML.hideWrap(shide, 'Table');

				if (GML.verbosity > 7) {
					sout += '<br>To better keep track of what is happening, we also have a look at the corresponding graph:';
				}

				sout += '<div id="div-xbw-' + tab  + '-env-graph">' +
						'</div>';

				if (GML.verbosity > 9) {
					sout += "Let's use the XBW to search for some string using <code>find()</code>, or go for the navigation functions directly:";
				}

				sout += '</div>';

				// for two entries in a row, use width 38% and width 9%
				// for three entries in a row, use width 21% and width 9%

				sout += '<div>' +
							'<input id="in-string-' + tab + '-xbw-find" class="up" onkeypress="GML_UI.in_func = [' + "'XBW', 'findHTML'" + ']; GML_UI.inputEnter(event);" type="text" value="AC" style="display: inline-block; width: 21%;"></input>' +
							'<div class="button" onclick="GML.XBWs[GML_UI.cur_tab].findHTML()" style="width:9%; margin-left:2%;display:inline-block;">find()</div>' +
							'<input id="in-string-' + tab + '-xbw-pref" class="up" onkeypress="GML_UI.in_func = [' + "'XBW', 'prefHTML'" + ']; GML_UI.inputEnter(event);" type="text" value="2" style="display: inline-block; width: 21%; margin-left:2%;"></input>' +
							'<div class="button" onclick="GML.XBWs[GML_UI.cur_tab].prefHTML()" style="width:9%; margin-left:2%;display:inline-block;">prefix()</div>' +
							'<div class="button" onclick="GML.XBWs[GML_UI.cur_tab].splitNodeHTML()" style="float:right; width:9%; margin-left:2%;">split node()</div>' +
							'<input id="in-string-' + tab + '-xbw-split-node" class="up" onkeypress="GML_UI.in_func = [' + "'XBW', 'splitNodeHTML'" + ']; GML_UI.inputEnter(event);" type="text" value="12" style="float:right; display: inline-block; width: 21%;"></input>' +
						'</div>';

				sout += '<div>' +
							'<div class="input-info-container" style="display: inline-block; width: 21%;">' +
								'<input id="in-string-' + tab + '-xbw-lf" class="up" onkeypress="GML_UI.in_func = [' + "'XBW', 'lfHTML'" + ']; GML_UI.inputEnter(event);" type="text" value="7,10,A" style="width:100%"></input>' +
								'<span class="infobtn" onclick="GML_UI.clickOnXBWInfo(event, 1)">Info</span>' +
							'</div>' +
							'<div class="button" onclick="GML.XBWs[GML_UI.cur_tab].lfHTML()" style="width:9%; margin-left:2%;display:inline-block;">LF()</div>' +
							'<div class="input-info-container" style="display: inline-block; width: 21%; margin-left:2%;">' +
								'<input id="in-string-' + tab + '-xbw-psi" class="up" onkeypress="GML_UI.in_func = [' + "'XBW', 'psiHTML'" + ']; GML_UI.inputEnter(event);" type="text" value="1,0" style="width:100%"></input>' +
								'<span class="infobtn" onclick="GML_UI.clickOnXBWInfo(event, 2)">Info</span>' +
							'</div>' +
							'<div class="button" onclick="GML.XBWs[GML_UI.cur_tab].psiHTML()" style="width:9%; margin-left:2%;display:inline-block;">&#936;()</div>' +
							'<div class="button" onclick="GML.XBWs[GML_UI.cur_tab].saveAsGML()" style="float:right; width:15%; margin-left:2%;">Save as GML file</div>' +
							'<div class="button" onclick="GML.XBWs[GML_UI.cur_tab].exportAsGML()" style="float:right; width:15%; margin-left:2%;">Export as GML file</div>' +
						'</div>';

				sout += '<div>' +
							'<input id="in-string-' + tab + '-xbw-select" class="up" onkeypress="GML_UI.in_func = [' + "'XBW', 'selectHTML'" + ']; GML_UI.inputEnter(event);" type="text" value="1,M,13" style="display: inline-block; width: 21%;"></input>' +
							'<div class="button" onclick="GML.XBWs[GML_UI.cur_tab].selectHTML()" style="width:9%; margin-left:2%;display:inline-block;">select()</div>' +
							'<input id="in-string-' + tab + '-xbw-rank" class="up" onkeypress="GML_UI.in_func = [' + "'XBW', 'rankHTML'" + ']; GML_UI.inputEnter(event);" type="text" value="1,F,13" style="display: inline-block; width: 21%; margin-left:2%;"></input>' +
							'<div class="button" onclick="GML.XBWs[GML_UI.cur_tab].rankHTML()" style="width:9%; margin-left:2%; display:inline-block;">rank()</div>' +
							'<div class="button" onclick="GML.XBWs[GML_UI.cur_tab].saveAsFFX()" style="float:right; width:15%; margin-left:2%;">Save as FFX file</div>' +
							'<div class="button" onclick="GML.XBWs[GML_UI.cur_tab].exportAsFFX()" style="float:right; width:15%; margin-left:2%;">Export as FFX file</div>' +
						'</div>';
			}

			sout += '<div>Result: <span id="span-' + tab + '-xbw-results">(none)</span>';

			sout += '<div id="xbw-info-box-1" style="margin-top:20px;display:none;">' +
					'Optionally, you can add two boolean parameters ' +
					'in the LF() input, e.g. ';
			if (role === 3) {
				sout += '(7;0)';
			} else {
				sout += '7';
			}
			sout += ',10,A,true,false.<br>' +
					'They control whether select() is called before LF() and ' +
					'whether rank() is called afterwards, respectively.' +
					'</div>';

			sout += '<div id="xbw-info-box-2" style="margin-top:20px;display:none;">' +
					'Optionally, you can add two boolean parameters ' +
					'in the &#936;() input, e.g. ';
			if (role === 3) {
				sout += '(1;0)';
			} else {
				sout += '1';
			}
			sout += ',0,true,false.<br>' +
					'They control whether select() is called before &#936;() and ' +
					'whether rank() is called afterwards, respectively.' +
					'</div>';

			// TODO :: check if this text actually makes sense
			if (GML.verbosity > 9) {
				sout += '<div style="margin-top:20px">' +
						'Just in case you are as forgetful as I am:<br>' +
						'Assume we have position <i>x</i>, then call ' +
						'LF(<i>x</i>,<i>x</i>,BWT[<i>x</i>]) ' +
						'to get all nodes <b>preceding</b> <i>x</i>, ' +
						'call &#936;(<i>x</i>,<i>0</i>) to get the first node <b>succeeding</b> <i>x</i> and ' +
						'&#936;(<i>x</i>,<i>1</i>) to get the second node <b>succeeding</b> <i>x</i>, etc.<br>' +
						'To get the prefix of node <i>x</i>, use BWT[&#936;(<i>x</i>,<i>0</i>)] + ' +
						'BWT[&#936;(&#936;(<i>x</i>,<i>0</i>),&#936;(<i>x</i>,<i>0</i>))] + ...<br>' +
						'(This here is just a very general reminder and completely ' +
						'ignores that you need to keep track of different indexing methods.)'
						'</div>';
			}

			sout += '</div>';

			// replace '^' with '#' before printout
			sout = sout.replace(/\^/g, '#');

			document.getElementById('div-xbw-' + tab).innerHTML = sout;

			if (role === 3) {

				this.refreshAllSubTablesExcept();
				this.refreshAllSubGraphsExcept();

			} else {

				this.refreshTable();
				this.refreshGraph();
			}
		},
		get_alph_and_C_str: function() {

			return ['{' + alph.join(', ') + '}', GML.printKeyValArr(alph, C, true)];
		},

		_generateGMLfilecontent: function(mainpath_offset, path_namespace) {

			if (!mainpath_offset) {
				mainpath_offset = 0;
			}
			if (!path_namespace) {
				path_namespace = '';
			}

			// create an automaton
			auto = GML.getAutomatonFromFindex(this._publishFindex());

			GML.checkAutomatonIsValid(auto);
			if (GML.error_flag) {
				return;
			}

			var further_edges = [];
			var done_edges = [];

			// put all edges out of the start node on the to-do-list
			for (var j=1; j < auto[0].n.length; j++) {
				further_edges.push(0 + '_' + auto[0].n[j]);
			}

			var auto_to_path = [];
			var k = 0;
			auto_to_path[0] = ['mp', k];

			var mainpath = '';
			var i = auto[0].n[0];

			while (auto[i].c !== GML.DS) {

				// put all edges out of the main row on the to-do-list
				for (var j=1; j < auto[i].n.length; j++) {
					further_edges.push(i + '_' + auto[i].n[j]);
				}
				k++;
				auto_to_path[i] = ['mp', k];

				mainpath += auto[i].c;
				var old_i = i;
				i = auto[i].n[0];
				done_edges.push(old_i + '_' + i);
			}

			var infoblocks = '';
			var fpa, from_n, to_n;

			// add direct paths from main row to main row
			for (var fp=0; fp < further_edges.length; fp++) {
				fpa = further_edges[fp].split('_');
				from_n = auto_to_path[fpa[0]];
				to_n = auto_to_path[fpa[1]];
				if (from_n && to_n && (from_n[0] === 'mp') && (to_n[0] === 'mp')) {
					infoblocks += ',' + (from_n[1] + mainpath_offset) + ',,' + (to_n[1] + mainpath_offset) + ';';
					done_edges.push(further_edges[fp]);
					further_edges.splice(fp, 1);
					fp--;
				}
			}

			// add all other edges to further_edges
			for (var fp=0; fp < further_edges.length; fp++) {
				fpa = further_edges[fp].split('_');
				for (var j=0; j < auto[fpa[1]].n.length; j++) {
					var newly_proposed_edge = fpa[1] + '_' + auto[fpa[1]].n[j];
					if ((done_edges.indexOf(newly_proposed_edge) < 0) &&
						(further_edges.indexOf(newly_proposed_edge) < 0)) {
						further_edges.push(newly_proposed_edge);
					}
				}
			}

			var named_path_amount = 0;
			var path_name;

			// add non-direct paths from main row to main row
			while (further_edges.length > 0) {
				for (var fp=0; fp < further_edges.length; fp++) {
					fpa = further_edges[fp].split('_');
					from_n = auto_to_path[fpa[0]];

					// if we find a path starting at a node which we already have in the GML file...
					if (from_n) {
						to_n = auto_to_path[fpa[1]];
						k = 0;
						named_path_amount++;
						path_name = 'p' + path_namespace + named_path_amount;
						path_contains = '';

						// ... then we advance from this node forwards until we emerge at another node
						// which we already have in the file ...
						// if the automaton is not malformed, then the check for (further_edges.length > 0)
						// is not really necessary, but better be safe than crash ^^
						while ((to_n === undefined) && (further_edges.length > 0)) {
							auto_to_path[fpa[1]] = [path_name, k];
							k++;
							path_contains += auto[fpa[1]].c;
							done_edges.push(further_edges[fp]);
							further_edges.splice(fp, 1);
							for (var fp2=0; fp2 < further_edges.length; fp2++) {
								if (further_edges[fp2].split('_')[0] === fpa[1]) {
									fp = fp2;
									fpa = further_edges[fp].split('_');
									to_n = auto_to_path[fpa[1]];
									break;
								}
							}
						}

						// if the automaton is not malformed, then the check for (to_n === undefined)
						// is not really necessary, but better be safe than crash ^^
						if (to_n === undefined) {
							infoblocks += 'error;';
						} else {
							// ... and add this path as new infoblock
							var from_node = from_n[0] + ':';
							if (from_node === 'mp:') {
								from_node = '';
								from_n[1] += mainpath_offset;
							}
							var to_node = to_n[0] + ':';
							if (to_node === 'mp:') {
								to_node = '';
								to_n[1] += mainpath_offset;
							}
							infoblocks += path_name + ',' + from_node + from_n[1] + ',' + path_contains + ',' + to_node + to_n[1] + ';';
						}
						done_edges.push(further_edges[fp]);
						further_edges.splice(fp, 1);
						break;
					}
				}
			}

			return [mainpath, infoblocks];
		},

		_generateGMLfile: function(mime) {

			var source;
			var infoblocks;

			GML.error_flag = false;

			switch (role) {
				case 1:
					source = '>regular_graph' + GML_UI.file_nl;

					var filecontent = this._generateGMLfilecontent();
					source += filecontent[0];
					infoblocks = filecontent[1];

					break;
				case 3:
					var mainpath = '';
					infoblocks = '';

					for (var sx = 0; sx < subXBWs.length; sx++) {
						var filecontent = subXBWs[sx]._generateGMLfilecontent(mainpath.length, '_'+sx+'_');
						mainpath += filecontent[0];
						infoblocks += filecontent[1];
					}
					source = '>fused_graph' + GML_UI.file_nl + mainpath;
					break;
				default:
					alert('This XBW environment is not currently performing a role in which it can save its contents.');
					return;
			}

			if (GML.error_flag) {
				alert('Cannot convert flat XBW table to automaton. \nAborting GML file generation.');
			}

			if (infoblocks.length > 0) {
				source += '|' + infoblocks.slice(0, infoblocks.length-1);
			}

			var url = "data:" + mime + ","+encodeURIComponent(source);

			window.open(url, '_blank');
		},
		exportAsGML: function() {
			this._generateGMLfile('text/plain');
		},
		saveAsGML: function() {
			this._generateGMLfile('application/octet-stream');
		},

		_generateFFXfilecontent: function() {

			var source = '';

			source += 'B\t' + BWT + GML_UI.file_nl;
			source += 'M\t' + M + GML_UI.file_nl;
			source += 'F\t' + F + GML_UI.file_nl;
			source += 'O\t' + GML.printKeyValArr(alph, ord, true, true) + GML_UI.file_nl;
			source += 'C\t' + GML.printKeyValArr(alph, C, true, true);

			return source;
		},
		_generateFFXfile: function(mime) {

			var source;

			switch (role) {
				case 1:
					source = '>regular_graph' + GML_UI.file_nl;
					source += this._generateFFXfilecontent();
					break;
				case 3:
					source = '';
					for (var sx = 0; sx < subXBWs.length; sx++) {
						if (sx > 0) {
							source += GML_UI.file_nl;
						}
						source += '>fused_graph_' + sx + GML_UI.file_nl;
						source += subXBWs[sx]._generateFFXfilecontent();
					}
					break;
				default:
					alert('This XBW environment is not currently performing a role in which it can save its contents.');
					return;
			}

			var url = "data:" + mime + ","+encodeURIComponent(source);

			window.open(url, '_blank');
		},
		exportAsFFX: function() {
			this._generateFFXfile('text/plain');
		},
		saveAsFFX: function() {
			this._generateFFXfile('application/octet-stream');
		},

		// in highlight_arr and extra_highlight_arr, we use
		// 0 for i, 1 for BWT, 2 for FiC, 3 for M and 4 for F
		generateTable: function(highlight_arr, extra_highlight_arr) {

			if (!highlight_arr) {
				highlight_arr = [];
			}
			if (!extra_highlight_arr) {
				extra_highlight_arr = [];
			}



			var pBWT = BWT;
			var pchar = char;
			var pM = M;
			var pF = F;

			var longlen = Math.max(pBWT.length, pchar.length, pM.length, pF.length);

			while (pBWT.length < longlen) {
				pBWT += ' ';
			}
			while (pchar.length < longlen) {
				pchar += ' ';
			}
			while (pM.length < longlen) {
				pM += ' ';
			}
			while (pF.length < longlen) {
				pF += ' ';
			}



			var sout = '';

			sout += '<table><tbody class="vbars">';

			sout += '<tr>';
			sout += GML.arr_to_extra_high_str(
				GML.count_up_array(longlen),
				highlight_arr[0],
				extra_highlight_arr[0]
			);
			sout += '<td>';
			sout += GML.di;
			sout += '</td></tr>';

			sout += '<tr>';
			sout += GML.arr_to_extra_high_str(pBWT, highlight_arr[1], extra_highlight_arr[1]);
			sout += '<td>BWT</td></tr>';

			sout += '<tr>';
			sout += GML.arr_to_extra_high_str(pchar, highlight_arr[2], extra_highlight_arr[2]);
			sout += '<td>First&nbsp;Column</td></tr>';

			sout += '<tr class="barless">';
			sout += GML.arr_to_extra_high_str(pM, highlight_arr[3], extra_highlight_arr[3]);
			sout += '<td>';
			sout += GML.DM;
			sout += '</td></tr>';

			sout += '<tr class="barless">';
			sout += GML.arr_to_extra_high_str(pF, highlight_arr[4], extra_highlight_arr[4]);
			sout += '<td>';
			sout += GML.DF;
			sout += '</td></tr>';

			sout += '</tbody></table>';


			sout = GML.makeVisualsNice(sout);

			return sout;
		},
		generateSubTables: function(hideMergedTable) {

			var r = '';
			for (var i=0; i < subXBWs.length; i++) {
				if (i > 0) {
					r += '&nbsp;&nbsp;&nbsp;';
				}

				var high_bwt = [];
				var high_fic = [];

				if (last_high_bwt[i] > -1) {
					high_bwt = [last_high_bwt[i]];
					for (var j=last_high_bwt[i]+1; j < last_high_bwt[i] + last_high_bwt_arr.length; j++) {
						high_bwt.push(j);
					}
				}
				if (last_high_fic[i] > -1) {
					high_fic = [last_high_fic[i]];
					for (var j=last_high_fic[i]+1; j < last_high_fic[i] + last_high_fic_arr.length; j++) {
						high_fic.push(j);
					}
				}

				// expand purple to right for the entire node
				var bwt_arr = [multi_cur_bwt_int[i]];
				var fic_arr = [multi_cur_fic_int[i]];

				var nodes = subXBWs[i]._publishAllNodes();
				var pF = nodes[3];
				var pM = nodes[2];

				var j = bwt_arr[0]+1;
				while (pF[j] === '0') {
					bwt_arr.push(j);
					j++;
				}

				var j = fic_arr[0]+1;
				while (pM[j] === '0') {
					fic_arr.push(j);
					j++;
				}

				r += subXBWs[i].generateTable([[], bwt_arr, fic_arr, fic_arr, bwt_arr], [[], high_bwt, high_fic, high_fic, high_bwt]);
			}

			if (!hideMergedTable) {
				r += '<br>' + this.generateTable([], [[], last_high_bwt_arr, last_high_fic_arr, last_high_fic_arr, last_high_bwt_arr]);
			}

			return r;
		},
		generateGraph: function(highnodes, show_vis_hl) {

			var sout = GML.visualize(auto, true, highnodes, show_vis_hl);

			// replace '^' with '#' before printout
			sout = sout.replace(/\^/g, '#');
			
			return sout;
		},
		// highnodes are nodes that are highlighted in purple; they use absolute indexing
		// tab is the integer designation of the tab on which we are calling this
		// if show_vis_hl is true, then the extra highlighted nodes and edges in red
		// are displayed too
		refreshGraph: function(highnodes, show_vis_hl, j) {

			var outerdiv;

			if (j === undefined) {
				outerdiv = document.getElementById('div-xbw-' + GML_UI.cur_tab + '-env-graph');
			} else {
				outerdiv = document.getElementById('div-xbw-' + GML_UI.cur_tab + '-env-graph-' + j);
			}

			var prevdispstyle = 'block';
			var prevdispcaption = 'Hide';
			
			if (outerdiv.childNodes[0]) {

				prevdispstyle = outerdiv.childNodes[0].childNodes[2].style.display;
				prevdispcaption = outerdiv.childNodes[0].childNodes[0].childNodes[0].innerHTML;
			}

			outerdiv.innerHTML = this.generateGraph(highnodes, show_vis_hl);

			outerdiv.childNodes[0].childNodes[2].style.display = prevdispstyle;
			outerdiv.childNodes[0].childNodes[0].childNodes[0].innerHTML = prevdispcaption;
		},
		refreshAllSubGraphsExcept: function(except, highnodes, show_vis_hl) {

			var full_auto;

			for (var sx=0; sx < subXBWs.length; sx++) {
				if (sx === 0) {
					full_auto = subXBWs[sx]._publishAuto();
				} else {
					full_auto = GML.mergeAutomata(full_auto, subXBWs[sx]._publishAuto());
				}
				for (var j=0; j < full_auto.length; j++) {
					if (full_auto[j]) {
						if (full_auto[j].o === undefined) {
							full_auto[j].o = sx;
						}
					}
				}

				if (sx !== except) {

					var outerdiv = document.getElementById('div-xbw-' + GML_UI.cur_tab + '-env-graph-' + sx);
					var prevdispstyle = 'block';
					var prevdispcaption = 'Hide';
					if (outerdiv.childNodes[0]) {
						prevdispstyle = outerdiv.childNodes[0].childNodes[2].style.display;
						prevdispcaption = outerdiv.childNodes[0].childNodes[0].childNodes[0].innerHTML;
					}

					outerdiv.innerHTML =
						subXBWs[sx].generateGraph(highnodes, show_vis_hl);

					outerdiv.childNodes[0].childNodes[2].style.display = prevdispstyle;
					outerdiv.childNodes[0].childNodes[0].childNodes[0].innerHTML = prevdispcaption;
				}
			}

			var outerdiv = document.getElementById('div-xbw-' + GML_UI.cur_tab + '-env-graph-together');
			var prevdispstyle = 'block';
			var prevdispcaption = 'Hide';
			if (outerdiv.childNodes[0]) {
				prevdispstyle = outerdiv.childNodes[0].childNodes[2].style.display;
				prevdispcaption = outerdiv.childNodes[0].childNodes[0].childNodes[0].innerHTML;
			}

			outerdiv.innerHTML =
				GML.visualize(full_auto, true, highnodes, show_vis_hl, except).replace(/\^/g, '#');

			outerdiv.childNodes[0].childNodes[2].style.display = prevdispstyle;
			outerdiv.childNodes[0].childNodes[0].childNodes[0].innerHTML = prevdispcaption;
		},
		_publishAuto: function() {
			return auto;
		},
		findHTML: function() {

			if (role === 3) {
				alert('This function has not yet been implemented for fused graphs!');
				return;
			}

			var searchfor = document.getElementById('in-string-' + GML_UI.cur_tab + '-xbw-find').value;

			// replace '#' with '^' before calculations
			searchfor = searchfor.toUpperCase().replace(/\#/g, '^');

			var spep = find(searchfor);

			var res = '[]';
			if (spep.length > 0) {
				res = '[' + (spep[0] + GML.ao) + ', ' + (spep[1] + GML.ao) + ']';
			}

			document.getElementById('span-' + GML_UI.cur_tab + '-xbw-results').innerHTML = res;

			this.show_spep_in_HTML(spep, ['i', 'char'], undefined, undefined, true);
		},
		prefHTML: function() {

			if (role === 3) {
				alert('This function has not yet been implemented for fused graphs!');
				return;
			}

			var inputs = document.getElementById('in-string-' + GML_UI.cur_tab + '-xbw-pref').value.split(',');

			pref_i = parseInt(inputs[0], 10) - GML.ao;

			if (inputs[1] === undefined) {
				inputs[1] = true;
			} else {
				inputs[1] = inputs[1].toUpperCase() === 'TRUE';
			}

			var prefix = this._publishPrefix(pref_i, inputs[1]);

			// replace '^' with '#' before output
			prefix = prefix.split('^').join('#');

			document.getElementById('span-' + GML_UI.cur_tab + '-xbw-results').innerHTML = prefix;

			this.show_spep_in_HTML([pref_i, pref_i], ['i', 'char'], undefined, undefined, true);
		},
		splitNodeHTML: function() {

			var sn_i = document.getElementById('in-string-' + GML_UI.cur_tab + '-xbw-split-node').value;

			sn_i = this.adjustSearchfor0(sn_i);

			if (role === 3) {

				sn_i[0] = parseInt(sn_i[0], 10) - GML.ao;

				subXBWs[sn_i[1]]._splitNode(sn_i[0]);

				subXBWs[sn_i[1]].initFromFindex(subXBWs[sn_i[1]]._publishFindex());
				
				document.getElementById('div-xbw-' + GML_UI.cur_tab + '-env-table-' + sn_i[1]).innerHTML =
					subXBWs[sn_i[1]].generateTable([]);

				subXBWs[sn_i[1]].refreshGraph(undefined, undefined, sn_i[1]);

				this.refreshAllSubTablesExcept(sn_i[1]);
				this.refreshAllSubGraphsExcept(sn_i[1]);

			} else {

				sn_i = parseInt(sn_i, 10) - GML.ao;

				// TODO :: make it more clear what this sn_i is... e.g. in the standard
				// merge example, we can enter i = 14 to achieve node splitting,
				// but why?
				this._splitNode(sn_i);

				// TODO :: do this without converting to findex and back... (curly being done for graph generation)
				this.initFromFindex(this._publishFindex());
				this.generateHTML();
				this.refreshGraph();
			}
		},
		lfHTML: function() {

			if (role === 3) {
				alert('This function has not yet been implemented for fused graphs!');
				return;
			}

			var searchfor = document.getElementById('in-string-' + GML_UI.cur_tab + '-xbw-lf').value;

			// replace '#' with '^' before calculations
			searchfor = searchfor.toUpperCase().replace(/\#/g, '^').split(',');

			var spep = lf(
				[parseInt(searchfor[0], 10) - GML.ao, parseInt(searchfor[1], 10) - GML.ao],
				searchfor[2],
				searchfor[3] !== 'FALSE', // default to true
				searchfor[4] !== 'FALSE'  // default to true
			);

			var res = '[]';
			if (spep.length > 0) {
				res = '[' + (spep[0] + GML.ao) + ', ' + (spep[1] + GML.ao) + ']';
			}
			document.getElementById('span-' + GML_UI.cur_tab + '-xbw-results').innerHTML = res;

			this.show_nodes_in_HTML(spep);
		},
		psiHTML: function() {

			if (role === 3) {
				alert('This function has not yet been implemented for fused graphs!');
				return;
			}

			var searchfor = document.getElementById('in-string-' + GML_UI.cur_tab + '-xbw-psi').value.split(',');

			var take_edge_no = searchfor[1];

			if (take_edge_no === undefined) {
				take_edge_no = 0;
			} else {
				take_edge_no = parseInt(take_edge_no, 10) - GML.ao;
			}

			var abs_next_i = psi(
				parseInt(searchfor[0], 10) - GML.ao,
				take_edge_no,
				searchfor[2] !== 'FALSE', // default to true
				searchfor[3] !== 'FALSE'  // default to true
			);

			document.getElementById('span-' + GML_UI.cur_tab + '-xbw-results').innerHTML = (abs_next_i + GML.ao);

			this.show_nodes_in_HTML([abs_next_i, abs_next_i]);
		},
		adjustSearchfor0: function(searchfor0) {

			if (role === 3) {
				searchfor0 = searchfor0.slice(1,searchfor0.length-1).split(';');
				searchfor0[1] = parseInt(searchfor0[1], 10);
			}

			return searchfor0;
		},
		refreshTable: function() {
			document.getElementById('div-xbw-' + GML_UI.cur_tab + '-env-table').innerHTML =
				this.generateTable();
		},
		refreshAllSubTablesExcept: function(except) {

			for (var sx=0; sx < subXBWs.length; sx++) {
				if (sx !== except) {
					document.getElementById('div-xbw-' + GML_UI.cur_tab + '-env-table-' + sx).innerHTML =
						subXBWs[sx].generateTable([]);
				}
			}
		},
		selectHTML: function() {

			var searchfor = document.getElementById('in-string-' + GML_UI.cur_tab + '-xbw-select').value;

			searchfor = searchfor.split(',');
			// replace '#' with '^' before calculations
			searchfor[0] = searchfor[0].toUpperCase().replace(/\#/g, '^');

			searchfor[0] = this.adjustSearchfor0(searchfor[0]);

			var i, j = parseInt(searchfor[2], 10) - GML.ao;

			i = select(searchfor[0], searchfor[1].toUpperCase(), j);

			if (role === 3) {
				document.getElementById('span-' + GML_UI.cur_tab + '-xbw-results').innerHTML =
					'(' + (i[0] + GML.ao) + ',' + i[1] + ')';

				this.show_spep_in_HTML([[i[0], i[0]], i[1]], ['i', searchfor[1].toUpperCase()], i[0], searchfor[0][0]);
			} else {
				document.getElementById('span-' + GML_UI.cur_tab + '-xbw-results').innerHTML =
					(i + GML.ao);

				this.show_spep_in_HTML([i, i], ['i', searchfor[1].toUpperCase()], i, searchfor[0]);
			}
		},
		rankHTML: function() {

			var searchfor = document.getElementById('in-string-' + GML_UI.cur_tab + '-xbw-rank').value;

			// replace '#' with '^' before calculations
			searchfor = searchfor.split(',');
			searchfor[0] = searchfor[0].toUpperCase().replace(/\#/g, '^');

			searchfor[0] = this.adjustSearchfor0(searchfor[0]);

			var i, j = parseInt(searchfor[2], 10) - GML.ao;

			i = rank(searchfor[0], searchfor[1].toUpperCase(), j);

			if (role === 3) {
				document.getElementById('span-' + GML_UI.cur_tab + '-xbw-results').innerHTML =
					'(' + (i[0] + GML.ao) + ',' + i[1] + ')';

				this.show_spep_in_HTML([[i[0], i[0]], i[1]], ['i', searchfor[1].toUpperCase()], j, searchfor[0][0]);
			} else {
				document.getElementById('span-' + GML_UI.cur_tab + '-xbw-results').innerHTML = (i + GML.ao);

				this.show_spep_in_HTML([i, i], ['i', searchfor[1].toUpperCase()], j, searchfor[0]);
			}
		},
		// highlight the nodes in HTML table and graph which lie in the abs_pos_range,
		// given with absolute indexing
		show_nodes_in_HTML: function(abs_pos_range) {

			var highnodes = [];
			var higharr_collection = [];
			
			higharr_collection[0] = []; // i
			higharr_collection[1] = []; // BWT
			higharr_collection[2] = []; // FC
			higharr_collection[3] = []; // M
			higharr_collection[4] = []; // F

			for (var abs_pos = abs_pos_range[0]; abs_pos < abs_pos_range[1] + 1; abs_pos++) {

				var fc_pos_start = select('1', M, abs_pos);
				var fc_pos_end = select('1', M, abs_pos + 1) - 1;

				var bwt_pos_start = select('1', F, abs_pos);
				var bwt_pos_end = select('1', F, abs_pos + 1) - 1;

				higharr_collection[0].push(abs_pos);

				// highnodes use absolute indexing
				highnodes.push(prefixes[abs_pos]);

				for (var i=fc_pos_start; i < fc_pos_end+1; i++) {
					higharr_collection[2].push(i);
					higharr_collection[3].push(i);
				}

				for (var i=bwt_pos_start; i < bwt_pos_end+1; i++) {
					higharr_collection[1].push(i);
					higharr_collection[4].push(i);
				}
			}

			document.getElementById('div-xbw-' + GML_UI.cur_tab + '-env-table').innerHTML =
				this.generateTable(higharr_collection);

			this.refreshGraph(highnodes, false);
		},
		show_spep_in_HTML: function(spep, highrows, override_last_row, override_with, show_vis_hl, j) {

			if (role === 3) {

				var highnodes = subXBWs[spep[1]].show_spep_in_HTML(
					spep[0], highrows, override_last_row, override_with, show_vis_hl, spep[1]);

				this.refreshAllSubTablesExcept(spep[1]);
				this.refreshAllSubGraphsExcept(spep[1], highnodes, show_vis_hl);

				return;
			}

			var higharr = [];
			var highnodes = [];

			if (spep.length > 0) {

				var sp = spep[0];
				var ep = spep[1];

				for (var i=sp; i < ep+1; i++) {
					higharr.push(i);
				}

				var graph_sp = rank('1', M, sp);
				var graph_ep = rank('1', M, ep);

				for (var i=graph_sp; i < graph_ep+1; i++) {
					highnodes.push(prefixes[i]);
				}
			}

			higharr_collection = [];

			if (highrows.indexOf('i') >= 0) {
				higharr_collection.push(higharr);
			} else {
				higharr_collection.push([]);
			}

			if (highrows.indexOf('BWT') >= 0) {
				higharr_collection.push(higharr);
			} else {
				higharr_collection.push([]);
			}

			if (highrows.indexOf('char') >= 0) {
				higharr_collection.push(higharr);
			} else {
				higharr_collection.push([]);
			}

			if (highrows.indexOf('M') >= 0) {
				higharr_collection.push(higharr);
			} else {
				higharr_collection.push([]);
			}

			if (highrows.indexOf('F') >= 0) {
				higharr_collection.push(higharr);
			} else {
				higharr_collection.push([]);
			}

			if (override_last_row !== undefined) {
				lastrow = [];

				switch (highrows[highrows.length-1]) {
					case 'BWT':
						for (var i=0; i <= override_last_row; i++) {
							if (BWT[i] == override_with) {
								lastrow.push(i);
							}
						}
						higharr_collection[1] = lastrow;
							break;
					case 'char':
						for (var i=0; i <= override_last_row; i++) {
							if (char[i] == override_with) {
								lastrow.push(i);
							}
						}
						higharr_collection[2] = lastrow;
							break;
					case 'M':
						for (var i=0; i <= override_last_row; i++) {
							if (M[i] == override_with) {
								lastrow.push(i);
							}
						}
						higharr_collection[3] = lastrow;
							break;
					case 'F':
						for (var i=0; i <= override_last_row; i++) {
							if (F[i] == override_with) {
								lastrow.push(i);
							}
						}
						higharr_collection[4] = lastrow;
							break;
				}
			}

			if (j === undefined) {
				// we are called from within a regular table
				document.getElementById('div-xbw-' + GML_UI.cur_tab + '-env-table').innerHTML =
					this.generateTable(higharr_collection);

				this.refreshGraph(highnodes, show_vis_hl);
			} else {
				// we are called from our host within a fused table
				document.getElementById('div-xbw-' + GML_UI.cur_tab + '-env-table-' + j).innerHTML =
					this.generateTable(higharr_collection);

				this.refreshGraph(highnodes, show_vis_hl, j);
			}

			return highnodes;
		},
	};
};
