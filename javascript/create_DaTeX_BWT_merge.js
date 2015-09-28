// A note about DaTeX:
// In DaTeX, we enclose maths expressions in dollarsigns, and to write an actual dollarsign, we write \S.
// Just sorting \$ would be a problem as lex(\) > lex(c) for a character c, but sorting $ \$ $ is fine,
// as that starts with the dollarsign anyway.

// takes in two unterminated strings
// returns a section in DaTeX about their merging behavior
function create_DaTeX_BWT_merge(h1, h2) {

	/************************************\
		initialization
	\************************************/
	
	// Does h1 contain a graph? - e.g. A(A|C)CA
	// EMRG - for now, this is just bruteforced and shoehorned in here, but it shall be better later on =)
	if (h1.indexOf('(') >= 0) {

		var i = h1.indexOf('(');
		var alt_A = h1.slice(i + 1, i + 2);
		var i = h1.indexOf('|');
		var alt_B = h1.slice(i + 1, i + 2);

		var h1_A = h1.replace('(', '');
		h1_A = h1_A.slice(0, h1_A.indexOf('|')) + h1_A.slice(h1_A.indexOf(')') + 1);

		var h1_B = h1.replace(')', '');
		h1_B = h1_B.slice(0, h1_B.indexOf('(')) + h1_B.slice(h1_B.indexOf('|') + 1);
	
		// create array forms of the input strings
		var h1a = h1.split('');
		var h1a_A = h1_A.split('');
		var h1a_B = h1_B.split('');
		var h2a = h2.split('');

		// create full array based on both strings
		var ha = h1a.concat(['$ \\$_1 $']).concat(h2a).concat(['$ \\$_2 $']);
		var ha_A = h1a_A.concat(['$ \\$_1 $']).concat(h2a).concat(['$ \\$_2 $']);
		var ha_B = h1a_B.concat(['$ \\$_1 $']).concat(h2a).concat(['$ \\$_2 $']);

		// append delimiter character to input arrays
		h1a[h1a.length] = '$ \\$ $';
		h1a_A[h1a_A.length] = '$ \\$ $';
		h1a_B[h1a_B.length] = '$ \\$ $';
		h2a[h2a.length] = '$ \\$ $';

		// create strings based on arrays with delimiters
		var h = ha.join('');
		h1 = h1a.join('');
		h2 = h2a.join('');

		// generate cyclic rotations
		var h_cr_A = create_cyclic_rotations(ha_A, 0, alt_A);
		var h_cr_B = create_cyclic_rotations(ha_B, 0, alt_B);
		var h_cr = h_cr_A.concat(h_cr_B);
		var h1_cr_A = create_cyclic_rotations(h1a_A, 0, alt_A);
		var h1_cr_B = create_cyclic_rotations(h1a_B, 0, alt_B);
		var h1_cr = h1_cr_A.concat(h1_cr_B);
		var h2_cr = create_cyclic_rotations(h2a, h1a.length, '');

		// EMRG - we just changed how pos works internally, now being an array of integer and letter,
		// rather than just an integer ^^

	} else {

		// create array forms of the input strings
		var h1a = h1.split('');
		var h2a = h2.split('');

		// create full array based on both strings
		var ha = h1a.concat(['$ \\$_1 $']).concat(h2a).concat(['$ \\$_2 $']);

		// append delimiter character to input arrays
		h1a[h1a.length] = '$ \\$ $';
		h2a[h2a.length] = '$ \\$ $';

		// create strings based on arrays with delimiters
		var h = ha.join('');
		h1 = h1a.join('');
		h2 = h2a.join('');

		// generate cyclic rotations
		var h_cr = create_cyclic_rotations(ha, 0, '');
		var h1_cr = create_cyclic_rotations(h1a, 0, '');
		var h2_cr = create_cyclic_rotations(h2a, h1a.length, '');

	}

	// sort cyclic rotations
	var h_scr = sort_cyclic_rotations(h_cr);
	var h1_scr = sort_cyclic_rotations(h1_cr);
	var h2_scr = sort_cyclic_rotations(h2_cr);

	// get the positions
	var h_pos = get_pos_from_scr(h_scr);
	var h1_pos = get_pos_from_scr(h1_scr);
	var h2_pos = get_pos_from_scr(h2_scr);

	// get the BWT
	var h_bwt = get_bwt_from_scr(h_scr);
	var h1_bwt = get_bwt_from_scr(h1_scr);
	var h2_bwt = get_bwt_from_scr(h2_scr);



	/************************************\
		write out text for DaTeX
	\************************************/
	
	var sout = '';
	
	sout += " Graph Alignment - BWT Merging\n";
	sout += "¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯\n"

	sout += "We are merging " + h1 + " and " + h2 + ".\n\n";
	
	sout += "That is, we have\n\n";
	
	sout += '$$ H = "' + h + '"           H_1 = "' + h1 + '"           H_2 = "' + h2 + '" $$\n\n';



	// BWT and pos for H

	sout += "To find the intended merging result, ";
	sout += "we first create the cyclic rotations of $ H $:\n\n";

	sout += print_arrofarr(h_cr);

	sout += "\n\n\nAll of the cyclic rotations sorted together:\n\n";
	
	sout += print_arrofarr(h_scr);

	sout += "\n\n\nSo overall we want to find the following positions ";
	sout += "and BWT through merging $ H_1 $ and $ H_2 $:\n\n";

	sout += h_pos.join(', ') + "\n\n";
	sout += h_bwt.join(', ') + "\n\n\n";



	// BWT and pos for H_1 and H_2

	sout += "To achieve this with merging, we generate the BWTs for $ H_1 $ and $ H_2 $.\n\n";

	sout += "We again first write down the cyclic rotations for $ H_1 $:\n\n";

	sout += print_arrofarr(h1_cr);

	sout += "\n\n\nAnd for $ H_2 $:\n\n";

	sout += print_arrofarr(h2_cr);

	sout += "\n\n\nWe now have all of the cyclic rotations sorted together for $ H_1 $:\n\n";

	sout += print_arrofarr(h1_scr);

	sout += "\n\n\nAnd for $ H_2 $:\n\n";

	sout += print_arrofarr(h2_scr);

	sout += "\n\n\nWhich gives us the following positions ";
	sout += "and BWT:\n\n";

	sout += "\\tab{l l}\n";
	sout += "For $ H_1 $: & For $ H_2 $: \\\\\n";
	sout += h1_pos.join(', ') + " & " + h2_pos.join(', ') + " \\\\\n";
	sout += h1_bwt.join(', ') + " & " + h2_bwt.join(', ') + "\n";
	sout += "\\endtab\n\n";

	sout += "Again, what we want to generate from this is\n\n";
	sout += h_pos.join(', ') + "\n\n";
	sout += h_bwt.join(', ') + "\n\n\n";



	// round 1

	sout += "To do so, we basically need to sort the two sorted cyclic ";
	sout += "rotation lists into one big sorted cyclic rotation list. ";
	sout += "However, if we just do that naively, then we are doing just ";
	sout += "as much work as we would have by starting with the full $ H $ in ";
	sout += "the beginning!\n\n";

	sout += "Instead, we can (and should!) take advantage of the fact that ";
	sout += "the two cyclic rotation lists of $ H_1 $ and $ H_2 $ have ";
	sout += "already been sorted.\n\n";

	sout += "One method to achieve this sorting, according to Holt2014, is as follows:\n\n";

	sout += "We create an interleave vector which is 1 in each position ";
	sout += "in which we choose the next element from $ H_1 $ and ";
	sout += "which is 2 in each position in which we choose the ";
	sout += "next element from $ H_2 $ (originally it's 0 and 1, but the ";
	sout += "difference is purely notational.) ";
	sout += "The first interleave vector that we create here simply corresponds "
	sout += "to fully writing out the information for $ H_1 $, followed by "
	sout += "fully writing out the information for $ H_2 $:\n\n";

	sout += "\\tab{" + repjoin(h1_pos.length, 'c', ' | ') + " | ";
	sout += repjoin(h2_pos.length, 'c', ' | ') + " | l}\n";
	sout += h1_pos.join(' & ') + ' & ' + h2_pos.join(' & ') + " & Position \\\\\n";
	sout += h1_bwt.join(' & ') + ' & ' + h2_bwt.join(' & ') + " & BWT \\\\\n";
	sout += repjoin(h1_pos.length, '1', ' & ') + " & ";
	sout += repjoin(h2_pos.length, '2', ' & ') + " & Interleave\n";
	sout += "\\endtab\n\n";

	sout += "The method that we will be using for the next steps is ";
	sout += "to use the first column (sorted alphabetically) ";
	sout += "instead of focusing on the last column (the BWT):\n\n"

	var h1_col1 = add_index_to_col(get_first_n_from_scr(h1_scr, 0), '1');
	var h2_col1 = add_index_to_col(get_first_n_from_scr(h2_scr, 0), '2');
	var h12_cols = sort_indexed_col(h1_col1.concat(h2_col1));
	var h12_itlv = get_index_from_col(h12_cols);
	var itlv_changed =
		repjoin(h1_pos.length, '1', '') + repjoin(h2_pos.length, '2', '') !==
		h12_itlv.join('');

	sout += "\\tab{" + repjoin(h1_col1.length, 'c', ' | ') + " | ";
	sout += repjoin(h2_col1.length, 'c', ' | ') + " | l}\n";
	sout += arr_to_str_wo_index(h1_col1, ' & ') + ' & ' + arr_to_str_wo_index(h2_col1, ' & ') + ' & $ 1^"st" $ column \\\\\n';
	sout += "\\endtab\n\n";

	sout += "Then to append an index to each letter that tells us its origin:\n\n";

	sout += "\\tab{" + repjoin(h1_col1.length, 'c', ' | ') + " | ";
	sout += repjoin(h2_col1.length, 'c', ' | ') + " | l}\n";
	sout += arr_to_str_w_index(h1_col1, ' & ') + ' & ' + arr_to_str_w_index(h2_col1, ' & ') + ' & $ 1^"st" $ column with indices \\\\\n';
	sout += "\\endtab\n\n";

	sout += "And to then sort the entire line alphabetically ";
	sout += "(and to keep track of the character boundaries we introduce extra vertical lines):\n\n";

	sout += "\\tab{" + get_tabline_from_col(h12_cols) + " | l}\n";
	sout += arr_to_str_w_index(h12_cols, ' & ') + ' & Sorted $ 1^"st" $ column \\\\\n';
	sout += "\\endtab\n\n";

	sout += "To finally arrive at the interleave vector by just looking at the indices:\n\n";

	sout += "\\tab{" + get_tabline_from_col(h12_cols) + " | l}\n";
	sout += arr_to_str_w_index(h12_cols, ' & ') + ' & Sorted $ 1^"st" $ column \\\\\n';
	sout += h12_itlv.join(' & ') + ' & New Interleave \\\\\n';
	sout += "\\endtab\n\n";

	sout += "Using this interleave vector to resort the position and BWT that we had before, we get:\n\n";

	var h12_pos = merge_with_interleave(h1_pos, h2_pos, h12_itlv);
	var h12_bwt = merge_with_interleave(h1_bwt, h2_bwt, h12_itlv);

	sout += "\\tab{" + repjoin(h1_col1.length, 'c', ' | ') + " | ";
	sout += repjoin(h2_col1.length, 'c', ' | ') + " | l}\n";
	sout += h1_pos.join(' & ') + ' & ' + h2_pos.join(' & ') + " & Old Position \\\\\n";
	sout += h1_bwt.join(' & ') + ' & ' + h2_bwt.join(' & ') + " & Old BWT \\\\\n";
	sout += repjoin(h1_pos.length, '1', ' & ') + " & ";
	sout += repjoin(h2_pos.length, '2', ' & ') + " & Old Interleave \\\\\n";
	sout += "\\endtab\n\n";

	sout += "\\tab{" + get_tabline_from_col(h12_cols) + " | l}\n";
	sout += h12_pos.join(' & ') + " & New Position \\\\\n";
	sout += h12_bwt.join(' & ') + " & New BWT \\\\\n";
	sout += h12_itlv.join(' & ') + ' & New Interleave \\\\\n';
	sout += "\\endtab\n\n";

	sout += "We have now achieved\n\n";

	sout += h12_pos.join(', ') + "\n\n";
	sout += h12_bwt.join(', ') + "\n\n\n";

	if (itlv_changed) {
		sout += "However, what we actually want is\n\n";
	} else {
		sout += "What we actually want is\n\n";
	}
	sout += h_pos.join(', ') + "\n\n";
	sout += h_bwt.join(', ') + "\n\n\n";

	sout += "In general, we can only be sure that we can stop when the interleave vector ";
	sout += "did not change between runs ";
	if (!itlv_changed) {
		sout += "(as it did now) "
	}
	sout += "or when we see that each letter is in its own group, ";
	sout += "as then no further changes can occur.\n\n\n";



	// rounds 2 and further

	var h12_itlv_new;
	var n = 1;

	while (itlv_changed) {

		n += 1;

		sout += "The previous interleave vector and the new interleave vector are not the same, ";
		sout += "so we need to carry on for another step ";
		sout += "by first selecting the next column.\n";
		sout += "We previously had column " + (n-1) + " (in step " + (n-1) + "), ";
		sout += "so in this step we consider column " + n + ". ";
		sout += "That is, each entry from the new BWT is replaced with the entry from ";
		sout += "column " + n + " that belongs to that position in the BWT.\n";

		sout += "We here keep track of the overall position, as the letter following the first “A” is ";
		sout += "not necessarily the same as the letter following the second “A”, etc.\n\n";

		sout += "To do so, let's have a quick look at the sorted cyclic rotations:\n\n";

		sout += "\\tab{l l}\n";
		sout += "For $ H_1 $:           & For $ H_2 $: \\\\\n";

		var len1 = h1_cr.length;
		var len2 = h2_cr.length;
		var len = Math.max(len1, len2);
		for (var i = 0; i < len; i++) {
			if (i < len1) {
				sout += h1_cr[i].join('');
			}
			sout += ' & ';
			if (i < len2) {
				sout += h2_cr[i].join('');
			}
			sout += ' \\\\\n';
		}
		sout += "\\endtab\n\n";

		var nth = n + '^"th"';
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

		sout += "We find the $ " + nth + " $ column always as the letter following the ";
		sout += "indicated BWT letter, that is, position ";
		sout += "(althewhile keeping track of the character groups that ";
		sout += "had been formed in the previous step, here represented with double lines):\n\n";

		var h12_col = add_indices_to_col(
							merge_with_interleave(
								get_first_n_from_scr(h1_scr, n - 1),
								get_first_n_from_scr(h2_scr, n - 1),
								h12_itlv),
							h12_itlv,
							h12_cols);

		sout += "\\tab{" + get_tabline_from_col(h12_cols) + " | l}\n";
		sout += h12_pos.join(' & ') + " & Position \\\\\n";
		sout += h12_bwt.join(' & ') + " & BWT \\\\\n";
		sout += h12_itlv.join(' & ') + ' & Interleave \\\\\n';
		sout += h12_col.join(' & ') + " & $ " + nth + " $ column \n";
		sout += "\\endtab\n\n";

		sout += "We now add the old interleave vector as indices:\n\n";

		sout += "\\tab{" + get_tabline_from_col(h12_cols) + " | l}\n";
		sout += h12_itlv.join(' & ') + ' & Old Interleave \\\\\n';
		sout += arr_to_str_w_index(h12_col, ' & ') + " & $ " + nth + " $ column \\\\\n";
		sout += "\\endtab\n\n";

		sout += "We now sort alphabetically WITHIN the character groups from the previous step:\n\n";

		h12_cols = sort_indexed_col(h12_col);
		h12_itlv_new = get_index_from_col(h12_cols);
		itlv_changed = did_itlvs_change(h12_itlv, h12_itlv_new);
		h12_itlv = h12_itlv_new;
		h12_pos = merge_with_interleave(h1_pos, h2_pos, h12_itlv);
		h12_bwt = merge_with_interleave(h1_bwt, h2_bwt, h12_itlv);

		sout += "\\tab{" + get_tabline_from_col(h12_cols) + " | l}\n";
		sout += arr_to_str_w_index(h12_cols, ' & ') + " & Sorted $ " + nth + " $ column\n";
		sout += "\\endtab\n\n";

		sout += "We arrive at the following new interleave vector, ";
		if (itlv_changed) {
			sout += "which is different from the old one - meaning that this was ";
			sout += "not the last step:\n\n";
		} else {
			sout += "which is the same as the old one - meaning that this was ";
			sout += "the last step:\n\n";
		}

		sout += "\\tab{" + get_tabline_from_col(h12_cols) + " | l}\n";
		sout += arr_to_str_w_index(h12_cols, ' & ') + " & Sorted $ " + nth + " $ column \\\\\n";
		sout += h12_itlv.join(' & ') + ' & New Interleave \\\\\n';
		sout += "\\endtab\n\n";

		sout += "We can now look at the BWT and the positions according to this interleave vector:\n\n";

		sout += "\\tab{" + get_tabline_from_col(h12_cols) + " | l}\n";
		sout += h12_pos.join(' & ') + " & New Position \\\\\n";
		sout += h12_bwt.join(' & ') + " & New BWT \\\\\n";
		sout += h12_itlv.join(' & ') + ' & New Interleave \\\\\n';
		sout += "\\endtab\n\n";

		sout += "We have now achieved\n\n";

		sout += h12_pos.join(', ') + "\n\n";
		sout += h12_bwt.join(', ') + "\n\n\n";

		if (itlv_changed) {
			sout += "However, what we actually want is\n\n";
		} else {
			sout += "What we actually want is\n\n";
		}
		sout += h_pos.join(', ') + "\n\n";
		sout += h_bwt.join(', ') + "\n\n\n";
	}

	sout += "So if all worked out right these two should be the same and we should be happy.\n\n";



	var currentdate = new Date();
	sout += "Reykjavík, " + currentdate.getDate() + ". " + (currentdate.getMonth() + 1) + ". " + currentdate.getFullYear();

	return sout;
}



// takes in an array of letters, an offset integer and an annotational
//   letter
// gives out an array containing each cyclic rotation of the array
//   (with the last column containing the position plus offset in an
//   array together with the annotational letter,
//   the second-to-last column containing whitespace,
//   and the third-to-last column containing the BWT)
function create_cyclic_rotations(ha, offset, offletter) {

	var aout = [];

	for (var i = 0; i < ha.length; i++) {

		// take the part of ha including and after i,
		// append the part before i,
		// append whitespace,
		// append i as position
		aout.push(ha.slice(i).concat(ha.slice(0, i)).concat(['      ', [i+offset, offletter]]));
	}

	return aout;
}



// takes in an array of cyclic rotations
// gives out the sorted cyclic rotations as array
function sort_cyclic_rotations(h_cr) {

	var aout = [];

	for (var i = 0; i < h_cr.length; i++) {
		aout.push(h_cr[i]);
	}

	aout = aout.sort();

	return aout;
}



// takes in an array of arrays
// gives out a string filled with \n\n separated strings that are the merged arrays in the array
function print_arrofarr(haa) {

	var aout = [];

	for (var i = 0; i < haa.length; i++) {
		aout.push(haa[i].join(''));
	}

	return aout.join("\n\n");
}



// takes in the length of an array arr, a replacement letter and a delimiter
// gives back arr.join(delimiter) after replacing every element of arr with the replacement letter
function repjoin(len, letter, delimiter) {

	len--;

	if (len < 0) {
		return '';
	}

	var sout = letter;

	for (var i = 0; i < len; i++) {
		sout += delimiter + letter;
	}

	return sout;
}



// takes in an array that contains just a column and an index character
// gives out an array that contains the original column in each zeroeth element,
//   the index character in each first element and an integer denoting how many
//   vertical bars follow that character (set to zero) in each second element
function add_index_to_col(h_col, index) {

	var aout = [];

	for (var i = 0; i < h_col.length; i++) {
		aout.push([h_col[i], index, 0]);
	}

	return aout;
}



// takes in an array that contains just a column, an array containing index
//   characters of the same length and an array containing a column with vertical bars
// gives out an array that contains the original column in each zeroeth element,
//   the index characters in each first element and an integer denoting how many
//   vertical bars follow that character (set to the value of the third input) in
//   each second element
function add_indices_to_col(h_col, indices, h_cols) {

	var aout = [];

	for (var i = 0; i < h_col.length; i++) {
		aout.push([h_col[i], indices[i], h_cols[i][2]]);
	}

	return aout;
}



// takes in an array containing a column with indexes
// gives out a string representing the column joined on the delimiter without showing the indexes
function arr_to_str_wo_index(h_col, delimiter) {

	return get_first_n_from_scr(h_col, 0).join(delimiter);
}



// takes in an array containing a column with indexes
// gives out a string representing the column joined on the delimiter while showing the indexes
function arr_to_str_w_index(h_col, delimiter) {

	var len = h_col.length;

	if (len < 1) {
		return '';
	}

	var sout;

	if (h_col[0][0][0] == '$') {
		sout = '$ \\$_' + h_col[0][1] + ' $';
	} else {
		sout = '$ "' + h_col[0][0] + '"_' + h_col[0][1] + ' $';	
	}

	for (var i = 1; i < len; i++) {

		if (h_col[i][0][0] == '$') {
			sout += delimiter + '$ \\$_' + h_col[i][1] + ' $';
		} else {
			sout += delimiter + '$ "' + h_col[i][0] + '"_' + h_col[i][1] + ' $';	
		}
	}

	return sout;
}



// takes in a column array with indices
// gives out the sorted column array (sorted mainly by letters, ties broken by indices)
function sort_indexed_col(h_col) {

	var colout = [[]];
	var colout_cur = 0;

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

	// update the vertical bar count
	for (var i = 0; i < cout.length-1; i++) {
		if ((cout[i][0] !== cout[i+1][0]) || (cout[i][2] > 0)) {
			cout[i][2] += 1;
		}
	}

	return cout;
}



// takes in a column array
// gives back an array containing just the indices
function get_index_from_col(h_col) {

	var aout = [];

	for (var i = 0; i < h_col.length; i++) {
		aout.push(h_col[i][1]);
	}

	return aout;
}



// takes in a column array
// gives back the appropriate tabline for DaTeX
function get_tabline_from_col(h_col) {

	var len = h_col.length;

	if (len < 1) {
		return '';
	}

	var sout = 'c';

	for (var i = 0; i < len-1; i++) {
		sout += ' |';
		for (var j = 0; j < h_col[i][2]; j++) {
			sout += '|';
		}
		sout += ' c';
	}

	return sout;
}



// takes in two arrays h1 and h2 and an interleave vector
// gives back the merged arrays as one array according to the interleave vector
function merge_with_interleave(h1, h2, h12_itlv) {

	var aout = [];
	var i1 = 0;
	var i2 = 0;

	for (var i = 0; i < h12_itlv.length; i++) {
		if (h12_itlv[i] == '1') {
			aout.push(h1[i1]);
			i1++;
		} else {
			aout.push(h2[i2]);
			i2++;
		}
	}

	return aout;
}



// takes in a sorted cyclic rotation array and an integer n
// gives back an array containing the elements of the nth column
function get_first_n_from_scr(h_scr, n) {

	var aout = [];

	for (var i = 0; i < h_scr.length; i++) {
		aout.push(h_scr[i][n]);
	}

	return aout;
}



// takes in a sorted cyclic rotation array and an integer n
// gives back an array containing the elements of the last - nth column
function get_last_n_from_scr(h_scr, n) {

	var aout = [];

	n++;

	for (var i = 0; i < h_scr.length; i++) {
		aout.push(h_scr[i][h_scr[i].length - n]);
	}

	return aout;
}



// takes in a sorted cyclic rotation array
// gives back an array containing the positions (the last column)
function get_pos_from_scr(h_scr) {

	return get_last_n_from_scr(h_scr, 0);
}



// takes in a sorted cyclic rotation array
// gives back an array containing the BWT (the third-last column)
function get_bwt_from_scr(h_scr) {

	return get_last_n_from_scr(h_scr, 2);
}



// takes in two interleave vectors
// gives back true if they are different (so if they changed),
//   and false if they are the same
function did_itlvs_change(itlv1, itlv2) {

	for (var i = 0; i < itlv1.length; i++) {
		if (itlv1[i] !== itlv2[i]) {
			return true;
		}
	}

	return false;
}



// takes nothing in
// gives back an example run
function example() {
	return create_DaTeX_BWT_merge('A(A|C)CA', 'CAAA');
}