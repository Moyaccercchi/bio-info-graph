c = {

	set_to_DaTeX: function() {

		this.give_out_HTML = false;

		this.nl = "\n"; // newline character
		this.nlnl = "\n\n"; // newline character
		this.nlnlnl = "\n\n\n"; // newline character
		this.DS = "$ \\$ $"; // $
		this.DS1 = '$ \\$_1 $'; // $_1
		this.DS2 = '$ \\$_2 $'; // $_2
		this.H1 = 'H_1'; // H_1 while in mathmode
		this.H2 = 'H_2'; // H_2 while in mathmode
		this.DH = '$ H $'; // H
		this.DH1 = '$ H_1 $'; // H_1
		this.DH2 = '$ H_2 $'; // H_2
		this.tabchar = '      '; // a tab (horizontal space)
	},

	set_to_HTML: function() {

		this.give_out_HTML = true;

		this.nl = '\n';
		this.nlnl = '<br>\n';
		this.nlnlnl = '<br><br>\n';
		this.DS = '$';
		this.DS1 = '$<span class="u">1</span>';
		this.DS2 = '$<span class="u">2</span>';
		this.H1 = 'H<span class="u">1</span>';
		this.H2 = 'H<span class="u">2</span>';
		this.DH = 'H';
		this.DH1 = 'H<span class="u">1</span>';
		this.DH2 = 'H<span class="u">2</span>';
		this.tabchar = '&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;';
	},



	// A note about DaTeX:
	// In DaTeX, we enclose maths expressions in dollarsigns, and to write an actual dollarsign, we write \S.
	// Just sorting \$ would be a problem as lex(\) > lex(c) for a character c, but sorting $ \$ $ is fine,
	// as that starts with the dollarsign anyway.

	// takes in two unterminated strings and a boolean (true for HTML output, false for DaTeX output)
	// returns a section in DaTeX about their merging behavior
	create_BWT_merge: function(h1, h2) {

		/************************************\
			initialization
		\************************************/
		
		// Does h1 or h2 contain a graph? - e.g. A(A|C)CA
		var h1_graph = h1.indexOf('(') >= 0;
		var h2_graph = h2.indexOf('(') >= 0;

		if (h1_graph) {

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

			var ha_A = h1a_A.concat([this.DS1]).concat(h2a).concat([this.DS2]);
			var ha_B = h1a_B.concat([this.DS1]).concat(h2a).concat([this.DS2]);

		} else {

			// create array forms of the input strings
			var h1a = h1.split('');
			var h2a = h2.split('');
		}

		// create full array based on both strings
		var ha = h1a.concat([this.DS1]).concat(h2a).concat([this.DS2]);

		// append delimiter character to input arrays
		h1a[h1a.length] = this.DS;
		h2a[h2a.length] = this.DS;

		if (h1_graph) {

			// append delimiter character to input arrays
			h1a_A[h1a_A.length] = this.DS;
			h1a_B[h1a_B.length] = this.DS;

			// generate cyclic rotations
			var h_cr_A = this.create_cyclic_rotations(ha_A, 0, alt_A);
			var h_cr_B = this.create_cyclic_rotations(ha_B, 0, alt_B);
			var h_cr = h_cr_A.concat(h_cr_B);
			var h1_cr_A = this.create_cyclic_rotations(h1a_A, 0, alt_A);
			var h1_cr_B = this.create_cyclic_rotations(h1a_B, 0, alt_B);
			var h1_cr = h1_cr_A.concat(h1_cr_B);

			// we are here implicitly assuming that h1a_A has the same length as h1a_B!
			var h1_len = h1a_A.length;

		} else {

			// generate cyclic rotations
			var h_cr = this.create_cyclic_rotations(ha, 0, '');
			var h1_cr = this.create_cyclic_rotations(h1a, 0, '');
			var h1_len = h1a.length;
		}

		var h2_cr = this.create_cyclic_rotations(h2a, h1_len, '');

		// create strings based on arrays with delimiters
		var h = ha.join('');
		h1 = h1a.join('');
		h2 = h2a.join('');

		// sort cyclic rotations
		var h_scr = this.sort_cyclic_rotations(h_cr);
		var h1_scr = this.sort_cyclic_rotations(h1_cr);
		var h2_scr = this.sort_cyclic_rotations(h2_cr);

		// get the positions
		var h_pos = this.get_pos_from_scr(h_scr);
		var h1_pos = this.get_pos_from_scr(h1_scr);
		var h2_pos = this.get_pos_from_scr(h2_scr);

		// get the BWT
		var h_bwt = this.get_bwt_from_scr(h_scr);
		var h1_bwt = this.get_bwt_from_scr(h1_scr);
		var h2_bwt = this.get_bwt_from_scr(h2_scr);



		/************************************\
			write out text for DaTeX
		\************************************/
		
		var sout = '';
		
		if(!this.give_out_HTML) {
			sout += " Graph Alignment - BWT Merging" + this.nl;
			sout += "¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯" + this.nl
		}

		sout += "We are merging " + this.h1 + " and " + this.h2 + "." + this.nlnl;
		
		sout += "That is, we have" + this.nlnl;
		
		if(this.give_out_HTML) {
			sout += this.nlnl + 'H = ' + this.h + this.tabchar+this.tabchar + this.H1 + ' = ' + this.h1 +
					this.tabchar+this.tabchar + this.H2 + ' = ' + h2 + this.nlnlnl;
		} else {
			sout += '$$ H = "' + this.h + '"' + this.tabchar+this.tabchar + this.H1 + ' = "' + this.h1 +
					'"' + this.tabchar+this.tabchar + this.H2 + ' = "' + h2 + '" $$' + this.nlnl;
		}



		// BWT and pos for H

		sout += "To find the intended merging result, ";
		sout += "we first create the cyclic rotations of " + this.DH + ":" + this.nlnl;

		sout += this.print_arrofarr(h_cr).join(this.nlnl);

		sout += this.nlnlnl + "All of the cyclic rotations sorted together:" + this.nlnl;
		
		sout += this.print_arrofarr(h_scr).join(this.nlnl);

		sout += this.nlnlnl + "So overall we want to find the following positions ";
		sout += "and BWT through merging " + this.DH1 + " and " + this.DH2 + ":" + this.nlnl;

		sout += h_pos.join(', ') + this.nlnl;
		sout += h_bwt.join(', ') + this.nlnlnl;



		// BWT and pos for H_1 and H_2

		sout += "To achieve this with merging, we generate the BWTs for " + this.DH1 +
				" and " + this.DH2 + "." + this.nlnl;

		sout += "We again first write down the cyclic rotations for " + this.DH1 + ":" + this.nlnl;

		sout += this.print_arrofarr(h1_cr).join(this.nlnl);

		sout += this.nlnlnl + "And for " + this.DH2 + ":" + this.nlnl;

		sout += this.print_arrofarr(h2_cr).join(this.nlnl);

		sout += this.nlnlnl + "We now have all of the cyclic rotations sorted together for " +
				this.DH1 + ":" + this.nlnl;

		sout += this.print_arrofarr(h1_scr).join(this.nlnl);

		sout += this.nlnlnl + "And for " + this.DH2 + ":" + this.nlnl;

		sout += this.print_arrofarr(h2_scr).join(this.nlnl);

		sout += this.nlnlnl + "Which gives us the following positions ";
		sout += "and BWT:" + this.nlnl;

		sout += "\\tab{l l}" + this.nl;
		sout += "For " + this.DH1 + ": & For " + this.DH2 + ": \\\\" + this.nl;
		sout += h1_pos.join(', ') + " & " + h2_pos.join(', ') + " \\\\" + this.nl;
		sout += h1_bwt.join(', ') + " & " + h2_bwt.join(', ') + this.nl;
		sout += "\\endtab" + this.nlnl;

		sout += "Again, what we want to generate from this is" + this.nlnl;
		sout += h_pos.join(', ') + this.nlnl;
		sout += h_bwt.join(', ') + this.nlnlnl;



		// round 1

		sout += "To do so, we basically need to sort the two sorted cyclic ";
		sout += "rotation lists into one big sorted cyclic rotation list. ";
		sout += "However, if we just do that naively, then we are doing just ";
		sout += "as much work as we would have by starting with the full " + this.DH + " in ";
		sout += "the beginning!" + this.nlnl;

		sout += "Instead, we can (and should!) take advantage of the fact that ";
		sout += "the two cyclic rotation lists of " + this.DH1 + " and " + this.DH2 + " have ";
		sout += "already been sorted." + this.nlnl;

		sout += "One method to achieve this sorting, according to Holt2014, is as follows:" + this.nlnl;

		sout += "We create an interleave vector which is 1 in each position ";
		sout += "in which we choose the next element from " + this.DH1 + " and ";
		sout += "which is 2 in each position in which we choose the ";
		sout += "next element from " + this.DH2 + " (originally it's 0 and 1, but the ";
		sout += "difference is purely notational.) ";
		sout += "The first interleave vector that we create here simply corresponds "
		sout += "to fully writing out the information for " + this.DH1 + ", followed by "
		sout += "fully writing out the information for " + this.DH2 + ":" + this.nlnl;

		sout += "\\tab{" + this.repjoin(h1_pos.length, 'c', ' | ') + " | ";
		sout += this.repjoin(h2_pos.length, 'c', ' | ') + " | l}" + this.nl;
		sout += h1_pos.join(' & ') + ' & ' + h2_pos.join(' & ') + " & Position \\\\" + this.nl;
		sout += h1_bwt.join(' & ') + ' & ' + h2_bwt.join(' & ') + " & BWT \\\\" + this.nl;
		sout += this.repjoin(h1_pos.length, '1', ' & ') + " & ";
		sout += this.repjoin(h2_pos.length, '2', ' & ') + " & Interleave" + this.nl;
		sout += "\\endtab" + this.nlnl;

		sout += "The method that we will be using for the next steps is ";
		sout += "to use the first column (sorted alphabetically) ";
		sout += "instead of focusing on the last column (the BWT):" + this.nlnl

		var h1_col1 = this.add_index_to_col(this.get_first_n_from_scr(h1_scr, 0), '1');
		var h2_col1 = this.add_index_to_col(this.get_first_n_from_scr(h2_scr, 0), '2');
		var h12_cols = this.sort_indexed_col(h1_col1.concat(h2_col1));
		var h12_itlv = this.get_index_from_col(h12_cols);
		var itlv_changed =
			this.repjoin(h1_pos.length, '1', '') + this.repjoin(h2_pos.length, '2', '') !==
			h12_itlv.join('');

		sout += "\\tab{" + this.repjoin(h1_col1.length, 'c', ' | ') + " | ";
		sout += this.repjoin(h2_col1.length, 'c', ' | ') + " | l}" + this.nl;
		sout += this.arr_to_str_wo_index(h1_col1, ' & ') + ' & ' +
				this.arr_to_str_wo_index(h2_col1, ' & ') + ' & $ 1^"st" $ column \\\\' + this.nl;
		sout += "\\endtab" + this.nlnl;

		sout += "Then to append an index to each letter that tells us its origin:" + this.nlnl;

		sout += "\\tab{" + this.repjoin(h1_col1.length, 'c', ' | ') + " | ";
		sout += this.repjoin(h2_col1.length, 'c', ' | ') + " | l}" + this.nl;
		sout += this.arr_to_str_w_index(h1_col1, ' & ') + ' & ' + this.arr_to_str_w_index(h2_col1, ' & ') +
				' & $ 1^"st" $ column with indices \\\\' + this.nl;
		sout += "\\endtab" + this.nlnl;

		sout += "And to then sort the entire line alphabetically ";
		sout += "(and to keep track of the character boundaries we introduce extra vertical lines):" + this.nlnl;

		sout += "\\tab{" + this.get_tabline_from_col(h12_cols) + " | l}" + this.nl;
		sout += this.arr_to_str_w_index(h12_cols, ' & ') + ' & Sorted $ 1^"st" $ column \\\\' + this.nl;
		sout += "\\endtab" + this.nlnl;

		sout += "To finally arrive at the interleave vector by just looking at the indices:" + this.nlnl;

		sout += "\\tab{" + this.get_tabline_from_col(h12_cols) + " | l}" + this.nl;
		sout += this.arr_to_str_w_index(h12_cols, ' & ') + ' & Sorted $ 1^"st" $ column \\\\' + this.nl;
		sout += h12_itlv.join(' & ') + ' & New Interleave \\\\' + this.nl;
		sout += "\\endtab" + this.nlnl;

		sout += "Using this interleave vector to resort the position and BWT that we had before, we get:" + this.nlnl;

		var h12_pos = this.merge_with_interleave(h1_pos, h2_pos, h12_itlv);
		var h12_bwt = this.merge_with_interleave(h1_bwt, h2_bwt, h12_itlv);

		sout += "\\tab{" + this.repjoin(h1_col1.length, 'c', ' | ') + " | ";
		sout += this.repjoin(h2_col1.length, 'c', ' | ') + " | l}" + this.nl;
		sout += h1_pos.join(' & ') + ' & ' + h2_pos.join(' & ') + " & Old Position \\\\" + this.nl;
		sout += h1_bwt.join(' & ') + ' & ' + h2_bwt.join(' & ') + " & Old BWT \\\\" + this.nl;
		sout += this.repjoin(h1_pos.length, '1', ' & ') + " & ";
		sout += this.repjoin(h2_pos.length, '2', ' & ') + " & Old Interleave \\\\" + this.nl;
		sout += "\\endtab" + this.nlnl;

		sout += "\\tab{" + this.get_tabline_from_col(h12_cols) + " | l}" + this.nl;
		sout += h12_pos.join(' & ') + " & New Position \\\\" + this.nl;
		sout += h12_bwt.join(' & ') + " & New BWT \\\\" + this.nl;
		sout += h12_itlv.join(' & ') + ' & New Interleave \\\\' + this.nl;
		sout += "\\endtab" + this.nlnl;

		sout += "We have now achieved" + this.nlnl;

		sout += h12_pos.join(', ') + this.nlnl;
		sout += h12_bwt.join(', ') + this.nlnlnl;

		if (itlv_changed) {
			sout += "However, what we actually want is" + this.nlnl;
		} else {
			sout += "What we actually want is" + this.nlnl;
		}
		sout += h_pos.join(', ') + this.nlnl;
		sout += h_bwt.join(', ') + this.nlnlnl;

		sout += "In general, we can only be sure that we can stop when the interleave vector ";
		sout += "did not change between runs ";
		if (!itlv_changed) {
			sout += "(as it did now) "
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

			sout += "\\tab{l l}" + this.nl;
			sout += "For " + this.DH1 + ":           & For " + this.DH2 + ": \\\\" + this.nl;

			var h1_cr_as = this.print_arrofarr(h1_cr);
			var h2_cr_as = this.print_arrofarr(h2_cr);
			var len1 = h1_cr_as.length;
			var len2 = h2_cr_as.length;
			var len = Math.max(len1, len2);
			for (var i = 0; i < len; i++) {
				if (i < len1) {
					sout += h1_cr_as[i];
				}
				sout += ' & ';
				if (i < len2) {
					sout += h2_cr_as[i];
				}
				sout += ' \\\\' + this.nl;
			}
			sout += "\\endtab" + this.nlnl;

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
			sout += "had been formed in the previous step, here represented with double lines):" + this.nlnl;

			var h12_col = this.add_indices_to_col(
								this.merge_with_interleave(
									this.get_first_n_from_scr(h1_scr, n - 1),
									this.get_first_n_from_scr(h2_scr, n - 1),
									h12_itlv),
								h12_itlv,
								h12_cols);

			sout += "\\tab{" + this.get_tabline_from_col(h12_cols) + " | l}" + this.nl;
			sout += h12_bwt.join(' & ') + " & BWT \\\\" + this.nl;
			sout += h12_pos.join(' & ') + " & Position \\\\" + this.nl;
			sout += h12_itlv.join(' & ') + ' & Interleave \\\\' + this.nl;
			sout += this.get_first_n_from_scr(h12_col, 0).join(' & ') + " & $ " + this.nth +
					" $ column " + this.nl;
			sout += "\\endtab" + this.nlnl;

			sout += "We now add the old interleave vector as indices:" + this.nlnl;

			sout += "\\tab{" + this.get_tabline_from_col(h12_cols) + " | l}" + this.nl;
			sout += h12_itlv.join(' & ') + ' & Old Interleave \\\\' + this.nl;
			sout += this.arr_to_str_w_index(h12_col, ' & ') + " & $ " + this.nth + " $ column \\\\" + this.nl;
			sout += "\\endtab" + this.nlnl;

			sout += "We now sort alphabetically WITHIN the character groups from the previous step:" + this.nlnl;

			h12_cols = this.sort_indexed_col(h12_col);
			h12_itlv_new = this.get_index_from_col(h12_cols);
			itlv_changed = this.did_itlvs_change(h12_itlv, h12_itlv_new);
			h12_itlv = h12_itlv_new;
			h12_pos = this.merge_with_interleave(h1_pos, h2_pos, h12_itlv);
			h12_bwt = this.merge_with_interleave(h1_bwt, h2_bwt, h12_itlv);

			sout += "\\tab{" + this.get_tabline_from_col(h12_cols) + " | l}" + this.nl;
			sout += this.arr_to_str_w_index(h12_cols, ' & ') + " & Sorted $ " + nth + " $ column" + this.nl;
			sout += "\\endtab" + this.nlnl;

			sout += "We arrive at the following new interleave vector, ";
			if (itlv_changed) {
				sout += "which is different from the old one - meaning that this was ";
				sout += "not the last step:" + this.nlnl;
			} else {
				sout += "which is the same as the old one - meaning that this was ";
				sout += "the last step:" + this.nlnl;
			}

			sout += "\\tab{" + this.get_tabline_from_col(h12_cols) + " | l}" + this.nl;
			sout += this.arr_to_str_w_index(h12_cols, ' & ') + " & Sorted $ " + nth + " $ column \\\\" + this.nl;
			sout += h12_itlv.join(' & ') + ' & New Interleave \\\\' + this.nl;
			sout += "\\endtab" + this.nlnl;

			sout += "We can now look at the BWT and the positions according to this interleave vector:" + this.nlnl;

			sout += "\\tab{" + this.get_tabline_from_col(h12_cols) + " | l}" + this.nl;
			sout += h12_pos.join(' & ') + " & New Position \\\\" + this.nl;
			sout += h12_bwt.join(' & ') + " & New BWT \\\\" + this.nl;
			sout += h12_itlv.join(' & ') + ' & New Interleave \\\\' + this.nl;
			sout += "\\endtab" + this.nlnl;

			sout += "We have now achieved" + this.nlnl;

			sout += h12_pos.join(', ') + this.nlnl;
			sout += h12_bwt.join(', ') + this.nlnlnl;

			if (itlv_changed) {
				sout += "However, what we actually want is" + this.nlnl;
			} else {
				sout += "What we actually want is" + this.nlnl;
			}
			sout += h_pos.join(', ') + this.nlnl;
			sout += h_bwt.join(', ') + this.nlnlnl;
		}

		var expanded = this.expand_pos(h12_pos, h12_bwt);
		if (expanded) {
			sout += "When expanding the merged BWT and its positions, we get" + this.nlnl;

			h12_pos = expanded[0];
			h12_bwt = expanded[1];

			sout += h12_pos.join(', ') + this.nlnl;
			sout += h12_bwt.join(', ') + this.nlnlnl;
		}

		if (this.pos_equals_pos(h12_pos, h_pos)) {
			sout += "We can see that we found exactly what we wanted to achieve, ";
			sout += "and therefore we are happy.";
		} else {
			sout += "So if all worked out right we should have found the same ";
			sout += "as what we wanted to achieve." + this.nl;
			sout += "But they are not actually the same which makes us sad.";
		}



		if (!this.give_out_HTML) {
			var currentdate = new Date();
			sout += this.nlnl+"Reykjavík, " + currentdate.getDate() + ". " + (currentdate.getMonth() + 1) + ". " + currentdate.getFullYear();
		}

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
	// gives back arr.join(delimiter) after replacing every element of arr with the replacement letter
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



	// takes in an array containing a column with indexes
	// gives out a string representing the column joined on the delimiter without showing the indexes
	arr_to_str_wo_index: function(h_col, delimiter) {

		return this.get_first_n_from_scr(h_col, 0).join(delimiter);
	},



	// takes in an array containing a column with indexes
	// gives out a string representing the column joined on the delimiter while showing the indexes
	arr_to_str_w_index: function(h_col, delimiter) {

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
	},



	// takes in a column array with indices
	// gives out the sorted column array (sorted mainly by letters, ties broken by indices)
	sort_indexed_col: function(h_col) {

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
	},



	// takes in a column array
	// gives back an array containing just the indices
	get_index_from_col: function(h_col) {

		var aout = [];

		for (var i = 0; i < h_col.length; i++) {
			aout.push(h_col[i][1]);
		}

		return aout;
	},



	// takes in a column array
	// gives back the appropriate tabline for DaTeX
	get_tabline_from_col: function(h_col) {

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
	},



	// takes in two arrays h1 and h2 and an interleave vector
	// gives back the merged arrays as one array according to the interleave vector
	merge_with_interleave: function(h1, h2, h12_itlv) {

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
	},



	// takes in a sorted cyclic rotation array and an integer n
	// gives back an array containing the elements of the nth column
	get_first_n_from_scr: function(h_scr, n) {

		var aout = [];

		for (var i = 0; i < h_scr.length; i++) {
			aout.push(h_scr[i][n]);
		}

		return aout;
	},



	// takes in a sorted cyclic rotation array and an integer n
	// gives back an array containing the elements of the last - nth column
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

		if (pos[1] == '') {
			// instead of `$ "1"_"" $`, just use `1`
			return pos[0];
		}

		if (this.give_out_HTML) {
			// generate `1<span class="u">2</span>`
			return pos[0] + '<span class="u">' + pos[1] + '</span>';
		} else {
			// generate `$ "1"_"2" $`
			return '$ "' + pos[0] + '"_"' + pos[1] + '" $';
		}
	},



	// takes in a sorted cyclic rotation array
	// gives back an array containing the positions (the last column), in the format
	//   $ "1"_"2" $ where 1 is the first element of the last column and 2 is the second
	get_pos_from_scr: function(h_scr) {

		aout = this.get_last_n_from_scr(h_scr, 0);

		for (var i = 0; i < aout.length; i++) {
			aout[i] = this.pos_to_str(aout[i]);
		}

		return aout;
	},



	// takes in a sorted cyclic rotation array
	// gives back an array containing the BWT (the third-last column)
	get_bwt_from_scr: function(h_scr) {

		return this.get_last_n_from_scr(h_scr, 2);
	},



	// takes in two interleave vectors
	// gives back true if they are different (so if they changed),
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
	// gives back the expanded pos and bwt arrays, in which every flat entry
	//   has been replaced by several graph-y entries,
	//   e.g. ['$ "9"_"A" $', '$ "9"_"C" $', '$ "10"_"A" $', '$ "10"_"C" $'],
	//   OR gives back false in case of no expanding happening at all!
	expand_pos: function(pos, bwt) {

		// collect all of the alternatives on which we want to expand
		var expand_on = [];

		if (this.give_out_HTML) {
			for (var i = 0; i < pos.length; i++) {
				if (pos[i][pos[i].length-1] == '>') {
					var ind = pos[i].slice(pos[i].indexOf('<span class="u">')+16, -7);
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
						opos.push(pos[i] + '<span class="u">' + expand_on[j] + '</span>');
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
	// gives back true if they have the same contents and false if not
	pos_equals_pos: function(pos1, pos2) {

		len = pos1.length;

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



	// takes nothing in
	// gives back an example run
	example: function() {
		return c.create_BWT_merge('A(A|C)CA', 'CAAA');
	},

}

c.set_to_DaTeX();