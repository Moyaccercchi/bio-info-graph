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

		this.no_face();
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

		// focus first input field on the tab, if the tab contains
		// the typical input fields (as opposed to e.g. the options tab)
		var el = document.getElementById('in-string-' + nexttab + '-1');
		if (!el) {
			el = document.getElementById('in-string-' + nexttab);
		}
		if (el) {
			el.focus();
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
					   'If you see this message for a long time, the page may have crashed.<br>' +
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
		Test Data
	*/

	// run predefined tests to check if invalid inputs are recognized
	do_invalid_tests: false,

	construct_tests: [
		'ACEG',
		'ACEG|,1,,3;,2,TB,4',
		'GACGTACCTG|,2,T,4;,3,,5;,6,,10;,6,,8',
		'GACGTACCTG|,2,,4',
		'ACGTAGATTTC|,4,,7',
		'TAATACGCGGGTC|,8,,9',
		'GACGTACTG|,5,C,8;,2,G,5;,5,GT,9',
		'GCTGGCGAGCAG|,3,T,6;,5,TC,12;,5,,7',
		'GCTGGCGAGCAG|,3,T,5;,5,GCTC,12',
		'GACGTACCTG|a,2,TAT,6;,a:0,C,a:2',
		'GACCTAATG|,5,C,8;,2,G,5',
		'ATTCACCACCAACCCGACATA|,4,CACG,9;,9,TCG,12',
		'CCACGCGCCATGGC|,1,CTG,14',
		'GCTGGCGAGCAG|,3,T,5;,5,,7',
		'GCTGGCGAATGTGGCAAGAATGTGGCAAGAGCAG|,3,T,5;,5,,7;,4,CACG,9;,9,TCG,12',
		'GCCGT|,3,,5',
		'ATCT|,2,A,3',
		'ATCGAT|,2,,5;,4,CG,6',
		'TGATGAG|,6,,7',
		'GTTAATGTGGCAAGT',
		'TCCCTGT',
		'CTACCAGGTGCTGTTATTCCAC|,16,A,22',
		'CTATGTTATTCCA|,8,A,13',
		'TATA|,2,A,4',
		'CCA|,1,A,2',
		'TTCGCAAGA',
		'CCTGCGTGTGGT|,3,,6',
		'CCGAGCTTATCGCAA',
		'ATGCGAGTTCGGGAC|,9,CCCG,14',
		'GCAGGGAGCTCCAGCCGTTAG|,4,CC,13;,1,A,18;,17,,19',
		'ATTGGAGAAGTCACGCTTGAC|,6,CTA,14',
		'CACTC',
		'TGCAAGTATGGCGCTT|,2,,8',
		'AACCAT|,4,GGG,6',
		'AACCAT|,4,GGG,6',
		'AGAGCACGCCGGT',
		'AGAGCACGCCGGT',
		'CGGCAATAGACGCTGTCCAATGC|,15,,18',
		'AGAGCAT',
		'ACTGGCATGATTTATCCCTTGG|,11,,12',
		'GGGCCCAGGGCGATCGACGTTC|,7,,19',
		'CCCAGCC|,4,,6',
		'CAGCC|,2,,4',
		'CACT',
		'CACT',
		'TC|,1,C,2',
		'ATCAACTTTCC|,6,CA,7',
		'GCGGTGAAGAGAAA|,2,CGG,7',
		'CCACGCTAAGTTATCGTGT',
		'ATCCAATCGTAAT|,7,T,8',
		'TAGCTTGGAC|,2,AG,9',
		'GACAAACACAAACATCACCCTGT',
		'CGTCGCC|,6,T,7',
		'AAAGTAT',
		'TACTTGTC',
		'GCAGAATTCCGCAGGAAAGC',
		'CGCGACAGTGGCCAATCT',
		'GGTACATGGGAT',
		'TCTGGTACGCTG|,1,,11',
		'GGGAACAGATGTCTGTGATCC|,13,CAT,14',
		'CGAGTTACGTGGCCGCCTCAT',
		'TGTCTGTTACAGATTGC',
		'CGAAGCCTA',
		'CGAAAGAGTGTGTCTAGGC|,11,TC,19',
		'TTTAGGTTAGTACCAC|,8,CTG,11',
		'TACTTCCCCAGGACGGGACGCTA',
		'AAAGGAGGT|,7,,8;,2,A,6',
		'GGTGGCCGAGTGC|,10,CTGC,12',
		'CGAATACCGTACTGAA|,12,AAAA,15;,14,GGTC,15',
		'TTCCAATGGTGAGTCTC',
		'AAGAT|,2,CTC,3;,1,C,4',
		'AACAAATT|,4,TCCT,8',
		'AAAAGAAGCTGAT|,4,,6',
		'ACGTGCCCACCCG|,2,A,8',
		'TAGGCGCGGT',
		'TTTGCATCATATC',
		'TTGTAG|,5,A,6',
		'CTATATAGCGGC|,4,CTA,7',
		'ATTAATACG|,2,G,5',
		'CGGCCT|,1,A,6;,2,TT,4',
		'CATCTTTC|,5,CA,7',
		'AGGCAGCTATCGACCATCTTGCG|,22,,23;,2,TT,9;,9,C,10;,22,,23',
		'AAGAGTCCAGAG|,7,,11;,5,TATA,9',
		'GCCACGACACCCTCAAGCT|,17,CA,18',
		'TTAAA|,4,GAT,5',
		'TACCAGGGCTTTTTTACTGGCT|,8,,19;,20,GCT,21',
		'TGAAGCGCATTCT|,9,,11;,3,,11',
		'CGGATAGCACTCTA|,9,TT,11',
		'TCCCTGGGC',
		'CAAGGTATTGTTAA',
		'CTCGGCTCT|,1,AAGG,2;,4,,6;,8,TG,9',
		'AGGGAGCCTTAACATTTTCG|,13,T,19;,11,AGCG,15;,10,A,12',
		'TCAGGGCGAGC|,6,G,9',
		'GGAGCCAGGCTTGCCC|,7,,11',
		'TGGCCCTCCCCCTACAT',
		'AGGCCATTGATGAAAA|,7,AT,9;,5,CCT,12',
		'GTATGCT|,6,TACC,7',
		'ATTATCGATCA|,3,TCT,7;,10,CGCT,11;,8,TTCA,11',
		'GTAACCTTGAGGAAGG',
		'GTCGAATGATTCGCC|,8,ATT,15',
		'CTATA',
		'GGAAGG',
		'GACGTACCTG|p1,1,AGCTTC,5;p2,1,AT,4;p3,5,A,8;p4,5,A,10;p9,p1:1,T,p2:1;p10,p2:1,ACT,p4:0;,p9:0,G,p10:1;,p2:1,,p4:0',
	],

	invalid_construct_tests: [
		'GACGTACCTG|,2,T,12;,3,,5;,6,,10;,6,,8',
		'GACGTACCTG|,4,T,2;,3,,5;,6,,10;,6,,8',
		'GACGTACCTG|,2,T,4;,a:3,,5;,6,,10;,6,,8',
		'ATCTCAG|,1,GG,4;a,2,C,6;,a:1,G,7',
	],

	merge_tests: [
		'ACEG and BDFK',
		'ACEG|,1,,3 and BDFK',
		'ACEG|,1,,3;,2,TB,4 and BDFK|,2,,4',
		'C and ACTG|,2,,4',
		'C and ACATG',
		'GACGT|,2,T,4;,3,,5 and ACCTG|,1,,5;,1,,3',
		'C and ACTG|,2,T,4',
		'GACGT|,2,C,4;,2,T,4;,3,,5 and ACCTG|,1,,5;,1,,3',
		'GACGT|,2,T,4;,1,C,4;,3,,5 and ACCTG|,1,,5;,1,,3',
		'TAT and C',
		'TT|,1,A,2 and C',
		'TCGTGCGAGG|,1,ACAA,10 and C',
		'TAATACGCGGGTC|,8,,9 and ACCTG|,1,,5;,1,,3',
		'AAGTTTCTTTCGTGCGAGGCCGT|,10,ACAA,19;,16,CGT,20 and ACCTG',
		'ATAGTCAATTGACTGCCGACG and CGGGGTAAAAAAGCGCC',
		'GCCG and GCGC',
		'GCCG and BDFK',
		'GG and CGG',
		'ATCT|,2,A,3 and CC',
		'ATCGAT|,2,,5;,4,CG,6 and C',
		'TGATGAG|,6,,7 and GCCAGGTCAGCCTAGTCCCTG',
		'GTTAATGTGGCAAGT and CTCGCGACGACTAAAGCTGGCC',
		'TCCCTGT and CTCAGCAGAGGCCCAGGCAAA|,13,TA,15',
		'CTACCAGGTGCTGTTATTCCAC|,16,A,22 and CATCGATTT|,3,CGAT,9',
		'CTATGTTATTCCA|,8,A,13 and C',
		'TATA|,2,A,4 and C',
		'CCA|,1,A,2 and CA',
		'TTCGCAAGA and TCCCCAAG',
		'CCTGCGTGTGGT|,3,,6 and CTCTTA|,3,TCG,5',
		'CCGAGCTTATCGCAA and GAGAGAGCAAC',
		'ATGCGAGTTCGGGAC|,9,CCCG,14 and TCCAA',
		'GCAGGGAGCTCCAGCCGTTAG|,4,CC,13;,1,A,18;,17,,19 and CGGAT',
		'ATTGGAGAAGTCACGCTTGAC|,6,CTA,14 and GTTAAAACAC|,6,TC,10',
		'CACTC and GGGCAGTACGTGG|,9,,11;,7,CGCG,8',
		'TGCAAGTATGGCGCTT|,2,,8 and GTGTAATTATGAAC|,13,GATG,14',
		'AACCAT|,4,GGG,6 and GTGCAGCTTTTGG',
		'AACCAT|,4,GGG,6 and TTAAGGTTATGGACCCCCC|,4,GGAT,8;,6,C,12',
		'AGAGCACGCCGGT and TTAAGGTTATGGACCCCCC|,4,GGAT,8;,6,C,12',
		'AGAGCACGCCGGT and CGTAAATAGAG|,7,G,9',
		'CGGCAATAGACGCTGTCCAATGC|,15,,18 and CGTAAATAGAG|,7,G,9',
		'AGAGCAT and AGTGAACACAC',
		'ACTGGCATGATTTATCCCTTGG|,11,,12 and TTTGTTTGACACGCTGC|,16,,17;,11,AGGG,14',
		'GGGCCCAGGGCGATCGACGTTC|,7,,19 and TTTGTTTGACACGCTGC|,16,,17;,11,AGGG,14',
		'CACT and GCGTACG|,5,,7;,1,C,5',
		'CACT and GCGTACG|,4,,7;,1,C,5',
		'TC|,1,C,2 and TC',
		'ATCAACTTTCC|,6,CA,7 and TACCGAACGCCGGTGTGATAA|,6,,8',
		'GCGGTGAAGAGAAA|,2,CGG,7 and GACTC',
		'CCACGCTAAGTTATCGTGT and GCTTTATACGTAAT|,10,CC,13',
		'ATCCAATCGTAAT|,7,T,8 and TCGAATTATCACGATAT|,13,AAA,17;,12,CG,15;,7,,15;,7,GAG,16',
		'TAGCTTGGAC|,2,AG,9 and GTGATGT|,5,,7',
		'GACAAACACAAACATCACCCTGT and TGAAATAGT|,4,T,6',
		'CGTCGCC|,6,T,7 and TCAATGTATAGTCGTGTCTGAAAT|,8,TGCA,23',
		'AAAGTAT and AAAGCGAGCAAAAGATTGACGAA',
		'TACTTGTC and CTCAACT|,6,,7',
		'GCAGAATTCCGCAGGAAAGC and CATGGCGTCTAGGA|,10,GC,12',
		'CGCGACAGTGGCCAATCT and TCGTCTGATGATCTTTGCTCTC|,19,,20',
		'GGTACATGGGAT and ATGCGCTCGAAGCACGAAGG|,6,AT,15',
		'TCTGGTACGCTG|,1,,11 and ATGCAAAAGTTTAAAGTTGTCCGT|,3,CGCT,18',
		'GGGAACAGATGTCTGTGATCC|,13,CAT,14 and TGAAAAAAACTCAGCGGGGAGT',
		'CGAGTTACGTGGCCGCCTCAT and CCGTA|,3,TGT,5',
		'TGTCTGTTACAGATTGC and AGACATCCATC|,4,,11;,10,TC,11',
		'CGAAGCCTA and TTGTGAATATCG|,6,CTG,9',
		'CGAAAGAGTGTGTCTAGGC|,11,TC,19 and AGAGCT',
		'TTTAGGTTAGTACCAC|,8,CTG,11 and CATAGCTTAGCC|,11,,12',
		'TACTTCCCCAGGACGGGACGCTA and GGTCAG',
		'AAAGGAGGT|,7,,8;,2,A,6 and GAGTTATGACG',
		'GGTGGCCGAGTGC|,10,CTGC,12 and CATTTGTGGCGCGCTTGATCTCTG|,10,,20',
		'CGAATACCGTACTGAA|,12,AAAA,15;,14,GGTC,15 and ACAGCGAGAGCAGACG|,14,TC,15;,8,,16',
		'TTCCAATGGTGAGTCTC and ATAAAG|,3,AG,5',
		'AAGAT|,2,CTC,3;,1,C,4 and ACGCTCCTTTGTGG',
		'AACAAATT|,4,TCCT,8 and GAGCGGAGAATCATACGGGC|,16,GAG,20;,1,CGC,17',
		'AAAAGAAGCTGAT|,4,,6 and CTTGAA',
		'ACGTGCCCACCCG|,2,A,8 and CCCCAACTGGGAATTC|,13,TCC,16;,4,TTT,14',
		'TAGGCGCGGT and CTGGGACGG|,4,GTG,7;,2,C,6;,4,TGA,7;,6,C,9',
		'TTTGCATCATATC and GAAACATTTATGACATTA|,4,,12',
		'TTGTAG|,5,A,6 and AACGCGT|,3,,6',
		'CTATATAGCGGC|,4,CTA,7 and ACGCATATTGGTTC',
		'ATTAATACG|,2,G,5 and TTTTTATGGTCCGACTAA',
		'CGGCCT|,1,A,6;,2,TT,4 and TTAATCT',
		'CATCTTTC|,5,CA,7 and GTTGGTGGCCATCGG|,11,,15;,4,GG,14',
		'AGGCAGCTATCGACCATCTTGCG|,22,,23;,2,TT,9;,9,C,10;,22,,23 and AGCTGGGAAAC',
		'AAGAGTCCAGAG|,7,,11;,5,TATA,9 and AGTGGCTGATTGGT',
		'GCCACGACACCCTCAAGCT|,17,CA,18 and TCAATTGCCGGTA',
		'TTAAA|,4,GAT,5 and GAACTCGGGGC|,3,TTGT,5',
		'TACCAGGGCTTTTTTACTGGCT|,8,,19;,20,GCT,21 and CCGGCGGATTGATAGTGC',
		'TGAAGCGCATTCT|,9,,11;,3,,11 and GGATGCGAGCAGTACGTCTCCTTC',
		'CGGATAGCACTCTA|,9,TT,11 and ATGTTGGGCCCTAA|,7,A,12;,8,T,11',
		'TCCCTGGGC and ACGATTCCCTAACCACCCGT|,17,ATG,19;,7,TT,16',
		'CAAGGTATTGTTAA and AGTCTAAGGCTTCGTGTTGGATC|,9,GTA,22',
		'CTCGGCTCT|,1,AAGG,2;,4,,6;,8,TG,9 and AAAGTCTTCAATGT',
		'AGGGAGCCTTAACATTTTCG|,13,T,19;,11,AGCG,15;,10,A,12 and CATGTTGGTTGT',
		'TCAGGGCGAGC|,6,G,9 and GACAACCCAGATTTTCCCGCATT|,10,,18;,15,TAA,17',
		'GGAGCCAGGCTTGCCC|,7,,11 and ACAGGATAGGTCATTGAGTGGGG|,11,CAAC,16',
		'TGGCCCTCCCCCTACAT and TAGGCCGAAGTTTCTCTATCTTG|,13,CAAA,18;,16,AAC,19',
		'AGGCCATTGATGAAAA|,7,AT,9;,5,CCT,12 and GACTG|,2,,3',
		'GTATGCT|,6,TACC,7 and GGACCTACGGCGGTTTAAAA|,8,A,14;,2,GCGA,12',
		'ATTATCGATCA|,3,TCT,7;,10,CGCT,11;,8,TTCA,11 and TGACAAGGTGCTCAGGTGAAA',
		'GTAACCTTGAGGAAGG and GGGATTCAGTAGTTC',
		'GGAAGG and GG',
		'AATTCGGTAATACCGAAG|,15,AT,17 and TAAGATAAGGCAAA|,7,TA,11',
		'CAGAGTATGCTCCGAG and CGATTACGCAT|,8,,10;,3,TA,9',
		'GACGCCGACAAGAGACAAG|,2,CCA,11;,17,TC,18 and AGGTAGT',
		'GTATAAATACAAGCTAAG|,17,CCA,18 and GATGA|,3,TCTC,4',
		'CTAGA|,2,CC,3 and AAGATGAAAGGACCCCCA|,16,,18',
		'GGGGAAAGCAAGATATCAGCCGT and CAAAAAGA|,4,,5;,3,C,5',
		'ATGATG|,4,TC,6 and AAATGAGAT|,7,TGC,9;,5,AG,8',
		'GCCAGA|,3,TAC,5 and CAACTCCAGGTTCCCTTATTG|,11,GTCA,13',
		'GATATGGCGT|,3,A,9;,7,GTCA,10 and AAGACTTATACGC|,11,,13;,3,G,10',
		'GCGGATATTTGGCGGCT|,6,A,14;,12,GTCA,15 and AAGGAAGAAGACTTATACGCC|,18,,20;,10,G,17',
		'TGGAAGCACACCC and TAGGGCGGAAGGG|,5,A,11;,5,TCCT,11;,8,GCCC,12',
		'GTGCCCTAGACCTCCTTAT|,5,,10;,13,CCA,14;,7,GT,11;,3,G,8;,12,TGGG,18 and GTGCA',
		'TGTACTGGCTCCTATAT and TAACACATTAGTCAACCACA|,13,,16;,5,,12',
		'CGACAG|,5,TTCC,6;,3,T,5 and GACCCACGCAAGAAACCGTGA|,10,TAAC,19',
		'CGTTTGCATGAATTTGTTT and AACGTACCATTAAAATAGCGA|,6,,9;,4,TG,20',
		'TACGATCGGA|,2,ATCG,6 and TAAGTTA|,2,CTG,3',
		'GAGTATAAGCCA|,9,,10 and GGCGCGGTCAGTT|,12,GC,13;,2,GAT,6',
		'AACCA|,4,CTTT,5;,4,CA,5;,4,ACG,5 and AGGTTATAGAT|,7,CAT,9;,2,,11',
		'TCCGCTT|,2,TTTG,6;,4,ACT,6;,3,GGTG,6;,5,AACG,7;,2,ATC,5;,4,,5;,3,AA,6;,3,GGA,7 and TCGACATTGCTGCAGGTT|,14,CT,18',
		'CCCAGCC|,4,,6 and TT|,1,AGG,2',
		'CAGCC|,2,,4 and TT|,1,AGG,2',
		'TTCCGCGGCTCCATG|,14,CGTA,15 and ACCGTCCTCA|,8,CCGT,9;,8,C,10;,4,,10',
		'ACTCTGAATGCTGTACACTCA|,5,,19;,13,C,14;,17,AGA,21 and GTTTGGGGA|,6,,9',
		'ACCTG|,3,AAC,5;,1,C,2;,1,C,2 and ACGATGAAACGGATTGTA',
		'TACACG|,4,AAT,6;,1,CCGA,5 and TGGTGTATTATTGCCCTTGGACGC|,20,GA,22;,5,G,17;,8,AATA,10',
		'TGTAGATGAGCATACCCCGA|,4,ACGA,20;,11,,19;,15,TT,19;,6,,11;,13,AA,20 and AAATATCTTTGTTG',
		'CTCATACGAGCGAA|,12,CG,13 and GTGCCCGGGTGCA|,1,TTTC,11',
		'TTTTAATAGTCACCGATCCGG|,17,CGTT,21;,19,TTT,20 and TCGGCATT|,4,ACGA,7;,6,GGA,8;,3,TGA,8',
		'CCCAATGACAATG|,7,TAGA,10;,5,ACCC,10;,4,C,11;,4,,8;,8,C,11;,7,AGGT,9 and GTGTC',
		'GCTAGTTACAACT and GCAAGCGAA|,3,TCG,4;,5,T,8',
		'CAGTTTACAACTGCAGCCGTTA|,7,,12 and TCGCCCGTAACTCAACGTTTAA|,16,CTGG,17;,19,A,20;,19,GC,20',
		'AACCCT|,1,ATCA,3;,2,C,4;,3,,5;,4,TCAT,6 and CCTTCGTACTAGCACAAACT|,5,GGTT,11',
		'TTGCTAACCAGC|,4,TC,5 and GGTGTTTGGGGCCCATACAAAT|,15,A,18;,1,GA,11;,5,CCGC,14;,10,,19;,5,,13',
		'GACGTACCTG|p1,1,AGCTTC,5;p2,1,AT,4;p3,5,A,8;p4,5,A,10;p9,p1:1,T,p2:1;p10,p2:1,ACT,p4:0;,p9:0,G,p10:1;,p2:1,,p4:0 and ACCTG',
	],

	invalid_merge_tests: [
		'CTATA and AAATATG|,3,AGTT,6',
		'GTCGAATGATTCGCC|,8,ATT,15 and GCTTGTGCCTAA|,8,GT,12;,7,TGT,10;,2,A,12',
		'TGGCC|,1,TA,2 and ACGCAGCTGCGAGAATA|,1,ACCC,5',
		'GTAGCAAAGCGGTTTGGGATTG and GGTACCTATAGGGAA|,2,C,5',
		'GAGTGGCCAAGTAGCCGGGTCACA|,23,GTC,24 and CGTACTG|,3,GAGC,7',
		'GGTGGGATGGACTCTGTTGTG|,12,CACA,18 and CTGACTGTCAGGAGCTGA|,3,,9',
		'GGTACGCGTTATCGT and CTGTCTATCTCTAACCC|,12,,16;,2,T,6;,11,GA,12',
		'GTAAC and TTTCACGCGT|,5,C,10;,5,CTC,6;,1,CG,3;,8,A,9',
		'GTAATGACGTAGCCC and ATCATCGCA|,3,CT,8',
		'GGGTTAGCTGTCTAAG|,6,C,9 and ATGATGATA|,5,ATAT,6;,1,TA,2',
		'GTAGAGAATGGACTACTG and TGTGTGTTC|,2,ACG,4',
		'ACGCAAGATGTATC|,3,GGAC,9;,10,G,11 and CCCCT|,2,CTT,5',
		'TCGTCGTGAG|,5,G,6 and GAACCCGGAATCTTA|,2,T,15',
		'ACTCGTGAAG and GGGCA|,1,CTCA,5',
		'ATACGTAATCGGTGGCTCGA|,14,TCG,20 and AGATCGAGCTG|,4,,10;,8,,10;,2,CTA,7',
		'ATGTACACTTCTGAGTTGTCC and GCGTTGCG|,1,G,4',
		'CGGAGACTATCCTATCC|,9,CAG,15;,11,,16 and CAGTCATCGATTAAACCACTCTG|,1,GG,5',
		'TGCTGCTTTGGAAAT|,1,ATTA,14 and GTCCGTAGCAA|,5,GG,7;,1,,7',
		'GCTCTCATGCAGCCCAA and TTTCAGCCTAACAGTACACGGGT|,1,GA,19',
		'CGCCC|,4,GAC,5;,4,,5;,3,ATG,4 and CAAGCTG|,1,GTTA,5;,5,GA,6',
		'ACTCCTACTCCAACTA|,9,GTT,13 and ACTACC|,4,ATG,6',
		'AACTAGGCGCATGTC|,3,CAG,8;,3,GTT,15 and CTTGGC|,3,AGA,6;,2,T,4;,2,CTAT,4;,1,AT,2',
	],

	fuse_tests: [
		'ACEG and BDFK',
		'ACEG|,1,,3 and BDFK',
		'ACEG|,1,,3;,2,TB,4 and BDFK|,2,,4',
		'C and ACTG|,2,,4',
		'C and ACATG',
		'GACGT|,2,T,4;,3,,5 and ACCTG|,1,,5;,1,,3',
		'C and ACTG|,2,T,4',
		'GACGT|,2,C,4;,2,T,4;,3,,5 and ACCTG|,1,,5;,1,,3',
		'GACGT|,2,T,4;,1,C,4;,3,,5 and ACCTG|,1,,5;,1,,3',
		'TAT and C',
		'TT|,1,A,2 and C',
		'TCGTGCGAGG|,1,ACAA,10 and C',
		'TAATACGCGGGTC|,8,,9 and ACCTG|,1,,5;,1,,3',
		'AAGTTTCTTTCGTGCGAGGCCGT|,10,ACAA,19;,16,CGT,20 and ACCTG',
		'ATAGTCAATTGACTGCCGACG and CGGGGTAAAAAAGCGCC',
		'GCCG and GCGC',
		'GCCG and BDFK',
		'GG and CGG',
		'ATCT|,2,A,3 and CC',
		'ATCGAT|,2,,5;,4,CG,6 and C',
		'TGATGAG|,6,,7 and GCCAGGTCAGCCTAGTCCCTG',
		'GTTAATGTGGCAAGT and CTCGCGACGACTAAAGCTGGCC',
		'TCCCTGT and CTCAGCAGAGGCCCAGGCAAA|,13,TA,15',
		'CTACCAGGTGCTGTTATTCCAC|,16,A,22 and CATCGATTT|,3,CGAT,9',
		'CTATGTTATTCCA|,8,A,13 and C',
		'TATA|,2,A,4 and C',
		'CCA|,1,A,2 and CA',
		'TTCGCAAGA and TCCCCAAG',
		'CCTGCGTGTGGT|,3,,6 and CTCTTA|,3,TCG,5',
		'CCGAGCTTATCGCAA and GAGAGAGCAAC',
		'ATGCGAGTTCGGGAC|,9,CCCG,14 and TCCAA',
		'GCAGGGAGCTCCAGCCGTTAG|,4,CC,13;,1,A,18;,17,,19 and CGGAT',
		'ATTGGAGAAGTCACGCTTGAC|,6,CTA,14 and GTTAAAACAC|,6,TC,10',
		'CACTC and GGGCAGTACGTGG|,9,,11;,7,CGCG,8',
		'TGCAAGTATGGCGCTT|,2,,8 and GTGTAATTATGAAC|,13,GATG,14',
		'AACCAT|,4,GGG,6 and GTGCAGCTTTTGG',
		'AACCAT|,4,GGG,6 and TTAAGGTTATGGACCCCCC|,4,GGAT,8;,6,C,12',
		'AGAGCACGCCGGT and TTAAGGTTATGGACCCCCC|,4,GGAT,8;,6,C,12',
		'AGAGCACGCCGGT and CGTAAATAGAG|,7,G,9',
		'CGGCAATAGACGCTGTCCAATGC|,15,,18 and CGTAAATAGAG|,7,G,9',
		'AGAGCAT and AGTGAACACAC',
		'ACTGGCATGATTTATCCCTTGG|,11,,12 and TTTGTTTGACACGCTGC|,16,,17;,11,AGGG,14',
		'GGGCCCAGGGCGATCGACGTTC|,7,,19 and TTTGTTTGACACGCTGC|,16,,17;,11,AGGG,14',
		'CACT and GCGTACG|,5,,7;,1,C,5',
		'CACT and GCGTACG|,4,,7;,1,C,5',
		'TC|,1,C,2 and TC',
		'ATCAACTTTCC|,6,CA,7 and TACCGAACGCCGGTGTGATAA|,6,,8',
		'GCGGTGAAGAGAAA|,2,CGG,7 and GACTC',
		'CCACGCTAAGTTATCGTGT and GCTTTATACGTAAT|,10,CC,13',
		'ATCCAATCGTAAT|,7,T,8 and TCGAATTATCACGATAT|,13,AAA,17;,12,CG,15;,7,,15;,7,GAG,16',
		'TAGCTTGGAC|,2,AG,9 and GTGATGT|,5,,7',
		'GACAAACACAAACATCACCCTGT and TGAAATAGT|,4,T,6',
		'CGTCGCC|,6,T,7 and TCAATGTATAGTCGTGTCTGAAAT|,8,TGCA,23',
		'AAAGTAT and AAAGCGAGCAAAAGATTGACGAA',
		'TACTTGTC and CTCAACT|,6,,7',
		'GCAGAATTCCGCAGGAAAGC and CATGGCGTCTAGGA|,10,GC,12',
		'CGCGACAGTGGCCAATCT and TCGTCTGATGATCTTTGCTCTC|,19,,20',
		'GGTACATGGGAT and ATGCGCTCGAAGCACGAAGG|,6,AT,15',
		'TCTGGTACGCTG|,1,,11 and ATGCAAAAGTTTAAAGTTGTCCGT|,3,CGCT,18',
		'GGGAACAGATGTCTGTGATCC|,13,CAT,14 and TGAAAAAAACTCAGCGGGGAGT',
		'CGAGTTACGTGGCCGCCTCAT and CCGTA|,3,TGT,5',
		'TGTCTGTTACAGATTGC and AGACATCCATC|,4,,11;,10,TC,11',
		'CGAAGCCTA and TTGTGAATATCG|,6,CTG,9',
		'CGAAAGAGTGTGTCTAGGC|,11,TC,19 and AGAGCT',
		'TTTAGGTTAGTACCAC|,8,CTG,11 and CATAGCTTAGCC|,11,,12',
		'TACTTCCCCAGGACGGGACGCTA and GGTCAG',
		'AAAGGAGGT|,7,,8;,2,A,6 and GAGTTATGACG',
		'GGTGGCCGAGTGC|,10,CTGC,12 and CATTTGTGGCGCGCTTGATCTCTG|,10,,20',
		'CGAATACCGTACTGAA|,12,AAAA,15;,14,GGTC,15 and ACAGCGAGAGCAGACG|,14,TC,15;,8,,16',
		'TTCCAATGGTGAGTCTC and ATAAAG|,3,AG,5',
		'AAGAT|,2,CTC,3;,1,C,4 and ACGCTCCTTTGTGG',
		'AAAAGAAGCTGAT|,4,,6 and CTTGAA',
		'ACGTGCCCACCCG|,2,A,8 and CCCCAACTGGGAATTC|,13,TCC,16;,4,TTT,14',
		'TAGGCGCGGT and CTGGGACGG|,4,GTG,7;,2,C,6;,4,TGA,7;,6,C,9',
		'TTTGCATCATATC and GAAACATTTATGACATTA|,4,,12',
		'TTGTAG|,5,A,6 and AACGCGT|,3,,6',
		'CTATATAGCGGC|,4,CTA,7 and ACGCATATTGGTTC',
		'ATTAATACG|,2,G,5 and TTTTTATGGTCCGACTAA',
		'CGGCCT|,1,A,6;,2,TT,4 and TTAATCT',
		'CATCTTTC|,5,CA,7 and GTTGGTGGCCATCGG|,11,,15;,4,GG,14',
		'AGGCAGCTATCGACCATCTTGCG|,22,,23;,2,TT,9;,9,C,10;,22,,23 and AGCTGGGAAAC',
		'AAGAGTCCAGAG|,7,,11;,5,TATA,9 and AGTGGCTGATTGGT',
		'GCCACGACACCCTCAAGCT|,17,CA,18 and TCAATTGCCGGTA',
		'TTAAA|,4,GAT,5 and GAACTCGGGGC|,3,TTGT,5',
		'TACCAGGGCTTTTTTACTGGCT|,8,,19;,20,GCT,21 and CCGGCGGATTGATAGTGC',
		'TGAAGCGCATTCT|,9,,11;,3,,11 and GGATGCGAGCAGTACGTCTCCTTC',
		'CGGATAGCACTCTA|,9,TT,11 and ATGTTGGGCCCTAA|,7,A,12;,8,T,11',
		'TCCCTGGGC and ACGATTCCCTAACCACCCGT|,17,ATG,19;,7,TT,16',
		'CAAGGTATTGTTAA and AGTCTAAGGCTTCGTGTTGGATC|,9,GTA,22',
		'CTCGGCTCT|,1,AAGG,2;,4,,6;,8,TG,9 and AAAGTCTTCAATGT',
		'AGGGAGCCTTAACATTTTCG|,13,T,19;,11,AGCG,15;,10,A,12 and CATGTTGGTTGT',
		'TCAGGGCGAGC|,6,G,9 and GACAACCCAGATTTTCCCGCATT|,10,,18;,15,TAA,17',
		'GGAGCCAGGCTTGCCC|,7,,11 and ACAGGATAGGTCATTGAGTGGGG|,11,CAAC,16',
		'TGGCCCTCCCCCTACAT and TAGGCCGAAGTTTCTCTATCTTG|,13,CAAA,18;,16,AAC,19',
		'AGGCCATTGATGAAAA|,7,AT,9;,5,CCT,12 and GACTG|,2,,3',
		'ATTATCGATCA|,3,TCT,7;,10,CGCT,11;,8,TTCA,11 and TGACAAGGTGCTCAGGTGAAA',
		'GTAACCTTGAGGAAGG and GGGATTCAGTAGTTC',
		'GGAAGG and GG',
		'AATTCGGTAATACCGAAG|,15,AT,17 and TAAGATAAGGCAAA|,7,TA,11',
		'GACGCCGACAAGAGACAAG|,2,CCA,11;,17,TC,18 and AGGTAGT',
		'GTATAAATACAAGCTAAG|,17,CCA,18 and GATGA|,3,TCTC,4',
		'CTAGA|,2,CC,3 and AAGATGAAAGGACCCCCA|,16,,18',
		'ATGATG|,4,TC,6 and AAATGAGAT|,7,TGC,9;,5,AG,8',
		'GCCAGA|,3,TAC,5 and CAACTCCAGGTTCCCTTATTG|,11,GTCA,13',
		'GATATGGCGT|,3,A,9;,7,GTCA,10 and AAGACTTATACGC|,11,,13;,3,G,10',
		'GCGGATATTTGGCGGCT|,6,A,14;,12,GTCA,15 and AAGGAAGAAGACTTATACGCC|,18,,20;,10,G,17',
		'TGGAAGCACACCC and TAGGGCGGAAGGG|,5,A,11;,5,TCCT,11;,8,GCCC,12',
		'GTGCCCTAGACCTCCTTAT|,5,,10;,13,CCA,14;,7,GT,11;,3,G,8;,12,TGGG,18 and GTGCA',
		'TGTACTGGCTCCTATAT and TAACACATTAGTCAACCACA|,13,,16;,5,,12',
		'CGACAG|,5,TTCC,6;,3,T,5 and GACCCACGCAAGAAACCGTGA|,10,TAAC,19',
		'CGTTTGCATGAATTTGTTT and AACGTACCATTAAAATAGCGA|,6,,9;,4,TG,20',
		'TCCGCTT|,2,TTTG,6;,4,ACT,6;,3,GGTG,6;,5,AACG,7;,2,ATC,5;,4,,5;,3,AA,6;,3,GGA,7 and TCGACATTGCTGCAGGTT|,14,CT,18',
		'TTCCGCGGCTCCATG|,14,CGTA,15 and ACCGTCCTCA|,8,CCGT,9;,8,C,10;,4,,10',
		'ACTCTGAATGCTGTACACTCA|,5,,19;,13,C,14;,17,AGA,21 and GTTTGGGGA|,6,,9',
		'ACCTG|,3,AAC,5;,1,C,2;,1,C,2 and ACGATGAAACGGATTGTA',
		'TACACG|,4,AAT,6;,1,CCGA,5 and TGGTGTATTATTGCCCTTGGACGC|,20,GA,22;,5,G,17;,8,AATA,10',
		'TGTAGATGAGCATACCCCGA|,4,ACGA,20;,11,,19;,15,TT,19;,6,,11;,13,AA,20 and AAATATCTTTGTTG',
		'TTTTAATAGTCACCGATCCGG|,17,CGTT,21;,19,TTT,20 and TCGGCATT|,4,ACGA,7;,6,GGA,8;,3,TGA,8',
		'CCCAATGACAATG|,7,TAGA,10;,5,ACCC,10;,4,C,11;,4,,8;,8,C,11;,7,AGGT,9 and GTGTC',
		'GCTAGTTACAACT and GCAAGCGAA|,3,TCG,4;,5,T,8',
		'CAGTTTACAACTGCAGCCGTTA|,7,,12 and TCGCCCGTAACTCAACGTTTAA|,16,CTGG,17;,19,A,20;,19,GC,20',
		'AACCCT|,1,ATCA,3;,2,C,4;,3,,5;,4,TCAT,6 and CCTTCGTACTAGCACAAACT|,5,GGTT,11',
		'GAGTGGCCAAGTAGCCGGGTCACA|,23,GTC,24 and CGTACTG|,3,GAGC,7',
		'ACTCCTACTCCAACTA|,9,GTT,13 and ACTACC|,4,ATG,6',
		'GACGTACCTG|p1,1,AGCTTC,5;p2,1,AT,4;p3,5,A,8;p4,5,A,10;p9,p1:1,T,p2:1;p10,p2:1,ACT,p4:0;,p9:0,G,p10:1;,p2:1,,p4:0 and ACCTG',
	],

	invalid_fuse_tests: [
		'CTATA and AAATATG|,3,AGTT,6',
		'GTCGAATGATTCGCC|,8,ATT,15 and GCTTGTGCCTAA|,8,GT,12;,7,TGT,10;,2,A,12',
		'TGGCC|,1,TA,2 and ACGCAGCTGCGAGAATA|,1,ACCC,5',
		'GTAGCAAAGCGGTTTGGGATTG and GGTACCTATAGGGAA|,2,C,5',
		'GGTGGGATGGACTCTGTTGTG|,12,CACA,18 and CTGACTGTCAGGAGCTGA|,3,,9',
		'GGTACGCGTTATCGT and CTGTCTATCTCTAACCC|,12,,16;,2,T,6;,11,GA,12',
		'GTAAC and TTTCACGCGT|,5,C,10;,5,CTC,6;,1,CG,3;,8,A,9',
		'GTAATGACGTAGCCC and ATCATCGCA|,3,CT,8',
		'GGGTTAGCTGTCTAAG|,6,C,9 and ATGATGATA|,5,ATAT,6;,1,TA,2',
		'GTAGAGAATGGACTACTG and TGTGTGTTC|,2,ACG,4',
		'ACGCAAGATGTATC|,3,GGAC,9;,10,G,11 and CCCCT|,2,CTT,5',
		'TCGTCGTGAG|,5,G,6 and GAACCCGGAATCTTA|,2,T,15',
		'ACTCGTGAAG and GGGCA|,1,CTCA,5',
		'ATACGTAATCGGTGGCTCGA|,14,TCG,20 and AGATCGAGCTG|,4,,10;,8,,10;,2,CTA,7',
		'ATGTACACTTCTGAGTTGTCC and GCGTTGCG|,1,G,4',
		'CGGAGACTATCCTATCC|,9,CAG,15;,11,,16 and CAGTCATCGATTAAACCACTCTG|,1,GG,5',
		'TGCTGCTTTGGAAAT|,1,ATTA,14 and GTCCGTAGCAA|,5,GG,7;,1,,7',
		'GCTCTCATGCAGCCCAA and TTTCAGCCTAACAGTACACGGGT|,1,GA,19',
		'CGCCC|,4,GAC,5;,4,,5;,3,ATG,4 and CAAGCTG|,1,GTTA,5;,5,GA,6',
		'AACTAGGCGCATGTC|,3,CAG,8;,3,GTT,15 and CTTGGC|,3,AGA,6;,2,T,4;,2,CTAT,4;,1,AT,2',
		'AACAAATT|,4,TCCT,8 and GAGCGGAGAATCATACGGGC|,16,GAG,20;,1,CGC,17',
		'GTATGCT|,6,TACC,7 and GGACCTACGGCGGTTTAAAA|,8,A,14;,2,GCGA,12',
		'CAGAGTATGCTCCGAG and CGATTACGCAT|,8,,10;,3,TA,9',
		'GGGGAAAGCAAGATATCAGCCGT and CAAAAAGA|,4,,5;,3,C,5',
		'TACGATCGGA|,2,ATCG,6 and TAAGTTA|,2,CTG,3',
		'GAGTATAAGCCA|,9,,10 and GGCGCGGTCAGTT|,12,GC,13;,2,GAT,6',
		'AACCA|,4,CTTT,5;,4,CA,5;,4,ACG,5 and AGGTTATAGAT|,7,CAT,9;,2,,11',
		'CCCAGCC|,4,,6 and TT|,1,AGG,2',
		'CAGCC|,2,,4 and TT|,1,AGG,2',
		'CTCATACGAGCGAA|,12,CG,13 and GTGCCCGGGTGCA|,1,TTTC,11',
		'TTGCTAACCAGC|,4,TC,5 and GGTGTTTGGGGCCCATACAAAT|,15,A,18;,1,GA,11;,5,CCGC,14;,10,,19;,5,,13',
	],

	test_something: function(tab, tests, what_are_we_doing, func, test_func_s) {

		this['last_' + tab + '_tests'] = tests;

		var old_verbosity = GML.verbosity;
		GML.verbosity = 1;
		var old_hide_xbw_env = GML.hideXBWenvironments;
		GML.hideXBWenvironments = true;

		var el = this.activateDivOut(tab, false, false);
		el.innerHTML = '';

		var err_count = 0;
		var dang_count = 0;

		for (var i=0; i < tests.length; i++) {

			el.innerHTML += '<div id="test_' + tab + '_' + i + '" class="panel">' +
					   '<div>Testing to ' + what_are_we_doing + ' ' + tests[i] + '.' +
					   '<span class="infobtn" onclick="GML_UI.' + test_func_s + '(' + i + ')">Run manually</span>' +
					   '</div>' +
					   '<div class="danger">Crash</div>' +
					   '</div>';

			var sout = '<div>Testing to ' + what_are_we_doing + ' ' + tests[i] + '.' +
					   '<span class="infobtn" onclick="GML_UI.' + test_func_s + '(' + i + ')">Run manually</span>' +
					   '</div>';

			var res = func(tests[i]);
			// we send a sad face right afterwards, so if the NEXT test destroys JS, then sad face is shown
			this.sad_face();

			if (res.indexOf('class="note"') > -1) {
				sout += '<div class="note">Invalid input</div>';
				dang_count++;
			} else {
				if (res.indexOf('class="error"') < 0) {
					sout += '<div class="success">Success</div>';
				} else {
					sout += '<div class="error">Failure</div>';
					err_count++;
				}
			}
			document.getElementById('test_' + tab + '_' + i).innerHTML = sout;
		}

		GML.verbosity = old_verbosity;
		GML.hideXBWenvironments = old_hide_xbw_env;

		sout = '<div>Overall, we executed ' + tests.length + ' tests. <br>' +
						(tests.length-(err_count+dang_count)) + ' of them were successful, ' +
						'while ' + err_count + ' failed.';
		if (dang_count > 0) {
			sout += '<br>Also, ' + dang_count + ' of them contained invalid input, ' +
					'not honoring the core assumptions of the algorithms used.';
		}
		if (err_count > 0) {
			sout += '<br><br>' +
					'<b>Info:</b> It is possible that reported failures actually represent ' +
					'invalid input which was not caught properly by the program.';
		}
		sout += '</div>';

		el.innerHTML += sout;

		if (err_count > 0) {
			this.sad_face();
		} else {
			this.happy_face();
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
		var type = 'in';
		var in_value = document.getElementById('in-string-2').value.toUpperCase();
		if ((in_value === '[FILE CONTENT]') && this.file_storage[2] && this.file_storage[2][1]) {
			type = this.file_storage[2][1][0];
			in_value = this.file_storage[2][1][1];
		}
		el.innerHTML = '<div>' + GML.generate_BWT_advanced(type, in_value) + '</div>';
	},

	run_test_generateAdvancedBWT: function(i) {

		document.getElementById('in-string-2').value = this.last_2_tests[i];

		this.generateAdvancedBWT();
	},

	test_generateAdvancedBWT: function(use_random_data) {

		var cur_tests = this.construct_tests;

		if (this.do_invalid_tests) {
			cur_tests = cur_tests.concat(this.invalid_construct_tests);
		}

		if (use_random_data) {
			cur_tests = [];
			for (var i=0; i < 10; i++) {
				cur_tests.push(GML.generateRandomGraphString());
			}
		}

		this.test_something(
			2,
			cur_tests,
			'generate table for',
			function (test) {return GML.generate_BWT_advanced('in', test);},
			'run_test_generateAdvancedBWT');
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

	run_test_mergeAdvancedBWTs: function(i) {

		var test = this.last_3_tests[i].split(' and ');

		document.getElementById('in-string-3-1').value = test[0];
		document.getElementById('in-string-3-2').value = test[1];

		this.mergeAdvancedBWTs();
	},

	test_mergeAdvancedBWTs: function(use_random_data) {

		var cur_tests = this.merge_tests;

		if (this.do_invalid_tests) {
			cur_tests = cur_tests.concat(this.invalid_merge_tests);
		}

		if (use_random_data) {
			cur_tests = [];
			for (var i=0; i < 10; i++) {
				cur_tests.push(GML.generateRandomGraphString() + ' and ' + GML.generateRandomGraphString());
			}
		}

		this.test_something(
			3,
			cur_tests,
			'merge',
			function (test) {return GML.merge_BWTs_advanced(test);},
			'run_test_mergeAdvancedBWTs');
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

	run_test_mergeGraphXBWs: function(i) {

		var test = this.last_5_tests[i].split(' and ');

		document.getElementById('in-string-5-1').value = test[0];
		document.getElementById('in-string-5-2').value = test[1];

		this.mergeGraphXBWs();
	},

	test_mergeGraphXBWs: function(use_random_data) {

		var cur_tests = this.merge_tests;

		if (this.do_invalid_tests) {
			cur_tests = cur_tests.concat(this.invalid_merge_tests);
		}

		if (use_random_data) {
			cur_tests = [];
			for (var i=0; i < 10; i++) {
				cur_tests.push(GML.generateRandomGraphString() + ' and ' + GML.generateRandomGraphString());
			}
		}

		this.test_something(
			5,
			cur_tests,
			'merge',
			function (test) {return GML.merge_XBWs(test);},
			'run_test_mergeGraphXBWs');
	},



	/*
		Tab 6 - Fuse Two XBWs
	*/

	fuseGraphXBWs: function() {
		var el = this.activateDivOut(6, true, true);

		el.innerHTML = '<div>' + GML.fuse_XBWs(
			document.getElementById('in-string-6-1').value.toUpperCase(),
			document.getElementById('in-string-6-2').value.toUpperCase()) + '</div>';
	},

	run_test_fuseGraphXBWs: function(i) {

		var test = this.last_6_tests[i].split(' and ');

		document.getElementById('in-string-6-1').value = test[0];
		document.getElementById('in-string-6-2').value = test[1];

		this.fuseGraphXBWs();
	},

	test_fuseGraphXBWs: function(use_random_data) {

		var cur_tests = this.fuse_tests;

		if (this.do_invalid_tests) {
			cur_tests = cur_tests.concat(this.invalid_fuse_tests);
		}

		if (use_random_data) {
			cur_tests = [];
			for (var i=0; i < 10; i++) {
				cur_tests.push(GML.generateRandomGraphString() + ' and ' + GML.generateRandomGraphString());
			}
		}

		this.test_something(
			6,
			cur_tests,
			'fuse',
			function (test) {return GML.fuse_XBWs(test);},
			'run_test_fuseGraphXBWs');
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

		var tables = document.getElementById("hide-cont-" + whichOne).getElementsByTagName('table');

		var source = '';

		for (var t=0; t < tables.length; t++) {

			if (t > 0) {
				source += this.file_nl+this.file_nl;
			}

			source += '<table>' + tables[t].innerHTML.replace('\n', '') + '</table>';
		}

		var url = "data:text/plain,"+encodeURIComponent(source);

		window.open(url, '_blank');
	},

	exportLaTeXTable: function(whichOne) {

		this.hideDropdown();

		// get the tbody
		var tables = document.getElementById("hide-cont-" + whichOne).getElementsByTagName('tbody');

		var source = '';

		for (var t=0; t < tables.length; t++) {

			var tbody = tables[t];

			if (t > 0) {
				source += this.file_nl+this.file_nl;
			}

			source += '\\begin{table}[htb]' + this.file_nl;
			source += '\\centering' + this.file_nl;
			source += '\\caption[GML table]{This is a table from GML.}' + this.file_nl;
			source += '\\begin{tabularx}{1.0\\textwidth}{ ';
			
			// iterate over the cells in the first row and define that many columns in the LaTeX table
			var rowlen = tbody.children[0].children.length;
			for (var i=0; i < rowlen; i++) {
				source += '| c ';
			}
			
			source += '| }' + this.file_nl;
			source += '\\hline' + this.file_nl;

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
					source += tbody.children[i].children[j].innerHTML.replace('\n', '');

					if (j < rowlen-1) {
						source += ' & ';
					} else {
						source += '} \\\\ \\hline ' + this.file_nl;
					}
				}
			}

			source += '\\end{tabularx}' + this.file_nl;
			source += '\\label{table:gml_table}' + this.file_nl;
			source += '\\end{table}';
		}

		// replace some HTML commands with some LaTeX commands which do essentially the same =)
		source = source.split('$').join('\\$');
		source = source.split('#').join('\\textbf{\\#}');
		source = source.split('\\textbf{<i>i</i>}').join('$\\boldsymbol{i}$');
		source = source.split('\\textbf{<i>M</i>}').join('$\\boldsymbol{M}$');
		source = source.split('\\textbf{<i>F</i>}').join('$\\boldsymbol{F}$');
		source = source.split('<i>').join('$');
		source = source.split('</i>').join('$');
		source = source.split('First&nbsp;Column').join('FC');
		source = source.split('&nbsp;').join('~');
		source = source.split('\\textbf{\\#}<span class="d">0</span>').join('$\\boldsymbol{\\#}_0$');
		source = source.split('\\$<span class="d">0</span>').join('$\\$_0$');
		source = source.split('$\\$_0$$\\boldsymbol{\\#}_0$').join('$\\$_0\\boldsymbol{\\#}_0$');

		var url = "data:text/plain,"+encodeURIComponent(source);

		window.open(url, '_blank');
	},



	last_file_was: [0, 0, '', ''], // [x, y, z, t] where the last opened file was on tab x the input y,
								   //              z is the ID of the input field and t is the type
								   //              (being either 'ffx' or 'gml')

	openfile: function(type, tab, input) {
		var el_id = tab;
		if (input !== undefined) {
			el_id += '-' + input;
		}
		var el = document.getElementById('file-input-' + el_id + '-' + type);
		if (el) {
			this.last_file_was = [tab, input, 'in-string-' + el_id, type];
			el.click();
		}
	},

	readfile: function(e) {
		var file = e.target.files[0];
		if (!file) {
			return;
		}
		var reader = new FileReader();
		reader.onload = function(e) {
			GML_UI.file_has_been_read(e.target.result);
		};
		reader.readAsText(file);
	},


	file_storage: [],

	file_has_been_read: function(file_contents) {

		// replace \r\n with \n, to be safe with both Windows-y and POSIX-y files
		file_contents = file_contents.replace(/\r?\n/g, "\n");

		// store file contents as array of lines
		file_contents = file_contents.split('\n');

		var el = document.getElementById(this.last_file_was[2]);
		el.value = '[FILE CONTENT]';

		var tab = this.last_file_was[0];
		if (this.file_storage[tab] === undefined) {
			this.file_storage[tab] = [];
		}
		var input = this.last_file_was[1];
		if (input === undefined) {
			input = 1;
		}
		this.file_storage[tab][input] = [this.last_file_was[3], file_contents];
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

	file_nl: '\r\n',

	applyOptions: function() {

		var TRUE = 'X';

		GML.ao = parseInt(document.getElementById('in-options-array-offset').value, 10);
		GML.loop_threshold = parseInt(document.getElementById('in-options-loop-threshold').value, 10);
		GML.vis_width_override = document.getElementById('in-options-svg-width-override').innerHTML == TRUE;
		GML.vis_width_override_value = parseInt(document.getElementById('in-options-svg-width-override-value').value, 10);
		GML.XPC = document.getElementById('in-options-use-xpc').innerHTML == TRUE;

		for (var i = 0; i < this.upToTabs; i++) {
			this.div_out_visibility[i] = false;
			this.xbw_visibility[i] = false;
			this.setJumpDispStyle(i, false);
		}

		GML.hideXBWenvironments = document.getElementById('in-options-show-xbw-envs').innerHTML !== TRUE;

		GML.set_to_HTML();

		var dispstyle = 'none';

		if (document.getElementById('in-options-show-graph').innerHTML == TRUE) {
			dispstyle = 'inline-block';
		}

		GML.show_auto_i = document.getElementById('in-options-show-autoi').innerHTML == TRUE;

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

		GML.do_prefix_doubling = document.getElementById('in-options-do-prefix-doubling').innerHTML == TRUE;

		this.svg_font_latex = document.getElementById('in-options-svg-font-latex').innerHTML == TRUE;
		this.do_invalid_tests = document.getElementById('in-options-do-invalid-tests').innerHTML == TRUE;

		if (document.getElementById('in-options-file-nl').innerHTML == TRUE) {
			this.file_nl = '\r\n';
		} else {
			this.file_nl = '\n';
		}

		GML.vis_show_hashtag = document.getElementById('in-options-show-hashtag').innerHTML == TRUE;
		GML.vis_show_dollarsign = document.getElementById('in-options-show-dollarsign').innerHTML == TRUE;
		GML.vis_always_search_path = document.getElementById('in-options-always-search-path').innerHTML == TRUE;
		GML.vis_find_shortest_path = document.getElementById('in-options-find-shortest-path').innerHTML == TRUE;
		GML.vis_alternate = document.getElementById('in-options-vis-alternate').innerHTML == TRUE;
		GML.vis_invert_colors = document.getElementById('in-options-svg-invert-colors').innerHTML == TRUE;
		GML.vis_add_IO_texts = document.getElementById('in-options-vis-add-io-texts').innerHTML == TRUE;

		this.changeOptions_verbosity_update();

		this.animateApplyBtn(false);
	},

	resetOptions: function() {

		var TRUE = 'X', FALSE = '&nbsp;';

		document.getElementById('in-options-array-offset').value = '0';
		document.getElementById('in-options-loop-threshold').value = '200';

		document.getElementById('in-options-show-xbw-envs').innerHTML = TRUE;
		document.getElementById('in-options-do-invalid-tests').innerHTML = FALSE;
		document.getElementById('in-options-do-prefix-doubling').innerHTML = FALSE;
		document.getElementById('in-options-file-nl').innerHTML = TRUE;
		document.getElementById('in-options-use-xpc').innerHTML = FALSE;
		document.getElementById('in-options-show-graph').innerHTML = FALSE;
		document.getElementById('in-options-show-autoi').innerHTML = FALSE;
		document.getElementById('in-options-show-hashtag').innerHTML = TRUE;
		document.getElementById('in-options-show-dollarsign').innerHTML = TRUE;
		document.getElementById('in-options-always-search-path').innerHTML = FALSE;
		document.getElementById('in-options-find-shortest-path').innerHTML = FALSE;
		document.getElementById('in-options-vis-alternate').innerHTML = TRUE;
		document.getElementById('in-options-svg-width-override').innerHTML = FALSE;
		document.getElementById('in-options-svg-width-override-value').value = '1041';
		document.getElementById('in-options-svg-invert-colors').innerHTML = FALSE;
		document.getElementById('in-options-vis-add-io-texts').innerHTML = FALSE;

		document.getElementById('in-options-svg-font-latex').innerHTML = TRUE;

		this.changeOptions_verbosity_compwidth = 100;

		this.applyOptions();
	},

	happy_face: function() {
		document.getElementsByTagName('body')[0].style.backgroundColor = '#2F2';

		this.lighten_up_slowly();
	},

	sad_face: function() {
		document.getElementsByTagName('body')[0].style.backgroundColor = '#F22';

		this.lighten_up_slowly();
	},

	no_face: function() {
		document.getElementsByTagName('body')[0].style.backgroundColor = '#FFF';
	},

	// TODO :: check if this works in chrome and other browsers (or if the value given
	// back might actually be in hex or in rgba or somesuch)
	lighten_up_id: -1,

	lighten_up_slowly: function() {

		if (this.lighten_up_id > -1) {
			window.clearInterval(this.lighten_up_id);
		}

		this.lighten_up_id = window.setInterval(function() {

			var el = document.getElementsByTagName('body')[0];

			if (el) {
				var col_now = el.style.backgroundColor;
				col_now = col_now.slice(4);
				col_now = col_now.slice(0, -1);
				col_now = col_now.split(', ');
				for (var i=0; i < 3; i++) {
					col_now[i] = Math.min(255, parseInt(col_now[i], 10) + 20);
				}
				el.style.backgroundColor = 'rgb(' + col_now.join(', ') + ')';
				if ((col_now[0] === 255) && (col_now[1] === 255) && (col_now[2] === 255)) {
					window.clearInterval(GML_UI.lighten_up_id);
				}
			}

		}, 50);
	},

};



document.getElementById('file-input-2-gml')
  .addEventListener('change', GML_UI.readfile, false);
document.getElementById('file-input-2-ffx')
  .addEventListener('change', GML_UI.readfile, false);



GML_UI.init();
