concrete NounIce of Noun = CatIce ** open MorphoIce, ResIce, Prelude in {

	flags optimize=all_subs ;

	lin

		--Build a noun phrase from a determiner and a common noun 
		--Det -> CN -> NP - the man
		DetCN det cn = {
			s = \\c => case det.b of {
				Def => det.s ! cn.g ! c ++ cn.adj ! det.n ! c ! det.d ++ cn.noun ! det.n ! det.b ! c ;
				Indef => cn.adj ! det.n ! c ! det.d ++ cn.noun ! det.n ! det.b ! c 
			};
			a = Ag cn.g det.n P3
		};

		-- PN -> NP - John
		UsePN pn = {
			s = \\c => pn.s ! c ;
			a = Ag pn.g Sg P3
		} ;

    		-- Pron -> NP - he
		UsePron p = p ;

		-- Quant -> Num -> Det - these five
		DetQuant quant num = {
			s = \\g,c => quant.s ! num.n ! g ! c ++ num.s ! Nom ;
			n = num.n ;
			b = quant.b ;
			d = quant.d
		} ;

		-- Quant - hinn(-inn)/hin(-in)/hið/(-ið)
		DefArt = {
			s = table {
				Sg => table {
					Masc	=> caseList "hinn" "hinn" "hinum" "hins" ;
					Fem 	=> caseList "hin" "hina" "hinni" "hinnar" ;
					Neutr	=> caseList "hið" "hið" "hinu" "hins" 
				} ;
				Pl => table {
					Masc	=> caseList "hinir" "hina" "hinum" "hinna" ;
					Fem 	=> caseList "hinar" "hinaar" "hinum" "hinna" ; 
					Neutr	=> caseList "hin" "hin" "hinum" "hinna"
				}
			} ;
			b = Def ;
			d = Weak
		} ;

		-- Quant - a/an
		IndefArt = {
			s = \\_,_,_ => [] ;
			b = Indef ;
			d = Strong
		} ;

		-- Build a common noun by elevating a noun:
		-- UseN N -> CN ; UseN2 N2 -> CN
		UseN, UseN2 = \n -> {
			noun = n.s ;
			adj = \\_,_,_ => [] ;
			g = n.g ;
			isPre = True
		} ;

		-- Build a new common noun by adding an adjective phrase to an existing common noun:
		-- AP -> CN -> CN - big house
		AdjCN ap cn = { 
			noun = \\n,c,b => cn.noun ! n ! c ! b ;
			adj = \\n,c,d => ap.s ! APosit d n cn.g ! c ;
			g = cn.g ;
			isPre = ap.isPre
		} ;

}
