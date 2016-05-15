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

		-- A noun phrase already formed can be modified by a $Predet$erminer.
    		-- Predet -> NP -> NP - only the man 
		PredetNP pred np = {
			s = \\c => pred.s ++ np.s ! c ;
			a = np.a
		} ;

		--NP -> V2  -> NP - the man seen
		PPartNP np v2 = {
			s = \\c => np.s ! c ++ v2.s ! VForm VPast Sg P3;-- FIXME verb agreement function
			a = np.a
		} ;

		-- NP -> RS  -> NP - Paris, which is here
		RelNP np rs = {
			s = \\c => np.s ! c ++ frontComma ++ rs.s ! np.a ++ finalComma ;
			a = np.a
		} ;

		-- NP -> Adv -> NP -- Paris today
		AdvNP np adv = {
			s = \\c => np.s ! c ++ adv.s ;
			a = np.a
		} ;

		-- NP -> Adv -> NP - boys, such as ..
		ExtAdvNP np adv = {
			s = \\c => np.s ! c ++ embedInCommas adv.s ;
			a = np.a
		} ;

		-- Quant -> Num -> Det - these five
		DetQuant quant num = {
			s = \\g,c => quant.s ! num.n ! g ! c ++ num.s ! Nom ;
			n = num.n ;
			b = quant.b ;
			d = quant.d
		} ;

		-- Quant -> Num -> Ord -> Det - these five best
		DetQuantOrd quant num ord = {
			s  =        quant.s  ! num.hasCard ! num.n ++ num.s ! Nom ++ ord.s ! Nom; 
			sp = \\c => quant.s  ! num.hasCard ! num.n ++ num.s ! Nom ++ ord.s ! npcase2case c ; 
			n  = num.n ;
			hasNum = True
		} ;

		-- Det -> NP - these five
		DetNP det = {
			-- s = case det.hasNum of {True => \\_ => det.s ; _ => \\c => det.sp ! c} ;
			s = det.sp ;
			a = agrP3 det.n
		} ;

		-- Pron -> Quant - my (house)
		PossPron p = {
			s = \\_,_ => p.s ! NCase Gen ;
			sp = \\_,_,c => p.sp ! npcase2case c
		} ;

		--   NumSg = {s = \\c => []; n = Sg ; hasCard = False} ;
		--   NumPl = {s = \\c => []; n = Pl ; hasCard = False} ;
		---b    NoOrd = {s = []} ;

		--   NumCard n = n ** {hasCard = True} ;

		-- Digits  -> Card - 51
		NumDigits n = {s = n.s ! NCard ; n = n.n} ;
		-- Digits  -> Ord - 51st
		OrdDigits n = {s = n.s ! NOrd} ;

		-- Numeral -> Card - fifty-one
		NumNumeral numeral = {s = numeral.s ! NCard; n = numeral.n} ;
		-- Numeral -> Ord - fifty-first
		OrdNumeral numeral = {s = numeral.s ! NOrd} ;

		-- AdN -> Card -> Card - almost 51
		AdNum adn num = {s = \\c => adn.s ++ num.s!c ; n = num.n} ;

		-- A -> Ord - warmest
		OrdSuperl a = {s = \\c => a.s ! AAdj Superl c } ;

		--Numeral -> A -> Ord - third largest
		OrdNumeralSuperl n a = {s = \\c => n.s ! NOrd ! Nom ++ a.s ! AAdj Superl c } ;

		-- The definate article is usually used as a suffix (for all genders) e.g. 
		-- X-inn in Masculinn Nominative (I think its always -inn).
		-- As a matter of fact, the free standing version of the definate article can only be 
		-- followed by and adjective, i.e. det ++ adj ++ noun, and is rare. I am not quite sure
		-- how to implement this functionality, i.e. having both options with the same meaning.
		-- Therefore, the suffixed version is always used for the time being.
		-- the_Det = mkDet "hinn" "hinn" "hinum" "hins" "hin" "hina" "hinni" "hinnar" "hið" "hið" "hinu" "hins" Sg True Weak; 

		-- DefArt Quant - hinn(-inn)/hin(-in)/hið/(-ið)
			--s  : Number => Gender => Case => Str ; 
		DefArt = {
			s = table {
				Sq => table {
					Masc	=> caseList "hinn" "hinn" "hinum" "hins";
					Fem 	=> caseList "hin" "hina" "hinni" "hinnar" 
					Neutr	=> caseList "hið" "hið" "hinu" "hins" 
				};
				Pl => table {
					Masc	=> caseList "hinir" "hina" "hinum" "hinna";
					Fem 	=> caseList "hinar" "hinaar" "hinum" "hinna"; 
					Neutr	=> caseList "hin" "hin" "hinum" "hinna"
				}
			};
			b = Def;
			d = Weak
		} ;

		-- Quant - a/an
		IndefArt = {
			s = \\_,_,_ => [] ;
			b = Indef;
			d = Strong
		} ;

		-- CN -> NP - (beer)
		MassNP cn = {
			s = \\c => cn.s ! Sg ! c ;
			a = agrP3 Sg
		} ;

		--Build a common noun by elevating a noun:
		--N -> CN
		UseN n = {
			noun = n.s;
			adj = \\_,_,_ => [];
			g = n.g;
			isPre = True
		};

		-- N2 -> CN - mother
		UseN2 n = n ;

		-- N3 -> N2 - distance (from this city)
		Use2N3 f = {
			s = \\n,c => f.s ! n ! Nom ;
			g = f.g ;
			c2 = f.c2
		} ;

		-- N3 -> N2 - distance (to Paris)
		Use3N3 f = {
			s = \\n,c => f.s ! n ! Nom ;
			g = f.g ;
			c2 = f.c3
		} ;

		-- N2 -> NP -> CN - mother of the king
		ComplN2 f x = {
			s = \\n,c => f.s ! n ! Nom ++ f.c2 ++ x.s ! NPAcc ; 
			g = f.g
		} ;

		-- N3 -> NP -> N2 - distance from this city (to Paris)
		ComplN3 f x = {
			s = \\n,c => f.s ! n ! Nom ++ f.c2 ++ x.s ! NPAcc ;
			g = f.g ;
			c2 = f.c3
		} ;

		--Build a new common noun by adding an adjective phrase to an existing common noun:
		--AP -> CN -> CN - big house
		AdjCN ap cn = { 
			noun = \\n,c,b => cn.noun ! n ! c ! b;
			adj = \\n,c,d => ap.s ! d ! n ! cn.g ! c ;
			g = cn.g;
			isPre = ap.isPre
		};

		-- CN -> RS  -> CN - house that John bought
		RelCN cn rs = {
			s = \\n,c => cn.s ! n ! c ++ rs.s ! agrgP3 n cn.g ;
			g = cn.g
		} ;

		-- CN -> Adv -> CN - house on the hill 
		AdvCN cn ad = {
			s = \\n,c => cn.s ! n ! c ++ ad.s ; 
			g = cn.g
		} ;

		-- CN -> SC  -> CN - question where she sleeps
		SentCN cn sc = {
			s = \\n,c => cn.s ! n ! c ++ sc.s ; 
			g = cn.g
		} ;

		-- CN -> NP -> CN - city Paris (, numbers x and y)
		ApposCN cn np = {
			s = \\n,c => cn.s ! n ! Nom ++ np.s ! NCase c ; 
			g = cn.g
		} ;

		-- CN -> NP -> CN - house of Paris, house of mine
		PossNP cn np = {
			s = \\n,c => cn.s ! n ! c ++ "of" ++ np.s ! NPNomPoss ; 
			g = cn.g
		} ;

		-- CN -> NP -> CN - glass of wine
		PartNP cn np = {
			s = \\n,c => cn.s ! n ! c ++ "of" ++ np.s ! NPAcc ;
			g = cn.g
		} ;

		-- Det -> NP -> NP - three of them, some of the boys
		CountNP det np = {
			s = \\c => det.sp ! c ++ "of" ++ np.s ! NPAcc ;
			a = agrP3 det.n
		} ;

		-- DAP -> AP -> DAP - the large (one)
		AdjDAP det ap = {
			s = det.s ++ ap.s ! agrgP3 det.n Masc ;
			n = det.n 
		} ;

		-- Det -> DAP - this (or that) 
		DetDAP d = d ;

}
