concrete NounIce of Noun = CatIce ** open MorphoIce, ResIce, Prelude in {

	flags optimize=all_subs ;

	lin
		-- Noun phrases

		--Build a noun phrase from a determiner and a common noun 
		--Det -> CN -> NP
		DetCN det cn = {
			s = \\c => case det.b of {
				Def => det.s ! cn.g ! c ++ cn.s ! det.n ! det.b ! det.d ! c ;
				Indef => cn.s ! det.n ! det.b ! det.d ! c
			} ;
			rc = cn.rc ! det.n ;
			adv = cn.adv ;
			a = Ag cn.g det.n P3
		} ;

		-- PN -> NP
		UsePN pn = {
			s = \\c => pn.s ! c ;
			rc, adv = [] ;
			a = Ag pn.g Sg P3
		} ;

    		-- Pron -> NP 
		UsePron p = p ** {rc, adv = [] };

		-- NP -> V2  -> NP
		PPartNP np v2 = {
			s = \\c => case np.a of {
				Ag g n p	=> np.s ! c ++ v2.pp ! PStrong n g c -- FIXME hardcoded strong declension 
			} ;
			rc = np.rc ;
			adv = np.adv ;
			a = np.a
		} ;

		-- NP -> Adv -> NP
		AdvNP np adv = np ** {
			adv = np.adv ++ adv.s
		} ;

		-- NP -> Adv -> NP
		ExtAdvNP np adv = np ** {
			adv = np.adv ++ embedInCommas adv.s
		} ;
	
		-- NP -> RS -> NP
		RelNP np rs = np ** {
			rc =  embedInCommas (rs.s ! np.a)
		} ;

		-- Det -> NP 
		DetNP det = {
			s = \\c => det.s ! Neutr ! c ;
			rc, adv  = [] ;
			a = Ag Neutr det.n P3
			
		} ;

		-- Determiners

		-- Quant -> Num -> Det
		DetQuant quant num = {
			s = \\g,c => quant.s ! num.n ! g ! c ++ num.s ! g ! c ;
			n = num.n ;
			b = quant.b ;
			d = quant.d
		} ;

		-- Quant -> Num -> Ord -> Det
		DetQuantOrd quant num ord = {
			s = \\g,c => quant.s ! num.n ! g ! c ++ num.s ! g ! c ++ ord.s ! num.n ! g ! c ;
			n = num.n ;
			b = quant.b ;
			d = quant.d
		} ;

		-- Num - [no numeral, but marked as singular]
		NumSg = {s = \\g,c => []; n = Sg ; hasCard = False} ;

		-- Num - [no numeral, but marked as plural]
		NumPl = {s = \\g,c => []; n = Pl ; hasCard = False} ;

		-- Card -> Num 
		NumCard n = n ** {hasCard = True} ;

		-- AdN -> Card -> Card
		AdNum adn num = {
				s = \\g,c => adn.s ++ num.s ! g ! c ;
				n = num.n
		} ;

		-- A -> Ord 
		--  FIXME : Hardcoded Strong declension and Sg.Masc for now
		--OrdSuperl a = { s = \\n,g,c => a.s ! ASuperl Strong n g ! c } ;

		-- Quant 
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

		-- Quant
		IndefArt = {
			s = \\_,_,_ => [] ;
			b = Indef ;
			d = Strong
		} ;

		-- CN -> NP
		MassNP cn = {
			s = \\c => cn.s ! Sg ! Indef ! Strong ! c ;
			rc = cn.rc ! Sg ;
			adv = cn.adv ;
			a =  Ag cn.g Sg P3
		} ;

		-- Pron -> Quant
		-- FIXME :
		-- 1 :
		-- In regards to the declension
		-- not sure if it should be
		-- mitt (rauða hús (indef))  or (rauða húsið (Def)) mitt
		-- (it could also be (húsið(Def)) mitt (rauða))
		-- atm it is mitt (rauða hús(Indef)) 
		-- 2 :
		-- How should gender be treated in this?
		PossPron p = {
			s = \\_,_,c => p.s ! c ;
			b = Indef ;
			d = Weak 
		} ;


		-- Common Noun

		-- UseN N -> CN ; UseN2 N2 -> CN
		UseN, UseN2 = \noun -> {
			s = \\n,s,_,c => noun.s ! n ! s ! c ;
			g = noun.g ;
			rc = \\_ => [] ;
			adv = []
		} ;

		-- N2 -> NP -> CN
		ComplN2 n2 np = {
			s = \\n,s,_,c => n2.s ! n ! s ! c ++ np.s ! Acc ;
			g = n2.g ;
			rc = \\_ => np.rc ;
			adv = np.adv 
		} ;

		-- N3 -> NP -> N2
		ComplN3 n3 np = {
			s = \\n,s,c => n3.s ! n ! s ! c ++ n3.c2 ++ np.s ! Dat ;
			g = n3.g ;
			c2 = n3.c3
	
		} ;

		-- N3 -> N2
		Use2N3 n3 = {
			s = \\n,s,c => n3.s ! n ! s ! c ;
			g = n3.g ;
			c2 = n3.c2
		} ;

		-- N3 -> N2
		Use3N3 n3 = {
			s = \\n,s,c => n3.s ! n ! s ! c ;
			g = n3.g ;
			c2 = n3.c3
		} ;

		-- AP -> CN -> CN 
		AdjCN ap cn = { 
			s = \\n,s,d,c => ap.s ! n ! cn.g ! d ! c ++ cn.s ! n ! s ! d ! c ;
			g = cn.g ;
			rc = \\n => cn.rc ! n ;
			adv = cn.adv 
		} ;

		-- CN -> RS -> CN
		RelCN cn rs = cn ** {rc = \\n => embedInCommas (rs.s ! Ag cn.g n P3) } ;

		-- CN -> Adv -> CN
		AdvCN cn adv = cn ** { adv = adv.s } ;
}
