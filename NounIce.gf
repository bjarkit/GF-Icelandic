concrete NounIce of Noun = CatIce ** open MorphoIce, ResIce, Prelude in {

	flags optimize=all_subs ;

	lin
		-- Noun phrases

		-- Höskuldur Þráinsson - the Syntax of Icelandic - p 102
		-- The order of the elements within the NP is quite fixed. If we have a
		-- quantifier, demonstrative pronoun, numeral and an adjective, the default
		-- order is shown...
		-- Allir þessir þrír íslensku málfræðingar ...
		-- all   these  three Icelandic linguists  ...
		-- Quant Dem.   Num.  Adj.      Noun

		--Det -> CN -> NP
		DetCN det cn = {
			s = \\c => case det.b of {
				Def => det.s ! cn.g ! c ++ cn.s ! det.n ! det.b ! det.d ! c ;
				_ => cn.s ! det.n ! det.b ! det.d ! c
			} ;
			a = Ag cn.g det.n P3
		} ;

		-- PN -> NP
		UsePN pn = {
			s = \\c => pn.s ! c ;
			a = Ag pn.g Sg P3
		} ;

    		-- Pron -> NP 
		UsePron p = p ;

		-- NP -> V2  -> NP
		PPartNP np v2 = {
			s = \\c => case np.a of {
				Ag g n p	=> np.s ! c ++ v2.pp ! PStrong n g c
			} ;
			a = np.a
		} ;

		-- NP -> Adv -> NP
		AdvNP np adv = np ** {s = \\c => np.s ! c ++ adv.s} ;

		-- NP -> Adv -> NP
		ExtAdvNP np adv = np ** {s = \\c => np.s ! c ++ adv.s} ;
	
		-- NP -> RS -> NP
		RelNP np rs = np ** {s = \\c => np.s ! c ++ rs.s ! np.a} ;

		-- Det -> NP 
		DetNP det = {
			s = \\c => det.s ! Neutr ! c ;
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
		--OrdSuperl a = {s = \\n,g,c => a.s ! ASuperl Strong n g ! c} ;

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
					Fem 	=> caseList "hinar" "hinar" "hinum" "hinna" ;
					Neutr	=> caseList "hin" "hin" "hinum" "hinna"
				}
			} ;
			b = Suffix ;
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
		-- according to Höskuldur Þráinsson in The syntax of Icelandic p 90
		-- the default is def.noun + Possesive pronoun
		-- 2 :
		-- How should gender be treated in this?
		-- 
		-- Pronouns (only possessive or ?) should be treated different 
		-- than generall quantifiers. Pronouns, on the default, go 
		-- behind the noun but genereall quantifiers at the front.
		PossPron p = {
			s = \\_,_,c => p.s ! c ;
			b = Indef ;
			d = Weak 
		} ;


		-- Common Noun

		-- Regarding CN's and NP's, I am pretty sure the RCL (rc field) 
		-- is unnecessary since (to my knowledge) it is never discontinuous. 
		-- The Adverb is a bit more tricky and unclear (to me).

		UseN, UseN2 = \noun -> {
			s = \\n,s,_,c => noun.s ! n ! s ! c ;
			g = noun.g
		} ;

		-- N2 -> NP -> CN
		ComplN2 n2 np = {
			s = \\n,s,_,c => n2.s ! n ! s ! c ++ np.s ! Acc ;
			g = n2.g
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

		-- Höskuldur Þráinsson - the Syntax of Icelandic - p 102
		-- When more than one adjective modifies the noun, their respective ordering 
		-- is not entirely free,...
		-- quality >	Size >	Shape	 >	colour >	origin
		-- fallegur	hár	kringlóttur	dökkur	bandarískur
		-- beautiful	high	round		dark	American

		-- AP -> CN -> CN 
		AdjCN ap cn = {
			s = \\n,s,d,c => ap.s ! n ! cn.g ! d ! c ++ cn.s ! n ! s ! d ! c ;
			g = cn.g
		} ;

		-- CN -> RS -> CN
		-- FIXME : not sure if this can be applyed more than once on each CN
		-- FIXME : not sure if RS should always be in third person - might need to add person field to CN
		RelCN cn rs = cn ** {s = \\n,s,d,c => cn.s ! n ! s ! d ! c ++ rs.s ! Ag cn.g n P3} ;

		-- CN -> Adv -> CN
		AdvCN cn adv = cn ** {s = \\n,s,d,c => cn.s ! n ! s ! d ! c ++ adv.s} ;
}
