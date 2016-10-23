concrete NounIce of Noun = CatIce ** open MorphoIce, ResIce, Prelude in {

	flags optimize=all_subs ;

	lin
		-- Noun phrases

		--Det -> CN -> NP
		DetCN det cn = {
			s = \\c => cn.comp ! npcaseToCase c ++ det.s ! cn.g ! npcaseToCase c ++ cn.s ! det.n ! det.b ! det.d ! npcaseToCase c ++ det.pron ! cn.g ! npcaseToCase c ;
			a = gennumperToAgr cn.g det.n P3 ;
			isPron = False
		} ;

		-- PN -> NP
		UsePN pn = {
			s = \\c => pn.s ! npcaseToCase c ;
			a = gennumperToAgr pn.g Sg P3 ;
			isPron = False
		} ;

    		-- Pron -> NP 
		UsePron p = p ** {isPron = True};

		-- Predet -> NP -> NP
		PredetNP pred np = {
			s = \\c => pred.s ! np.a.n ! np.a.g ! (npcaseToCase c) ++ np.s ! c ;
			a = np.a ;
			isPron = False
		} ;

		-- NP -> V2  -> NP
		PPartNP np v2 = {
			s = \\c => np.s ! c ++ v2.p ! PStrong np.a.n np.a.g (npcaseToCase c) ;
			a = np.a ;
			isPron = False
		} ;

		-- NP -> Adv -> NP
		AdvNP np adv = np ** {s = \\c => np.s ! c ++ adv.s} ;

		-- NP -> Adv -> NP
		ExtAdvNP np adv = np ** {s = \\c => np.s ! c ++ adv.s} ;
	
		-- NP -> RS -> NP
		RelNP np rs = np ** {s = \\c => np.s ! c ++ rs.s ! np.a} ;

		-- Det -> NP 
		DetNP det = {
			s = \\c => det.s ! Neutr ! npcaseToCase c ;
			a = gennumperToAgr Neutr det.n P3 ;
			isPron = False
		} ;

		-- Determiners

		-- Quant -> Num -> Det
		DetQuant quant num = {
			s = \\g,c => case quant.b of {
				Free => quant.s ! num.n ! g ! c ++ num.s ! g ! c ;
				Suffix => num.s ! g ! c 
			} ;
			pron = \\g,c => case quant.isPron of {
				False	=> [] ;
				True	=> quant.s ! num.n ! g ! c
			} ;
			n = num.n ;
			b = quant.b ;
			d = quant.d
		} ;

		-- Quant -> Num -> Ord -> Det
		DetQuantOrd quant num ord = {
			s = \\g,c => case quant.b of {
				Free => quant.s ! num.n ! g ! c ++ num.s ! g ! c ++ ord.s ! quant.d ! num.n ! g ! c ;
				Suffix => num.s ! g ! c ++ ord.s ! quant.d ! num.n ! g ! c
			} ;
			pron = \\g,c => case quant.isPron of {
				False	=> [] ;
				True	=> quant.s ! num.n ! g ! c
			} ;
			n = num.n ;
			b = quant.b ;
			d = quant.d ;
		} ;

		-- Num - [no numeral, but marked as singular]
		NumSg = {s = \\g,c => []; n = Sg ; hasCard = False} ;

		-- Num - [no numeral, but marked as plural]
		NumPl = {s = \\g,c => []; n = Pl ; hasCard = False} ;

		-- Card -> Num 
		NumCard n = n ** {hasCard = True} ;

		-- Digits -> Card
		NumDigits d = {
			s = \\g,c => d.s ! NCard d.n g c;
			n = d.n
		} ;
		-- Numeral -> Card
		NumNumeral d = {
			s = \\g,c => d.s ! NCard Sg g c;
			n = d.n
		} ;
		-- AdN -> Card -> Card
		AdNum adn num = {
				s = \\g,c => adn.s ++ num.s ! g ! c ;
				n = num.n
		} ;

		-- Digits -> Ord
		OrdDigits d = {
			s = \\_,n,g,c => d.s ! NOrd n g c ;
		} ;

		-- Numeral -> Ord
		OrdNumeral d = {
			s = \\_,n,g,c => d.s ! NOrd n g c ;
		} ;

		-- A -> Ord
		OrdSuperl a = {s = \\d,n,g,c => a.s ! ASuperl d n g c} ;

		-- Numeral -> A -> Ord
		OrdNumeralSuperl num a = {s = \\d,n,g,c => num.s ! NOrd n g c ++ a.s ! ASuperl d n g c} ;
		

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
			b = Suffix;
			d = Weak ;
			isPron = False
		} ;

		-- Quant
		IndefArt = {
			s = \\_,_,_ => [] ;
			b = Free ;
			d = Strong ;
			isPron = False
		} ;

		-- CN -> NP
		MassNP cn = {
			s = \\c => cn.s ! Sg ! Free ! Strong ! npcaseToCase c ;
			a = gennumperToAgr cn.g Sg P3 ;
			isPron = False
		} ;

		-- Pron -> Quant
		PossPron p = {
			s = \\n,g,c => p.s ! NPPoss n g c ;
			b = Suffix ;
			d = Weak ;
			isPron = True
		} ;


		-- Common Noun

		UseN, UseN2 = \noun -> {
			s = \\n,s,_,c => noun.s ! n ! s ! c ;
			comp = \\_ => [] ;
			g = noun.g
		} ;

		-- N2 -> NP -> CN
		ComplN2 n2 np = {
			s = \\n,s,_,c => n2.s ! n ! s ! c ++ n2.c2.s ++ np.s ! NCase n2.c2.c ;
			comp = \\_ => [] ;
			g = n2.g
		} ;

		-- N3 -> NP -> N2
		ComplN3 n3 np = {
			s = \\n,s,c => n3.s ! n ! s ! c ++ n3.c2.s ++ np.s ! NCase n3.c2.c ;
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
			comp = \\_ => [] ;
			g = cn.g
		} ;

		-- CN -> RS -> CN
		-- FIXME : not sure if this can be applyed more than once on each CN
		-- FIXME : not sure if RS should always be in third person - might need to add person field to CN
		RelCN cn rs = cn ** {s = \\n,s,d,c => cn.s ! n ! s ! d ! c ++ rs.s ! gennumperToAgr cn.g n P3} ;

		-- CN -> Adv -> CN
		AdvCN cn adv = cn ** {s = \\n,s,d,c => cn.s ! n ! s ! d ! c ++ adv.s} ;

		-- CN -> SC -> CN
		SentCN cn sc = {
			s = \\n,s,d,c	=> cn.s ! n ! s ! d ! c ++ sc.s ;
			comp = \\_ => [] ;
			g = cn.g
		} ;
{-
--2 Apposition

-- This is certainly overgenerating.

    ApposCN : CN -> NP -> CN ;    -- city Paris (, numbers x and y)

--2 Possessive and partitive constructs

-- (New 13/3/2013 AR; Structural.possess_Prep and part_Prep should be deprecated in favour of these.)

    PossNP  : CN -> NP -> CN ;     -- house of Paris, house of mine
    PartNP  : CN -> NP -> CN ;     -- glass of wine

-- This is different from the partitive, as shown by many languages.
-}

		-- 2 Apposition

		-- CN -> NP -> CN
		ApposCN cn np = {
			s = \\n,s,d,c	=>  cn.s ! n ! s ! d ! Nom ; 
			comp = \\c	=> np.s ! NCase c ;
			g = cn.g
		} ;

		-- 2 Possessive and partitive constructs

		-- FIXME!!!!
		-- When making possesive noun magic it should be
		-- the girls book (the girl owns the book) = bók stelpunnar/book girl-the
		-- see more Höskuldur Þráinsson's The Icelandic Syntax p 91-92
		-- CN -> NP -> CN
		PossNP cn np = {
			-- FIXME !
			-- The suffix (kona-an mín, hestur-inn minn) is always used 
			-- for prossesive pronouns except when the subject is a member 
			-- of the family, e.g., mother. However, I am not sure if this
			-- is always the case when a possessive pronoun is not used, e.g.,
			-- house of Paris.
			s = \\n,s,d,c	=> case np.isPron of {
				True => cn.s ! n ! Suffix ! d ! Nom ++ np.s ! NPPoss n cn.g c ;
				Fasle => cn.s ! n ! Free ! d ! c ++ np.s ! NPPoss n cn.g c -- how is the possessive treated if its not a pronoun?
			} ;
			comp = \\_ => [] ;
			g = cn.g
		} ;

		-- CN -> NP -> CN
		PartNP cn np = {
			s = \\n,s,d,c	=> cn.s ! n ! s ! d ! Nom ++ "af" ++ np.s ! NCase Dat ;
			comp = \\_ => [] ;
			g = cn.g
		} ;

		-- Det -> NP -> NP
		CountNP det np = {
			s = \\c	=> det.s ! np.a.g ! npcaseToCase c ++ "af" ++  np.s ! NCase Dat ;
			a = np.a ;
			isPron = False
		} ;


		-- 3 Conjoinable determiners and ones with adjectives

		-- DAP -> AP -> DAP
		AdjDAP dap ap = {
			s = \\g,c	=> dap.s ! g ! c ++ ap.s ! dap.n ! g ! dap.d ! c ;
			n = dap.n ;
			b = dap.b ;
			d = dap.d
		} ;

		-- Det -> DAP
		DetDAP det = det ;
}
