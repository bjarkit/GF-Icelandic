concrete NounIce of Noun = CatIce ** open MorphoIce, ResIce, Prelude in {

	flags optimize=all_subs ;

	lin
		-- Noun phrases

		--Det -> CN -> NP
		DetCN det cn = {
			s = \\c => det.s ! cn.g ! npcaseToCase c
				++ cn.s ! det.n ! det.b ! det.d ! npcaseToCase c
				++ det.pron ! cn.g ! npcaseToCase c
				++ cn.comp ! det.n ! npcaseToCase c ;
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
			s = \\c => cn.s ! Sg ! Free ! Strong ! npcaseToCase c ++ cn.comp ! Sg ! npcaseToCase c;
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
			comp = \\_,_ => [] ;
			g = noun.g
		} ;

		-- N2 -> NP -> CN
		ComplN2 n2 np = {
			s = \\n,s,_,c => n2.s ! n ! s ! c ++ n2.c2.s ++ np.s ! NCase n2.c2.c ;
			comp = \\_,_ => [] ;
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
			comp = cn.comp ;
			g = cn.g
		} ;

		-- CN -> RS -> CN
		RelCN cn rs = cn ** {
			s = \\n,s,d,c => cn.s ! n ! s ! d ! c ++ rs.s ! gennumperToAgr cn.g n P3 ;
			comp = \\n,c => cn.comp ! n ! c ++ rs.s ! gennumperToAgr cn.g n P3 
		} ;

		-- CN -> Adv -> CN
		AdvCN cn adv = cn ** {
			s = \\n,s,d,c => cn.s ! n ! s ! d ! c ;
			comp = \\n,c => cn.comp ! n ! c ++ adv.s
		} ;

		-- CN -> SC -> CN
		SentCN cn sc = {
			s = \\n,s,d,c	=> cn.s ! n ! s ! d ! c ;
			comp = \\n,c => cn.comp ! n ! c ++ sc.s ;
			g = cn.g
		} ;

		-- 2 Apposition

		-- CN -> NP -> CN
		ApposCN cn np = {
			s = \\n,s,d,c	=>  cn.s ! n ! s ! d ! Nom ; 
			comp = \\n,c	=> cn.comp ! n ! c ++ np.s ! NCase c ;
			g = cn.g
		} ;

		-- 2 Possessive and partitive constructs

		-- CN -> NP -> CN
		PossNP cn np = {
			s = \\n,s,d,c	=> case np.isPron of {
				True => cn.s ! n ! Suffix ! d ! Nom ++ np.s ! NPPoss n cn.g c ;
				False => cn.s ! n ! Free ! d ! c ++ np.s ! NPPoss n cn.g Gen
				-- This is tricky, if its not a pronoun then there are 3 possible constructions
				-- bók stelpunnar - book the girl - the girls book
				-- bók stelpu	-- book girls - girls book
				-- bókin hennar stelpu -- the book her girl
				-- in the last example the personal pronoun (corresponding to the gender in question)
				-- is used as a propial article. The first two are used (depending on the NP), since
				-- they can always work. The last example (with the propial article) is generally 
				-- only used with personal pronouns. It must be noted that possessor/subject is 
				-- always in the in the genative case.
			} ;
			comp = cn.comp ;
			g = cn.g
		} ;

		-- CN -> NP -> CN
		PartNP cn np = {
			s = \\n,s,d,c	=> cn.s ! n ! s ! d ! Nom ;
			comp = \\n,c => cn.comp ! n ! Nom ++ "af" ++ np.s ! NCase Dat ;
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
