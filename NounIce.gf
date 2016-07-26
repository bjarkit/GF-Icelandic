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
			s = \\c => case <det.b,det.isPre> of {
				<Suffix,True>		=> cn.s ! det.n ! det.b ! det.d ! npcaseToCase c ;
				<_,True>		=> det.s ! cn.g ! npcaseToCase c ++ cn.s ! det.n ! det.b ! det.d ! npcaseToCase c ; 
				<_,False>		=> cn.s ! det.n ! det.b ! det.d ! npcaseToCase c ++ det.s ! cn.g ! npcaseToCase c
			} ;
			a = gennumperToAgr cn.g det.n P3
		} ;

		-- PN -> NP
		UsePN pn = {
			s = \\c => pn.s ! npcaseToCase c ;
			a = gennumperToAgr pn.g Sg P3
		} ;

    		-- Pron -> NP 
		UsePron p = p ;

		-- Predet -> NP -> NP
		PredetNP pred np = {
			s = \\c => pred.s ! np.a.n ! np.a.g ! (npcaseToCase c) ++ np.s ! c ;
			a = np.a
		} ;

		-- NP -> V2  -> NP
		PPartNP np v2 = {
			s = \\c => np.s ! c ++ v2.pp ! PStrong np.a.n np.a.g (npcaseToCase c) ;
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
			s = \\c => det.s ! Neutr ! npcaseToCase c ;
			a = gennumperToAgr Neutr det.n P3
		} ;

		-- Determiners

		-- Quant -> Num -> Det
		DetQuant quant num = {
			s = \\g,c => quant.s ! num.n ! g ! c ++ num.s ! g ! c ;
			n = num.n ;
			b = quant.b ;
			d = quant.d ;
			isPre = quant.isPre
		} ;

		-- Quant -> Num -> Ord -> Det
		DetQuantOrd quant num ord = {
			s = \\g,c => quant.s ! num.n ! g ! c ++ num.s ! g ! c ++ ord.s ! num.n ! g ! c ;
			n = num.n ;
			b = quant.b ;
			d = quant.d ;
			isPre = quant.isPre
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
			b = Suffix;
			d = Weak ;
			isPre = True
		} ;

		-- Quant
		IndefArt = {
			s = \\_,_,_ => [] ;
			b = Indef;
			d = Strong ;
			isPre = True
		} ;

		-- CN -> NP
		MassNP cn = {
			s = \\c => cn.s ! Sg ! Indef ! Strong ! npcaseToCase c ;
			a = gennumperToAgr cn.g Sg P3
		} ;

		-- Pron -> Quant
		PossPron p = {
			s = \\n,g,c => p.s ! NPPoss n g c ;
			b = Suffix ;
			d = Weak ;
			isPre = False
		} ;


		-- Common Noun

		UseN, UseN2 = \noun -> {
			s = \\n,s,_,c => noun.s ! n ! s ! c ;
			g = noun.g
		} ;

		-- N2 -> NP -> CN
		ComplN2 n2 np = {
			s = \\n,s,_,c => n2.s ! n ! s ! c ++ np.s ! NCase Acc ;
			g = n2.g
		} ;

		-- N3 -> NP -> N2
		ComplN3 n3 np = {
			s = \\n,s,c => n3.s ! n ! s ! c ++ n3.c2 ++ np.s ! NCase Dat ;
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
		RelCN cn rs = cn ** {s = \\n,s,d,c => cn.s ! n ! s ! d ! c ++ rs.s ! gennumperToAgr cn.g n P3} ;

		-- CN -> Adv -> CN
		AdvCN cn adv = cn ** {s = \\n,s,d,c => cn.s ! n ! s ! d ! c ++ adv.s} ;

---- Nouns can also be modified by embedded sentences and questions.
---- For some nouns this makes little sense, but we leave this for applications
---- to decide. Sentential complements are defined in [Verb Verb.html].

--    SentCN  : CN -> SC  -> CN ;   -- question where she sleeps

		-- 2 Apposition

		-- CN -> NP -> CN
		ApposCN cn np = {
			s = \\n,s,d,c	=>  cn.s ! n ! s ! d ! Nom ++ np.s ! NCase c ;
			g = cn.g
		} ;

		-- 2 Possessive and partitive constructs

		-- CN -> NP -> CN
		PossNP cn np = {
			-- The suffix (kona-an mín, hestur-inn minn) is always used 
			-- for prossesive pronouns except when the subject is a member 
			-- of the family, e.g., mother. However, I am not sure if this
			-- is always the case when a possessive pronoun is not used, e.g.,
			-- house of Paris.
			s = \\n,s,d,c	=> cn.s ! n ! Suffix ! d ! Nom ++ np.s ! NPPoss n cn.g c ;
			g = cn.g
		} ;

		-- CN -> NP -> CN
		PartNP cn np = {
			s = \\n,s,d,c	=> cn.s ! n ! s ! d ! Nom ++ "af" ++ np.s ! NCase Dat ;
			g = cn.g
		} ;

		-- Det -> NP -> NP
		CountNP det np = {
			s = \\c	=> det.s ! np.a.g ! npcaseToCase c ++ "af" ++  np.s ! NCase Dat ;
			a = np.a
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
