concrete IdiomIce of Idiom = CatIce ** open Prelude, ResIce in {

	lin
		-- VP -> Cl
		ImpersCl vp = mkClause "það" vp {g = Neutr ; n = Sg ; p = P3} ;

		-- VP -> Cl
		GenericCl vp = mkClause "maður" vp {g = Masc ; n = Sg ; p = P3} ;

		-- NP -> RS -> Cl
		CleftNP np rs = let vp = (predV verbBe) in
			mkClause "það" (vp ** {n2 = \\_ => np.s ! rs.c ++ rs.s ! np.a}) np.a ;

		-- Adv -> S -> Cl
		CleftAdv ad s = let vp = (predV verbBe) in
			mkClause "það" (vp ** {n2 = \\_ => ad.s ++ "sem" ++ s.s}) {g = Neutr ; n = Sg ; p = P3} ;

		-- NP -> Cl
		ExistNP np = let vp = (predV verbBe) in
			mkClause "til" (vp ** {n2 = \\_ => np.s ! NCase Nom}) np.a ;

		-- IP -> QCl
		-- this is hardcoded in the masculine atm
		ExistIP ip = let 
				vp = (predV verbBe) ;
				cl = mkClause (ip.s ! Masc ! Nom) vp {g = Masc ; n = ip.n ; p = P3}
			in {s = \\ten,ant,pol,_ => cl.s ! ten ! ant ! pol ! ODir} ;

		-- NP -> Adv -> Cl
		ExistNPAdv np adv = let vp = (predV verbBe) in
			mkClause "til" (vp ** {n2 = \\_ => np.s ! NCase Nom ++ adv.s}) np.a ;

		-- IP -> Adv -> QCl
		-- this is hardcoded in the masculine atm
		ExistIPAdv ip adv = let
				vp = (predV verbBe) ;
				cl = mkClause (ip.s ! Masc ! Nom) vp {g = Masc ; n = ip.n ; p = P3}
			in {s = \\ten,ant,pol,_ => cl.s ! ten ! ant ! pol ! ODir ++ adv.s} ;

		-- VP -> VP
		-- FIXME!
		-- For more complex cases than just "be sleeping" I think 
		-- VP needs to have special field for adverbs. Although
		-- I am quite certain the adverb can be both between the verb
		-- and the object or following the object.
		ProgrVP vp = let vvp = predV verbBe in
			vvp ** {n2 = \\a => vp.p ! PPres ++ vp.n2 ! a} ;

{-
		-- these are a bit tricky in icelandic. The imperative only exist in the 2nd person

		-- VP -> Utt
		ImpPl1 vp = {s = vp.verb ! VPres Active Subjunctive Pl P3 ++ vp.n2 ! {g = Neutr ; n = Pl ; p = P3}} ;

		-- This is hardcoded in the present tense.
		-- NP -> VP -> Utt
		ImpP3 np vp = {s = verbLet.s ! VPres Active Indicative np.a.n np.a.p ++ np.s ! NCase Acc ++ vp.verb ! VInf} ;
-}
{-
    SelfAdvVP : VP -> VP ;        -- is at home himself
    SelfAdVVP : VP -> VP ;        -- is himself at home
    SelfNP    : NP -> NP ;        -- the president himself (is at home)
-}

		-- VP -> VP
		SelfAdvVP vp = vp ** {obj = \\a => vp.obj ! a ++ reflPron a.n a.g Nom} ;

		-- VP -> VP
		SelfAdVVP vp = vp ** {obj = \\a => reflPron a.n a.g Nom ++ vp.obj ! a} ;

		-- NP -> NP
		SelfNP np = {
			s = \\c => reflPron np.a.n np.a.g Nom ++ np.s ! c ;
			a = np.a ;
			isPron = False
		} ;
}

