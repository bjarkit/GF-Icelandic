concrete IdiomIce of Idiom = CatIce ** open Prelude, ResIce in {

	lin
		-- VP -> Cl
		ImpersCl vp = mkClause "það" vp {g = Neutr ; n = Sg ; p = P3} ;

		-- VP -> Cl
		GenericCl vp = mkClause "maður" vp {g = Masc ; n = Sg ; p = P3} ;

		-- NP -> RS -> Cl
		CleftNP np rs = let vp = (predV verbBe) in
			mkClause "það" (vp ** {obj = \\_ => np.s ! rs.c ++ rs.s ! np.a}) np.a ;

		-- Adv -> S -> Cl
		CleftAdv ad s = let vp = (predV verbBe) in
			mkClause "það" (vp ** {obj = \\_ => ad.s ++ "sem" ++ s.s}) {g = Neutr ; n = Sg ; p = P3} ;

		-- NP -> Cl
		ExistNP np =  let vp = (predV verbBe) in
			mkClause "til" (vp ** {obj = \\_ => np.s ! NCase Nom}) np.a ;

--    ExistIP   : IP -> QCl ;       -- which houses are there

		-- NP -> Adv -> Cl
		ExistNPAdv np adv = let vp = (predV verbBe) in
			mkClause "til" (vp ** {obj = \\_ => np.s ! NCase Nom ++ adv.s}) np.a ;

--    ExistIPAdv : IP -> Adv -> QCl ;   -- which houses are there in Paris

		-- VP -> VP
		-- For more complex cases than just "be sleeping" I think 
		-- VP needs to have special field for adverbs. Although
		-- I am quite certain the adverb can be both between the verb
		-- and the object or following the object.
		ProgrVP vp = let vvp = predV verbBe in
			vvp ** {obj = \\a => vp.verb ! VPresPart ++ vp.obj ! a} ;

		-- VP -> Utt
		ImpPl1 vp = {s = vp.verb ! VPres Active Subjunctive Pl P3 ++ vp.obj ! {g = Neutr ; n = Pl ; p = P3}} ;

		-- This is hardcoded in the present tense.
		-- NP -> VP -> Utt
		ImpP3 np vp = {s = verbLet.s ! VPres Active Indicative np.a.n np.a.p ++ np.s ! NCase Acc ++ vp.verb ! VInf} ;

		-- VP -> VP
		SelfAdvVP vp = vp ** {obj = \\a => vp.obj ! a ++ reflPron a.n a.g Nom} ;

		-- VP -> VP
		SelfAdVVP vp = vp ** {obj = \\a => reflPron a.n a.g Nom ++ vp.obj ! a} ;

		-- NP -> NP
		SelfNP np = {
			s = \\c => reflPron np.a.n np.a.g Nom ++ np.s ! c ;
			a = np.a
		} ;
}

