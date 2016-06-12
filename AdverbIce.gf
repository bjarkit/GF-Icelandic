concrete AdverbIce of Adverb = CatIce ** open ResIce, Prelude in {
	lin
		-- A -> Adv
		PositAdvAdj a = { s = a.s ! AAdv} ;

		-- Prep -> NP -> Adv
		PrepNP p np = { s = p.s ++ np.s ! p.c} ;

		-- CAdv -> A -> S -> Adv
		ComparAdvAdjS cadv a s = {
			s = cadv.s ++ a.s ! AAdv ++ cadv.p ++ s.s -- until S has been implemented
		} ;

		-- CAdv -> A -> NP -> Adv
		ComparAdvAdj cadv a np = {
			s = cadv.s ++ a.s ! AAdv ++ cadv.p ++ np.s ! Nom
		} ;

		-- AdA -> Adv -> Adv
		AdAdv ad adv = { s = ad.s ++ adv.s} ;

		-- A -> AdA
		PositAdAAdj a = { s = a.s ! AAdv} ;

		-- Subj -> S -> Adv
		SubjS sub s = { s = sub.s ++ s.s } ; -- until S has been implemented

		-- CAdv -> AdN
		AdnCAdv cadv = { s = cadv.s ++ "en"} ; -- hardcoded atm
}
