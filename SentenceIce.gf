concrete SentenceIce of Sentence = CatIce ** open Prelude, ResIce in {

	flags optimize=all_subs ;

	lin
		--NP -> VP -> Cl
		PredVP np vp = mkClause (np.s ! Nom) vp np.a ;

		-- SC -> VP -> Cl
--	 	PredSCVP sc vp = mkClause sc.s 

		--2 Clauses missing object noun phrases

		--2 Imperatives

		--2 Embedded sentences

		--2 Sentences
--		Temp  = {s : Str ; t : R.Tense ; a : R.Anteriority} ;

		-- Temp -> Pol -> Cl -> S
		UseCl t p cl = {
			s = t.s ++ p.s ++ cl.s ! t.t ! t.a ! p.p ! ODir
		} ;

}
