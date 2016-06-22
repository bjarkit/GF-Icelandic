concrete SentenceIce of Sentence = CatIce ** open Prelude, ResIce in {

	flags optimize=all_subs ;

	lin
		--NP -> VP -> Cl
		PredVP = mkClause ;


		--Tense -> Pol -> Cl -> S
		UseCl t p cl = {
			s = t.s ++ p.s ++ cl.s ! t.t ! t.a ! p.p ! ODir
		} ;
}
