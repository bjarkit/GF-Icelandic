concrete PhraseIce of Phrase = CatIce ** open Prelude, ResIce in {
	lin
		-- PConj -> Utt -> Voc -> Phr
		PhrUtt pconj utt voc = {s = pconj.s ++ utt.s ++ voc.s } ;

		-- S -> Utt
		UttS s = {s = s.s } ;

		-- NP   -> Utt
		UttNP np = {s = np.s ! Nom } ;

		-- PConj
		NoPConj = {s = [] } ;

		-- Voc
		NoVoc = {s = [] } ;
}
