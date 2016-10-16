concrete PhraseIce of Phrase = CatIce ** open Prelude, ResIce in {
	lin
		-- PConj -> Utt -> Voc -> Phr
		PhrUtt pconj utt voc = {s = pconj.s ++ utt.s ++ voc.s} ;

		-- S -> Utt
		UttS s = {s = s.s} ;

		-- Pol -> Imp -> Utt
		UttImpSg pol imp = {s = imp.s ! pol.p ! Sg} ;

		-- Pol -> Imp -> Utt
		UttImpPl pol imp = {s = imp.s ! pol.p ! Pl} ;

		-- Pol -> Imp -> Utt
		UttImpPol pol imp = {s = imp.s ! pol.p ! Sg} ;

		-- IP -> Utt
    		UttIP ip = {s = ip.s ! Masc ! Nom} ;

		-- QS  -> Utt
		UttQS qs = {s = qs.s ! QDir} ;

		-- IAdv -> Utt
		UttIAdv adv = adv ;

		-- NP   -> Utt
		UttNP np = {s = np.s ! NCase Nom} ;

		-- Adv -> Utt
		UttAdv adv = adv ;

		-- VP -> Utt
		UttVP vp = {s = "að" ++ vp.verb ! VInf} ;

		-- CN -> Utt
		UttCN cn = {s = cn.s ! Sg ! Free ! Strong ! Nom} ;

		--    UttCard   : Card -> Utt ;               -- five

		-- AP -> Utt
		UttAP ap = {s = ap.s ! Sg ! Neutr ! Strong ! Nom} ;

		-- Interj -> Utt
		UttInterj i = i ;

		-- PConj
		NoPConj = {s = []} ;

		-- Conj -> PConj
		PConjConj conj = {s = conj.s2} ;

		-- Voc
		NoVoc = {s = []} ;

		-- NP -> Voc
		VocNP np = {s = "," ++ np.s ! NCase Nom} ;
}
