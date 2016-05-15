concrete SentenceIce of Sentence = CatIce ** open Prelude, ResIce in {

	flags optimize=all_subs ;

	lin

		--Build a clause from a noun phrase (= the subject) and a verb phrase:
		--NP -> VP -> Cl
		PredVP np vp = {
			s = \\t,p => 
				let
					subj = np.s ! Nom ;
					verb = agrV vp.s np.a t p ;
					obj = vp.obj ! np.a
				in case <t,p> of {
					<TPres, False>	=> subj ++ verb ++ obj ++ "ekki" ;
					_		=> subj ++ verb ++ obj
				} ;
		} ;

		--Build a sentence from a tense, a polarity and a clause:
		--Tense -> Pol -> Cl -> S
		UseCl t p cl = {
			s = cl.s ! t.t ! p.b ++ t.s ++ p.s
		} ;
}

