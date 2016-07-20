concrete SentenceIce of Sentence = CatIce ** open Prelude, ResIce in {

	flags optimize=all_subs ;

	lin
		--NP -> VP -> Cl
		PredVP np vp = mkClause (np.s ! Nom) vp np.a ;

---- Using an embedded sentence as a subject is treated separately.
---- This can be overgenerating. E.g. "whether you go" as subject
---- is only meaningful for some verb phrases.
--
--    PredSCVP  : SC -> VP -> Cl ;         -- that she goes is good
		-- SC -> VP -> Cl
--	 	PredSCVP sc vp = mkClause sc.s vp ?agr?

		--2 Clauses missing object noun phrases

---- This category is a variant of the 'slash category' $S/NP$ of
---- GPSG and categorial grammars, which in turn replaces
---- movement transformations in the formation of questions
---- and relative clauses. Except $SlashV2$, the construction 
---- rules can be seen as special cases of function composition, in
---- the style of CCG.
---- *Note* the set is not complete and lacks e.g. verbs with more than 2 places.
--
--    SlashVP   : NP -> VPSlash -> ClSlash ;      -- (whom) he sees
--    AdvSlash  : ClSlash -> Adv -> ClSlash ;     -- (whom) he sees today
--    SlashPrep : Cl -> Prep -> ClSlash ;         -- (with whom) he walks 
--    SlashVS   : NP -> VS -> SSlash -> ClSlash ; -- (whom) she says that he loves

		--2 Imperatives

		-- VP -> Imp
		ImpVP vp = {s = \\pol,num => case pol of {
			Pos	=> vp.verb ! VImp Active num ++ vp.obj ! Ag Masc num P2 ;
			Neg	=> vp.verb ! VImp Active num  ++ "ekki" ++ vp.obj ! Ag Masc num P2
			} ;
		} ;


		--2 Embedded sentences
--
---- Sentences, questions, and infinitival phrases can be used as
---- subjects and (adverbial) complements.
--
--    EmbedS    : S  -> SC ;               -- that she goes
--    EmbedQS   : QS -> SC ;               -- who goes
--    EmbedVP   : VP -> SC ;               -- to go
--

		--2 Sentences

--		Temp  = {s : Str ; t : R.Tense ; a : R.Anteriority} ;

		-- Temp -> Pol -> Cl -> S
		UseCl t p cl = {
			s = t.s ++ p.s ++ cl.s ! t.t ! t.a ! p.p ! ODir
		} ;

		-- Temp -> Pol -> RCl -> RS
		UseRCl t p rcl = {
			s = \\agr => t.s ++ p.s ++ rcl.s ! t.t ! t.a ! p.p ! ODir ! agr ;
			c = Nom
		} ;

--    UseQCl   : Temp -> Pol -> QCl -> QS ;  -- who had not slept
--    UseSlash : Temp -> Pol -> ClSlash -> SSlash ; -- (that) she had not seen

		-- Adv -> S -> S
		AdvS adv s = {s = adv.s ++ s.s} ;
	
		-- Adv -> S -> S
		ExtAdvS adv s = {s = adv.s ++ "," ++ s.s} ;


		-- S -> Subj -> S -> S
		SSubjS sx subj sy = {s = sx.s ++ subj.s ++ sy.s} ;

		-- S -> RS -> S
		-- TODO : Add Agr to S and Cl, otherwise RS will always 
		-- have the same gender, person and number.
		RelS s rs = { s = s.s ++ rs.s ! Ag Neutr Sg P3 } ;

		-- S -> Subj -> S -> S
		ModSubjS sx subj sy = {s = sx.s ++ "," ++ subj.s ++ sy.s} ;
}
