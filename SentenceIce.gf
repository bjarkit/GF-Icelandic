concrete SentenceIce of Sentence = CatIce ** open Prelude, ResIce in {

	flags optimize=all_subs ;

	lin
		--NP -> VP -> Cl
		PredVP np vp = mkClause (np.s ! NCase Nom) vp np.a ;

		-- SC -> VP -> Cl
	 	PredSCVP sc vp = mkClause sc.s vp {g = Neutr;  n = Sg ; p = P3} ;

		--2 Clauses missing object noun phrases
		-- TODO

		--2 Imperatives

		-- VP -> Imp
		ImpVP vp = {s = \\pol,num => case pol of {
			Pos	=> vp.verb ! VImp Active num ++ vp.obj ! gennumperToAgr Masc num P2 ;
			Neg	=> vp.verb ! VImp Active num  ++ "ekki" ++ vp.obj ! gennumperToAgr Masc num P2
			} ;
		} ;


		--2 Embedded sentences

		-- S -> SC
		EmbedS ss = {s = "að" ++ ss.s} ;

		--    EmbedQS   : QS -> SC ;               -- who goes
		-- TODO

		-- VP -> SC
		EmbedVP vp = {s = infVP vp {g = Neutr;  n = Sg ; p = P3}} ;

		--2 Sentences

		-- Temp -> Pol -> Cl -> S
		UseCl t p cl = {
			s = t.s ++ p.s ++ cl.s ! t.t ! t.a ! p.p ! ODir
		} ;

		-- Temp -> Pol -> RCl -> RS
		UseRCl t p rcl = {
			s = \\agr => t.s ++ p.s ++ rcl.s ! t.t ! t.a ! p.p ! ODir ! agr ;
			c = NCase Nom
		} ;

		--    UseQCl   : Temp -> Pol -> QCl -> QS ;  -- who had not slept
		-- TODO
		--    UseSlash : Temp -> Pol -> ClSlash -> SSlash ; -- (that) she had not seen
		-- TODO

		-- Adv -> S -> S
		AdvS adv s = {s = adv.s ++ s.s} ;
	
		-- Adv -> S -> S
		ExtAdvS adv s = {s = adv.s ++ "," ++ s.s} ;

		-- S -> Subj -> S -> S
		SSubjS sx subj sy = {s = sx.s ++ subj.s ++ sy.s} ;

		-- S -> RS -> S
		-- TODO : Add Agr to S and Cl, otherwise RS will always 
		-- have the same gender, person and number.
		-- This is possible only a problem when numbers differ or is this
		-- a problem at all?
		RelS s rs = { s = s.s ++ rs.s ! gennumperToAgr Neutr Sg P3 } ;

		-- S -> Subj -> S -> S
		ModSubjS sx subj sy = {s = sx.s ++ "," ++ subj.s ++ sy.s} ;
}
