concrete SentenceIce of Sentence = CatIce ** open Prelude, ResIce in {

	flags optimize=all_subs ;

	lin

		--NP -> VP -> Cl
		PredVP np vp = mkClause (np.s ! NCase Nom) vp np.a ;

		-- SC -> VP -> Cl
	 	PredSCVP sc vp = mkClause sc.s vp {g = Neutr;  n = Sg ; p = P3} ;

		--2 Clauses missing object noun phrases

		-- NP -> VPSlash -> ClSlash
		SlashVP np vps = mkClause (np.s ! NCase Nom) vps np.a ** {
			c2 = vps.c2 ;
			n3 = \\a => vps.nn1 ! a ++ vps.nn2 ! a
		} ;

		-- ClSlash -> Adv -> ClSlash
		AdvSlash cls adv = {
			s =\\ten,ant,pol,ord => cls.s ! ten ! ant ! pol ! ord ++ adv.s ;
			c2 = cls.c2 ;
			n3 = cls.n3
		} ;

		-- Cl -> Prep -> ClSlash
		SlashPrep cl prep = cl ** {
			c2 = prep ;
			n3 =\\_ => []
		} ;

		SlashVS np vs ssl = {
			s = \\ten,ant,pol,ord => let
				cl = mkClause (np.s ! NCase Nom) (predV vs) np.a
			in
				cl.s ! ten ! ant ! pol ! ord ++ ssl.s ! ord ;
			c2 = ssl.c2 ;
			n3 = ssl.n3
		} ;

		--2 Imperatives

		-- VP -> Imp
		ImpVP vp = {s =\\pol,num =>
			let
				agr = gennumperToAgr Masc num P2 ;
				pron = vp.n1 ! agr ;
				obj = vp.n2 ! agr ;
				adv = vp.a2 ;
				verb = vp.s ! VPImp ! pol ! agr
			in case vp.en1p1 of {
				False	=> verb.fin ++ verb.a1.p1 ++ verb.inf ++ obj ++ pron.p2 ++ verb.a1.p2 ++ adv ;
				True	=> verb.fin ++ verb.a1.p1 ++ verb.inf ++ pron.p1 ++ pron.p2 ++ verb.a1.p2 ++ obj ++ adv
			} ;
		} ;

		--2 Embedded sentences

		-- S -> SC
		EmbedS ss = {s = "að" ++ ss.s} ;

		-- QS -> SC
		EmbedQS qs = {s = qs.s ! QDir} ;

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

		-- Temp -> Pol -> QCl -> QS
		UseQCl t p qcl = {
			s = \\qf => t.s ++ p.s ++ qcl.s ! t.t ! t.a ! p.p ! qf
		} ;

		-- Temp -> Pol -> ClSlash -> SSlash
		UseSlash t p cls = {
			s = \\o => cls.s ! t.t ! t.a ! p.p ! o ;
			c2 = cls.c2 ;
			n3 = cls.n3
		} ;
		

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
		-- or just add another function in ExtraIce..
		-- a problem at all?
		RelS s rs = { s = s.s ++ rs.s ! gennumperToAgr Neutr Sg P3 } ;
}
