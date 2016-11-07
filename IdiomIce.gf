concrete IdiomIce of Idiom = CatIce ** open Prelude, ResIce in {

	lin
		-- VP -> Cl
		ImpersCl vp = mkClause "það" vp {g = Neutr ; n = Sg ; p = P3} ;

		-- VP -> Cl
		GenericCl vp = mkClause "maður" vp {g = Masc ; n = Sg ; p = P3} ;

		-- NP -> RS -> Cl
		CleftNP np rs = let vp = (predV verbBe) in
			mkClause "það" (vp ** {n2 = \\_ => np.s ! rs.c ++ rs.s ! np.a}) np.a ;

		-- Adv -> S -> Cl
		CleftAdv ad s = let vp = (predV verbBe) in
			mkClause "það" (vp ** {n2 = \\_ => ad.s ++ "sem" ++ s.s}) {g = Neutr ; n = Sg ; p = P3} ;

		-- NP -> Cl
		ExistNP np = let vp = (predV verbBe) in
			mkClause "til" (vp ** {n2 = \\_ => np.s ! NCase Nom}) np.a ;

		-- IP -> QCl
		-- this is hardcoded in the masculine atm
		ExistIP ip = let 
				vp = (predV verbBe) ;
				cl = mkClause (ip.s ! Masc ! Nom) vp {g = Masc ; n = ip.n ; p = P3}
			in {s = \\ten,ant,pol,_ => cl.s ! ten ! ant ! pol ! ODir} ;

		-- NP -> Adv -> Cl
		ExistNPAdv np adv = let vp = (predV verbBe) in
			mkClause "til" (vp ** {n2 = \\_ => np.s ! NCase Nom ++ adv.s}) np.a ;

		-- IP -> Adv -> QCl
		ExistIPAdv ip adv = let
				vp = (predV verbBe) ;
				cl = mkClause (ip.s ! Masc ! Nom) vp {g = Masc ; n = ip.n ; p = P3}
			in {s = \\ten,ant,pol,_ => cl.s ! ten ! ant ! pol ! ODir ++ adv.s} ;

		-- VP -> VP
		ProgrVP vp = let vvp = predV verbBe in
			vvp ** {n2 = \\a => vp.p ! PPres ++ vp.n2 ! a} ;

		-- VP -> Utt
			--| VPMood Tense Anteriority -- is this a describing name ?
			--a = gennumperToAgr g n p ;
		ImpPl1 vp = { s = let
				agr = gennumperToAgr Masc Pl P1 ;
				verb = vp.s ! VPMood Pres Simul ! Pos ! agr
			in verb.inf ++ vp.n2 ! agr } ;

		-- NP -> VP -> Utt
		ImpP3 np vp = {s = let
				verb = vp.s ! VPMood Pres Simul ! Pos ! np.a
			in verbLet.s ! VPres Active Indicative np.a.n np.a.p ++ np.s ! NCase Acc ++ verb.inf} ;

		-- VP -> VP
		SelfAdvVP vp = vp ** {obj = \\a => vp.obj ! a ++ reflPron a.p a.n a.g Nom} ;

		-- VP -> VP
		SelfAdVVP vp = vp ** {obj = \\a => reflPron a.p a.n a.g Nom ++ vp.obj ! a} ;

		-- NP -> NP
		SelfNP np = {
			s = \\c => reflPron np.a.p np.a.n np.a.g Nom ++ np.s ! c ;
			a = np.a ;
			isPron = False
		} ;
}

