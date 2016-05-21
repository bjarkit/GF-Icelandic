concrete ExtraIce of ExtraIceAbs = CatIce ** 
  open ResIce, Coordination, Prelude, MorphoIce, ParadigmsIce in {

	lin
		-- Det -> NP
		DetNPMasc det = {
			s = \\c => det.s ! Masc ! c ; 
			a = Ag Masc det.n P3
		} ;

		-- Det -> NP 
		DetNPFem det = {
			s = \\c => det.s ! Fem ! c ; 
			a = Ag Fem det.n P3
		} ;
} 
