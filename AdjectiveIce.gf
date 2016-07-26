concrete AdjectiveIce of Adjective = CatIce ** open ResIce, Prelude in {

	lin
		-- A -> AP 
		PositA a = {
			s = \\n,g,d,c => a.s ! APosit d n g c
		} ;

		-- A -> NP -> AP 
		ComparA a np = {
			s = \\n,g,d,c => a.s ! ACompar n g c ++ "heldur en" ++ np.s ! NCase Nom
		} ;

		-- A2 -> NP -> AP
		ComplA2 a2 np = {
			s = \\n,g,d,c => a2.s ! APosit d n g c ++ a2.c2.s ++ np.s ! NCase a2.c2.c
		} ;

		-- A2 -> AP
		ReflA2 a2 = {
			s = \\n,g,d,c => a2.s ! APosit d n g c ++ a2.c2.s ++ reflPron n g a2.c2.c
		} ;

		-- A2 -> AP
		UseA2 a2 = {
			s = \\n,g,d,c => a2.s ! APosit d n g c
		} ;

		-- A -> AP
		UseComparA a = {
			s = \\n,g,d,c => a.s ! ACompar n g c
		} ;

		-- CAdv -> AP -> NP -> AP
		CAdvAP cadv ap np = {
			s = \\n,g,d,c => cadv.s ++ ap.s ! n ! g ! d ! c ++ cadv.p ++ np.s ! NCase Nom
		} ;

-- The superlative use is covered in $Ord$.

-- AdjOrd  : Ord -> AP ;       -- warmest

		-- AP -> SC -> AP
		SentAP ap sc = {
			s = \\n,g,d,c => ap.s ! n ! g ! d ! c ++ sc.s
		} ;

		--AdA -> AP -> AP
		AdAP ad ap = { 
			s = \\n,g,d,c => ad.s ++ ap.s ! n ! g ! d ! c 
		} ;

		-- AP -> Adv -> AP
		AdvAP ap adv = {
			s = \\n,g,d,c => ap.s ! n ! g ! d ! c ++ adv.s
		} ;
}
