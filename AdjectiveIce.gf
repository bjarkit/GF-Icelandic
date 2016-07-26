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
			s = \\n,g,d,c => a2.s ! APosit d n g c ++ a2.c2 ++ np.s ! NCase Dat
		} ;

		-- A2 -> AP
		UseA2 a2 = {
			s = \\n,g,d,c => a2.s ! APosit d n g c
		} ;

		-- A -> AP
		UseComparA a = {
			s = \\n,g,d,c => a.s ! ACompar n g c
		} ;

		--AdA -> AP -> AP
		AdAP ad ap = { 
			s = \\n,g,d,c => ad.s ++ ap.s ! n ! g ! d ! c 
		} ;


}
