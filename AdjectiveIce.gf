concrete AdjectiveIce of Adjective = CatIce ** open ResIce, Prelude in {

	lin
		-- A -> AP 
		Posit a = {
			s = \\n,g,d,c => a.s ! APosit d n g ! c
		} ;

		-- A -> NP -> AP 
		ComparA a np = {
			s = \\n,g,d,c => a.s ! ACompar n g ! c ++ "heldur en" ++ np.s ! Nom
		} ;

		-- A -> AP
		UseComparA a = {
			s = \\n,g,d,c => a.s ! ACompar n g ! c
		} ;

		--AdA -> AP -> AP
		AdAP ad ap = { 
			s = \\n,g,d,c => ad.s ++ ap.s ! n ! g ! d ! c 
		} ;


}
