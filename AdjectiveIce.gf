concrete AdjectiveIce of Adjective = CatIce ** open ResIce, Prelude in {

	lin
		-- Build a new adjective phrase by adding an 
		-- ad-adjective to an existing adjective phrase:
		--AdA -> AP -> AP
		AdAP ad ap = { 
			s = \\af,c => ad.s ++ ap.s ! af ! c ;
			isPre = ap.isPre
		};


}
