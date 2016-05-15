concrete AdjectiveIce of Adjective = CatIce ** open ResIce, Prelude in {

	lin
		-- Build a new adjective phrase by adding an 
		-- ad-adjective to an existing adjective phrase:
		--AdA -> AP -> AP
		AdAP ad ap = { 
			s = \\n,g,d,c => ad.s ++ ap.s ! n ! g ! d ! c;
			isPre = ap.isPre
		};


}
