concrete AdjectiveIce of Adjective = CatIce ** open ResIce, Prelude in {

	lin

		--AdA -> AP -> AP
		AdAP ad ap = { 
			s = \\n,g,d,c => ad.s ++ ap.s ! n ! g ! d ! c 
		};


}
