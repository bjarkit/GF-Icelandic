concrete RelativeIce of Relative = CatIce ** open ResIce in {

	lin
		-- Cl -> RCl
		RelCl cl = {
			s = \\t,ant,p,o,_ => "þannig að" ++ cl.s ! t ! ant ! p ! o
		} ;

		-- RP -> VP -> RCl
		RelVP rp vp = {
			s = \\t,ant,p,o,agr => 
				let 
					cl = mkClause rp.s vp agr
				in
					cl.s ! t ! ant ! p ! o
		} ;

		--    RelSlash : RP -> ClSlash -> RCl ; -- whom John loves

		-- There are two relative pronouns in Icelandic, "sem" and "er", and
		-- are used in exactly the same way - "sem" is a lot more common. Both
		-- also appear as other types of words, e.g., "er" as a form of the verb
		-- "að vera" (e. to be).
		-- RP
		IdRP = {s = "sem" } ;

		-- Prep -> NP -> RP -> RP
		FunRP prep np rp = {
			s = prep.s ++ np.s ! NCase prep.c ++ rp.s
		} ;
}
