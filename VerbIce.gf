concrete VerbIce of Verb = CatIce ** open ResIce, Prelude in {

--  flags optimize=all_subs ;

	lin
		-- V -> VP
		UsuV = predV ;

		-- VV  -> VP -> VP
		ComplVV vv vp =
			let
				vvp = predV vv
			in
				vvp ** {
					obj = \\a => vvp.obj ! a ++ vv.c2 ++ vp.obj ! a
				} ;

		-- VS  -> S  -> VP
		ComplVS vs s =
			let
				vvs = predV vs
			in
				vvs ** {
					obj = \\a => vvs.obj ! a ++ s.s
				} ;

		-- VQ  -> QS -> VP ;   wonder who runs
		ComplVQ vq qs =
			let
				vvq = predV vq
			in
				vvq ** {
					obj = \\a => vvq.obj ! a ++ qs.s
				} ;

		-- VA  -> AP -> VP ;  -- they become red
		ComplVA va ap =
			let
				vp = (predV va)
			in
				vp ** {
			obj = \\a => case a of {
				Ag g n p 	=> ap.s ! n ! g ! Weak ! Nom ++ vp.obj ! a
			}
		} ;

}
