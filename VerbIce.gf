concrete VerbIce of Verb = CatIce ** open ResIce, Prelude in {

--  flags optimize=all_subs ;

	lin
		-- V -> VP
		UseV = predV ;

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

		-- V2 -> VPSlash
		SlashV2a v = predV v ** {
			n = \\_	=> [] ;
			c2 = v.c2
		} ;

		-- V3 -> NP -> VPSlash
		Slash2V3 v3 np = predV v3 ** {
			obj = \\_ => v3.c2.s ++ np.s ! v3.c2.c ;
			n = \\_	=> [] ; 
			c2 = v3.c3
		} ;

		-- V3 -> NP -> VPSlash
		Slash3V3 v3 np = predV v3 ** {
			obj = \\_ => v3.c3.s ++ np.s ! v3.c3.c ;
			n = \\_ => [] ;
			c2 = v3.c2
		} ;

		-- VP -> Adv -> VP
		AdvVP vp adv = vp ** { 
			obj = \\a => adv.s ++ vp.obj ! a
		} ;

		-- VP -> Adv -> VP 
		ExtAdvVP vp adv = vp ** {
			obj = \\a => embedInCommas (adv.s ++ vp.obj ! a)
		} ;
}
