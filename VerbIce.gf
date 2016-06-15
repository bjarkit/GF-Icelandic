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
					obj = \\a => vvp.obj ! a ++ vv.c2.s ++ infVP vp a 
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

		-- V2V -> VP -> VPSlash
		SlashV2V v2v vp = predV v2v ** {
			n = \\a => v2v.c3.s ++ infVP vp a ;
			c2 = v2v.c2 ;
		} ;

		-- V2S -> S -> VPSlash
		SlashV2S v2s s = predV v2s ** {
			n = \\_ => s.s ;
			c2 = v2s.c2
		} ;

		-- V2Q -> QS -> VPSlash
		SlashV2Q v2q qs = predV v2q ** {
			n = \\_ => qs.s ;
			c2 = v2q.c2
		} ;

		-- V2A -> AP -> VPSlash
		SlashV2A v2a ap = predV v2a ** {
			n = \\a => case a of {
				Ag g n p 	=> ap.s ! n ! g ! Weak ! Nom 
			} ;
			c2 = v2a.c2
		} ;

		-- VPSlash -> NP -> VP
		ComplSlash vps np = {
			s = vps.s ;
			obj = \\a => vps.n ! a ++ vps.obj ! a ++ np.s ! vps.c2.c 
		} ;

		-- VV -> VPSlash -> VPSlash
		SlashVV vv vps = predV vv ** {
			obj = \\a => vv.c2.s ++ infVP vps a ;
			n = \\_ => [] ;
			c2 = vv.c2
		} ;

		-- V2V -> NP -> VPSlash -> VPSlash
		SlashV2VNP v2v np vps = predV v2v ** {
			obj = \\a => v2v.c2.s ++ np.s ! v2v.c2.c ++ v2v.c3.s ++ infVP vps a ;
			n = \\_ => [] ;
			c2 = v2v.c2
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
