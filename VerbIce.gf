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

		-- VQ  -> QS -> VP
		ComplVQ vq qs =
			let
				vvq = predV vq
			in
				vvq ** {
					obj = \\a => vvq.obj ! a ++ qs.s ! QDir
				} ;

		-- VA  -> AP -> VP
		ComplVA va ap =
			let
				vp = (predV va)
			in
				vp ** {
					obj = \\a => ap.s ! a.n ! a.g ! Weak ! Nom ++ vp.obj ! a
				} ;

		-- V2 -> VPSlash
		SlashV2a v = predV v ** {
			c2 = v.c2 ;
			n1,n2 = \\_ => [] ;
		} ;

		-- V3 -> NP -> VPSlash
		Slash2V3 v3 np = predV v3 ** {
			n1 = \\a => v3.c2.s ++ np.s ! NCase v3.c2.c ;
			n2 = \\_ => [] ;
			c2 = v3.c3
		} ;

		-- V3 -> NP -> VPSlash
		Slash3V3 v3 np = predV v3 ** {
			n1 = \\_ => [] ;
			n2 = \\a => v3.c3.s ++ np.s ! NCase v3.c3.c ;
			c2 = v3.c2
		} ;

		-- V2V -> VP -> VPSlash
		SlashV2V v2v vp = predV v2v ** {
			n1 = \\_ => [] ;
			n2 = \\a => v2v.c3.s ++ infVP vp a ;
			c2 = v2v.c2 ;
		} ;

		-- V2S -> S -> VPSlash
		SlashV2S v2s s = predV v2s ** {
			n1 = \\_ => [] ;
			n2 = \\_ => s.s ;
			c2 = v2s.c2
		} ;

		-- I wonder if the preposition will ever be in its wrong place like this?
		-- V2Q -> QS -> VPSlash
		SlashV2Q v2q qs = predV v2q ** {
			n1 = \\_ => [] ;
			n2 = \\_ => qs.s ! QDir ;
			c2 = v2q.c2
		} ;

		-- V2A -> AP -> VPSlash
		SlashV2A v2a ap = predV v2a ** {
			n1 = \\_ => [] ;
			n2 = \\a =>  ap.s ! a.n ! a.g ! Weak ! v2a.c2.c ; -- or is it always Acc ?
			c2 = v2a.c2
		} ;

		-- VPSlash -> NP -> VP
		ComplSlash vps np = {
			s = vps.s ;
			obj = \\a => vps.n1 ! a ++ vps.c2.s ++ np.s ! NCase vps.c2.c ++ vps.n2 ! a ;
			verb = vps.verb ;
			pp = vps.pp ;
			a2 = vps.a2
		} ;

		-- VV -> VPSlash -> VPSlash
		SlashVV vv vps = predV vv ** {
			n1 = \\a => vv.c2.s ++ infVP vps a ;
			n2 = \\_ => [] ;
			c2 = vps.c2
		} ;

		-- V2V -> NP -> VPSlash -> VPSlash
		SlashV2VNP v2v np vps = predV v2v ** {
			n1 = \\a => v2v.c2.s ++ np.s ! NCase v2v.c2.c ++ v2v.c3.s ++ infVP vps a ;
			n2 = \\_ => [] ;
			c2 = vps.c2
		} ;

		-- VPSlash -> VP
		ReflVP vps = {
			s = vps.s ;
			obj = \\a => vps.n1 ! a ++ vps.c2.s ++ vps.n2 ! a ;
			verb = vps.verb ;
			pp = vps.pp ;
			a2 = vps.a2
		} ;

		-- Comp -> VP
		UseComp comp = predV verbBe ** {
			obj = \\a => comp.s ! a
		} ;

		-- V2 -> VP
		PassV2 v2 = 
			let
				vp = predV verbBe
			in
				{
					s = \\ten,ant,pol,agr => vf (vp.s ! ten ! ant ! pol ! agr).fin (v2.pp ! PStrong agr.n agr.g Nom) (negation pol) ;
					verb = \\vform 	=> v2.s ! vform ;
					pp = \\pform	=> v2.pp ! pform ;
					obj = \\agr	=> vp.obj ! agr ;
					a2 = [] ;
				} ;

		-- VP -> Adv -> VP
		AdvVP vp adv = vp ** {a2 = vp.a2 ++ adv.s} ;

		-- I am not sure about this function.
		-- VP -> Adv -> VP 
		ExtAdvVP vp adv = vp ** {obj = \\a => adv.s ++ vp.obj ! a} ;

		-- AdV -> VP -> VP
		AdVVP adv vp = insertAdV adv.s vp ;

		-- VPSlash -> Adv -> VPSlash
		AdvVPSlash vps adv = vps ** {a2 = vps.a2 ++ adv.s} ;

		-- AdV -> VPSlash -> VPSlash
		AdVVPSlash adv vps = (insertAdV adv.s vps) ** {c2 = vps.c2 ; n1 = vps.n1 ; n2 = vps.n2} ;

		-- VP -> Prep -> VPSlash
		VPSlashPrep vp prep = vp ** {c2 = prep ; n1,n2 = \\_ => []} ;

		-- AP -> Comp
		CompAP ap = { 
			s = \\a => ap.s ! a.n ! a.g ! Strong ! Nom ;
		} ;
				
		-- NP -> Comp
		CompNP np = {s = \\_ => np.s ! NCase Nom} ;

		-- Adv -> Comp
		CompAdv adv = {s = \\_ => adv.s} ;

		-- CN -> Comp
		CompCN cn = {
			s = \\a	=> cn.s ! a.n ! Free ! Strong ! Nom ;
		} ;

		-- VP
		UseCopula = predV verbBe ;

	oper
		insertAdV  : Str -> VP -> VP = \adv,vp -> vp ** {
			s = \\ten,ant,pol,agr => 
				let
					vps = vp.s ! ten ! ant ! pol ! agr
				in {
					fin = vps.fin ;
					inf = vps.inf ;
					a1 = vps.a1 ++ adv;
				} ;
		};
}
