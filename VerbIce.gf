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
					n2 = \\a => vvp.n2 ! a ++ vv.c2.s ++ infVP vp a
				} ;

		-- VS  -> S  -> VP
		ComplVS vs s =
			let
				vvs = predV vs
			in
				vvs ** {
					n2 = \\a => vvs.n2 ! a ++ s.s
				} ;

		-- VQ  -> QS -> VP
		ComplVQ vq qs =
			let
				vvq = predV vq
			in
				vvq ** {
					n2 = \\a => vvq.n2 ! a ++ qs.s ! QDir
				} ;

		-- VA  -> AP -> VP
		ComplVA va ap =
			let
				vp = (predV va)
			in
				vp ** {
					n2 = \\a => ap.s ! a.n ! a.g ! Weak ! Nom ++ vp.n2 ! a
				} ;

		-- V2 -> VPSlash
		SlashV2a v = predV v ** {
			c2 = v.c2 ;
			nn1,nn2 = \\_ => [] ;
		} ;

		-- V3 -> NP -> VPSlash
		Slash2V3 v3 np = predV v3 ** {
			n1 = \\a => case np.isPron of {
				True	=> <[],v3.c2.s ++ np.s ! NCase v3.c2.c> ;
				False	=> <[],[]>
			} ;
			nn1 = \\_ => [] ;
			nn2 = \\a => case np.isPron of {
				True	=> [] ;
				False	=> v3.c2.s ++ np.s ! NCase v3.c2.c
			} ;
			c2 = v3.c3 ;
			en1p1 = False
		} ;

		-- V3 -> NP -> VPSlash
		Slash3V3 v3 np = predV v3 ** {
			n1 = \\a => case np.isPron of {
				True	=> <v3.c3.s ++ np.s ! NCase v3.c3.c,[]> ;
				False	=> <[],[]>
			} ;
			nn1 = \\a => case np.isPron of {
				True	=> [] ;
				False	=> v3.c3.s ++ np.s ! NCase v3.c3.c
			} ;
			nn2 = \\_ => [] ;
			c2 = v3.c2 ;
			en1p1 = case np.isPron of {
				True	=> True ;
				False	=> False
			}
		} ;

		-- V2V -> VP -> VPSlash
		SlashV2V v2v vp = predV v2v ** {
			nn1 = \\_ => [] ;
			nn2 = \\a => v2v.c3.s ++ infVP vp a ;
			c2 = v2v.c2 ;
		} ;

		-- V2S -> S -> VPSlash
		SlashV2S v2s s = predV v2s ** {
			nn1 = \\_ => [] ;
			nn2 = \\_ => s.s ;
			c2 = v2s.c2
		} ;

		-- I wonder if the preposition will ever be in its wrong place like this?
		-- V2Q -> QS -> VPSlash
		SlashV2Q v2q qs = predV v2q ** {
			nn1 = \\_ => [] ;
			nn2 = \\_ => qs.s ! QDir ;
			c2 = v2q.c2
		} ;

		-- V2A -> AP -> VPSlash
		SlashV2A v2a ap = predV v2a ** {
			nn1 = \\_ => [] ;
			nn2 = \\a =>  ap.s ! a.n ! a.g ! Strong ! v2a.c2.c ;
			c2 = v2a.c2
		} ;

		-- VPSlash -> NP -> VP
		ComplSlash vps np = {
			s = vps.s ;
			n1 = \\a => vps.n1 ! a ;
			n2 = \\a => vps.nn1 ! a ++ vps.c2.s ++ np.s ! NCase vps.c2.c ++ vps.nn2 ! np.a ;
			verb = vps.verb ;
			p = vps.p ;
			a2 = vps.a2 ;
			en1p1 = vps.en1p1
		} ;

		-- VV -> VPSlash -> VPSlash
		SlashVV vv vps = predV vv ** {
			nn1 = \\a => vv.c2.s ++ infVP vps a ;
			nn2 = \\_ => [] ;
			c2 = vps.c2
		} ;

		-- V2V -> NP -> VPSlash -> VPSlash
		SlashV2VNP v2v np vps = predV v2v ** {
			nn1 = \\a => v2v.c2.s ++ np.s ! NCase v2v.c2.c ++ v2v.c3.s ++ infVP vps a ;
			nn2 = \\_ => [] ;
			c2 = vps.c2
		} ;

		-- VPSlash -> VP
		ReflVP vps = {
			s = vps.s ;
			n1 = \\a => vps.n1 ! a ;
			n2 = \\a => vps.nn1 ! a ++ vps.c2.s ++ vps.nn2 ! a ++ reflPron a.n a.g vps.c2.c ;
			verb = vps.verb ;
			p = vps.p ;
			a2 = vps.a2 ;
			en1p1 = vps.en1p1
		} ;

		-- Comp -> VP
		UseComp comp = predV verbBe ** {
			n2 = \\a => comp.s ! a
		} ;

		-- V2 -> VP
		PassV2 v2 = 
			let
				vp = predV verbBe
			in
				{
					s = \\vpform,pol,agr => vf (vp.s ! vpform ! pol ! agr).fin (v2.p ! PStrong agr.n agr.g Nom) (negation pol) True;
					verb = \\vform 	=> v2.s ! vform ;
					p = \\pform	=> v2.p ! pform ;
					n1 = \\_	=> <[],[]> ;
					n2 = \\agr	=> vp.n2 ! agr ;
					a2 = [] ;
					en1p1 = False
				} ;

		-- VP -> Adv -> VP
		AdvVP vp adv = vp ** {a2 = vp.a2 ++ adv.s} ;

		-- VP -> Adv -> VP 
		ExtAdvVP vp adv = vp ** {n2 = \\a => adv.s ++ vp.n2 ! a} ;

		-- AdV -> VP -> VP
		AdVVP adv vp = insertAdV adv.s vp ;

		-- VPSlash -> Adv -> VPSlash
		AdvVPSlash vps adv = vps ** {a2 = vps.a2 ++ adv.s} ;

		-- AdV -> VPSlash -> VPSlash -- what the hell was I smoking?
		-- AdVVPSlash : AdV -> VPSlash -> VPSlash ;  -- always use (it)
		AdVVPSlash adv vps = (insertAdV adv.s vps) ** {
			c2 = vps.c2 ;
			n1 = vps.n1 ;
			nn1 = vps.nn1 ;
			nn2 = vps.nn2 ; 
			en1p1 = vps.en1p1
		} ;

		-- VP -> Prep -> VPSlash
		VPSlashPrep vp prep = vp ** {c2 = prep ; nn1,nn2 = \\_ => []} ;

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
			s = \\vpform,pol,agr => 
				let
					vps = vp.s ! vpform ! pol ! agr
				in {
					fin = vps.fin ;
					inf = vps.inf ;
					a1 = case vpform of {
						VPImp | VPMood Pres Simul | VPMood Past Simul
								=> <vps.a1.p1, vps.a1.p2 ++ adv> ;
						_		=> <vps.a1.p1 ++ adv, vps.a1.p2>
					};
				} ;
		};
}
