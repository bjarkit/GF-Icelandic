concrete QuestionIce of Question = CatIce ** open ResIce, Prelude in {

	lin
		-- Cl -> QCl
		QuestCl cl = {
			s = \\ten,ant,pol => table {
				QDir		=> cl.s ! ten ! ant ! pol ! OQuestion ;
				QIndir		=> "ef" ++ cl.s ! ten ! ant ! pol ! ODir
			}
		} ;

		-- Added Fem and Neutr functions in extra.
		-- Not sure how the gender can be extracted from the VP, since, it might not
		-- even contain a reference to it...
		-- Also, nominative seems to be a naturall case here. But it is not general.
		-- IP -> VP -> QCl
		QuestVP ip vp = 
			let
				cl = mkClause (ip.s ! Masc ! Nom) vp {g = Masc; n = ip.n; p = P3}

			in {s = \\ten,ant,pol,_ => cl.s ! ten ! ant ! pol ! ODir} ;

		--QuestSlash  : IP -> ClSlash -> QCl ; -- whom does John love
		--missing ClSlash

		-- IComp -> NP -> QCl
		QuestIComp icomp np = 
			let
				cl = mkClause (np.s ! NCase Nom) (predV verbBe) np.a ;
				why = icomp.s ! np.a.n ! np.a.g ! Nom
			in {
				s = \\ten,ant,pol => table {
					QDir	=> why ++ cl.s ! ten ! ant ! pol ! OQuestion ;
					QIndir	=> why ++ cl.s ! ten ! ant ! pol ! ODir
				}
			} ;

		-- IAdv -> Cl -> QCl
		QuestIAdv adv cl = {
			s = \\ten,ant,pol => table {
				QDir		=> adv.s ++ cl.s ! ten ! ant ! pol ! OQuestion ;
				QIndir		=> adv.s ++ "ef" ++ cl.s ! ten ! ant ! pol ! ODir 
			}
		} ;

		-- IDet -> CN -> IP
		IdetCN idet cn = {
			s = \\_,c	=> idet.s ! cn.g ! c ++ cn.s ! idet.n ! Free ! Weak ! c ;
			n = idet.n
		} ;
		
		-- IDet -> IP
		IdetIP idet = {
			s = \\g,c	=> idet.s ! g ! c ;
			n = idet.n
		} ;

		-- IP -> Adv -> IP
		AdvIP ip adv = {
			s = \\g,c	=> ip.s ! g ! c ++ adv.s ;
			n = ip.n
		} ;

		-- IQuant -> Num -> IDet
		IdetQuant iquant num = {
			s = \\g,c => iquant.s ! num.n ! g ! c ++ num.s ! g ! c ;
			n = num.n
		} ;

		-- for feminine and neuter version, see Extra
		-- Prep -> IP -> IAdv
		PrepIP prep ip = {
			s = prep.s ++ ip.s ! Masc ! prep.c
		} ;

		-- IAdv -> Adv -> IAdv
		AdvIAdv iadv adv = {s = iadv.s ++ adv.s} ;

		-- IAdv -> IComp
		CompIAdv iadv = {s = \\_,_,_ => iadv.s} ;

		-- IP -> IComp
		CompIP ip = {s = \\_,_,_ => ip.s ! Masc ! Nom} ;

	lincat
		-- buy what where
		QVP = ResIce.VP ;
	lin
		-- VPSlash -> IP -> QVP
		ComplSlashIP vps ip = {
			s = vps.s ;
			obj = \\a => vps.n1 ! a ++ ip.s ! a.g ! vps.c2.c ++ vps.n2 ! a ;
			verb = vps.verb ;
			pp = vps.pp ;
			a2 = vps.a2
		} ;

		-- VP -> IAdv -> QVP
		AdvQVP vp iadv = vp ** {obj = \\a => vp.obj ! a ++ iadv.s} ;

		-- QVP -> IAdv -> QVP
		AddAdvQVP qvp iadv = qvp ** {obj = \\a => qvp.obj ! a ++ iadv.s} ;

		-- IP -> QVP -> QCl
		QuestQVP ip vp = 
			let
				cl = mkClause (ip.s ! Masc ! Nom) vp {g = Masc; n = ip.n; p = P3}
			in { s = \\ten,ant,pol,_ =>  cl.s ! ten ! ant ! pol ! ODir } ;

}
