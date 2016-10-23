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

		-- need other extra functions to capture some agreement functionalities (like in QuestVP)
		-- IP -> ClSlash -> QCl ; -- whom does John love
		QuestSlash ip cls = {
			s = \\ten,ant,pol,_ => ip.s ! Masc ! Nom ++ cls.s ! ten ! ant ! pol ! ODir ++ cls.c2.s ++ cls.n3 ! gennumperToAgr Neutr Sg P3 
		} ;

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
			n1 = vps.n1 ;
			n2 = \\a => vps.nn1 ! a ++ ip.s ! a.g ! vps.c2.c ++ vps.nn2 ! a ;
			verb = vps.verb ;
			p = vps.p ;
			a2 = vps.a2 ;
			en1p1 = vps.en1p1
		} ;

		-- VP -> IAdv -> QVP
		AdvQVP vp iadv = vp ** {n2 = \\a => vp.n2 ! a ++ iadv.s} ;

		-- QVP -> IAdv -> QVP
		AddAdvQVP qvp iadv = qvp ** {n2 = \\a => qvp.n2 ! a ++ iadv.s} ;

		-- IP -> QVP -> QCl
		QuestQVP ip vp = 
			let
				cl = mkClause (ip.s ! Masc ! Nom) vp {g = Masc; n = ip.n; p = P3}
			in { s = \\ten,ant,pol,_ =>  cl.s ! ten ! ant ! pol ! ODir } ;

}
