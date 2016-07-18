concrete StructuralIce of Structural = CatIce ** 
	open MorphoIce, ResIce, ParadigmsIce, MakeStructuralIce, 
	(C = ConstructX), Prelude in {

	lin
		above_Prep = mkPrep "ofan" genitive ;
		after_Prep = mkPrep "eftir" dative ;
		almost_AdA = mkAdA "næstum" ;
		before_Prep = mkPrep "fyrir" dative ;
		behind_Prep = mkPrep "fyrir aftan" dative ;
		between_Prep = mkPrep "á milli" genitive ;
		almost_AdN = mkAdN "næstum" ;
		although_Subj = ss "þó" ;
		by8agent_Prep = mkPrep "hjá" dative ;
		by8means_Prep = mkPrep "hjá" dative ;
		during_Prep = mkPrep "á meðan" nominative ;
		every_Det = {
			s = table {
				Masc	=> caseList "sérhver" "sérhvern" "sérhverjum" "sérhvers" ;
				Fem	=> caseList "sérhver" "sérhverja" "sérhverri" "sérhverrar" ;
				Neutr	=> caseList "sérhvert" "sérhvert" "sérhverju" "sérhvers"
			} ;
			n = Sg ;
			b = Def ;
			d = Strong -- or weak?
		} ;
		everywhere_Adv = mkAdv "alls staðar" ;
		few_Det = {
			s = table {
				Masc	=> caseList "fáeinir" "fáeina" "fáeinum" "fáeinna" ;
				Fem	=> caseList "fáeinar" "fáeinar" "fáeinum" "fáeinna" ;
				Neutr	=> caseList "fáein" "fáein" "fáeinum" "fáeinna"
			} ;
			n = Pl ;
			b = Def ;
			d = Strong
		} ;
		for_Prep = mkPrep "fyrir" dative ;
		from_Prep = mkPrep "frá" dative ;
		here_Adv = mkAdv "hérna" ;
		here7to_Adv = mkAdv ["hingað"] ;
		here7from_Adv = mkAdv ["héðan"] ;
		in8front_Prep = mkPrep ["fyrir framan"] accusative ;
		in_Prep = mkPrep "í" dative ;
		no_Utt = ss "nei" ;
		part_Prep = mkPrep "af" dative ;
		quite_Adv = mkAdv "alveg" ;
		someSg_Det = {
			s = table {
				Masc	=> caseList "nokkur" "nokkurn" "nokkrum" "nokkurs" ;
				Fem	=> caseList "nokkur" "nokkra" "nokkurri" "nokkurrar" ;
				Neutr	=> caseList "nokkurt" "nokkurt" "nokkru" "nokkurs"
			} ;
			n = Sg ;
			b = Def ;
			d = Weak
		} ;
		somePl_Det = {
			s = table {
				Masc	=> caseList "nokkrir" "nokkra" "nokkrum" "nokkurra" ;
				Fem	=> caseList "nokkrar" "nokkrar" "nokkrum" "nokkurra" ;
				Neutr	=> caseList "nokkur" "nokkur" "nokkrum" "nokkurra"
			} ;
			n = Pl ;
			b = Def ;
			d = Weak
		} ;
		through_Prep = mkPrep "gegnum" accusative ;
		too_AdA = mkAdA "líka" ;
		to_Prep = mkPrep "til" genitive ;
		very_AdA = mkAdA "mjög" ;
		without_Prep = mkPrep "án" genitive ;
		yes_Utt = ss "já" ;
		at_least_AdN = mkAdN "að minnsta kosti" ;
		at_most_AdN = mkAdN "í mesta lagi" ;
		except_Prep = mkPrep "nema" nominative;

		so_AdA = mkAdA "svo" ;
		somewhere_Adv = mkAdv "einhvers staðar" ;
		there_Adv = mkAdv "þarna" ;
		therefore_PConj = ss "þar af leiðandi" ;

		-- To my knowledge there is no special difference when reffering to a specific known object (or person) 
		-- that is far away or close (emotionally or physically) in Icelandic - without specifiying the distance further, 
		-- e.g., with an adverb "þessi hlutur hérna" = "this object here" and "þessi hlutur þarna" = "that object there".
		-- But one could argue that "þessi/sá" =~ "this/that". There is also another demonstrative determiner in 
		-- Icelandic, "hinn" = "the other one". Atm I use the "þessi/sá" = "this/that". 
		-- Later I will change to "þessi hér" == "this" and "þessi þarna" == "that".
		this_Quant = {
			s = table {
				Sg	=> table {
						Masc	=> caseList "þessi" "þennan" "þessum" "þessa" ;
						Fem	=> caseList "þessi" "þessa" "þessari" "þessarar" ;
						Neutr	=> caseList "þetta" "þetta" "þessu" "þessa"
				} ;
				Pl	=> table {
						Masc	=> caseList "þessir" "þessa" "þessum" "þessara" ;
						Fem	=> caseList "þessar" "þessar" "þessum" "þessara" ;
						Neutr	=> caseList "þessi" "þessi" "þessum" "þessara"
				}
			} ;
			b = Def ;
			d = Weak
		} ;
		that_Quant =  {
			s = table {
				Sg	=> table {
						Masc	=> caseList "sá" "þann" "þeim" "þess" ;
						Fem	=> caseList "sú" "þá" "þeirri" "þeirrar" ;
						Neutr	=> caseList "það" "það" "því" "þess"
				} ;
				Pl	=> table {
						Masc	=> caseList "þeir" "þá" "þeim" "þeirra" ;
						Fem	=> caseList "þær" "þær" "þeim" "þeirra" ;
						Neutr	=> caseList "þau" "þau" "þeim" "þeirra"
				}
			} ;
			b = Def ;
			d = Weak
		} ;
		and_Conj = mkConj "og" ;
		or_Conj = mkConj "eða" singular ;
		if_then_Conj = mkConj "ef" "þá" singular ;
		either7or_DConj = mkConj "annaðhvort" "eða" singular ;
		otherwise_PConj = ss "annars" ;
		that_Subj = ss "að" ;
		because_Subj = ss "af því að" ;
		both7and_DConj = mkConj "bæði" "og";
		but_PConj = ss "en" ;
		how_IAdv = ss "hvernig" ;
		how8much_IAdv = ss "hversu mikið" ;
		if_Subj = ss "ef" ;
		please_Voc = ss "vinsamlegast" ;
		when_Subj = ss "þegar" ;
		-- where_IAdv = ss "þar" ; þar, hvar, hvert?
		why_IAdv = ss "af hverju" ;
		yes_Phr = ss "já" ;
}
