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
			b = Indef ;
			d = Strong ;
			isPre = True
		} ;
		everywhere_Adv = mkAdv "alls staðar" ;
		few_Det = {
			s = table {
				Masc	=> caseList "fáeinir" "fáeina" "fáeinum" "fáeinna" ;
				Fem	=> caseList "fáeinar" "fáeinar" "fáeinum" "fáeinna" ;
				Neutr	=> caseList "fáein" "fáein" "fáeinum" "fáeinna"
			} ;
			n = Pl ;
			b = Indef ;
			d = Strong ;
			isPre = True
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
			b = Indef ;
			d = Strong ;
			isPre = True
		} ;
		somePl_Det = {
			s = table {
				Masc	=> caseList "nokkrir" "nokkra" "nokkrum" "nokkurra" ;
				Fem	=> caseList "nokkrar" "nokkrar" "nokkrum" "nokkurra" ;
				Neutr	=> caseList "nokkur" "nokkur" "nokkrum" "nokkurra"
			} ;
			n = Pl ;
			b = Indef ;
			d = Strong ;
			isPre = True
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
			d = Weak ;
			isPre = True
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
			d = Weak ;
			isPre = True
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
		i_Pron  = mkPronPers "ég" "mig" "mér" "mín" "minn" "minn" "mínum" "míns" "mín" "mína" "minni" "minnar" "mitt" "mitt" "mínu" "míns" "mínir" "mína" "mínum" "minna" "mínar" "mínar" "mín" "mín" Masc Sg P1 ;
		youSg_Pron = mkPronPers "þú" "þig" "þér" "þín" "þinn" "þinn" "þínum" "þíns" "þín" "þína" "þinni" "þinnar" "þitt" "þitt" "þínu" "þíns" "þínir" "þína" "þínum" "þinna" "þínar" "þínar" "þín" "þín" Masc Sg P2 ;
		-- He, she and it are complicated regarding possessions. Sinn is 
		-- used for thrid persons singular, but only if it is the subject 
		-- of the sentence, otherwise the genitive of the personal pronoun 
		-- (hans) is used.
		--  he_Pron = mkPronPers "hann" "hann" "honum" "hans" Masc Sg P3 ;
		--  she_Pron = mkPronPers "hún" "hana" "henni" "hennar" Fem Sg P3 ;
		--  it_Pron = mkPronPers "það" "það" "því" "þess" Neutr Sg P3 ;
		--  they_Pron = mkPronPers depends on gender,i.e. has 3x4 forms for personal pronouns and is complicated as a possessive
		--  we_Pron = mkPronPers "við" "okkur" "okkur" "okkar" the possesive equivalent, vor, is mostly used in elevated style.
		--  youPl_Pron = mkPronPers "þið" "ykkur" "ykkur" "ykkar" the possesive equivalent is complicated 
		--  youPol_Pron = mkPron "you" "you" "your" "yours" singular P2 human ;

--  whatPl_IP = mkIP "what" "what" "what's" plural ;
--  whatSg_IP = mkIP "what" "what" "what's" singular ;
--  whoPl_IP = mkIP "who" "whom" "whose" plural ;
--  whoSg_IP = mkIP "who" "whom" "whose" singular ;

}
