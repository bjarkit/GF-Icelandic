--# -path=.:../abstract:../common:../../prelude

--1 Icelandic auxiliary operations.

-- This module contains operations that are needed to make the
-- resource syntax work. To define everything that is needed to
-- implement $Test$, it moreover contains regular lexical
-- patterns needed for $Lex$.

resource ResIce = ParamX ** open Prelude in {

	flags optimize=all ;

	--------------------------------------------
	--PARAMETERS DEFINITIONS
	--------------------------------------------

	-- Some parameters, such as $Number$, are inherited from $ParamX$.

	--2 For $Noun$

	param

		-- These are the standard four-value case and three-value gender as needed when inflecting nouns.
		Case = Nom | Acc | Dat | Gen ;
		Gender = Masc | Fem | Neutr ;

		-- There is no indefinite article in Icelandic, i.e. "a house" will "hús". The definite
		-- article on the other hand can be either freestanding or used as a suffix (for all genders).
		-- The freestanding version is rare and can only be followed by an adjective and the suffix 
		-- depends on gender and the ending of the noun.
		Species = Def | Indef | Suffix;
	
		-- Agreement of noun phrases has three parts
		Agr = Ag Gender Number Person ;


	--2 For $Verb$

		Mood = Indicative | Subjunctive ;

		Voice = Active | Middle ;--| Passive -> just aux (að vera - to be) + past participle - to be added :D

		PForm = PWeak Number Gender Case | PStrong Number Gender Case ;

		VForm =
			VInf
			| VPres Voice Mood Number Person
			| VPast Voice Mood Number Person
			| VImp Voice Number
			| VPresPart -- It doesn really inflect, i.e. same form in all cases, genders and number
			| VSup Voice
			;

	--2 For $Adjective$

		Declension = Weak | Strong ;

		AForm = 
			  APosit Declension Number Gender Case
			| ACompar Number Gender Case
			| ASuperl Declension Number Gender Case
			| AAdv
			;
		
	--2 For $Sentence$

		-- This is a direct copy paste from english atm
		Order = ODir | OQuestion ;

	--------------------------------------------
	--TYPE DEFINITIONS + WORST-CASE CONSTRUCTORS
	--------------------------------------------

	-- For $Lex$.
	oper

		caseList : (_,_,_,_ : Str) -> Case => Str =
			\n,a,d,g -> table {
				Nom => n ;
				Acc => a ;
				Dat => d ;
				Gen => g
			} ;

		-- For $Nouns$

		N : Type = { 
			s : Number => Species => Case => Str ;
			g : Gender
		} ;

		NP : Type = {
			s : Case => Str;
			a : Agr
		} ;

		mkNoun : (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_ :  Str) -> Gender -> N =
			\hestur,hest,hesti,hests,
			hesturinn,hestinn,hestinum,hestsins,
			hestar,hestaAcc,hestum,hestaGen,
			hestarnir,hestana,hestunum,hestanna,g -> {
				s = table {
					Sg => table {
						Suffix	=> caseList hesturinn hestinn hestinum hestsins ;
						_	=> caseList hestur hest hesti hests
					} ;
					Pl => table {
						Suffix	=> caseList hestarnir hestana hestunum hestanna ;
						_	=> caseList hestar hestaAcc hestum hestaGen
					}
				} ;
				g = g
		} ;

		-- For $Adjectives$

		A : Type = {
			s : AForm => Str
		} ;

		mkAdjective : (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_ : Str) -> A =
			\sgMascNom,sgMascAcc,sgMascDat,sgMascGen,
			sgFemNom,sgFemAcc,sgFemDat,sgFemGen,
			sgNeutNom,sgNeutAcc,sgNeutDat,sgNeutGen,
			plMascNom,plMascAcc,plMascDat,plMascGen,
			plFemNom,plFemAcc,plFemDat,plFemGen,
			plNeutNom,plNeutAcc,plNeutDat,plNeutGen,
			weakSgMascNom,weakSgMascAccDatGen,
			weakSgFemNom,weakSgFemAccDatGen,
			weakSgNeut,weakPl,
			comSgMascNom,comSgNeutrNom,comPl,
			supSgMascNom,supSgMascAcc,supSgMascDat,supSgMascGen,
			supSgFemNom,supSgFemAcc,supSgFemDat,supSgFemGen,
			supSgNeutNom,supSgNeutAcc,supSgNeutDat,supSgNeutGen,
			supPlMascNom,supPlMascAcc,supPlMascDat,supPlMascGen,
			supPlFemNom,supPlFemAcc,supPlFemDat,supPlFemGen,
			supPlNeutNom,supPlNeutAcc,supPlNeutDat,supPlNeutGen,
			supWeakSgMascNom,supWeakSgMascAccDatGen,
			supWeakSgFemNom,supWeakSgFemAccDatGen,
			supWeakSgNeut,supWeakPl,adv -> {
				s = table {
					APosit Weak Sg Masc c		=> caseList weakSgMascNom weakSgMascAccDatGen weakSgMascAccDatGen weakSgMascAccDatGen ! c ;
					APosit Weak Sg Fem c		=> caseList weakSgFemNom weakSgFemAccDatGen weakSgFemAccDatGen weakSgFemAccDatGen ! c ;
					APosit Weak Sg Neutr c		=> caseList weakSgNeut weakSgNeut weakSgNeut weakSgNeut ! c ;
					APosit Weak Pl _ c 		=> caseList weakPl weakPl weakPl weakPl ! c ;
					APosit Strong Sg Masc c		=> caseList sgMascNom sgMascAcc sgMascDat sgMascGen ! c ;
					APosit Strong Sg Fem c		=> caseList sgFemNom sgFemAcc sgFemDat sgFemGen ! c ;
					APosit Strong Sg Neutr c	=> caseList sgNeutNom sgNeutAcc sgNeutDat sgNeutGen ! c ;
					APosit Strong Pl Masc c		=> caseList plMascNom plMascAcc plMascDat plMascGen ! c ;
					APosit Strong Pl Fem c		=> caseList plFemNom plFemAcc plFemDat plFemGen ! c ;
					APosit Strong Pl Neutr c	=> caseList plNeutNom plNeutAcc plNeutDat plNeutGen ! c ;
					ACompar Sg Masc c		=> caseList comSgMascNom comSgMascNom comSgMascNom comSgMascNom ! c ;
					ACompar Sg Fem c		=> caseList comSgMascNom comSgMascNom comSgMascNom comSgMascNom ! c ;
					ACompar Sg Neutr c		=> caseList comSgNeutrNom comSgNeutrNom comSgNeutrNom comSgNeutrNom ! c ;
					ACompar Pl _ c			=> caseList comPl comPl comPl comPl ! c ;
					ASuperl Weak Sg Masc c		=> caseList supWeakSgMascNom supWeakSgMascAccDatGen supWeakSgMascAccDatGen supWeakSgMascAccDatGen ! c ;
					ASuperl Weak Sg Fem c		=> caseList supWeakSgFemNom supWeakSgFemAccDatGen supWeakSgFemAccDatGen supWeakSgFemAccDatGen ! c ;
					ASuperl Weak Sg Neutr c		=> caseList supWeakSgNeut supWeakSgNeut supWeakSgNeut supWeakSgNeut ! c ;
					ASuperl Weak Pl _ c 		=> caseList supWeakPl supWeakPl supWeakPl supWeakPl ! c ;
					ASuperl Strong Sg Masc c	=> caseList supSgMascNom supSgMascAcc supSgMascDat supSgMascGen ! c ;
					ASuperl Strong Sg Fem c		=> caseList supSgFemNom supSgFemAcc supSgFemDat supSgFemGen ! c ;
					ASuperl Strong Sg Neutr c	=> caseList supSgNeutNom supSgNeutAcc supSgNeutDat supSgNeutGen ! c ;
					ASuperl Strong Pl Masc c	=> caseList supPlMascNom supPlMascAcc supPlMascDat supPlMascGen ! c ;
					ASuperl Strong Pl Fem c		=> caseList supPlFemNom supPlFemAcc supPlFemDat supPlFemGen ! c ;
					ASuperl Strong Pl Neutr c	=> caseList supPlNeutNom supPlNeutAcc supPlNeutDat supPlNeutGen ! c ;
					AAdv				=> adv
				}
		} ;

		-- For $Verb$.

		V : Type = {
			s : VForm => Str ;
			pp : PForm => Str
		} ;

		mkVoice : Voice -> Str -> Str = \v,s ->
			case v of {
				Active 	=> s ;
				Middle	=> case s of {
					base + "ur"	=> base + "st" ;
					base + "r"	=> base + "st" ;
					base + "ð"	=> base + "st" ;
					base + "ðu"	=> base + "stu" ;
					_		=> s + "st"
				}
		} ;

		mkVerb : (x1,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x59 : Str) -> V =
			\fljúga,flýg,flýgur2,flýgur3,fljúgum,fljúgið,fljúga,flaug1,flaugst,flaug2,flugum,fluguð,flugu,
			fljúgi1,fljúgir,fljúgi3,fljúgumS,fljúgiðS,fljúgi,flygi1,flygir,flygi2,flygjum,flygjuð,flygju,
			fljúgðu,fljúgið,fljúgandi,floginn,sgMascAcc,sgMascDat,sgMascGen,sgFemNom,sgFemAcc,sgFemDat,sgFemGen,
			sgNeutNom,sgNeutAcc,sgNeutDat,sgNeutGen,plMascNom,plMascAcc,plMascDat,plMascGen,
			plFemNom,plFemAcc,plFemDat,plFemGen,plNeutNom,plNeutAcc,plNeutDat,plNeutGen,
			weakSgMascNom,weakSgMascAccDatGen,weakSgFemNom,weakSgFemAccDatGen,weakSgNeut,weakPl,flogið -> {
				s = table { -- This, along with pp, can and will be simplified much further in paradigms
					VInf				=> fljúga ;
					VPres v Indicative Sg P1	=> mkVoice v flýg ;
					VPres v Indicative Sg P2	=> mkVoice v flýgur2 ;
					VPres v Indicative Sg P3	=> mkVoice v flýgur3 ;
					VPres v Indicative Pl P1	=> mkVoice v fljúgum ;
					VPres v Indicative Pl P2	=> mkVoice v fljúgið ;
					VPres v Indicative Pl P3	=> mkVoice v fljúga ;
					VPast v Indicative Sg P1	=> mkVoice v flaug1 ;
					VPast v Indicative Sg P2	=> mkVoice v flaugst ;
					VPast v Indicative Sg P3	=> mkVoice v flaug2 ;
					VPast v Indicative Pl P1	=> mkVoice v flugum ;
					VPast v Indicative Pl P2	=> mkVoice v fluguð ;
					VPast v Indicative Pl P3	=> mkVoice v flugu ;
					VPres v Subjunctive Sg P1	=> mkVoice v fljúgi1 ;
					VPres v Subjunctive Sg P2	=> mkVoice v fljúgir ;
					VPres v Subjunctive Sg P3	=> mkVoice v fljúgi3 ;
					VPres v Subjunctive Pl P1	=> mkVoice v fljúgumS ;
					VPres v Subjunctive Pl P2	=> mkVoice v fljúgiðS ;
					VPres v Subjunctive Pl P3	=> mkVoice v fljúgi ;
					VPast v Subjunctive Sg P1	=> mkVoice v flygi1 ;
					VPast v Subjunctive Sg P2	=> mkVoice v flygir ;
					VPast v Subjunctive Sg P3	=> mkVoice v flygi2 ;
					VPast v Subjunctive Pl P1	=> mkVoice v flygjum ;
					VPast v Subjunctive Pl P2	=> mkVoice v flygjuð ;
					VPast v Subjunctive Pl P3	=> mkVoice v flygju ;
					VImp v Sg			=> mkVoice v fljúgðu ;
					VImp v Pl			=> mkVoice v fljúgið ;
					VPresPart 			=> fljúgandi ;
					VSup v				=> mkVoice v flogið
				} ;
				pp = table {
					PWeak Sg Masc Nom	=> weakSgMascNom ;
					PWeak Sg Masc _		=> weakSgMascAccDatGen ;
					PWeak Sg Fem Nom	=> weakSgFemNom ;
					PWeak Sg Fem _		=> weakSgFemAccDatGen ;
					PWeak Sg Neutr _	=> weakSgNeut ;
					PWeak Pl _ _		=> weakPl ;
					PStrong Sg Masc c	=> caseList floginn sgMascAcc sgMascDat sgMascGen ! c ;
					PStrong Sg Fem c	=> caseList sgFemNom sgFemAcc sgFemDat sgFemGen ! c ;
					PStrong Sg Neutr c	=> caseList sgNeutNom sgNeutAcc sgNeutDat sgNeutGen ! c ;
					PStrong Pl Masc c	=> caseList plMascNom plMascAcc plMascDat plMascGen ! c ;
					PStrong Pl Fem c	=> caseList plFemNom plFemAcc plFemDat plFemGen ! c ;
					PStrong Pl Neutr c	=> caseList plNeutNom plNeutAcc plNeutDat plNeutGen ! c
				}
		} ;

		VP : Type = {
			s 	: Tense => Anteriority => Polarity => Agr => Str ;
			obj 	: Agr => Str;
		} ;


		-- FIXME : Hardcoded VInf for the sake of testing -> todo general VForm
		infVP : VP -> Agr -> Str = \vp,agr -> infVPPlus vp Pres Simul Pos agr ;

		infVPPlus : VP -> Tense -> Anteriority -> Polarity -> Agr -> Str = \vp,ten,ant,pol,ag -> 
			vp.s ! ten ! ant ! pol ! ag ++ vp.obj ! ag ;

		--Agr = Ag Gender Number Person ;
		predV : V -> VP = \v -> {
			s = \\ten,ant,pol,agr => case <ten,ant,pol,agr> of {
				-- hann sefur 'he sleeps'
				<Pres,Simul,Pos,Ag g n p>	=> v.s ! VPres Active Indicative n p ;
				-- hann sefur ekki 'he doesn't sleep'
				<Pres,Simul,Neg,Ag g n p>	=> v.s ! VPres Active Indicative n p ++ "ekki" ;
				-- hann hefur sofið 'he has slept'
				<Pres,Anter,Pos,Ag g n p>	=> verbHave.s ! VPres Active Indicative n p ++ v.s ! VSup Active ;
	 			-- hann hefur ekki sofið 'he hasn't slept'
				<Pres,Anter,Neg,Ag g n p>	=> verbHave.s ! VPres Active Indicative n p ++ "ekki" ++ v.s ! VSup Active ;
				-- hann svaf 'he slept'
				<Past,Simul,Pos,Ag g n p> 	=> v.s ! VPast Active Indicative n p ;
	 			-- hann svaf ekki 'he didn't sleep'
				<Past,Simul,Neg,Ag g n p> 	=> v.s ! VPast Active Indicative n p ++ "ekki" ;
				-- hann hafði sofið 'he had slept'
				<Past,Anter,Pos,Ag g n p> 	=> verbHave.s ! VPast Active Indicative n p ++ v.s ! VSup Active ;
	 			-- hann hafði ekki sofið 'he hadn't slept'
				<Past,Anter,Neg,Ag g n p> 	=> verbHave.s ! VPast Active Indicative n p ++ "ekki" ++ v.s ! VSup Active ;
	 			-- hann mun sofa 'he will sleep'
				<Fut,Simul,Pos,Ag g n p>	=> verbWill.s ! VPres Active Indicative n p ++ v.s ! VInf ;
	 			-- hann mun ekki sofa 'he won't sleep'
				<Fut,Simul,Neg,Ag g n p>	=> verbWill.s ! VPres Active Indicative n p ++ "ekki" ++ v.s ! VInf ;
				-- hann mun hafa sofið 'he will have slept'	
				<Fut,Anter,Pos,Ag g n p>	=> verbWill.s ! VPres Active Indicative n p ++ verbHave.s ! VInf ++ v.s ! VSup Active ;
				-- hann mun ekki hafa sofið 'he won't have slept'
				<Fut,Anter,Neg,Ag g n p>	=> verbWill.s ! VPres Active Indicative n p ++ "ekki" ++ verbHave.s ! VInf ++ v.s ! VSup Active ;
				-- hann myndi sofa 'he would sleep'
				<Cond,Simul,Pos,Ag g n p>	=> verbWill.s ! VPast Active Subjunctive n p ++ v.s ! VInf ;
				-- hann myndi ekki sofa 'he wouldn't sleep'
				<Cond,Simul,Neg,Ag g n p>	=> verbWill.s ! VPast Active Subjunctive n p ++ "ekki" ++ v.s ! VInf ;
				-- hann myndi hafa sofið 'he would have slept'
				<Cond,Anter,Pos,Ag g n p>	=> verbWill.s ! VPast Active Subjunctive n p ++ verbHave.s ! VInf ++ v.s ! VSup Active ;
				-- hann myndi ekki hafa sofið 'he wouldn't have slept'
				<Cond,Anter,Neg,Ag g n p>	=> verbWill.s ! VPast Active Subjunctive n p ++ "ekki" ++  verbHave.s ! VInf ++ v.s ! VSup Active
			} ;
			obj = \\_ => []
		} ;

		-- Auxilary verbs --

		-- Auxilary verbs do not forma special group in Icelandic. But many of them do have or rather dont have 
		-- the same forms as other verbs. As an example the verb "að vera" (e. to be) does not have the past 
		-- participle nor does it exist in the middle voice or passive voice. Therefore, I will (for the time being)
		-- fill in the remaining with the infinitive "vera". This goes also for the rest of the auxileries.

		verbBe : V = mkVerb "vera" "er" "ert" "er" "erum" "eruð" "eru" "var" "varst" "var" "vorum" "voruð" "voru"
					"sé" "sért" "sé" "séum" "séuð" "séu" "væri" "værir" "væri" "værum" "voruð" "væru"
					"vertu" "verið" "verandi" "vera" "vera" "vera" "vera" "vera" "vera" "vera" "vera"
					"vera" "vera" "vera" "vera" "vera" "vera" "vera" "vera"
					"vera" "vera" "vera" "vera" "vera" "vera" "vera" "vera"
					"vera" "vera" "vera" "vera" "vera" "vera" "verið" ;

		verbBecome : V = mkVerb "verða" "verð" "verður" "verður" "verðum" "verðið" "verða" "varð" "varðst" "varð" "urðum" "urðuð" "urðu"
					"verði" "verðir" "verði" "verðum" "verðið" "verði" "yrði" "yrðir" "yrði" "yrðum" "yrðuð" "yrðu"
					"verðið" "verðið" "verðandi" "orðinn" "orðinn" "orðnum" "orðsins" "orðin" "orðna" "orðinni" "orðinnar"
					"orðið" "orðið" "orðnu" "orðins" "orðnir" "orðna" "orðnum" "orðinna" "orðnar" "orðnar" "orðnum" "orðinna"
					"orðin" "orðin" "orðnum" "orðinna" "orðni" "orðna" "orðna" "orðnu" "orðna" "orðnu" "orðið" ;

		verbHave : V = mkVerb "hafa" "hef" "hefur" "hefur" "höfum" "hafið" "hafa" "hafði" "hafðir" "hafði" "höfðum" "höfðuð" "höfðu"
					"hafi" "hafir" "hafi" "höfðum" "hafið" "hafi" "hefði" "hefðir" "hefði" "hefðum" "hefðuð" "hefðu"
					"hafðu" "hafið" "hafandi" "hafður" "hafðan" "höfðum" "hafðs" "höfð" "hafða" "hafðri" "hafðrar" "haft"
					"haft" "höfðu" "hafðs" "hafðir" "hafða" "höfðum" "hafðra" "hafðar" "hafðar" "höfðum" "hafðra" "höfð"
					"höfð" "höfðum" "hafðra" "hafa" "hafa" "hafa" "hafa" "hafa" "hafa" "haft" ;

		verbWill : V = mkVerb "munu" "mun" "munt" "mun" "munum" "munuð" "munu" "munu" "munu" "munu" "munu" "munu" "munu"
					"muni" "munir" "muni" "munum" "munið" "muni" "myndi" "myndir" "myndi" "myndum" "mynduð" "myndu"
					"munu" "munu" "munu" "munu" "munu" "munu" "munu" "munu" "munu" "munu" "munu" "munu"
					"munu" "munu" "munu" "munu" "munu" "munu" "munu" "munu" "munu" "munu" "munu" "munu"
					"munu" "munu" "munu" "munu" "munu" "munu" "munu" "munu" "munu" "munu" ;

		Preposition : Type = {
			s : Str ;
			c : Case
		} ;

		Cl : Type = {
			s : Tense => Anteriority => Polarity => Order => Str
		} ;

		mkClause : Str -> VP -> Agr -> Cl = \subj,vp,agr -> {
			s = \\ten,ant,pol,order =>
				let
					verb = vp.s ! ten ! ant ! pol ! agr ;
					obj = vp.obj ! agr
				in case order of {
					_	=> subj ++ verb ++ obj
				} ;
		} ;

}
