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
		Species = Def | Indef ;
	
		-- Agreement of noun phrases has three parts
		Agr = Ag Gender Number Person ;


	--2 For $Verb$

		Mood = Indicative | Subjunctive ;

		Voice = Active | Middle ;--| Passive -> just aux (að vera - to be) + past participle

		PForm = PWeak Number Gender Case | PStrong Number Gender Case ;

		VForm =
			VInf
			| VPres Voice Mood Number Person
			| VPast Voice Mood Number Person
			| VImp Voice Number
			| VPresPart -- It doesn really inflect, i.e. same form in all cases, genders and number
			| VPastPart PForm -- inflects also in weak/strong, number, genders and cases, is sometimes used as an adjective
			| VSup Voice
			;

	--2 For $Adjective$

		Declension = Weak | Strong ;

		AForm = 
			  APosit Declension Number Gender 
			| ACompar Number Gender  -- the comparative has only the weak declension
			| ASuperl Declension Number Gender
			;
		

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
			rc : Str ;
			adv : Str ;
			a : Agr
		} ;

		mkNoun : (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_ :  Str) -> Gender -> N =
			\hestur,hest,hesti,hests,
			hesturinn,hestinn,hestinum,hestsins,
			hestar,hestaAcc,hestum,hestaGen,
			hestarnir,hestana,hestunum,hestanna,g -> {
				s = table {
					Sg => table {
						Indef	=> caseList hestur hest hesti hests ;
						Def	=> caseList hesturinn hestinn hestinum hestsins 
					} ;
					Pl => table {
						Indef	=> caseList hestar hestaAcc hestum hestaGen ;
						Def	=> caseList hestarnir hestana hestunum hestanna
					}
				} ;
				g = g
		} ;

		-- For $Adjectives$

		A : Type = {
			s : AForm => Case => Str
		} ; 

		mkAdjective : (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_ : Str) -> A =
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
			supWeakSgNeut,supWeakPl -> {
				s = table {
					APosit Weak Sg Masc	=> caseList weakSgMascNom weakSgMascAccDatGen weakSgMascAccDatGen weakSgMascAccDatGen ;
					APosit Weak Sg Fem 	=> caseList weakSgFemNom weakSgFemAccDatGen weakSgFemAccDatGen weakSgFemAccDatGen ;
					APosit Weak Sg Neutr	=> caseList weakSgNeut weakSgNeut weakSgNeut weakSgNeut ;
					APosit Weak Pl _ 	=> caseList weakPl weakPl weakPl weakPl ;
					APosit Strong Sg Masc	=> caseList sgMascNom sgMascAcc sgMascDat sgMascGen ;
					APosit Strong Sg Fem	=> caseList sgFemNom sgFemAcc sgFemDat sgFemGen ;
					APosit Strong Sg Neutr	=> caseList sgNeutNom sgNeutAcc sgNeutDat sgNeutGen ;
					APosit Strong Pl Masc	=> caseList plMascNom plMascAcc plMascDat plMascGen ;
					APosit Strong Pl Fem	=> caseList plFemNom plFemAcc plFemDat plFemGen ;
					APosit Strong Pl Neutr	=> caseList plNeutNom plNeutAcc plNeutDat plNeutGen ;
					ACompar Sg Masc		=> caseList comSgMascNom comSgMascNom comSgMascNom comSgMascNom ;
					ACompar Sg Fem		=> caseList comSgMascNom comSgMascNom comSgMascNom comSgMascNom ;
					ACompar Sg Neutr	=> caseList comSgNeutrNom comSgNeutrNom comSgNeutrNom comSgNeutrNom ;
					ACompar Pl _		=> caseList comPl comPl comPl comPl ;
					ASuperl Weak Sg Masc	=> caseList supWeakSgMascNom supWeakSgMascAccDatGen supWeakSgMascAccDatGen supWeakSgMascAccDatGen ;
					ASuperl Weak Sg Fem 	=> caseList supWeakSgFemNom supWeakSgFemAccDatGen supWeakSgFemAccDatGen supWeakSgFemAccDatGen ;
					ASuperl Weak Sg Neutr	=> caseList supWeakSgNeut supWeakSgNeut supWeakSgNeut supWeakSgNeut ;
					ASuperl Weak Pl _ 	=> caseList supWeakPl supWeakPl supWeakPl supWeakPl ;
					ASuperl Strong Sg Masc	=> caseList supSgMascNom supSgMascAcc supSgMascDat supSgMascGen ;
					ASuperl Strong Sg Fem	=> caseList supSgFemNom supSgFemAcc supSgFemDat supSgFemGen ;
					ASuperl Strong Sg Neutr	=> caseList supSgNeutNom supSgNeutAcc supSgNeutDat supSgNeutGen ;
					ASuperl Strong Pl Masc	=> caseList supPlMascNom supPlMascAcc supPlMascDat supPlMascGen ;
					ASuperl Strong Pl Fem	=> caseList supPlFemNom supPlFemAcc supPlFemDat supPlFemGen ;
					ASuperl Strong Pl Neutr	=> caseList supPlNeutNom supPlNeutAcc supPlNeutDat supPlNeutGen
				}
		} ;

		-- For $Verb$.

		V : Type = {
			s : VForm => Str
		} ;

		mkVoice : Voice -> Str -> Str = \v,s ->
			case v of {
				Active 	=> s ;
				Middle	=> case s of {
					base + "ur"	=> base + "st" ;
					base + "r"	=> base + "st" ;
					base + "ð"	=> base + "st" ;
					base + "ðu"	=> base + "stu" 
				}
		} ;

		mkVerb : (x1,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x30 : Str) -> V =
			\fljúga,flýg,flýgur2,flýgur3,fljúgum,fljúgið,fljúga,flaug1,flaugst,flaug2,flugum,fluguð,flugu,
			fljúgi1,fljúgir,fljúgi3,fljúgumS,fljúgiðS,fljúgi,flygi1,flygir,flygi2,flygjum,flygjuð,flygju,
			fljúgðu,fljúgið,fljúgandi,floginn,flogið -> {
				s = table {
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
					VPresPart			=> fljúgandi ;
					VPastPart _			=> floginn ; -- atm
					VSup v				=> mkVoice v flogið
				}
		} ;

		VP : Type = {
			s 	: V;
			obj 	: Agr => Str; 
		} ;

		-- For Predication
		agrV : V -> Agr -> Tense -> Bool -> Str = \v,a,t,neg -> 
			-- should be moved/changed - legacy from the Miniature resource grammar
			-- some complex verbal constructions (taken from The Germanic Languages (Auwera & König) - Icelandic (by Höskuldur Þráinsson) p 163: 
			--  Perfect : aux (hafa) + supine
			--  Future  : aux (munu) + infinitive
			--  Passive : aux (vera) + past participle 
			let 
				hafa = mkVerb "hafa" "hef" "hefur" "hefur" "höfum" "hafið" "hafa" "hafði" "hafðir" "hafði" "höfðum" "höfðuð" "höfðu" "hafi" "hafir" "hafi" "höfum" "hafið" "hafi" "hefði" "hefðir" "hefði" "hefðum" "hefðuð" "hefðu" "hafðu" "hafið" "hafandi" "hafður" "haft"
			in case <t,a,neg> of {
				<Pres,Ag _ n p,_> => v.s ! VPres Active Indicative n p ;
				<Perf,Ag _ n p,True> => hafa.s ! VPres Active Indicative n p ++ v.s ! VSup Active;
				<Perf,Ag _ n p,False> => hafa.s ! VPres Active Indicative n p ++ "ekki" ++  v.s ! VSup Active
		};



}
