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

		Species = Free | Suffix ;

		NPCase = NCase Case | NPPoss Number Gender Case ;
	
	--2 For $Verb$

		Mood = Indicative | Subjunctive ;

		Voice = Active | Middle ;

		PForm = PWeak Number Gender Case | PStrong Number Gender Case | PPres ;

		VForm =
			VInf
			| VPres Voice Mood Number Person
			| VPast Voice Mood Number Person
			| VImp Voice Number
			;

		VPForm = VPInf
			| VPImp
			| VPMood Tense Anteriority -- is this a describing name ?
		;

	--2 For $Adjective$

		Declension = Weak | Strong ;

		AForm = 
			  APosit Declension Number Gender Case
			| ACompar Number Gender Case
			| ASuperl Declension Number Gender Case
			--| AAdv -- lexicon difficulities. its not always clear what the adverb equivalance of the adjective looks like.
			;
		
	--2 For $Sentence$

		Order = ODir | OQuestion ;

	--2 For $Numerals$

		CardOrd = NOrd Number Gender Case | NCard Number Gender Case ; -- only "einn" ("one") inflects for gender as a cardinal

	--------------------------------------------
	--TYPE DEFINITIONS + WORST-CASE CONSTRUCTORS
	--------------------------------------------

	-- For $Lex$.
	oper
		-- Agreement of noun phrases has three parts
		Agr : PType = {g : Gender ; n : Number ; p : Person} ;

		gennumperToAgr : Gender -> Number -> Person -> Agr =
			\g,n,p	-> {g = g ; n = n ; p = p} ;

		caseList : (_,_,_,_ : Str) -> Case => Str =
			\n,a,d,g -> table {
				Nom => n ;
				Acc => a ;
				Dat => d ;
				Gen => g
			} ;

		persList : (_,_,_ : Str) -> Person => Str =
			\p1,p2,p3 -> table {
				P1 => p1 ;
				P2 => p2 ;
				P3 => p3
			} ;

		npcaseToCase : NPCase -> Case = \npc	-> case npc of {
				NCase c	=> c ;
				NPPoss _ _ c => c
		} ;

		-- For $Nouns$

		N : Type = { 
			s : Number => Species => Case => Str ;
			g : Gender
		} ;

		-- I think another field is needed, when it is just "hann", "það", or "hún"
		-- because "ég ber hann ekki" but not "ég ber ekki hann"
		-- and "ég las hana ekki but not "ég las ekki hana"
		NP : Type = {
			s : NPCase => Str ;
			a : Agr ;
			isPron : Bool
		} ;

		-- For $Adjectives$

		A : Type = {
			s : AForm => Str
		} ;

		-- For $Verb$.

		V : Type = {
			s : VForm => Str ;
			p : PForm => Str ;
			sup : Voice => Str
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

		-- is this needed for anything else but the auxiliary verbs below?
		mkVerb : (x1,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x59 : Str) -> V =
			\fljúga1,flýg,flýgur2,flýgur3,fljúgum,fljúgið,fljúga2,flaug1,flaugst,flaug2,flugum,fluguð,flugu,
			fljúgi1,fljúgir,fljúgi3,fljúgumS,fljúgiðS,fljúgi,flygi1,flygir,flygi2,flygjum,flygjuð,flygju,
			fljúgðu,fljúgið,fljúgandi,floginn,sgMascAcc,sgMascDat,sgMascGen,sgFemNom,sgFemAcc,sgFemDat,sgFemGen,
			sgNeutNom,sgNeutAcc,sgNeutDat,sgNeutGen,plMascNom,plMascAcc,plMascDat,plMascGen,
			plFemNom,plFemAcc,plFemDat,plFemGen,plNeutNom,plNeutAcc,plNeutDat,plNeutGen,
			weakSgMascNom,weakSgMascAccDatGen,weakSgFemNom,weakSgFemAccDatGen,weakSgNeut,weakPl,flogið -> {
				s = table {
					VInf				=> fljúga1 ;
					VPres v Indicative Sg P1	=> mkVoice v flýg ;
					VPres v Indicative Sg P2	=> mkVoice v flýgur2 ;
					VPres v Indicative Sg P3	=> mkVoice v flýgur3 ;
					VPres v Indicative Pl P1	=> mkVoice v fljúgum ;
					VPres v Indicative Pl P2	=> mkVoice v fljúgið ;
					VPres v Indicative Pl P3	=> mkVoice v fljúga2 ;
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
					VImp v Pl			=> mkVoice v fljúgið
				} ;
				p = table {
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
					PStrong Pl Neutr c	=> caseList plNeutNom plNeutAcc plNeutDat plNeutGen ! c ;
					PPres 			=> fljúgandi
				} ;
				sup =\\v	=> mkVoice v flogið
		} ;

		VP : Type = {
			s	: VPForm => Polarity => Agr => {
				fin	: Str ;
				inf	: Str ;
				a1	: Str * Str 	-- p1 : with inf - ég hef ekki elskað þig
							-- p2 : without inf - ég elska þig ekki
			} ;
			p	: PForm => Str ; -- past and present participles
			n1	: Agr => Str * Str; -- p1, p2 : Pron obj - hann/hún/það/ég/þú/sig - in case of ditransitive verbs we have two fields
			n2	: Agr => Str ; -- General obj - e.g. þessi maður/stóra bókin/hestur
			a2	: Str ;
			en1p1	: Bool -- indicates if the p1 field in n1 exists or is empty..
		} ;

		infVP : VP -> Agr -> Str = \vp,agr -> infVPPlus vp (VPMood Pres Simul) Pos agr ;

		infVPPlus : VP -> VPForm -> Polarity -> Agr -> Str = \vp,vpform,pol,ag -> 
			let
				s = vp.s ! vpform ! pol ! ag ;
				pron = vp.n1 ! ag
			in case vp.en1p1 of {
				False	=> s.fin ++ s.a1.p1 ++ s.inf ++ vp.n2 ! ag ++ pron.p2 ++ s.a1.p2 ++ vp.a2 ;
				True	=> s.fin ++ s.a1.p1 ++ s.inf ++ pron.p1 ++ pron.p2 ++ s.a1.p2 ++ vp.n2 ! ag ++ vp.a2
			} ;

		predV : V -> VP = \v -> {
			s = \\vpform,pol,agr => case vpform of {
				VPInf	=> vf (v.s ! VInf) [] (negation pol) False ;
				VPImp	=> vf (v.s ! VImp Active agr.n) (negation pol) [] False ;
				VPMood ten ant => vff v ten ant pol agr
			} ;
			verb = \\vform	=> v.s ! vform ;
			p = \\pform => v.p ! pform ;
			n1 = \\_ => <[],[]> ;
			n2 = \\_ => [] ;
			a2 = [] ;
			en1p1 = False
		} ;

		negation : Polarity -> Str = \pol -> case pol of {
			Pos	=> [] ;
			Neg	=> "ekki"
		} ;

		vf : Str -> Str -> Str -> Bool -> { fin,inf : Str ; a1 : Str * Str } =\fin,inf,a1,hasInf -> {
				fin = fin ;
				inf = inf ;
				a1 = case hasInf of {
					True	=> <a1,[]> ;
					False	=> <[],a1>
				} ;
		} ;

		vff : V -> Tense -> Anteriority -> Polarity -> Agr -> {fin,inf : Str ; a1 : Str * Str} 
			=\v,ten,ant,pol,agr -> case <ten,ant> of {
			-- hann sefur []/ekki - he []/doesn't sleep
			<Pres,Simul>	=> vf (v.s ! VPres Active Indicative agr.n agr.p) [] (negation pol) False;

			-- hann hefur []/ekki sofið - he has/hasn't slept
			<Pres,Anter>	=> vf (verbHave.s ! VPres Active Indicative agr.n agr.p) (v.sup ! Active) (negation pol) True ;

			-- hann svaf []/ekki - he []/didn't sleep
			<Past,Simul> 	=> vf (v.s ! VPast Active Indicative agr.n agr.p) [] (negation pol) False ;

			-- hann hafði []/ekki sofið - he had/hadn't slept
			<Past,Anter> 	=> vf (verbHave.s ! VPast Active Indicative agr.n agr.p) (v.sup ! Active) (negation pol) True ;

			-- hann mun []/ekki sofa - he will/won't sleep
			<Fut,Simul>	=> vf (verbWill.s ! VPres Active Indicative agr.n agr.p) (v.s ! VInf) (negation pol) True ;

			-- hann mun []/ekki hafa sofið - 'he will/won't have slept'	
			<Fut,Anter>	=> vf (verbWill.s ! VPres Active Indicative agr.n agr.p) (verbHave.s ! VInf ++ v.sup ! Active) (negation pol) True ;

			-- hann myndi []/ekki sofa 'he would/wouldn't sleep'
			<Cond,Simul>	=> vf (verbWill.s ! VPast Active Subjunctive agr.n agr.p) (v.s ! VInf) (negation pol) True ;

			-- hann myndi []/ekki hafa sofið 'he would/wouldn't have slept'
			<Cond,Anter>	=> vf (verbWill.s ! VPast Active Subjunctive agr.n agr.p) (verbHave.s ! VInf ++ v.sup ! Active) (negation pol) True
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

		-- Not really an axuilary verb but then again there is no exclusive club of axuilary verbs in Icelandic (or so have I been told).
		-- verbLet is nevertheless needed in Idiom. - or is it?
		verbLet : V = mkVerb "láta" "læt" "lætur" "lætur" "látum" "látið" "láta" "lét" "lést" "lét" "létum" "létuð" "létu"
					"láti" "látir" "láti" "látum" "látið" "láti" "léti" "létir" "léti" "létum" "létuð" "létu" "láttu" "látið"
					"látandi" "látinn" "látinn" "látnum" "látins" "látin" "látna" "látinni" "látinnar" "látið" "látið"
					"látnu" "látins" "látnir" "látna" "látnum" "látinna" "látnar" "látnar" "látnum" "látinnar" "látin"
					"látin" "látnum" "látinna" "látni" "látna" "látna" "látnu" "látna" "látnu" "látið" ;

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
					verb = vp.s ! VPMood ten ant ! pol ! agr ;
					pron = vp.n1 ! agr ;
					obj = vp.n2 ! agr ;
					adv = vp.a2
				in case <order,vp.en1p1> of {
					<ODir,False>		=> subj ++ verb.fin ++ verb.a1.p1 ++ verb.inf ++ obj ++ pron.p2 ++ verb.a1.p2 ++ adv ;
					<ODir,True>		=> subj ++ verb.fin ++ verb.a1.p1 ++ verb.inf ++ pron.p1 ++ pron.p2 ++ verb.a1.p2 ++ obj ++ adv ;
					<OQuestion,False>	=> verb.fin ++ subj ++ verb.a1.p1 ++ verb.inf ++ obj ++ pron.p2 ++ verb.a1.p2 ++ adv ;
					<OQuestion,True>	=> verb.fin ++ subj ++ verb.a1.p1 ++ verb.inf ++ pron.p1 ++ pron.p2 ++ verb.a1.p2 ++ obj ++ adv
				} ;
		} ;


		-- 2 Pronouns

		Pron : Type = {
			s : NPCase => Str ;
			a : Agr
		} ;

		mkPronPers : (ég,mig,mér,mín,minn1,minn2,mínum,míns,mínF,mína,minni,minnar,mitt1,mitt2,mínu,mínsN,mínir,mínaPl,mínumPl,minnaPl,mínar1,mínar2,mínPl1,mínPl2 : Str) -> Gender -> Number -> Person -> Pron =
			\ég,mig,mér,mín,minn1,minn2,mínum,míns,mínF,mína,minni,minnar,mitt1,mitt2,mínu,mínsN,mínir,mínaPl,mínumPl,minnaPl,mínar1,mínar2,mínPl1,mínPl2,g,n,p -> {
			s = table {
				NCase c			=> caseList ég mig mér mín ! c ;
				NPPoss Sg Masc c	=> caseList minn1 minn2 mínum míns ! c ;
				NPPoss Pl Masc c	=> caseList mínir mínaPl mínumPl minnaPl ! c ;
				NPPoss Sg Fem c		=> caseList mínF mína minni minnar ! c ;
				NPPoss Pl Fem c		=> caseList mínar1 mínar2 mínumPl minnaPl ! c ;
				NPPoss Sg Neutr c	=> caseList mitt1 mitt2 mínu mínsN ! c ;
				NPPoss Pl Neutr c	=> caseList mínPl1 mínPl2 mínumPl minnaPl ! c
				} ;
			a = gennumperToAgr g n p ;
		} ;

		-- There is only one relfective pronoun in icelandic "sig". It is the same in all genders and numbers and 
		-- does not technically exist in the nominative - the personal pronoun of the subject is then used (in the nominative)
		-- (usually) along with pronoun sjálfur.
		reflPron : Number -> Gender -> Case -> Str =
			\n,g,c -> case <n,g,c> of {
				<Sg,Masc,Nom>	=> "hann" ++ "sjálfur" ;
				<Pl,Masc,Nom>	=> "þeir" ++ "sjálfir" ;
				<Sg,Fem,Nom>	=> "hún" ++ "sjálf" ;
				<Pl,Fem,Nom>	=> "þær" ++ "sjálfar" ;
				<Sg,Neutr,Nom>	=> "það" ++ "sjálft" ;
				<Pl,Neutr,Nom>	=> "þau" ++ "sjálf" ;
				<_,_,Acc>	=> "sig" ;
				<_,_,Dat>	=> "sér" ;
				<_,_,Gen>	=> "sín"
			};

		-- thoughts on "or" conjugation with agreement in gender
		conjGender : Gender -> Gender -> Gender = \g,h -> case <g,h> of {
				<Masc,Masc>	=> Masc;
				<Fem,Fem>	=> Fem;
				_		=> Neutr
		};

		conjAgr : (_,_ : Agr) -> Agr = \a,b -> {
			g = conjGender a.g b.g ;
			n = conjNumber a.n b.n ;
			p = conjPerson a.p b.p
		} ;
}
