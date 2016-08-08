--# -path=.:../abstract:../../prelude:../common

--1 Icelandic Lexical Paradigms
--
-- This is an API for the user of the resource grammar 
-- for adding lexical items. It gives functions for forming
-- expressions of open categories: nouns, adjectives, verbs.
-- 
-- Closed categories (determiners, pronouns, conjunctions) are
-- accessed through the resource syntax API, $Structural.gf$. 
--
-- The main difference with $MorphoIce.gf$ is that the types
-- referred to are compiled resource grammar types. We have moreover
-- had the design principle of always having existing forms, rather
-- than stems, as string arguments of the paradigms.
--
-- The structure of functions for each word class $C$ is the following:
-- first we give a handful of patterns that aim to cover all
-- regular cases. Then we give a worst-case function $mkC$, which serves as an
-- escape to construct the most irregular words of type $C$.
-- However, this function should only seldom be needed: we have a
-- separate module [``IrregIce`` ../../icelandic/IrregIce.gf], 
-- which covers irregular verbss.

resource ParadigmsIce = open 
  (Predef=Predef), 
  Prelude, 
  MorphoIce,
  ResIce,
  CatIce
  in {
	--2 Parameters 

	oper
		N : Type ;
		A : Type ;
		V : Type ;

		-- To abstract over gender names, we define the following identifiers.	

		Gender : Type ; 

		masculine : Gender ;
		feminine  : Gender ;
		neuter    : Gender ;

		--- To abstract over number names, we define the following.

		Number : Type ; 

		singular : Number ; 
		plural   : Number ;

		-- To abstract over case names, we define the following.

		Case : Type ;

		nominative : Case ;
		accusative : Case ;
		dative     : Case ;
		genitive   : Case ;

		--2 Nouns

		-- Nouns are constructed by the function $mkN$, which takes a varying
		-- number of arguments.

		-- new mk operation in progress ;)
		mkN = overload {

			-- Given Sg.Nom. 
			mkN : Str -> Gender -> N = mk1N ;

			-- Given Sg.Nom and Pl.Nom - different Pl.Nom part
			mkN : (_,_ : Str) -> Gender -> N = mk2N ;

			-- Given Sg.Nom, Sg.Gen, and Pl.Nom - also different Sg.Gen part
			--mkN : (_,_,_ : Str) -> Gender -> N = mk3N ;

			-- Given Sg.Nom, Sg.Gen, Pl.Nom and Pl.Gen - also different Pl.Gen part
			--mkN : (_,_,_,_ : Str) -> Gender -> N = mk4N ;

			-- Worst case, all eight forms.
			mkN : (x1,_,_,_,_,_,_,x8 : Str) -> Gender -> N = mk8N ;
		} ;

		mk1N : Str -> Gender -> N = \s,g -> case g of {
			Neutr		=> lin N (nForms2NeutrNoun (neutrNForms1 s)) ;
			Masc		=> lin N (nForms2MascNoun (mascNForms1 s)) ;
			Fem		=> lin N (nForms2FemNoun (femNForms1 s))
		} ;

		mk2N : (_,_ : Str) -> Gender -> N = \x,y,g -> case g of {
			Neutr		=> lin N (nForms2NeutrNoun (neutrNForms2 x y)) ;
			Masc		=> lin N (nForms2MascNoun (mascNForms2 x y)) ;
			Fem		=> lin N (nForms2FemNoun (femNForms2 x y))
		} ;

		mk8N : (x1,_,_,_,_,_,_,x8 : Str) -> Gender -> N = \a,b,c,d,e,f,g,h,gend ->
			let nfs = nForms8 a b c d e f g h
			in lin N (nForms2Noun nfs nfs gend) ;

		neutrNForms1 : Str -> NForms = \s -> case s of {
			front + middle@("g" | "k") + "j" + "a"	=> dAuga s (front + middle + "na") ;
			_ + ("r" | "s" | "n" | "j") + "a"	=> dAuga s s ;
			stem + "a"				=> dAuga s (stem + "na") ;
			--  stem + "a" - I Don't think this is the general case, a counter example 
			--  would be "þema" - "þema". Contacted a linguist about this and am waiting 
			--  for an answer.
			front + ("ki" | "gi")			=> dKvæði s ((a2ö front) + "jum") ;
			front + "i"				=> dKvæði s ((a2ö front) + "um") ;
			front + "ur"				=> dSumar s s ;
			front + "ar"				=> dSumar s (front + "ur") ;
			--front + end@("að" | "al" | "ald" | "an" | "ang") =>
			_					=> dBarn s (a2ö s)
		} ;

		neutrNForms2 : (_,_ : Str) -> NForms = \sg,pl -> case <sg,pl> of {
			_					=> dBarn sg pl
		} ;

		mascNForms1 : Str -> NForms = \s -> case s of {
			front + "andi"			=> dNemandi s (front + "endur") ;
			front + "óndi"			=> dNemandi s (front + "ændur") ;
			front + "ndi"			=> dNemandi s s ;
			_ + "i" 			=> dSími s
		} ;

		mascNForms2 : (_,_ : Str) -> NForms = \sg,pl -> case <sg,pl> of {
			_					=> dNemandi sg pl
		} ;

		femNForms1 : Str -> NForms = \s -> case s of {
			_ + "ing"				=> dFylking s (s + "ar") ;
			front + middle@("g" | "k") + "j" + "a"	=> dSaga s (front + middle + "na") ;
			_ + ("r" | "s" | "n" | "j") + "a"	=> dSaga s s ; --  I Don't think this is the general case
			stem + "a"				=> dSaga s s --  I Don't think this is the general case
		} ;

		femNForms2 : (_,_ : Str) -> NForms = \sg,pl -> case <sg,pl> of {
			<_ + "un",_ + "ir">				=> dVerslun sg pl ;
			<_ + "i",_ + "ir">				=> dKeppni sg pl ;
			<_ + "ur",_ + "ir">				=> dBrúður sg pl ;
			<_,_ + "ir">					=> dÞökk sg pl ;
			<_,_ + ("rar" | "var" | "jar")>			=> dLifur sg pl ;
			<_ + "ur", _ + "ar">				=> dÆður sg pl ;
			<_,_ + "ar">					=> dNál sg pl ;
			<_ + ("í" | "ú" | "ei" | "æ" | "á" | "ó" | "au") + ("t"* | "k"*),_>	=> dBók sg pl ;
			<"móðir" | "dóttir" | "systir",_>		=> dMóðir sg pl ;-- not sure if this is good practice, but there are only these three words that go like this
			<_ + "á", _ + "ær">				=> dTá sg pl ;
			<_ + "ó", _ + "ær">				=> dTá sg pl ;
			<_ + "ú", _ + "ýr">				=> dTá sg pl ;
			<_ + "á", _ + "á" + _>				=> dÁ sg pl ;
			<_ + "ó", _ + "ó" + _>				=> dÁ sg pl ;
			<_ + "ú", _ + "ú" + _>				=> dÁ sg pl ;-- in some cases the Sg.Gen becomes ú-ar instead of ú-r, I do not know atm why.
			<_ + "ús",_>					=> dMús sg pl
		} ;

		mkPN = overload {

			mkPN : Str -> Gender -> PN = 
				\name,g	-> regPN name g ;	

		} ;

		--2 Adjectives

		mkA = overload {

			-- The theoretical worst case
			mkA : (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_ : Str) -> A =
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
				supWeakSgNeut,supWeakPl,adv -> 
				lin A (mkAdjective
				sgMascNom sgMascAcc sgMascDat sgMascGen 
				sgFemNom sgFemAcc sgFemDat sgFemGen 
				sgNeutNom sgNeutAcc sgNeutDat sgNeutGen 
				plMascNom plMascAcc plMascDat plMascGen 
				plFemNom plFemAcc plFemDat plFemGen 
				plNeutNom plNeutAcc plNeutDat plNeutGen 
				weakSgMascNom weakSgMascAccDatGen 
				weakSgFemNom weakSgFemAccDatGen 
				weakSgNeut weakPl 
				comSgMascNom comSgNeutrNom comPl 
				supSgMascNom supSgMascAcc supSgMascDat supSgMascGen 
				supSgFemNom supSgFemAcc supSgFemDat supSgFemGen 
				supSgNeutNom supSgNeutAcc supSgNeutDat supSgNeutGen 
				supPlMascNom supPlMascAcc supPlMascDat supPlMascGen 
				supPlFemNom supPlFemAcc supPlFemDat supPlFemGen 
				supPlNeutNom supPlNeutAcc supPlNeutDat supPlNeutGen 
				supWeakSgMascNom supWeakSgMascAccDatGen 
				supWeakSgFemNom supWeakSgFemAccDatGen 
				supWeakSgNeut supWeakPl adv) ;


			-- Given the positive strong Sg.Masc.Nom. and Sg.Fem.Nom.
			mkA : (_,_ : Str) -> A = \sgMascNom,sgFemNom ->
				regA sgMascNom sgFemNom ;


			-- Irregular comparison and i-umlaut patterns
			-- Given the positive strong Sg.Masc.Nom. and Sg.Fem.Nom. and
			-- the comparitive Sg.Masc
			-- mkA : (_,_,_ : Str) -> A = \sgMascNom,sgFemNom,comSgMascNom ->
			--	irreg sgMascNom sgFemNom comSgMascNom ;
		} ;

		--2 Verbs

		-- Verbs are constructed by the function $mkV$, which takes a varying
		-- number of arguments.

		mkV = overload {

			-- The theoretical worst case
			mkV : (x1,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x59 : Str) -> V =
				\fljúga,flýg,flýgur2,flýgur3,fljúgum,fljúgið,fljúga,flaug1,flaugst,flaug2,flugum,fluguð,flugu,
				fljúgi1,fljúgir,fljúgi3,fljúgumS,fljúgiðS,fljúgi,flygi1,flygir,flygi2,flygjum,flygjuð,flygju,
				fljúgðu,fljúgið,fljúgandi,floginn,sgMascAcc,sgMascDat,sgMascGen,sgFemNom,sgFemAcc,sgFemDat,sgFemGen,
				sgNeutNom,sgNeutAcc,sgNeutDat,sgNeutGen,plMascNom,plMascAcc,plMascDat,plMascGen,
				plFemNom,plFemAcc,plFemDat,plFemGen,plNeutNom,plNeutAcc,plNeutDat,plNeutGen,
				weakSgMascNom,weakSgMascAccDatGen,weakSgFemNom,weakSgFemAccDatGen,weakSgNeut,weakPl,flogið -> 
				lin V (mkVerb fljúga flýg flýgur2 flýgur3 fljúgum fljúgið fljúga flaug1 flaugst flaug2 flugum fluguð flugu 
				fljúgi1 fljúgir fljúgi3 fljúgumS fljúgiðS fljúgi flygi1 flygir flygi2 flygjum flygjuð flygju 
				fljúgðu fljúgið fljúgandi floginn sgMascAcc sgMascDat sgMascGen sgFemNom sgFemAcc sgFemDat sgFemGen 
				sgNeutNom sgNeutAcc sgNeutDat sgNeutGen plMascNom plMascAcc plMascDat plMascGen 
				plFemNom plFemAcc plFemDat plFemGen plNeutNom plNeutAcc plNeutDat plNeutGen 
				weakSgMascNom weakSgMascAccDatGen weakSgFemNom weakSgFemAccDatGen weakSgNeut weakPl flogið) ;
		};

		--3 Two-place verbs

		-- Two-place verbs need a preposition, except the special case with direct object.
		-- (transitive verbs). Notice that a particle comes from the $V$.

		mkV2 = overload {

			-- Theoretical worst case.
			mkV2 : (x1,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x59 : Str) -> Case -> V =
				\fljúga,flýg,flýgur2,flýgur3,fljúgum,fljúgið,fljúga,flaug1,flaugst,flaug2,flugum,fluguð,flugu,
				fljúgi1,fljúgir,fljúgi3,fljúgumS,fljúgiðS,fljúgi,flygi1,flygir,flygi2,flygjum,flygjuð,flygju,
				fljúgðu,fljúgið,fljúgandi,floginn,sgMascAcc,sgMascDat,sgMascGen,sgFemNom,sgFemAcc,sgFemDat,sgFemGen,
				sgNeutNom,sgNeutAcc,sgNeutDat,sgNeutGen,plMascNom,plMascAcc,plMascDat,plMascGen,
				plFemNom,plFemAcc,plFemDat,plFemGen,plNeutNom,plNeutAcc,plNeutDat,plNeutGen,
				weakSgMascNom,weakSgMascAccDatGen,weakSgFemNom,weakSgFemAccDatGen,weakSgNeut,weakPl,flogið,c -> 
				lin V (mkVerb fljúga flýg flýgur2 flýgur3 fljúgum fljúgið fljúga flaug1 flaugst flaug2 flugum fluguð flugu 
				fljúgi1 fljúgir fljúgi3 fljúgumS fljúgiðS fljúgi flygi1 flygir flygi2 flygjum flygjuð flygju 
				fljúgðu fljúgið fljúgandi floginn sgMascAcc sgMascDat sgMascGen sgFemNom sgFemAcc sgFemDat sgFemGen 
				sgNeutNom sgNeutAcc sgNeutDat sgNeutGen plMascNom plMascAcc plMascDat plMascGen 
				plFemNom plFemAcc plFemDat plFemGen plNeutNom plNeutAcc plNeutDat plNeutGen 
				weakSgMascNom weakSgMascAccDatGen weakSgFemNom weakSgFemAccDatGen weakSgNeut weakPl flogið) ** {c = c} ;
		};

		--2 Definitions of paradigms

		-- The definitions should not bother the user of the API. So they are
		-- hidden from the document.

		Gender = ResIce.Gender ; 
		Number = ResIce.Number ;
		Case   = ResIce.Case ;
		masculine = Masc ;
		feminine  = Fem ;
		neuter    = Neutr ;
		singular = Sg ;
		plural = Pl ;
		nominative = Nom ;
		accusative = Acc ;
		dative = Dat ;
		genitive = Gen ;

		N = ResIce.N ;
		A = ResIce.A ;
		V = ResIce.V ;

		vowel : pattern Str = #("a" | "á" | "e" | "é" | "i" | "í" | "o" | "ó" | "u" | "ú" | "y" | "ý" | "æ" | "ö") ;

		consonant : pattern Str = #("b" | "d" | "ð" | "f" | "g" | "h" | "j" | "k" | "l" | "m" | "n" | "p" | "r" | "s" | "t" | "v" | "x" | "þ") ;

		-- For pattern matching nouns to suffix the definate article.
		-- The suffix , "-inn","-in","-ið", loses the "-i-" when the noun ends with
		-- "-a", "-i", "-u", and most cases of "-é".
		noIVowel : pattern Str = #("a" | "i" | "u" | "é") ;

		regPN : Str -> Gender -> PN = \name,g -> case <name,g> of {
				<base + "i",Masc>	=> lin PN {s = caseList name (base + "a") (base + "a") (base + "a") ; g = Masc} ;
				<base + "a",Masc>	=> lin PN {s = caseList name (base + "u") (base + "u") (base + "u") ; g = Masc} ;
				<base + "ur",Masc>	=> lin PN {s = caseList name base (base + "i") (base + "s") ; g = Masc} ;
				<base + "l",Masc>	=> lin PN {s = caseList name name name (name + "s") ; g = Masc} ;
				<base + "s",Masc>	=> lin PN {s = caseList name name (name + "i") (name + "ar") ; g = Masc} ;
				<base + #consonant,Masc>	=> lin PN {s = caseList name name (name + "i") (name + "s") ; g = Masc}
		} ;

		-- Most adjectives can be predicted from the Sg.Fem.Nom in the positive. Here we denote
		-- the positive Sg.Fem.Nom. with fem and the positve Sg.Masc.Nom. with mas.
		regA : (_,_ : Str) -> A = \mas,fem -> case <mas,fem> of {

		-- note on u-shift :
		-- Adjectives with a in the root syllable u-shifts it ö. All other root vowels
		-- are unchanged throughout the inflexion (in the positve). 
		-- a in suffixes is similarly u-shifted to u.
		-- i-shift is note taken into account here.
		
		-- 2 Adjectives having -(u)r (expanded -r) 
		<baseA + "ur", baseÖ + "ur">		=> lin A ( mkAdjective mas (baseA + "ran") (baseÖ + "rum") (mas + "s") -- strong Sg.Masc.
							fem (baseA + "ra") (mas + "ri") (mas + "rar") -- strong Sg.Fem.
							(mas + "t") (mas + "t") (baseÖ + "ru") (mas + "s") -- strong Sg.Netur.
							(baseA + "rir") (baseA + "ra") (baseÖ + "rum") (mas + "ra") -- strong Pl.Masc.
							(baseA + "rar") (baseA + "rar") (baseÖ + "rum") (mas + "ra") -- strong Pl.Fem.
							fem fem (baseÖ + "rum") (mas + "ra") -- strong Pl.Neutr.
							(baseA + "ri") (baseA + "ra") -- weak Sg.Masc.
							(baseA + "ra") (baseÖ + "ru") -- weak Sg.Fem.
							(baseA + "ra") -- weak Sg.Neutr.
							(baseÖ + "ru") -- weak Pl.
							(baseA + "rari") (baseA + "rara") (baseA + "rari") -- comparative
							(baseA + "rastur") (baseA + "rastan") (baseÖ + "rustum") (baseA + "rasts") -- superlative strong Sg.Masc.
							(baseÖ + "rust") (baseA + "rasta") (baseA + "rastri") (baseA + "rastrar") -- superlative strong Sg.Fem.
							(baseA + "rast") (baseA + "rast") (baseÖ + "rustu") (baseA + "rasts") -- superlative strong Sg.Neutr.
							(baseA + "rastir") (baseA + "rasta") (baseÖ + "rustum") (baseA + "rasta") -- superlative strong Pl.Masc.
							(baseA + "rar") (baseA + "rar") (baseÖ + "rustum") (baseA + "rasta") -- superlative strong Pl.Fem.
							(baseÖ + "rust") (baseÖ + "rust") (baseÖ + "rustum") (baseA + "rasta") -- superlative strong Pl.Neutr.
							(baseA + "rasti") (baseA + "rasta") -- superlative weak Sg.Masc.
							(baseA + "rasta") (baseÖ + "rustu") -- superlative weak Sg.Fem.
							(baseA + "rasta") -- superlative weak Sg.Neutr.
							(baseÖ + "rustu") -- superlative weak Pl.
							(baseÖ + "lega")) ; 
		-- 1 Adjectives ending in -ur in the Sg.Masc.Nom.
		<baseA + "ur", baseÖ> 		=> lin A (mkAdjective
							(baseA + "ur") (baseA + "an") (baseÖ + "um") (baseA + "s") -- strong Sg.Masc.
							baseÖ (baseA + "a") (baseA + "ri") (baseA + "rar") -- strong Sg.Fem.
							(baseA + "t") (baseA + "t") (baseÖ + "u") (baseA + "s") -- strong Sg.Netur.
							(baseA + "ir") (baseA + "a") (baseÖ + "um") (baseA + "ra") -- strong Pl.Masc.
							(baseA + "ar") (baseA + "ar") (baseÖ + "um") (baseA + "ra") -- strong Pl.Fem.
							baseÖ baseÖ (baseÖ + "um") (baseA + "ra") -- strong Pl.Neutr.
							(baseA + "i") (baseA + "a") -- weak Sg.Masc.
							(baseA + "a") (baseÖ + "u") -- weak Sg.Fem.
							(baseA + "a") -- weak Sg.Neutr.
							(baseA + "u") -- weak Pl.
							(baseA + "ari") (baseA + "ara") (baseA + "ari") -- comparative
							(baseA + "astur") (baseA + "astan") (baseÖ + "ustum") (baseA + "asts") -- superlative strong Sg.Masc.
							(baseÖ + "ust") (baseA + "asta") (baseA + "astri") (baseA + "astrar") -- superlative strong Sg.Fem.
							(baseA + "ast") (baseA + "ast") (baseÖ + "ustu") (baseA + "asts") -- superlative strong Sg.Neutr.
							(baseA + "astir") (baseA + "asta") (baseÖ + "ustum") (baseA + "astra") -- superlative strong Pl.Masc.
							(baseA + "ar") (baseA + "ar") (baseÖ + "ustum") (baseA + "astra") -- superlative strong Pl.Fem.
							(baseÖ + "ust") (baseÖ + "ust") (baseÖ + "ustum") (baseA + "astra") -- superlative strong Pl.Neutr.
							(baseA + "asti") (baseA + "asta") -- superlative weak Sg.Masc.
							(baseA + "asta") (baseÖ + "ustu") -- superlative weak Sg.Fem.
							(baseA + "asta") -- superlative weak Sg.Neutr.
							(baseÖ + "ustu") -- superlative weak Pl.
							(baseÖ + "lega") ) ;
		
		-- 5 Adjectives having stems that end in -r, -s, (consonant +) -n 
		<baseA + "r", baseÖ + "r">		=> sameStem mas fem ;
		<baseA + "s", baseÖ + "s">		=> sameStem mas fem ;
		-- <baseA + #consonant + "n", baseÖ + #consonant + "n">		=> sameStem mas fem ;

		-- 4  Adjectivs having stem vowels -ý or -æ
		<base + "r", _ + ("ý" | "æ")>	=> lin A (mkAdjective
							(base + "r") (base + "jan") (base + "jum") (base + "s") -- strong Sg.Masc.
							base (base + "a") (base + "rri") (base + "rrar") -- strong Sg.Fem.
							(base + "tt") (base + "t") (base + "ju") (base + "s") -- strong Sg.Netur.
							(base + "ir") (base + "ja") (base + "jum") (base + "rra") -- strong Pl.Masc.
							(base + "jar") (base + "jar") (base + "jum") (base + "rra") -- strong Pl.Fem.
							base base (base + "jum") (base + "rra") -- strong Pl.Neutr.
							(base + "i") (base + "ja") -- weak Sg.Masc.
							(base + "ja") (base + "ju") -- weak Sg.Fem.
							(base + "ja") -- weak Sg.Neutr.
							(base + "ju") -- weak Pl.
							(base + "rri") (base + "rra") (base + "rri") -- comparative
							(base + "jastur") (base + "jastan") (base + "justum") (base + "jasts") -- superlative strong Sg.Masc.
							(base + "just") (base + "jasta") (base + "jastri") (base + "jastrar") -- superlative strong Sg.Fem.
							(base + "jast") (base + "jast") (base + "justu") (base + "jasts") -- superlative strong Sg.Neutr.
							(base + "jastir") (base + "jasta") (base + "justum") (base + "jastra") -- superlative strong Pl.Masc.
							(base + "jar") (base + "jar") (base + "justum") (base + "jastra") -- superlative strong Pl.Fem.
							(base + "just") (base + "just") (base + "justum") (base + "jastra") -- superlative strong Pl.Neutr.
							(base + "jasti") (base + "jasta") -- superlative weak Sg.Masc.
							(base + "jasta") (base + "justu") -- superlative weak Sg.Fem.
							(base + "jasta") -- superlative weak Sg.Neutr.
							(base + "justu") -- superlative weak Pl.
							(base + "lega") ) ;

		-- 3 Adjectives having stem vowels -á, -ó, -ú 
		<base + "r", _ + ("á" | "ó" | "ú")>	=> lin A (mkAdjective
							(base + "r") (base + "an") (base + "um") (base + "s") -- strong Sg.Masc.
							base (base + "a") (base + "rri") (base + "rrar") -- strong Sg.Fem.
							(base + "tt") (base + "tt") (base + "u") (base + "s") -- strong Sg.Netur.
							(base + "ir") (base + "a") (base + "um") (base + "rra") -- strong Pl.Masc.
							(base + "ar") (base + "ar") (base + "um") (base + "rra") -- strong Pl.Fem.
							base base (base + "um") (base + "rra") -- strong Pl.Neutr.
							(base + "i") (base + "a") -- weak Sg.Masc.
							(base + "a") (base + "u") -- weak Sg.Fem.
							(base + "a") -- weak Sg.Neutr.
							(base + "u") -- weak Pl.
							(base + "rri") (base + "rra") (base + "rri") -- comparative
							(base + "astur") (base + "astan") (base + "ustum") (base + "asts") -- superlative strong Sg.Masc.
							(base + "ust") (base + "asta") (base + "astri") (base + "astrar") -- superlative strong Sg.Fem.
							(base + "ast") (base + "ast") (base + "ustu") (base + "asts") -- superlative strong Sg.Neutr.
							(base + "astir") (base + "asta") (base + "ustum") (base + "astra") -- superlative strong Pl.Masc.
							(base + "ar") (base + "ar") (base + "ustum") (base + "astra") -- superlative strong Pl.Fem.
							(base + "ust") (base + "ust") (base + "ustum") (base + "astra") -- superlative strong Pl.Neutr.
							(base + "asti") (base + "asta") -- superlative weak Sg.Masc.
							(base + "asta") (base + "ustu") -- superlative weak Sg.Fem.
							(base + "asta") -- superlative weak Sg.Neutr.
							(base + "ustu") -- superlative weak Pl.
							(base + "lega") ) ;

		<_ + "inn" , base + "in">		=> lin A (mkAdjective
							(base + "inn") (base + "inn") (base + "num") (base + "ins") -- strong Sg.Masc.
							(base + "in")  (base + "na") (base + "inni") (base + "innar") -- strong Sg.Fem.
							(base + "ið") (base + "ið") (base + "nu") (base + "ins") -- strong Sg.Netur.
							(base + "nir") (base + "na") (base + "num") (base + "inna") -- strong Pl.Masc.
							(base + "nar") (base + "nar") (base + "num") (base + "inna") -- strong Pl.Fem.
							(base + "in") (base + "in") (base + "num") (base + "inna") -- strong Pl.Neutr.
							(base + "ni") (base + "na") -- weak Sg.Masc.
							(base + "na") (base + "nu") -- weak Sg.Fem.
							(base + "na") -- weak Sg.Neutr.
							(base + "nu") -- weak Pl.
							(base + "nari") (base + "nara") (base + "nari") -- comparative
							(base + "nastur") (base + "nastan") (base + "nustum") (base + "nasts") -- superlative strong Sg.Masc.
							(base + "nust") (base + "nasta") (base + "nastri") (base + "nastrar") -- superlative strong Sg.Fem.
							(base + "nast") (base + "nast") (base + "nustu") (base + "nasts") -- superlative strong Sg.Neutr.
							(base + "nastir") (base + "nasta") (base + "nustum") (base + "nastra") -- superlative strong Pl.Masc.
							(base + "nar") (base + "nar") (base + "nustum") (base + "nastra") -- superlative strong Pl.Fem.
							(base + "nust") (base + "nust") (base + "nustum") (base + "nastra") -- superlative strong Pl.Neutr.
							(base + "nasti") (base + "nasta") -- superlative weak Sg.Masc.
							(base + "nasta") (base + "nustu") -- superlative weak Sg.Fem.
							(base + "nasta") -- superlative weak Sg.Neutr.
							(base + "nustu") -- superlative weak Pl.
							(base + "lega") )
		} ;

		sameStem : (_,_ : Str) -> A = \baseA,baseÖ-> lin A (mkAdjective 
			baseA (baseA + "an") (baseÖ + "um") (baseA + "s") -- strong Sg.Masc.
			baseÖ (baseA + "a") (baseA + "ri") (baseA + "rar") -- strong Sg.Fem.
			(baseA + "t") (baseA + "t") (baseÖ + "u") (baseA + "s") -- strong Sg.Netur.
			(baseA + "ir") (baseA + "a") (baseÖ + "um") (baseA + "ra") -- strong Pl.Masc.
			(baseA + "ar") (baseA + "ar") (baseÖ + "um") (baseA + "ra") -- strong Pl.Fem.
			baseÖ baseÖ (baseÖ + "um") (baseA + "ra") -- strong Pl.Neutr.
			(baseA + "i") (baseA + "a") -- weak Sg.Masc.
			(baseA + "a") (baseÖ + "u") -- weak Sg.Fem.
			(baseA + "a") -- weak Sg.Neutr.
			(baseÖ + "u") -- weak Pl.
			(baseA + "ari") (baseA + "ara") (baseA + "ari") -- comparative
			(baseA + "astur") (baseA + "astan") (baseÖ + "ustum") (baseA + "asts") -- superlative strong Sg.Masc.
			(baseÖ + "ust") (baseA + "asta") (baseA + "astri") (baseA + "astrar") -- superlative strong Sg.Fem.
			(baseA + "ast") (baseA + "ast") (baseÖ + "ustu") (baseA + "asts") -- superlative strong Sg.Neutr.
			(baseA + "astir") (baseA + "asta") (baseÖ + "ustum") (baseA + "asta") -- superlative strong Pl.Masc.
			(baseA + "ar") (baseA + "ar") (baseÖ + "ustum") (baseA + "asta") -- superlative strong Pl.Fem.
			(baseÖ + "ust") (baseÖ + "ust") (baseÖ + "ustum") (baseA + "asta") -- superlative strong Pl.Neutr.
			(baseA + "asti") (baseA + "asta") -- superlative weak Sg.Masc.
			(baseA + "asta") (baseÖ + "ustu") -- superlative weak Sg.Fem.
			(baseA + "asta") -- superlative weak Sg.Neutr.
			(baseÖ + "ustu") -- superlative weak Pl.
			(baseÖ + "lega") ) ;

		-- A helper function that can take in and list out all cases in the positive degree would be nice.

		-- Irregular comparison and i-umlaut patterns
		-- Here (as for regA) mas and fem stand for the positive Sg.Masc.Nom. and Sg.Fem.Nom.,
		-- but com for the comparative Sg.Masc.Nom.
		-- irregA : (_,_,_ : Str) -> \mas,fem,com -> case <mas,fem> of {
		--}


		-- 3 Determiners and quantifiers

		mkOrd : Str -> Ord = \sjötti -> 
			let
				sjött = init sjötti
			in lin Ord { s = table {
				Sg	=> table {
						Masc	=> caseList sjötti (sjött + "a") (sjött + "a") (sjött + "a") ;
						Fem	=> caseList (sjött + "a") (sjött + "u") (sjött + "u") (sjött + "u") ;
						Neutr	=> caseList (sjött + "a") (sjött + "a") (sjött + "a") (sjött + "a") 
				} ;
				Pl	=> \\_,_	=> sjött + "u"
			} 
		} ;


		-- 3 Adverbs
		mkAdv : Str -> Adv = \x -> lin Adv (ss x) ;

		mkAdA : Str -> AdA = \x -> lin AdA (ss x) ;
		
		mkAdN : Str -> AdN = \x -> lin AdN (ss x) ;

		-- 3 Prepositions

		mkPrep : Str -> Case -> Prep = \s,c -> lin Prep {s = s ; c = c } ;

		-- 2 Conjunctions

		mkConj = overload {
			mkConj : Str -> Conj = \y -> mk2Conj [] y plural ;
			mkConj : Str -> Number -> Conj =\y,n -> mk2Conj [] y n ;
			mkConj : Str -> Str -> Conj =\x,y -> mk2Conj x y plural ;
			mkConj : Str -> Str -> Number -> Conj =\x,y,n -> mk2Conj x y n ;
		} ;

		mk2Conj : Str -> Str -> Number -> Conj = \x,y,n ->
			lin Conj (sd2 x y ** {n = n}) ;
} ;
