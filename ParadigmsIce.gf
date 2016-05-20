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

		mkN = overload {

			---- The theoretical worst case, give all eight forms (four in 
			---- Sg and four in Pl) and all eight forms with the suffixed
			---- definite article.
			mkN : (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_ :  Str) -> Gender -> N =
				\hestur,hest,hesti,hests,
				hesturinn,hestinn,hestinum,hestsins,
				hestar,hestaAcc,hestum,hestaGen,
				hestarnir,hestana,hestunum,hestanna,g -> 
				lin N (mkNoun 
				hestur hest hesti hests 
				hesturinn hestinn hestinum hestsins
				hestar hestaAcc hestum hestaGen 
				hestarnir hestana hestunum hestanna g) ;

			-- Given Nom.Sg., Nom.Pl, and Gender.
			mkN : (_,_ : Str) -> Gender -> N =
				\bóndi,bændur,g -> regN bóndi bændur g ;


			-- Given Nom.Sg, Gen.Sg, Nom.Pl, and Gender.
			mkN : (_,_,_ : Str) -> Gender -> N =
				\steik,steikar,steikur,g -> reg3N steik steikar steikur g ;

		} ;

		--2 Adjectives

		mkA = overload {

			-- The theoretical worst case
			mkA : (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_ : Str) -> Bool -> A =
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
				supWeakSgNeut,supWeakPl,b -> 
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
				supWeakSgNeut supWeakPl b) ;


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
			mkVerb : (_,_,_,_,_,_,_,_,_,_,_,_,_,_ : Str) -> V
				= \vera,er1,ert,er3,erum,eruð,eru,var1,varst,var3,vorum,voruð,voru,verið ->
				lin V (mkVerb vera er1 ert er3 erum eruð eru var1 varst var3 vorum voruð voru verið) ; 
		};

		--3 Two-place verbs

		-- Two-place verbs need a preposition, except the special case with direct object.
		-- (transitive verbs). Notice that a particle comes from the $V$.

		mkV2 = overload {

			-- Theoretical worst case.
			mkV2 : (_,_,_,_,_,_,_,_,_,_,_,_,_,_ : Str) -> Case -> V2
				= \vera,er1,ert,er3,erum,eruð,eru,var1,varst,var3,vorum,voruð,voru,verið,c -> 
					lin V2 (mkVerb vera er1 ert er3 erum eruð eru var1 varst var3 vorum voruð voru verið ** {c = c});
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

		-- For regN and reg3N, it might be "better" to change all occurances of base to stem. idk
		
		regN : (_,_ : Str) -> Gender -> N = \nomSg,nomPl,Gender -> case <nomSg,nomPl,Gender> of {
			-- Weak Nouns --

			-- aug-a , aug-u
			<baseSg + "a",basePl + "u",Neutr>	=> lin N (mkNoun nomSg nomSg nomSg nomSg 
								(nomSg + "ð") (nomSg + "ð") (nomSg + "nu") (nomSg + "ns") 
								nomPl nomPl (nomPl + "m") (basePl + "na")
								(nomPl + "n") (nomPl + "n") (nomPl + "num") (basePl +"nanna") Neutr ) ;
			-- tím-i , tím-ar
			<baseSg + "i",basePl + "ar",Masc>	=> lin N (mkNoun nomSg (baseSg + "a") (baseSg + "a") (baseSg + "a")
								(nomSg + "nn") (baseSg + "ann") (baseSg + "anum") (baseSg + "ans")
								nomPl (basePl + "a") (basePl + "um") (basePl + "a") -- some nouns have the suffix -n-a
								(nomPl + "nir") (basePl + "ana") (basePl + "unum") (basePl + "anna") Masc ) ;
			-- bónd-i , bænd-ur
			<baseSg + "i", basePl + "ur",Masc>	=> lin N (mkNoun nomSg (baseSg + "a") (baseSg + "a") (baseSg + "a")
								(nomSg + "nn") (baseSg + "ann") (baseSg + "anum") (baseSg + "ans")
								nomPl nomPl (basePl + "um") (basePl + "a")
								(basePl + "urnir") (basePl + "urnir") (basePl + "unum") (basePl + "anna") Masc ) ;
			-- lilj-a , lilj-ur
			<baseSg + "a", basePl + "ur",Fem>	=> lin N (mkNoun nomSg (baseSg + "u") (baseSg + "u") (baseSg + "u")
								(nomPl + "n") (baseSg + "una") (baseSg + "unni") (baseSg + "unnar")
								nomPl nomPl (basePl + "um") (basePl + "a")
								(basePl + "rinar") (basePl + "rinar") (basePl + "unum") (basePl + "anna") Fem ) ;
			-- æf-i , æf-ir
			<baseSg + "i", basePl + "ir",Fem> 	=> lin N (mkNoun nomSg nomSg nomSg nomSg
								(nomSg + "n") (nomSg + "na") (nomSg + "nni") (nomSg + "nnar")
								nomPl nomPl (basePl + "um") (basePl + "a")
								(nomPl + "nar") (nomPl + "nar") (basePl + "unum") (basePl + "anna") Fem ) ;
			-- lyg-i , lyg-ar
			<baseSg + "i", basePl + "ar",Fem>	=> lin N (mkNoun nomSg nomSg nomSg nomSg
								(nomSg + "n") (nomSg + "na") (nomSg + "nni") (nomSg + "nnar")
								nomPl nomPl (basePl + "um") (basePl + "a")
								(nomPl + "nar") (nomPl + "nar") (basePl + "unum") (basePl + "anna") Fem ) ;
			-- Strong Nouns --

			-- kvæð-i- , kvæð-i-
			<baseSg + "i", basePl + "i",Neutr>	=> lin N (mkNoun nomSg nomSg nomSg (nomSg + "s")
								(nomSg + "ð") (nomSg + "ð") (nomSg + "nu") (nomSg + "ins")
								nomPl nomPl (basePl + "um") (basePl + "a")
								(nomPl + "n") (nomPl + "n") (basePl + "unum") (basePl + "anna") Neutr ) ;
			-- hreið-ur , hreið-ur
			<baseSg + "ur", basePl + "ur",Neutr>	=> lin N (mkNoun nomSg nomSg (baseSg + "ri") (nomSg + "s")
								(baseSg + "rið") (baseSg + "rið") (baseSg + "rinu") (nomSg + "sins")
								nomPl nomPl (basePl + "rum") (basePl + "ra")
								(basePl + "rin") (basePl + "rin") (basePl + "runum") (basePl + "ranna") Neutr ) ;
			-- borð- , borð-
			<_,_,Neutr>				=> lin N (mkNoun nomSg nomSg (nomSg + "i") (nomSg + "s")
								(nomSg + "ið") (nomSg + "ið") (nomSg + "inu") (nomSg + "sins")
								nomPl nomPl (nomPl + "um") (nomPl + "a")
								(nomPl + "in") (nomPl + "in") (nomPl + "unum") (nomPl + "anna") Neutr ) ;
			-- tíð- , tíð-ir
			<_,basePl + "ir",Fem>			=> lin N (mkNoun nomSg nomSg nomSg (nomSg + "ar")
								(nomSg + "in") (nomSg + "ina") (nomSg + "inni") (nomSg + "arinnar")
								nomPl nomPl (basePl + "um") (basePl + "a")
								(nomPl + "nar") (nomPl + "nar") (basePl + "unum") (basePl + "anna") Fem ) ;
			-- móð-ir , mæð-ur
			<baseSg + "ir", basePl + "ur",Fem>	=> lin N (mkNoun nomSg (baseSg + "ur") (baseSg + "ur") (baseSg + "ur")
								(nomSg + "in") (baseSg + "rina") (baseSg + "rinni") (baseSg + "rinnar")
								nomPl nomPl (basePl + "rum") (basePl + "ra")
								(nomPl + "nar") (nomPl + "nar") (basePl + "runum") (basePl + "ranna") Fem ) ;
			-- hest-ur , hest-ar
			<baseSg + "ur" , basePl + "ar",Masc>	=> lin N (mkNoun nomSg baseSg (baseSg + "i") (baseSg + "s")
								(nomSg + "inn") (baseSg + "inn") (baseSg + "inum") (baseSg + "sins")
								nomPl (basePl + "a") (baseSg + "um") (baseSg + "a")
								(nomPl + "nir") (basePl + "ana") (basePl + "unum") (basePl + "anna") Masc )
		} ;

		reg3N : (_,_,_ : Str) -> Gender -> N = \nomSg,genSg,nomPl,g -> case <nomSg,genSg,nomPl,g> of {
			-- Strong Nouns -- 
			
			-- steik- , steik-ar , steik-ur
			<_,_ + "ar",basePl + "ur",Fem>		=> lin N (mkNoun nomSg nomSg nomSg genSg
								(nomSg + "in") (nomSg + "ina") (nomSg + "inni") (genSg + "innar")
								nomPl nomPl (basePl + "um") (basePl + "a")
								(nomPl + "nar") (nomPl + "nar") (basePl + "unum") (basePl + "anna") Fem ) ;
			-- sæng- , sæng-ur , sæng-ur
			<_,_ + "ur", basePl + "ur",Fem>		=> lin N (mkNoun nomSg nomSg nomSg genSg
								(nomSg + "in") (nomSg + "ina") (nomSg + "inni") (genSg + "innar")
								nomPl nomPl (basePl + "um") (basePl + "a")
								(nomPl + "nar") (nomPl + "nar") (basePl + "unum") (basePl + "anna") Fem ) ;
			-- kerl-ing- , kerl-ing-ar , kerl-ing-ar
			<_ + "ing", _ + "ing" + "ar", basePl + "ar",Fem>	=> lin N (mkNoun nomSg (nomSg + "u") (nomSg + "u") genSg
										(nomSg + "in") (nomSg + "una") (nomSg + "unni") (genSg + "innar")
										nomPl nomPl (basePl + "um") (basePl + "a")
										(nomPl + "nar") (nomPl + "nar") (basePl + "unum") (basePl + "anna") Fem ) ;
			-- lif-ur , lif-r-ar , lif-r-ar
			<_ + "ur",baseSg + "r" + "ar", basePl + "ar",Fem>	=> lin N (mkNoun nomSg nomSg nomSg genSg
										(baseSg + "rin") (baseSg + "rina") (baseSg + "rinni") (genSg + "innar")
										nomPl nomPl (basePl + "um") (basePl + "a")
										(nomPl + "nar") (nomPl + "nar") (basePl + "unum") (basePl + "anna") Fem ) ;
			-- æð-ur , æð-ar , æð-ar
			<_ + "ur" , baseSg + "ar" , basePl + "ar", Fem>		=> lin N (mkNoun nomSg (baseSg + "i") (baseSg + "i") genSg
										(baseSg + "in") (baseSg + "ina") (baseSg + "innni") (genSg + "innar")
										nomPl nomPl (basePl + "um") (basePl + "a")
										(nomPl + "nar") (nomPl + "nar") (basePl + "unum") (basePl + "anna") Fem ) ;
			-- heið-i , heið-ar , heið-ar
			<_ + "i" , baseSg + "ar" , basePl + "ar", Fem>		=> lin N (mkNoun nomSg nomSg nomSg genSg
										(baseSg + "in") (baseSg + "ina") (baseSg + "inni") (genSg + "innar")
										nomPl nomPl (basePl + "um") (basePl + "a")
										(nomPl + "nar") (nomPl + "nar") (basePl + "unum") (basePl + "anna") Fem ) ;

			-- á- , á-r , á-r - not sure if á (e. river) is the only word that behaves like this or not..

			-- stöð- , stöð-v-ar , stöð-v-ar 
			-- skel- , skel-j-ar , skel-j-ar
			-- kinn- , kinn-ar , kinn-ar
			<_ , baseSg + "ar", basePl + "ar", Fem>			=> lin N (mkNoun nomSg nomSg nomSg genSg
										(nomSg + "in") (nomSg + "ina") (nomSg + "inni") (genSg + "innar")
										nomPl nomPl (basePl + "um") (basePl + "a")
										(nomPl + "nar") (nomPl + "nar") (basePl + "unum") (basePl + "anna") Fem )
		} ;

		-- Most adjectives can be predicted from the Sg.Fem.Nom in the positive. Here we denote
		-- the positive Sg.Fem.Nom. with fem and the positve Sg.Masc.Nom. with mas.
		regA : (_,_ : Str) -> A = \mas,fem -> case <mas,fem> of {

		-- note on u-shift :
		-- Adjectives with a in the root syllable u-shifts it ö. All other root vowels
		-- are unchanged throughout the inflexion. 
		-- a in suffixes is similarly u-shifted to u.
		
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
							(baseÖ + "rustu") False) ;-- superlative weak Pl.
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
							(baseÖ + "ustu") False); -- superlative weak Pl.
		
		-- 5 Adjectives having stems that end in -r, -s, (consonant +) -n 
		<baseA + "r", baseÖ + "r">		=> sameStem mas fem ;
		<baseA + "s", baseÖ + "s">		=> sameStem mas fem ;
		--<baseA@(*consonant + "n"), baseÖ@(*consonant + "n")>		=> sameStem mas fem ;

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
							(base + "justu") False) ; -- superlative weak Pl.

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
							(base + "ustu") False) ; -- superlative weak Pl.

		<_ + "inn" , base + "in")>		=> lin A (mkAdjective
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
							(base + "nustu") False)  -- superlative weak Pl.
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
			(baseÖ + "ustu") False) ;-- superlative weak Pl.

		-- A helper function that can take in and list out all cases in the positive degree would be nice.

		-- Irregular comparison and i-umlaut patterns
		-- Here (as for regA) mas and fem stand for the positive Sg.Masc.Nom. and Sg.Fem.Nom.,
		-- but com for the comparative Sg.Masc.Nom.
		-- irregA : (_,_,_ : Str) -> \mas,fem,com -> case <mas,fem> of {
		--}
} ;
