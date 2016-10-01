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

			-- Given Sg.Nom. 
			mkN : Str -> Gender -> N = mk1N ;

			-- Given Sg.Nom and Pl.Nom - different Pl.Nom part
			mkN : (_,_ : Str) -> Gender -> N = mk2N ;

			-- Given Sg.Nom, Sg.Gen, and Pl.Nom - also different Sg.Gen part
			mkN : (_,_,_ : Str) -> Gender -> N = mk3N ;

			-- Given Sg.Nom, Sg.Gen, Pl.Nom and Pl.Gen - also different Pl.Gen part
			mkN : (_,_,_,_ : Str) -> Gender -> N = mk4N ;

			-- Worst case, all eight forms.
			mkN : (x1,_,_,_,_,_,_,x8 : Str) -> Gender -> N = mk8N ;

		} ;


		-- Some weak declensions of neuter and feminine nouns differ in the Pl Gen
		-- with a "-n-" in the ending but differ in no other way.
		-- This goes only for weak feminine and neuter nouns, the operation is not for
		-- masculine nouns.
		mkNPlGen : Str -> Gender -> N = \stelpa,g -> case <stelpa,g> of {
			<front + "a",Fem>		=> lin N (nForms2FemNoun (dSaga stelpa (front + "na"))) ;
			<front + "a",_>			=> lin N (nForms2NeutrNoun (dAuga stelpa (front + "na")))
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

		mk3N : (_,_,_ : Str) -> Gender -> N =\x,y,z,g -> case g of {
			Neutr		=> lin N (nForms2NeutrNoun (neutrNForms3 x y z)) ;
			Masc		=> lin N (nForms2MascNoun (mascNForms3 x y z)) ;
			Fem		=> lin N (nForms2FemNoun (femNForms3 x y z))
		} ;

		mk4N : (_,_,_,_ : Str) -> Gender -> N =\a,b,c,d,g -> case g of {
			Neutr		=> lin N (nForms2NeutrNoun (neutrNForms4 a b c d)) ;
			Masc		=> lin N (nForms2MascNoun (mascNForms4 a b c d)) ;
			Fem		=> lin N (nForms2FemNoun (femNForms4 a b c d))
		} ;

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

		-- Currently not used! Should be added at least some cases for 2 forms and maybe for 3 forms as well.
		neutrNForms2 : (_,_ : Str) -> NForms = \sg,pl -> case <sg,pl> of {
			_					=> dBarn sg pl
		} ;

		neutrNForms3 : (_,_,_ : Str) -> NForms = \nom,gen,pl -> case <nom,gen,pl> of {
			_					=> dBarn nom pl
		} ;

		neutrNForms4 : (_,_,_,_ : Str) -> NForms = \sgNom,sgGen,plNom,plGen -> case <sgNom,sgGen,plNom,plGen> of {
			_					=> dBarn sgNom plNom
		} ;

		mascNForms1 : Str -> NForms = \s -> case s of {
			front + "andi"					=> dNemandi s (front + "endur") ;
			front + "óndi"					=> dNemandi s (front + "ændur") ;
			front + "ndi"					=> dNemandi s s ;
			front + middle@("ing" | "ung" | "dóm") + "ur"	=> dArmur s (front + middle + "ar") ;
			front + middle@"und" + "ur"			=> dHöfundur s (front + middle + "ar") ;
			front + middle@("ang" | "ald") + "ur"		=> dAkur s (front + middle + "rar") ;
			front + ("a" | "i" | "u") + end@("nn" | "ll")	=> dHiminn s (front + (init end) + "ar") ;
			#consonant* + #vowel + ("ll" | "nn")		=> dStóll s ;
			stem + "ur"					=> dArmur s (stem + "ar") ; -- the most common masc noun type
			_ + "ór"					=> dMór s ; -- some words ending in "ór" do not behave like this, e.g., "kór"
			_ + "i" 					=> dSími s
		} ;

		mascNForms2 : (_,_ : Str) -> NForms = \sg,pl -> case <sg,pl> of {
			<_ + "i",_ + "ir">			=> dDani sg pl ;
			<_ + "ur",_ + "rar">			=> dAkur sg pl ;
			<_ + "ur",_ + "ar">			=> dArmur sg pl ; -- maybe not needed since stem + "ur" is already catched?
			<_ + ("a" | "i" | "u") + end@("nn" | "ll"), _ + "ar">	=> dHiminn sg pl -- for words like himinn that have a i-shift in the plural
		} ;

		mascNForms3 : (_,_,_ : Str) -> NForms =\nom,gen,pl -> case <nom,gen,pl> of {
			<_ + "ur", _ + "ar", _ + "ar">		=> dHöfundur nom pl ;
			<_ + "ur", _ + "ar", _ + "ir">		=> dSöfnuður nom gen pl
		} ;

		mascNForms4 : (_,_,_,_ : Str) -> NForms = \sgNom,sgGen,plNom,plGen -> case <sgNom,sgGen,plNom,plGen> of {
			_					=> dNemandi sgNom plNom -- dummy case so the operation doesn't give error
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
			<_ + "ur",_ + "rir">				=> dFjöður sg pl ;
			<_ + "ur",_ + "ir">				=> dBrúður sg pl ;
			<_,_ + "ir">					=> dÞökk sg pl ;
			<_,_ + ("rar" | "var" | "jar")>			=> dLifur sg pl ;
			<_ + "ur", _ + "ar">				=> dÆður sg pl ;
			<_,_ + "ar">					=> dNál sg pl ;

			-- this is not general
			--<_ + ("í" | "ú" | "ei" | "æ" | "á" | "ó" | "au") + ("t"* | "k"*),_>	=> dBók sg pl ;

			<"móðir" | "dóttir" | "systir",_>		=> dMóðir sg pl ;
			<_ + "á", _ + "ær">				=> dTá sg pl ;
			<_ + "ó", _ + "ær">				=> dTá sg pl ;
			<_ + "ú", _ + "ýr">				=> dTá sg pl ;
			<_ + "á", _ + "á" + _>				=> dÁ sg pl ;
			<_ + "ó", _ + "ó" + _>				=> dÁ sg pl ;

			<_ + "ú", _ + "ú" + _>				=> dÁ sg pl ;-- in some cases the Sg.Gen becomes ú-ar instead of ú-r, I do not know atm why.

			<_ + "ús",_>					=> dMús sg pl
		} ;

		femNForms3 : (_,_,_ : Str) -> NForms = \nom,gen,pl -> case <nom,gen,pl> of {
			<_ + "i" , _ + "ar", _ + "ar">			=> dHeiði nom pl ;
			<_ + "i" , _ , _ + "ar">			=> dLygi nom pl ;
			<_ , _ + "ar" , _ + "ur">			=> dNögl nom gen pl
		} ;

		femNForms4 : (_,_,_,_ : Str) -> NForms = \sgNom,sgGen,plNom,plGen -> case <sgNom,sgGen,plNom,plGen> of {
			<_,_ + "ur",_ + "ur",_>		=> dMörk sgNom plNom plGen
		} ;

		mk8N : (x1,_,_,_,_,_,_,x8 : Str) -> Gender -> N = \a,b,c,d,e,f,g,h,gend ->
			let nfs = nForms8 a b c d e f g h
			in lin N (nForms2Noun nfs (nForms2Suffix nfs gend) gend) ;

		mkPN = overload {

			mkPN : Str -> Gender -> PN = 
				\name,g	-> regPN name g ;	

		} ;

		--2 Adjectives

		-- Adjectives are constructed by the function $mkA$, which takes a varying
		-- number of arguments.


		mkA = overload {

			-- Given Sg.Masc.Nom of the positive comparision
			-- FIXME - generic adverb operation - I havent really found much research on this matter. Therefore
			-- I took it out in the whole resource grammar until I find something more than a couple of lines on
			-- wikipedia.
			mkA : Str -> A = mk1A ;

--			-- Given Sg.Masc.Nom of the positive comparision and the adverb derived from the adjective
--			mkA : Str -> Str -> A = mk1A ;

			-- Given also the Sg.fem.Nom of the positive comparision
			mkA : (_,_ : Str) -> A = mk2A ;

			-- Given also the Sg.Masc.Nom of the comparitive comparision
			mkA : (_,_,_ : Str) -> A = mk3A ;
		} ;

		mk1A : Str -> A = \s -> lin A (aForms2Adjective 
			(strongPosit1 s) (weakPosit s []) (compar1 s) (weakSuperl s []) (strongSuperl1 s)) ;

		mk2A : (_,_ : Str) -> A = \mas,fem -> lin A (aForms2Adjective
			(strongPosit2 mas fem) (weakPosit mas fem) (compar2 mas fem) (weakSuperl mas fem) (strongSuperl2 mas fem)) ;

		mk3A : (_,_,_ : Str) -> A = \mas,fem,com -> lin A (aForms2Adjective
			(strongPosit2 mas fem) (weakPosit mas fem) (compar1 com) (weakSuperl com []) (strongSuperl1 com)) ;

		strongPosit1 : Str -> AForms = \s -> case s of {
			#consonant* + "ei" + ("ll" | "nn")	=> dSeinn s ;
			_ + "inn"				=> dFarinn s ;
			_ + "ill"				=> dLítill s ;
			#consonant* + #vowel + ("ll" | "nn")	=> dSeinn s ;
			stem + "ur"				=> dFalur s (a2ö stem)
		} ;

		strongPosit2 : (_,_ : Str) -> AForms = \mas,fem -> case <mas,fem> of {
			<_,_ + ("á" | "ó" | "ú")>	=> dSmár fem ;
			<front + "ur",_ + "ur"> 	=> dFagur mas fem ;
			<front + "ur",_>		=> dFalur mas fem ;
			<_,_ + ("r" | "s" | (#consonant + "n"))>	=> dDýr mas ; -- Should this also be moved to strongPosit1 ?
			<_, _ + ("ý" | "æ")>		=> dNýr fem ;
			<_ + "ill",_>			=> dLítill mas ;
			<_ + "inn",_>			=> dFarinn mas
		} ;

		weakPosit : (_,_ : Str) -> AForms = \mas,fem -> case <mas,fem> of {
			<front + "ur",_ + "ur">		=> dPositW (front + "r") ;
			<stem + "ur",_>			=> dPositW stem ;
			<front + "ill",_>		=> dPositW (í2i front + "l") ;
			<front + "inn",_>		=> dPositW (front + "n") ;
			_				=> dPositW fem
		} ;

		compar1 : Str -> AForms = \s -> case s of {
			front + "ni"		=> dI (init s) ;
			stem + "ari"		=> dAri stem ;
			stem + "ri"		=> dRi stem ;
			front + mid@("leg" | "ug") + "ur"	=> dRi (front + mid) ;
			stem + "ur"		=> dAri stem ;
			front + "inn"		=> dAri (front + "n") ;
			_ + ("ll" | "nn")	=> dI s
		} ;

		compar2 : (_,_ : Str) -> AForms = \mas,fem -> case <mas,fem> of {
			<front + "ur",_ + "ur">		=> dAri (front + "r") ;
			<front + mid@("leg" | "ug") + "ur",_>	=> dRi (front + mid) ;
			<stem + "ur", _>		=> dAri stem ;
			<front + "inn",_>		=> dAri (front + "n") ;
			<_ + ("ll" | "nn"),_>		=> dI mas ;
			<_ + "r", _ + ("á" | "ó" | "ú" | "ý" | "æ")>	=> dRi fem ; 
			<_,_ + ("r" | "s" | (#consonant + "n"))>	=> dAri fem
		} ;

		weakSuperl : (_,_ : Str) -> AForms = \mas,fem -> case <mas,fem> of {
			<front + "ni",_>		=> dSuperlW (front + "nst") (front + "nust") ;
			<stem + "ari",_>		=> dSuperlW (stem + "ast") (stem + "ust") ;
			<stem + "rri",_>		=> dSuperlW (stem + "st") (stem + "st") ;
			<stem + "ri",_>			=> dSuperlW (stem + "st") (stem + "st") ;
			<frontm + "ur",frontf + "ur">	=> dSuperlW (frontm + "rast") (frontf + "rust") ;
			<front + "ur",_>		=> dSuperlW (front + "ast") (fem + "ust") ;
			<front + end@("ll" | "nn"),_>	=> dSuperlW (front + (init end) + "ast") ((a2ö front) + (init end) + "ust") ;
			<_,_ + ("ý" | "æ")>		=> dSuperlW (fem + "jast") (fem + "just") ;
			_				=> dSuperlW (fem + "ast") (fem + "ust")
		} ;

		strongSuperl1 : Str -> AForms = \s -> case s of {
			front + "ni"		=> dFalastur (front + "nstur") (front + "nst") ;
			stem + "ari"		=> dFalastur (stem + "astur") (stem + "ust") ;
			stem + "rri"		=> dFalastur (stem + "stur") (stem + "st") ;
			stem + "ri"		=> dFalastur (stem + "stur") (stem + "st") ;
			front + "inn" 		=> dFalastur (front + "nastur") ((a2ö front) + "nust") ;
			stem + "ur"		=> dFalastur (stem + "astur") ((a2ö stem) + "ust") ;
			front + end@("ll" | "nn")	=> dFalastur (front + (init end) + "astur") ((a2ö front) + (init end) + "ust")
		} ;

		strongSuperl2 : (_,_ : Str) -> AForms = \mas,fem -> case <mas,fem> of {
			<frontm + "ur",frontf + "ur">		=> dFalastur (frontm + "rastur") (frontf + "rust") ;
			<frontm + "ur", _>			=> dFalastur (frontm + "astur") (fem + "ust") ;
			<_, _ + ("á" | "ú" | "ó")>		=> dFalastur (fem + "astur") (fem + "ust") ;
			<_, _ + ("ý" | "æ")>			=> dFalastur (fem + "jastur") (fem + "just") ;
			<front + end@("ll" | "nn"),_>		=> dFalastur (front + (init end) + "astur") ((a2ö front) + (init end) + "ust") ;
			<_,_ + ("r" | "s" | (#consonant + "n"))>	=> dFalastur (fem + "astur") (fem + "ust")
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

		mkAdV : Str -> AdV = \x -> lin AdV (ss x) ;

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
