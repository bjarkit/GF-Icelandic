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
				\bóndi,bændur,g -> lin N (regN bóndi bændur g) ;
		} ;

		--2 Adjectives

		mkA = overload {

			-- The theoretical worst case
			mkA : (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_ : Str) -> Bool -> A =
				\sgMascNom,sgMascAcc,sgMascDat,sgMascGen,
				sgFemNom,sgFemAcc,sgFemDat,sgFemGen,
				sgNeutNom,sgNeutAcc,sgNeutDat,sgNeutGen,
				plMascNom,plMascAcc,plMascDat,plMascGen,
				plFemNom,plFemAcc,plFemDat,plFemGen,
				plNeutNom,plNeutAcc,plNeutDat,plNeutGen,
				weakSgMascNom,weakSgMascAccDatGen,
				weakSgFemNom,weakSgFemAccDatGen,
				weakSgNeut,weakPl,b -> 
				lin A (mkAdjective 
				sgMascNom sgMascAcc sgMascDat sgMascGen
				sgFemNom sgFemAcc sgFemDat sgFemGen
				sgNeutNom sgNeutAcc sgNeutDat sgNeutGen
				plMascNom plMascAcc plMascDat plMascGen
				plFemNom plFemAcc plFemDat plFemGen
				plNeutNom plNeutAcc plNeutDat plNeutGen
				weakSgMascNom weakSgMascAccDatGen
				weakSgFemNom weakSgFemAccDatGen
				weakSgNeut weakPl b) ;
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

		vowel : pattern Str = #("a" | "á" | "e" | "é" | "i" | "í" | "o" | "ó" | "u" | "ú" | "y" | "ý" | "æ" | "ö") ;

		consonant : pattern Str = #("b" | "d" | "ð" | "f" | "g" | "h" | "j" | "k" | "l" | "m" | "n" | "p" | "r" | "s" | "t" | "v" | "x" | "þ") ;

		-- For pattern matching nouns to suffix the definate article.
		-- The suffix , "-inn","-in","-ið", loses the "-i-" when the noun ends with
		-- "-a", "-i", "-u", and most cases of "-é".
		noIVowel : pattern Str = #("a" | "i" | "u" | "é") ;
		
		regN : (_,_ : Str) -> Gender -> N = \nomSg,nomPl,Gender -> case <nomSg,nomPl,Gender> of {
			-- Weak Nouns --

			-- aug-a , aug-u
			<baseSg + "a",basePl + "u",Neutr>	=> mkNoun nomSg nomSg nomSg nomSg 
								(nomSg + "ð") (nomSg + "ð") (nomSg + "nu") (nomSg + "ns") 
								nomPl nomPl (nomPl + "m") (basePl + "na")
								(nomPl + "n") (nomPl + "n") (nomPl + "num") (basePl +"nanna") Neutr ;
			-- tím-i , tím-ar
			<baseSg + "i",basePl + "ar",Masc>	=> mkNoun nomSg (baseSg + "a") (baseSg + "a") (baseSg + "a")
								(nomSg + "nn") (baseSg + "ann") (baseSg + "anum") (baseSg + "ans")
								nomPl (basePl + "a") (basePl + "um") (basePl + "a") -- some nouns have the suffix -n-a
								(nomPl + "nir") (basePl + "ana") (basePl + "unum") (basePl + "anna") Masc ;
			-- bónd-i , bænd-ur
			<baseSg + "i", basePl + "ur",Masc>	=> mkNoun nomSg (baseSg + "a") (baseSg + "a") (baseSg + "a")
								(nomSg + "nn") (baseSg + "ann") (baseSg + "anum") (baseSg + "ans")
								nomPl nomPl (basePl + "um") (basePl + "a")
								(basePl + "urnir") (basePl + "urnir") (basePl + "unum") (basePl + "anna") Masc ;
			-- lilj-a , lilj-ur
			<baseSg + "a", basePl + "ur",Fem>	=> mkNoun nomSg (baseSg + "u") (baseSg + "u") (baseSg + "u")
								(nomPl + "n") (baseSg + "una") (baseSg + "unni") (baseSg + "unnar")
								nomPl nomPl (basePl + "um") (basePl + "a")
								(basePl + "rinar") (basePl + "rinar") (basePl + "unum") (basePl + "anna") Fem ;
			-- æf-i , æf-ir
			<baseSg + "i", basePl + "ir",Fem> 	=> mkNoun nomSg nomSg nomSg nomSg
								(nomSg + "n") (nomSg + "na") (nomSg + "nni") (nomSg + "nnar")
								nomPl nomPl (basePl + "um") (basePl + "a")
								(nomPl + "nar") (nomPl + "nar") (basePl + "unum") (basePl + "anna") Fem ;
			-- lyg-i , lyg-ar
			<baseSg + "i", basePl + "ar",Fem>	=> mkNoun nomSg nomSg nomSg nomSg
								(nomSg + "n") (nomSg + "na") (nomSg + "nni") (nomSg + "nnar")
								nomPl nomPl (basePl + "um") (basePl + "a")
								(nomPl + "nar") (nomPl + "nar") (basePl + "unum") (basePl + "anna") Fem ;
			-- Strong Nouns --

			-- kvæð-i- , kvæð-i-
			<baseSg + "i", basePl + "i",Neutr>	=> mkNoun nomSg nomSg nomSg (nomSg + "s")
								(nomSg + "ð") (nomSg + "ð") (nomSg + "nu") (nomSg + "ins")
								nomPl nomPl (basePl + "um") (basePl + "a")
								(nomPl + "n") (nomPl + "n") (basePl + "unum") (basePl + "anna") Neutr ;
			-- hreið-ur , hreið-ur
			<baseSg + "ur", basePl + "ur",Neutr>	=> mkNoun nomSg nomSg (baseSg + "ri") (nomSg + "s")
								(baseSg + "rið") (baseSg + "rið") (baseSg + "rinu") (nomSg + "sins")
								nomPl nomPl (basePl + "rum") (basePl + "ra")
								(basePl + "rin") (basePl + "rin") (basePl + "runum") (basePl + "ranna") Neutr ;
			-- borð- , borð-
			<_,_,Neutr>				=> mkNoun nomSg nomSg (nomSg + "i") (nomSg + "s")
								(nomSg + "ið") (nomSg + "ið") (nomSg + "inu") (nomSg + "sins")
								nomPl nomPl (nomPl + "um") (nomPl + "a")
								(nomPl + "in") (nomPl + "in") (nomPl + "unum") (nomPl + "anna") Neutr ;
			-- tíð- , tíð-ir
			<_,basePl + "ir",Fem>			=> mkNoun nomSg nomSg nomSg (nomSg + "ar")
								(nomSg + "in") (nomSg + "ina") (nomSg + "inni") (nomSg + "arinnar")
								nomPl nomPl (basePl + "um") (basePl + "a")
								(nomPl + "nar") (nomPl + "nar") (basePl + "unum") (basePl + "anna") Fem ;
			-- móð-ir , mæð-ur
			<baseSg + "ir", basePl + "ur",Fem>	=> mkNoun nomSg (baseSg + "ur") (baseSg + "ur") (baseSg + "ur")
								(nomSg + "in") (baseSg + "rina") (baseSg + "rinni") (baseSg + "rinnar")
								nomPl nomPl (basePl + "rum") (basePl + "ra")
								(nomPl + "nar") (nomPl + "nar") (basePl + "runum") (basePl + "ranna") Fem
		} ;
} ;
