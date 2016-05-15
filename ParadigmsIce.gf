--# -path=.:../abstract:../../prelude:../common

--1 Icelandic Lexical Paradigms
--
-- Comment taken from Aarne Rantas English code:
-- Aarne Ranta 2003--2005
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
-- separate module [``IrregIce`` ../../english/IrregIce.gf], 
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
} ;
