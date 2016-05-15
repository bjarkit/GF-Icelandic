--# -path=.:../../prelude

--1 A Simple Icelandic Resource Morphology

-- This resource morphology contains definitions needed in the resource
-- syntax. To build a lexicon, it is better to use $ParadigmsIce$, which
-- gives a higher-level access to this module.

resource MorphoIce = open Prelude, (Predef=Predef), ResIce in {

	flags optimize=all ;

	oper 
		--2 Nouns

		-- vowel and noIVowel should (I think) be moved to MorphoIce.gf

		vowel : pattern Str = #("a" | "á" | "e" | "é" | "i" | "í" | "o" | "ó" | "u" | "ú" | "y" | "ý" | "æ" | "ö");

		-- For pattern matching nouns to suffix the definate article.
		-- The suffix , "-inn","-in","-ið", loses the "-i-" when the noun ends with
		-- "-a", "-i", "-u", and most cases of "-é".
		noIVowel : pattern Str = #("a" | "i" | "u" | "é") ;
		
		-- Pattern matching Weak nouns (S Einarsson p 47-48). The tradition is to 
		-- partition Icelandic nouns into two groups, strong and weak. Strong
		-- nouns end in a consonant in the Sg.Nom. and weak end in a vowell in
		-- the Sg.Nom. These groups of strong and weak nouns are then grouped
		-- together by the case endings in the Gen.Sg. and Nom.Pl. (called kenniföll).
		-- However, for the weak group of nouns, it is possible to partition them
		-- by the ending in Nom.Sg. and Nom.Pl.
		--
		-- A third class is introduced in the Germanic Languages (van der Auwera and König) 
		-- p 142-189 with Gen.Sg. -a and Nom.Pl -ir for masculine weak nouns. But that
		-- introduction does not include the other case endings for that class. I suspect it
		-- was a type error or confusion with the feminine classes.
		regN : (_,_ : Str) -> Gender -> N = \nomSg,nomPl,Gender -> case <nomSg,nomPl,Gender> of {
			-- Weak Nouns --

			-- aug-a , augu-u
			<baseSg + "a",basePl + "u",Neutr>	=> mkNoun nomSg nomSg nomSg nomSg 
								(nomSg + "ð") (nomSg + "ð") (nomSg + "nu") (nomSg + "ns") 
								nomPl nomPl (nomPl + "m") (basePl + "na")
								(nomPl + "n") (nomPl + "n") (nomPl + "num") (basePl +"nanna") Neutr ;
			-- tím-i , tíma-ar
			<baseSg + "i",basePl + "ar",Masc>	=> mkNoun nomSg (baseSg + "a") (baseSg + "a") (baseSg + "a")
								(nomSg + "nn") (baseSg + "ann") (baseSg + "anum") (baseSg + "ans")
								nomPl (basePl + "a") (basePl + "um") (basePl + "a") -- some nouns have the suffix -n-a
								(nomPl + "nir") (basePl + "ana") (basePl + "num") (basePl + "anna") Masc ;
			-- bónd-i , bænd-ur
			<baseSg + "i", basePl + "ur",Masc>	=> mkNoun nomSg (baseSg + "a") (baseSg + "a") (baseSg + "a")
								(nomSg + "nn") (baseSg + "ann") (baseSg + "anum") (baseSg + "ans")
								nomPl nomPl (basePl + "um") (basePl + "a")
								(basePl + "urnir") (basePl + "urnir") (basePl + "num") (basePl + "anna") Masc ;
			-- lij-a , lij-ur
			<baseSg + "a", basePl + "ur",Fem>	=> mkNoun nomSg (baseSg + "u") (baseSg + "u") (baseSg + "u")
								(nomPl + "n") (baseSg + "una") (baseSg + "unni") (baseSg + "unnar")
								nomPl nomPl (basePl + "um") (basePl + "a")
								(basePl + "rinar") (basePl + "rinar") (basePl + "num") (basePl + "anna") Fem ;
			-- æf-i , æf-ir
			<baseSg + "i", basePl + "ir",Fem> 	=> mkNoun nomSg nomSg nomSg nomSg
								(nomSg + "n") (nomSg + "na") (nomSg + "nni") (nomSg + "nnar")
								nomPl nomPl (basePl + "um") (basePl + "a")
								(nomPl + "nar") (nomPl + "nar") (basePl + "num") (basePl + "anna") Fem ;
			-- lyg-i , lyg-ar
			<baseSg + "i", basePl + "ar",Fem>	=> mkNoun nomSg nomSg nomSg nomSg
								(nomSg + "n") (nomSg + "na") (nomSg + "nni") (nomSg + "nnar")
								nomPl nomPl (basePl + "um") (basePl + "a")
								(nomPl + "nar") (nomPl + "nar") (basePl + "num") (basePl + "anna") Fem 
			-- Strong Nouns --

			-- kvæð-i- , kvæði-i-
			-- some words take -j-um and -j-a in Dat.Pl. and Gen.Pl. instead of just -um and -a, e.g., rík-i- , rík-i-.
			--<baseSg + "i", baseSg + "i",Neutr>	=> mkNoun nomSg nomSg nomSg (nomSg + "s")
			--					(nomSg + "ð") (nomSg + "ð")
			-- borð- , borð-
			--<_,_,Neutr>				=> mkNoun nomSg nomSg (nomSg + "i") (nomSg + "s")
			--					(nomSg + "ið")
		} ;

		--2 Pronouns

		mkPron : (ég,mig,mér,mín : Str) -> Gender -> Number -> Person -> {s : Case => Str ; a : Agr} =
			\ég,mig,mér,mín,g,n,p -> {
			s = table {
				Nom => ég ;
				Acc => mig ;
				Dat => mér ;
				Gen => mín
				} ;
			a = Ag g n p ;
		} ;

} ;

