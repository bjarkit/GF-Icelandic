--# -path=.:../../prelude

--1 A Simple Icelandic Resource Morphology

-- This resource morphology contains definitions needed in the resource
-- syntax. To build a lexicon, it is better to use $ParadigmsIce$, which
-- gives a higher-level access to this module.

resource MorphoIce = ResIce ** open Prelude, (Predef=Predef), ResIce in {

	flags optimize=all ;

	oper 

		-----------------------------
		-- Neuter Noun Declensions --
		-----------------------------

		dAuga : Str -> Str -> NForms = \auga,augna ->
			let
				aug = init auga ;
				uaug = uUmlaut aug
			in nForms8
				auga auga auga auga
				(uaug + "u") (uaug + "u") (uaug + "um") augna ;

		dKvæði : (_,_ : Str) -> NForms = \kvæði,kvæðum ->
			let kvæð = init kvæði
			in nForms8
				kvæði kvæði kvæði (kvæði + "s")
				kvæði kvæði kvæðum (kvæð + "a") ;

		dBarn : (_,_ : Str) -> NForms = \barn,börn ->
			nForms8
				barn barn (barn + "i") (barn + "s")
				börn börn (börn + "um") (barn + "a") ;

		dSumar : (_,_ : Str) -> NForms = \sumar,sumur ->
			let sum = init (init sumar)
			in nForms8
				sumar sumar (sum + "ri") (sumar + "s")
				sumur sumur (sum + "rum") (sum + "ra") ;

		-- Only two (three) words goe like this.
		-- "tré" and "hné" (similarily goes fé, but 
		-- it rarely takes "fés" in the genative singular 
		-- generally has "fjár" and is debateble if 
		-- it has plural or not). 
		dTré : Str -> NForms = \tré ->
			let tr = init tré
			in nForms8
				tré tré tré (tré + "s")
				tré tré (tr + "jám") (tr + "jáa") ;

		-------------------------------
		-- Feminine Noun Declensions --
		-------------------------------

		dSaga : (_,_ : Str) -> NForms = \saga,sagna ->
			let
				sag = init saga ;
				sög = uUmlaut sag ;
				sögu = sög + "u"
			in nForms8
				saga sögu sögu sögu
				(sög + "ur") (sög + "ur") (sög + "um") sagna ;

		--------------------------------
		-- Masculine Noun Declensions -- 
		--------------------------------

		dSími : Str -> NForms = \sími ->
			let
				sím = init sími ;
				usím = uUmlaut sím ;
				síma = sím + "a" 
			in nForms8
				sími síma síma síma
				(sím + "ar") síma (usím + "um") síma ;

		dNemandi : (_,_ : Str) -> NForms = \nemandi,nemend ->
			let
				nemand = init nemandi ;
				nemanda = nemand + "a" ;
			in nForms8
				nemandi nemanda nemanda nemanda
				(nemend + "ur") (nemend + "ur") (nemend + "um") (nemend + "a") ;

		-- Very few words go like this. Currently not used.
		dDani : (_,_ : Str) -> NForms = \dani,danir ->
			let
				dan = init dani ;
				udan = uUmlaut dan ;
				dana = dan + "a"
			in nForms8
				dani dana dana dana
				danir dana (udan + "um") dana ;





		nForms2NeutrNoun : NForms -> N = \nfs -> nForms2Noun nfs nfs Neutr ; -- (nFormsNeuterSuffix nfs) Neutr ;

		nForms2MascNoun : NForms -> N = \nfs -> nForms2Noun nfs nfs Masc ; -- (nFormsNeuterSuffix nfs) Masc ;

		nForms2FemNoun : NForms -> N = \nfs -> nForms2Noun nfs nfs Fem ; -- (nFormsFemSuffix nfs) ;

		nForms2Noun : NForms -> NForms -> Gender -> N = \free,suffix,g -> {
				s = table {
					Sg => table {
						Suffix	=> caseList (suffix ! 0) (suffix ! 1) (suffix ! 2) (suffix ! 3) ;
						_	=> caseList (free ! 0) (free ! 1) (free ! 2) (free ! 3)
					} ;
					Pl => table {
						Suffix	=> caseList (suffix ! 4) (suffix ! 5) (suffix ! 6) (suffix ! 7) ;
						_	=> caseList (free ! 4) (free ! 5) (free ! 6) (free ! 7)
					}
				} ;
				g = g
		} ;
			


	---------------------
	-- Noun Auxilaries --
	---------------------

		vowel : pattern Str = #("a" | "á" | "e" | "é" | "i" | "í" | "o" | "ó" | "u" | "ú" | "y" | "ý" | "æ" | "ö" | "au" | "ei" | "ey") ;

		consonant : pattern Str = #("b" | "d" | "ð" | "f" | "g" | "h" | "j" | "k" | "l" | "m" | "n" | "p" | "r" | "s" | "t" | "v" | "x" | "þ") ;

		-- This function is still naive. Only takes into count words where only one "a" changes to "ö".
		-- Therefore words like "banani - bönunum" are not included. And even in such cases there are
		-- must also be taken into account compound words. Then only the last word should decline and
		-- experience any shift in a to ö.
	
		-- maybe I should rename this to aShifts2ö and make a similar function for "a" shifting to "u" ?
		uUmlaut : Str -> Str = \barn -> case barn of {
			-- is this powerful enough?
			front + "a" + back@(#consonant*)		=> front + "ö" + back ;
			_						=> barn
		} ;


		-- this is still naive - fewer forms are possible needed.
		NForms : Type = Predef.Ints 7 => Str ; 

		nForms8 : (x1,_,_,_,_,_,_,x8 : Str) -> NForms =
			\sgNom,sgAcc,sgDat,sgGen,plNom,plAcc,plDat,plGen -> table {
				0	=> sgNom ;
				1	=> sgAcc ;
				2	=> sgDat ;
				3	=> sgGen ;
				4	=> plNom ;
				5	=> plAcc ;
				6	=> plDat ;
				7	=> plGen
			} ;
} ;

