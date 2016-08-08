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

		dAuga : (SgNom,PlGen : Str) -> NForms = \auga,augna ->
			let
				aug = init auga ;
				uaug = a2ö aug
			in nForms8
				auga auga auga auga
				(uaug + "u") (uaug + "u") (uaug + "um") augna ;

		dKvæði : (SgNom,PlDat : Str) -> NForms = \kvæði,kvæðum ->
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

		dSaga : (SgNom,PlGen : Str) -> NForms = \saga,sagna ->
			let
				sag = init saga ;
				sög = a2ö sag ;
				sögu = sög + "u"
			in nForms8
				saga sögu sögu sögu
				(sög + "ur") (sög + "ur") (sög + "um") sagna ;

		dÞökk : (_,_ : Str) -> NForms = \þökk,þakkir ->
			let þakk = init (init þakkir)
			in nForms8
				þökk þökk þökk (þakk + "ar")
				þakkir þakkir (þökk + "um") (þakk + "a") ;

		dVerslun : (_,_ : Str) -> NForms = \verslun,verslanir ->
			let verslan = init (init verslanir)
			in nForms8
				verslun verslun verslun (verslun + "ar")
				verslanir verslanir (verslun + "um") (verslan + "a") ;

		dKeppni : (_,_ : Str) -> NForms = \keppni,keppnir ->
			let keppn = init keppni
			in nForms8
				keppni keppni keppni keppni
				keppnir keppnir (keppn + "um") (keppn + "a") ;

		dFjöður : (_,_ : Str) -> NForms = \fjöður,fjaðrir ->
			let
				fjöð = init (init fjöður) ;
				fjaðr = init (init fjaðrir)
			in nForms8
				fjöður fjöður fjöður (fjaðr + "ar")
				fjaðrir fjaðrir (fjöð + "rum") (fjaðr + "a") ;

		dBrúður : (_,_ : Str) -> NForms = \brúður,brúðir ->
			let brúð = init (init brúður)
			in nForms8
				brúður (brúð + "i") (brúð + "i") (brúð + "ar")
				brúðir brúðir (brúð + "um") (brúð + "a") ;


		dFylking : (_,_ : Str) -> NForms = \fylking,fylkingar ->
			nForms8
				fylking (fylking + "u") (fylking + "u") fylkingar
				fylkingar fylkingar (fylking + "um") (fylking + "a") ;

		dNál : (_,_ : Str) -> NForms = \nál,nálar ->
			nForms8
				nál nál nál nálar
				nálar nálar (nál + "um") (nál + "a") ;

		dLifur : (_,_ : Str) -> NForms = \lifur,lifrar ->
			let lifr = init (init lifrar)
			in nForms8
				lifur lifur lifur lifrar
				lifrar lifrar (lifr + "um") (lifr + "a") ;

		dÆður : (_,_ : Str) -> NForms = \æður,æðar ->
			let æð = init (init æður)
			in nForms8
				æður (æð + "i") (æð + "i") æðar
				æðar æðar (æð + "um") (æð + "a") ;

		dHeiði : (_,_ : Str) -> NForms = \heiði,heiðar ->
			let heið = init heiði
			in nForms8
				heiði heiði heiði heiðar
				heiðar heiðar (heið + "um") (heið + "a") ;

		dLygi : (_,_ : Str) -> NForms = \lygi,lygar ->
			let lyg = init lygi
			in nForms8
				lygi lygi lygi lygi
				lygar lygar (lyg + "um") (lyg + "ar") ;

		dNögl : (_,_,_ : Str) -> NForms = \nögl,naglar,neglur ->
			let
				nagl = init (init naglar)
			in nForms8
				nögl nögl nögl naglar
				neglur neglur (nögl + "um") (nagl + "a") ;

		dMörk : (SgNom,SgGen,PlGen : Str) -> NForms = \mörk,merkur,marka ->
			nForms8
				mörk mörk mörk merkur
				merkur merkur (mörk + "um") marka ;

		dMóðir : (_,_ : Str) -> NForms = \móðir,mæður ->
			let
				móð = init (init móðir) ;
				móður = móð + "ur" ;
				mæð = init (init mæður)
			in nForms8
				móðir móður móður móður
				mæður mæður (mæð + "rum") (mæð + "ra") ;

		dTá : (_,_ : Str) -> NForms = \tá,tær ->
			nForms8
				tá tá tá (tá + "ar")
				tær tær (tá + "um") (tá + "a") ;

		dÁ : (_,_ : Str) -> NForms = \á,ár ->
			nForms8
				á á á ár
				ár ár (á + "m") (á + "a") ;

		dMús : (_,_ : Str) -> NForms = \mús,mýs ->
			nForms8
				mús mús mús (mús + "ar")
				mýs mýs (mús + "um") (mús + "a") ;

	--------------------------------
	-- Masculine Noun Declensions -- 
	--------------------------------

		dSími : Str -> NForms = \sími ->
			let
				sím = init sími ;
				usím = a2ö sím ;
				síma = sím + "a" 
			in nForms8
				sími síma síma síma
				(sím + "ar") síma (usím + "um") síma ;

		dNemandi : (_,_ : Str) -> NForms = \nemandi,nemendur ->
			let
				nemand = init nemandi ;
				nemanda = nemand + "a" ;
				nemend = init (init nemendur)
			in nForms8
				nemandi nemanda nemanda nemanda
				nemendur nemendur (nemend + "um") (nemend + "a") ;

		-- Very few words go like this. Currently not used.
		dDani : (_,_ : Str) -> NForms = \dani,danir ->
			let
				dan = init dani ;
				udan = a2ö dan ;
				dana = dan + "a"
			in nForms8
				dani dana dana dana
				danir dana (udan + "um") dana ;

	-----------------------
	-- Noun Construction -- 
	-----------------------

		nForms2NeutrNoun : NForms -> N = \nfs -> nForms2Noun nfs (nForms2Suffix nfs Neutr) Neutr ;

		nForms2MascNoun : NForms -> N = \nfs -> nForms2Noun nfs (nForms2Suffix nfs Neutr) Masc ;

		nForms2FemNoun : NForms -> N = \nfs -> nForms2Noun nfs (nForms2Suffix nfs Fem) Fem ;

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

	-----------------------------------
	-- Suffixed Article Construction --
	-----------------------------------

		nForms2Suffix : NForms -> Gender -> NForms = \nfs,g -> 
				let
					sgNom = suffixSgNom (nfs ! 0) (nfs ! 4) g ;
					sgAcc = suffixSgAcc (nfs ! 1) (nfs ! 4) g ;
					sgDat = suffixSgDat (nfs ! 2) (nfs ! 4) g ;
					sgGen = suffixSgGen (nfs ! 3) g ;
					plNom = suffixPlNom (nfs ! 4) g ;
					plAcc = suffixPlAcc (nfs ! 5) g ;
					plDat = suffixPlDat (nfs ! 6) g ;
					plGen = suffixPlGen (nfs ! 7) g
				in nForms8
					sgNom sgAcc sgDat sgGen
					plNom plAcc plDat plGen ;

		-- The plural form is given to deterimine if "-ur" is a part of the stem, i.e. an extened "-r", for
		-- feminine nouns. In that case the "-u-" drops before the suffixed article. The "-u-" also drops
		-- for neuter nouns, but that seems to be in general for neuter nouns with "-ur" in the stem - I
		-- am no entirely sure about this and can't find much in literature about this...

		--  hinn - hin - hið
		suffixSgNom : (Nom,Pl : Str) -> Gender -> Str = \s,p,g -> case <s,p,g> of {
			<_ + ("a" | "i" | "u" | "é"),_,Masc>		=> s + "nn" ;
			<_,_,Masc>					=> s + "inn" ;
			<_ + ("a" | "i" | "u" | "é"),_,Fem>		=> s + "n" ;
			<front + "ur",_ + ("rar" | "rur" | "rir"),Fem>	=> front + "rin" ;
			<_,_,Fem>					=> s + "in" ;
			<_ + ("a" | "i" | "u" | "é"),_,Neutr>		=> s + "ð" ;
			<front + "ur",_,Neutr>				=> front + "rið" ;
			<_,_,Neutr>					=> s + "ið"
		} ;

		-- hinn - hina - hið
		suffixSgAcc : (Acc,Pl : Str) -> Gender -> Str = \s,p,g -> case <s,p,g> of {
			<_ + ("a" | "i" | "u" | "é"),_,Masc>		=> s + "nn" ;
			<_,_,Masc>					=> s + "inn" ;
			<front + "ur",_ + ("rar" | "rur" | "rir"),Fem>	=> front + "rina" ;
			<_ + #consonant,_,Fem>				=> s + "ina" ;
			<_,_,Fem>					=> s + "na" ;
			<_ + ("a" | "i" | "u" | "é"),_,Neutr>		=> s + "ð" ;
			<front + "ur",_,Neutr>				=> front + "rið" ;
			<_,_,Neutr>					=> s + "ið"
		} ;

		-- hinum - hinni - hinu
		suffixSgDat : (Dat,Pl : Str) -> Gender -> Str = \s,p,g -> case <s,p,g> of {
			<_,_,Masc>					=> s + "num" ;
			<front + "ur",_ + ("rar" | "rur" | "rir"),Fem>	=> front + "rinni" ;
			<_ + #consonant,_,Fem>				=> s + "inni" ;
			<_,_,Fem>					=> s + "nni" ;
			<_,_,Neutr>					=> s + "nu"
		} ;

		-- hins - hinnar - hins
		suffixSgGen : Str -> Gender -> Str = \s,g -> case <s,g> of {
			<_ + ("a" | "i" | "u" | "é"),Masc>	=> s + "ns" ;
			<_,Masc>				=> s + "ins" ;
			<_ + #consonant,Fem>			=> s + "innar" ;
			<_,Fem>					=> s + "nnar" ;
			<_ + ("a" | "i" | "u" | "é"),Neutr>	=> s + "ns" ;
			<_,Neutr>				=> s + "ins"
		} ;

		-- hinir - hinar - hin
		suffixPlNom : Str -> Gender -> Str = \s,g -> case <s,g> of {
			<_ , Masc>				=> s + "nir" ;
			<_ , Fem>				=> s + "nar" ;
			<_ + ("a" | "i" | "u" | "é"),Neutr>	=> s + "n" ;
			<front + "ur",Neutr>			=> front + "rin" ;
			<_,Neutr>				=> s + "in"
		} ;

		-- hina - hinar - hin
		suffixPlAcc : Str -> Gender -> Str = \s,g -> case <s,g> of {
			<_,Masc>				=> s + "na" ;
			<_,Fem>					=> s + "nar" ;
			<_ + ("a" | "i" | "u" | "é"),Neutr>	=> s + "n" ;
			<front + "ur",Neutr>			=> front + "rin" ;
			<_,Neutr>				=> s + "in"
		} ;

		-- hinum
		suffixPlDat : Str -> Gender -> Str = \s,g -> case <s,g> of {
			<stem + "m", _>		=> stem + "num"
		} ;

		-- hinna
		suffixPlGen : Str -> Gender -> Str = \s,g -> case <s,g> of {
			<front + end@("á" | "ó" | "ú") + "a",_>	=> front + end + "nna" ; -- not entirely sure if this goes for all masculine and neuter nouns
			<_, _>			=> s + "nna"
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
	
		a2ö : Str -> Str = \barn -> case barn of {
			-- is this powerful enough?
			front + "a" + back@(#consonant*)		=> front + "ö" + back ;
			_						=> barn
		} ;

		-- I am fairly certain it works the same way as a2ö
		ö2a : Str -> Str = \þökk -> case þökk of {
			front + "ö" + back@(#consonant*)		=> front + "a" + back ;
			_						=> þökk
		} ;


		-- this is still naive - fewer forms are possible needed.
		NForms : Type = Predef.Ints 7 => Str ; 

		-- another (maybe) possible option (and maybe more optimal) would be to have nForms just two, Sg.Nom and Pl.Nom
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
