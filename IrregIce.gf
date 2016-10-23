--# -path=.:prelude:../abstract:../common

concrete IrregIce of IrregIceAbs = CatIce ** open ParadigmsIce in {

flags optimize=values ;

	lin

	-- present preterie verbs
	-- dMuna : (_,_,_,_ : Str) -> MForms = \muna,man,mundi,myndi -> 

	-- eiga (own) á átti ætti 
	-- eiga á átti ætti
	
	-- mega (may) má mátti mætti
	
	-- unna (love) ann unni ynni

	-- kunna (know/know how to) kann kunni kynni

	-- þurfa (need) þarf þurfti þyrfti

	-- vita (know) veit vissi vissi 

	-- vilja (want/will) vil vildi vildi
	
	-- muna (remember) man mundi myndi

	-- munu (will/shall) mun _ myndi

	-- skulu (shall) skal _ skyldi

	-- (vera (to be) - gray area)
		-- mkIrreg
		--mk4V : (_,_,_,_ : Str) -> V = \telja,tel,taldi,talinn ->
		--	lin V (vForms2Verb telja (indsub3 telja tel taldi) (impSg taldi) (impPl telja) (presPart telja) (sup telja) (weakPP talinn) (strongPP talinn)) ;

	-- -ri verbs
	-- róa (row) ræ reri - róinn
	-- gróa (grow) græ greri - gróinn
	-- núa (rub) ný neri - núinn 
	-- snúa (turn/twist) sný sneri - snúinn

	-- Strong verbs

		bíða_V = irregV "bíða" "beðinn" ;
		bíta_V = irregV "bíta" "bitinn" ;
		bresta_V = irregV "bresta" "brostinn" ;
		brjóta_V = irregV "brjóta" "brotinn" ;
		drífa_V = irregV "drífa" "drifinn" ;
		fara_V = irregV "fara" "farinn" ;
		fljúga_V = irregV "fljúga" "floginn" ;
		fljóta_V = irregV "fljóta" "flotinn" ;
		frjósa_V = irregV "frjósa" "frosinn" ;
		gjósa_V = irregV "gjósa" "gosinn" ;
		grípa_V = irregV "grípa" "gripinn" ;
		hljóta_V = irregV "hljóta" "hlotinn" ;
		hvína_V = irregV "hvína" "hvininn" ;
		hrína_V = irregV "hrína" "hrininn" ;
		hrífa_V = irregV "hrífa" "hrifinn" ;
		hníga_V = irregV "hníga" "hniginn" ;
		kvíða_V = irregV "kvíða" "kviðinn" ;
		ljúga_V = irregV "ljúga" "loginn" ;
		líða_V = irregV "líða" "liðinn" ;
		líta_V = irregV "líta" "litinn" ;
		míga_V = irregV "míga" "miginn" ;
		ríða_V = irregV "ríða" "riðinn" ;
		rísa_V = irregV "rísa" "risinn" ;
		síga_V = irregV "síga" "siginn" ;
		sjúga_V = irregV "sjúga" "soginn" ;
		strjúka_V = irregV "strjúka" "strokinn" ;
		svífa_V	= irregV "svífa" "svifinn" ;
		svíkja_V = irregV "svíkja" "svikinn" ;
		stíga_V = irregV "stíga" "stiginn" ;
		sníða_V = irregV "sníða" "sniðinn" ;
		slíta_V = irregV "slíta" "slitinn" ;
		skína_V = irregV "skína" "skininn" ;
		skríða_V = irregV "skríða" "skriðinn" ;
		víkja_V = irregV "víkja" "vikinn" ;
		þrífa_V = irregV "þrífa" "þrifinn" ;
		hnjóta_V = irregV "hnjóta" "hnotinn" ;
		kjósa_V = irregV "kjósa" "kosinn" ;
		njóta_V = irregV "njóta" "notinn" ;
		sjóða_V = irregV "sjóða" "soðinn" ;
		þjóta_V = irregV "þjóta" "þotinn" ;
		þrjóta_V = irregV "þrjóta" "þrotinn" ;
		drjúpa_V = irregV "drjúpa" "dropinn" ;
		fjúka_V = irregV "fjúka" "fokinn" ;
		kljúfa_V = irregV "kljúfa" "klofinn" ;
		ljúka_V = irregV "ljúka" "lokinn" ;
		rjúfa_V = irregV "rjúfa" "rofinn" ;
		sjúga_V = irregV "sjúga" "soginn" ;
		súpa_V = irregV "súpa" "sopinn" ;
		lúka_V = irregV "lúka" "lokinn" ;
		lúta_V = irregV "lúta" "lotinn" ;
		smjúga_V = irregV "smjúga" "smoginn" ;
		detta_V = irregV "detta" "dottinn" ;
		skella_V = irregV "skella" "skollinn" ;
		skreppa_V = irregV "skreppa" "skroppinn" ;
		sleppa_V = irregV "sleppa" "sloppinn" ;
		smella_V = irregV "smella" "smollinn" ;
		snerta_V = irregV "snerta" "snortinn" ;
		spretta_V = irregV "spretta" "sprotinn" ;
		verða_V = irregV "verða" "orðinn" ;
		svelta_V = irregV "svelta" "soltinn" ;
		sverfa_V = irregV "sverfa" "sorfinn" ;
		vella_V = irregV "vella" "ollinn" ;
		velta_V = irregV "velta" "oltinn" ;
		verpa_V = irregV "verpa" "orpinn" ;
		þverra_V = irregV "þverra" "þorrinn" ;
{-

		spinna_V : V ;
		finna_V : V ;
		vinna_V : V ;
		binda_V : V ;
		vinda_V : V ;
		hrinda_V : V ;

		springa_V : V ; -- sprakk
		stinga_V : V ; -- stakk

		drekka_V : V ;
		brenna_V : V ;
		renna_V : V ;

		gjalda_V : V ;
		skjálfa_V : V ;

		hrökkva_V : V ;
		slökkva_V : V ;
		stökkva_V : V ;
		sökkva_V : V ;

		syngja_V : V ;
		tyggja_V : V ;

-}
}
