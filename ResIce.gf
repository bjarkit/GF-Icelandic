--# -path=.:../abstract:../common:../../prelude

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
		Species = Def | Indef ;
	
		-- Agreement of noun phrases has three parts
		Agr = Ag Gender Number Person ;


	--2 For $Verb$

		VForm = 
			  VInf 
			| VPres Number Person 
			| VPast Number Person 
			| V1Part
			;

	--2 For $Adjective$

		Declension = Weak | Strong ;

		AForm = 
			  APosit Declension Number Gender 
			| ACompar Number Gender  -- the comparative has only the weak declension
			| ASuperl Declension Number Gender
			;
		

	--------------------------------------------
	--TYPE DEFINITIONS + WORST-CASE CONSTRUCTORS
	--------------------------------------------

	-- For $Lex$.
	oper

		caseList : (_,_,_,_ : Str) -> Case => Str =
			\n,a,d,g -> table {
				Nom => n ;
				Acc => a ; 
				Dat => d ; 
				Gen => g
			} ;

		-- For $Nouns$

		N : Type = { 
			s : Number => Species => Case => Str ; 
			g : Gender
		} ;

		NP : Type = {
			s : Case => Str;
			a : Agr
		} ;

		mkNoun : (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_ :  Str) -> Gender -> N =
			\hestur,hest,hesti,hests,
			hesturinn,hestinn,hestinum,hestsins,
			hestar,hestaAcc,hestum,hestaGen,
			hestarnir,hestana,hestunum,hestanna,g -> {
				s = table {
					Sg => table {
						Indef	=> caseList hestur hest hesti hests ;
						Def	=> caseList hesturinn hestinn hestinum hestsins 
					} ;
					Pl => table {
						Indef	=> caseList hestar hestaAcc hestum hestaGen ;
						Def	=> caseList hestarnir hestana hestunum hestanna
					}
				} ;
				g = g
		} ;

		-- For $Adjectives$

		A : Type = {
			s : AForm => Case => Str ;
			isPre : Bool -- is this needed in Icelandic?
		} ; 

		mkAdjective : (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_ : Str) -> Bool -> A =
			\sgMascNom,sgMascAcc,sgMascDat,sgMascGen,
			sgFemNom,sgFemAcc,sgFemDat,sgFemGen,
			sgNeutNom,sgNeutAcc,sgNeutDat,sgNeutGen,
			plMascNom,plMascAcc,plMascDat,plMascGen,
			plFemNom,plFemAcc,plFemDat,plFemGen,
			plNeutNom,plNeutAcc,plNeutDat,plNeutGen,
			weakSgMascNom,weakSgMascAccDatGen,
			weakSgFemNom,weakSgFemAccDatGen,
			weakSgNeut,weakPl,b -> {
				s = table {
					APosit Weak Sg Masc	=> caseList weakSgMascNom weakSgMascAccDatGen weakSgMascAccDatGen weakSgMascAccDatGen ;
					APosit Weak Sg Fem 	=> caseList weakSgFemNom weakSgFemAccDatGen weakSgFemAccDatGen weakSgFemAccDatGen ;
					APosit Weak Sg Neutr	=> caseList weakSgNeut weakSgNeut weakSgNeut weakSgNeut ;
					APosit Weak Pl _ 	=> caseList weakPl weakPl weakPl weakPl ;
					APosit Strong Sg Masc	=> caseList sgMascNom sgMascAcc sgMascDat sgMascGen ;
					APosit Strong Sg Fem	=> caseList sgFemNom sgFemAcc sgFemDat sgFemGen ;
					APosit Strong Sg Neutr	=> caseList sgNeutNom sgNeutAcc sgNeutDat sgNeutGen ;
					APosit Strong Pl Masc	=> caseList plMascNom plMascAcc plMascDat plMascGen ;
					APosit Strong Pl Fem	=> caseList plFemNom plFemAcc plFemDat plFemGen ;
					APosit Strong Pl Neutr	=> caseList plNeutNom plNeutAcc plNeutDat plNeutGen ;
					
					ACompar Sg Masc		=> caseList weakPl weakPl weakPl weakPl ;  
					ACompar Sg Fem		=> caseList weakPl weakPl weakPl weakPl ;  
					ACompar Sg Neutr	=> caseList weakPl weakPl weakPl weakPl ;  
					ACompar Pl _		=> caseList weakPl weakPl weakPl weakPl ;  
					ASuperl Weak Sg Masc	=> caseList weakPl weakPl weakPl weakPl ;  
					ASuperl Weak Sg Fem	=> caseList weakPl weakPl weakPl weakPl ;  
					ASuperl Weak Sg Neutr	=> caseList weakPl weakPl weakPl weakPl ;  
					ASuperl Weak Pl _	=> caseList weakPl weakPl weakPl weakPl ;  
					ASuperl Strong Sg Masc	=> caseList weakPl weakPl weakPl weakPl ;  
					ASuperl Strong Sg Fem 	=> caseList weakPl weakPl weakPl weakPl ;  
					ASuperl Strong Sg Neutr => caseList weakPl weakPl weakPl weakPl ;  
					ASuperl Strong Pl Masc  => caseList weakPl weakPl weakPl weakPl ;  
					ASuperl Strong Pl Fem  	=> caseList weakPl weakPl weakPl weakPl ;  
					ASuperl Strong Pl Neutr	=> caseList weakPl weakPl weakPl weakPl
				} ;
				isPre = b
		} ;

		-- For $Verb$.

		V : Type = {
			s : VForm => Str
		} ;

		mkVerb : (_,_,_,_,_,_,_,_,_,_,_,_,_,_ : Str) -> V =
			\vera,er1,ert,er3,erum,eruð,eru,var1,varst,var3,vorum,voruð,voru,verið -> {
				s = table {
					VInf 		=> vera ;
					VPres Sg Per1	=> er1 ;
					VPres Sg Per2	=> ert ;
					VPres Sg Per3	=> er3 ;
					VPres Pl Per1	=> erum ; 
					VPres Pl Per2	=> eruð ;
					VPres Pl Per3	=> eru ;
					VPast Sg Per1	=> var1 ;
					VPast Sg Per2	=> varst ;
					VPast Sg Per3	=> var3 ;
					VPast Pl Per1	=> vorum ;
					VPast Pl Per2	=> voruð ; 
					VPast Pl Per3	=> voru;
					V1Part		=> verið
				} ;
		} ;	

		VP : Type = {
			s 	: V;
			obj 	: Agr => Str; 
		};

		-- For Predication
		agrV : V -> Agr -> Tense -> Bool -> Str = \v,a,t,neg -> 
			-- 1	In these scenarios, the only auxilary verb used (as far as I know)
			--	 is 'að hafa' ('to have') - therefore it is hardcoded atm.
			-- 2	'hef' and 'hefur' are dominant in modern Icelandic - but 'hefi' and
			--	'hefir' are rather common in written texts.
			let 
				aux = mkVerb "hafa" "hef" "hefur" "hefur" "höfum" "hafið" "hafa" "hafði" "hafðir" "hafði" "höfðum" "höfðuð" "höfðu" "haft"
			in case <t,a,neg> of {
				<TPres,Ag _ n p,_> => v.s ! VPres n p ;
				<TPerf,Ag _ n p,True> => aux.s ! VPres n p ++ v.s ! V1Part;
				<TPerf,Ag _ n p,False> => aux.s ! VPres n p ++ "ekki" ++  v.s ! V1Part
		};

}
