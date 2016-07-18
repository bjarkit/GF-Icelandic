concrete ConjunctionIce of Conjunction = 
  CatIce ** open ResIce, Coordination, Prelude in {

	lin
		-- Conj -> ListS -> S
		ConjS = conjunctDistrSS ;

		-- Conj -> ListRS -> RS
		ConjRS conj ss = conjunctDistrTable Agr conj ss ** {c = ss.c} ;

		--    Conj -> ListCN -> CN
		--    ConjCN co ns = conjunctDistrTable4 Number Species Declension Case co ns ** {g = Neutr} ;

		--    ConjAP   : Conj -> ListAP -> AP ;     -- cold and warm
		--    ConjNP   : Conj -> ListNP -> NP ;     -- she or we
		--    ConjDet  : Conj -> ListDAP -> Det ;   -- his or her

		-- Conj -> ListAdv -> Adv
		ConjAdv = conjunctDistrSS ;

		-- Conj -> ListAdV -> AdV
		ConjAdV = conjunctDistrSS ;
		
		-- Conj -> ListIAdv -> IAdv
		ConjIAdv = conjunctDistrSS ;

		-- These are fun's generated from the list cat's.

		-- S -> S -> ListS
		BaseS = twoSS ;

		-- S -> ListS -> ListS
		ConsS = consrSS comma ;

		-- AdV -> AdV -> ListAdV
		BaseAdV = twoSS ;
		
		-- AdV -> ListAdV -> ListAdV
		ConsAdV = consrSS comma ;

		-- Adv -> Adv -> ListAdv
		BaseAdv = twoSS ;

		-- Adv -> ListAdv -> ListAdv
		ConsAdv = consrSS comma ;

		-- IAdv -> IAdv -> ListIAdv
		BaseIAdv = twoSS ;

		-- IAdv -> ListIAdv -> ListIAdv
		ConsIAdv = consrSS comma ;

		-- RS -> RS -> ListRS
		BaseRS x y = twoTable Agr x y ** {c = y.c} ;

		-- RS -> ListRS -> ListRS
		ConsRS xs x = consrTable Agr comma xs x ** {c = xs.c} ;

	lincat
		[S] = {s1,s2 : Str} ;
		[Adv] = {s1,s2 : Str} ;
		[AdV] = {s1,s2 : Str} ;
		[IAdv] = {s1,s2 : Str} ;
		[NP] = {s1,s2 : Case => Str ; a : Agr} ;
		[CN] = {s1,s2 : Number => Species => Declension => Case => Str} ;
		[AP] = {s1,s2 : Number => Gender => Declension => Case => Str} ;
		[RS] = {s1,s2 : Agr => Str ; c : Case} ;

		--  BaseAP : AP -> AP -> ListAP ;       -- red, white
		--  ConsAP : AP -> ListAP -> ListAP ;   -- red, white, blue
		--
		--  BaseCN : CN -> CN -> ListCN ;      -- man, woman
		--  ConsCN : CN -> ListCN -> ListCN ;  -- man, woman, child
		--
		--  BaseNP : NP -> NP -> ListNP ;      -- John, Mary
		--  ConsNP : NP -> ListNP -> ListNP ;  -- John, Mary, Bill
}
