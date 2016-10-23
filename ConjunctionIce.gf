concrete ConjunctionIce of Conjunction = 
  CatIce ** open ResIce, Coordination, Prelude in {

	lin
		-- Conj -> ListS -> S
		ConjS = conjunctDistrSS ;

		-- Conj -> ListRS -> RS
		ConjRS conj ss = conjunctDistrTable Agr conj ss ** {c = ss.c} ;

		-- Conj -> ListCN -> CN
		ConjCN co ns = conjunctDistrTable4 Number Species Declension Case co ns ** {comp = \\_ => [] ; g = Neutr} ;

		-- Conj -> ListAP -> AP
		ConjAP co as = conjunctDistrTable4 Number Gender Declension Case co as ;

		-- In case of pronouns, it is possible(obligatory?) to form :
		-- [] las hana,hann og það ekki
		-- Conj -> ListNP -> NP
		ConjNP co ns = conjunctDistrTable NPCase co ns ** {
			a = {g = ns.a.g ; n = conjNumber co.n ns.a.n ; p = ns.a.p} ;
			isPron = False
		} ;

		-- Conj -> ListDAP -> Det
		ConjDet co ds = let cds = (conjunctDistrTable2 Gender Case co ds).s in {
			s = cds ;
			pron = \\_,_ => [] ;
			n = ds.n ;
			b = ds.b ;
			d = ds.d ;
		} ;

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

		-- CN -> CN -> ListCN
		BaseCN = twoTable4 Number Species Declension Case ;

		-- CN -> ListCN -> ListCN
		ConsCN = consrTable4 Number Species Declension Case comma ;

		-- AP -> AP -> ListAP
		BaseAP x y = twoTable4 Number Gender Declension Case x y ;

		-- AP -> ListAP -> ListAP
		ConsAP xs x = consrTable4 Number Gender Declension Case comma xs x ;

		-- NP -> NP -> ListNP
		BaseNP x y = twoTable NPCase x y ** {a = conjAgr x.a y.a} ;

		-- NP -> ListNP -> ListNP
		ConsNP xs x = consrTable NPCase comma xs x ** {a = conjAgr xs.a x.a} ;

		-- DAP -> DAP -> ListDAP or is it Det -> Det -> ListDAP ?
		BaseDAP x y = twoTable2 Gender Case x y ** {n = y.n ; b = y.b ; d = y.d} ;

		-- DAP -> ListDAP -> ListDAP or is it Det -> ListDAP -> ListDAP ?
		ConsDAP x xs = consrTable2 Gender Case comma x xs ** {n = xs.n ; b = xs.b ; d = xs.d} ;

	lincat
		[S] = {s1,s2 : Str} ;
		[Adv] = {s1,s2 : Str} ;
		[AdV] = {s1,s2 : Str} ;
		[IAdv] = {s1,s2 : Str} ;
		[NP] = {s1,s2 : NPCase => Str ; a : Agr} ;
		[CN] = {s1,s2 : Number => Species => Declension => Case => Str} ;
		[AP] = {s1,s2 : Number => Gender => Declension => Case => Str} ;
		[RS] = {s1,s2 : Agr => Str ; c : NPCase} ;
    		[DAP] = {s1,s2 : Gender => Case => Str ; n : Number ; b : Species ; d : Declension} ;
}
