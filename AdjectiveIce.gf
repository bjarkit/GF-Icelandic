concrete AdjectiveIce of Adjective = CatIce ** open ResIce, Prelude in {

	lin
		-- A -> AP 
		PositA a = {
			s = \\n,g,d,c => a.s ! APosit d n g c
		} ;

		-- A -> NP -> AP 
		ComparA a np = {
			s = \\n,g,d,c => a.s ! ACompar n g c ++ "heldur en" ++ np.s ! NCase Nom
		} ;

		-- A2 -> NP -> AP
		ComplA2 a2 np = {
			s = \\n,g,d,c => a2.s ! APosit d n g c ++ a2.c2 ++ np.s ! NCase Dat
		} ;

    --ReflA2  : A2 -> AP ;        -- married to itself

		-- A2 -> AP
		UseA2 a2 = {
			s = \\n,g,d,c => a2.s ! APosit d n g c
		} ;

		-- A -> AP
		UseComparA a = {
			s = \\n,g,d,c => a.s ! ACompar n g c
		} ;

		-- CAdv -> AP -> NP -> AP
		CAdvAP cadv ap np = {
			s = \\n,g,d,c => cadv.s ++ ap.s ! n ! g ! d ! c ++ cadv.p ++ np.s ! NCase Nom
		} ;

-- The superlative use is covered in $Ord$.

    -- AdjOrd  : Ord -> AP ;       -- warmest

-- Sentence and question complements defined for all adjectival
-- phrases, although the semantics is only clear for some adjectives.
 
    -- SentAP  : AP -> SC -> AP ;  -- good that she is here

		--AdA -> AP -> AP
		AdAP ad ap = { 
			s = \\n,g,d,c => ad.s ++ ap.s ! n ! g ! d ! c 
		} ;

-- An adjectival phrase can be modified by an *adadjective*, such as "very".

    -- AdAP    : AdA -> AP -> AP ; -- very warm

-- It can also be postmodified by an adverb, typically a prepositional phrase.

    -- AdvAP   : AP -> Adv -> AP ; -- warm by nature
}
