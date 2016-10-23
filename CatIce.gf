concrete CatIce of Cat = CommonX ** open ResIce, Prelude in {

  flags optimize=all_subs ;

	lincat

		--2 Sentences and clauses

		S = {s : Str} ;

		QS = {s : QForm => Str} ;

		RS = {
			s : Agr => Str ;
			c : NPCase
		} ;

		Cl = ResIce.Cl ;

		ClSlash = {
			s : Tense => Anteriority => Polarity => Order => Str ;
			c2 : Preposition ;
			n3 : Agr => Str
		} ;

		SSlash = {
			s : Order => Str ;
			c2 : Preposition ;
			n3 : Agr => Str
		} ;

		Imp = {s : Polarity => Number => Str} ;


		--2 Questions and interrogatives

		QCl = {s : Tense => Anteriority => Polarity => QForm => Str} ; -- is Agr needed?

		IP = {
			s : Gender => Case => Str ; 
			n : Number
		} ;

		IComp = {s : Number => Gender => Case => Str} ;

		IDet = {s : Gender => Case => Str ; n : Number} ;

		IQuant = {s : Number => Gender => Case => Str} ;


		--2 Relative clauses and pronouns

		RCl = {s : Tense => Anteriority => Polarity => Order => Agr => Str} ;

		RP = {s : Str} ;


		--2 Verb phrases

		VP = ResIce.VP ;

		Comp = {s : Agr => Str} ; 

		VPSlash = ResIce.VP ** {
			c2 : Preposition ;
			nn1 : Agr => Str ; -- maybe rename these into direct and indirect objects?
			nn2 : Agr => Str -- or is a single field sufficent?
		} ;	

		--2 Adjectival phrases

		AP = {s : Number => Gender => Declension => Case => Str} ;


		--2 Nouns and noun phrases

		CN = {
			s : Number => Species => Declension => Case => Str ;
			comp : Case => Str ; -- used for possessive appositions
			g : Gender
		} ;

		NP =  ResIce.NP ;

		Pron = ResIce.Pron ;

		Det = {
			s : Gender => Case => Str ;
			pron : Gender => Case => Str ; -- pronouns generally follow the noun that they describe, but numbers and ordinals/adjectivs preced it
			n : Number ; 
			b : ResIce.Species ; 
			d : ResIce.Declension
		} ;

		Predet = {
			s : Number => Gender => Case => Str
		} ;

		Quant = {
			s : Number => Gender => Case => Str ;
			b : ResIce.Species ; -- for nouns, indication if the suffixed article is used or not.
			d : ResIce.Declension ; -- for adjectives, indication if the weak or strong form of the adjective is used.
			isPron : Bool -- pronouns generally follow the noun that they describe
		} ;

		Num  = {
			s : Gender => Case => Str ; 
			n : Number ; 
			hasCard : Bool
		} ;

		Card = {
			s : Gender => Case => Str ; 
			n : Number
		} ;

		-- This needs to account for the weak and strong declensions of adjectives, since 
		-- the superlative is used through Ord.
		Ord = {
			s : ResIce.Declension => Number => Gender => Case => Str
		} ; 

		DAP = {
			s : Gender => Case => Str ;
			n : Number ;
			b : ResIce.Species ;
			d : ResIce.Declension
		} ;


		--2 Numerals

		Numeral = {s : CardOrd => Str ; n : Number} ;
		Digits = {s : CardOrd => Str ; n : Number} ;


		--2 Structural words

		Conj = {s1,s2 : Str ; n : Number} ;
		Prep = ResIce.Preposition ;


		--2 Words of open classes

		V, VS, VQ, VA = ResIce.V;
		VV, V2, V2A, V2S, V2Q = ResIce.V ** {c2 : Preposition} ;
		V3, V2V = ResIce.V ** {c2,c3 : Preposition} ;

		A = ResIce.A ;
		A2 = ResIce.A ** {c2 : Preposition} ;

		N = ResIce.N ;
		N2 = ResIce.N ** {c2 : Preposition} ;
		N3 = ResIce.N ** {c2,c3 : Preposition} ;
		PN = {s : Case => Str ; g : Gender} ;
}
