concrete CatIce of Cat = CommonX ** open ResIce, Prelude in {

  flags optimize=all_subs ;

	lincat

		--2 Sentences and clauses

		S = {s : Str} ;

		QS = {s : QForm => Str} ;

		RS = {
			s : Agr => Str ;
			c : Case
		} ;

		Cl = ResIce.Cl ;

		ClSlash = {
			s : Tense => Anteriority => Polarity => Order => Str
		} ;

		SSlash = {
			s : Tense => Anteriority => Polarity => Order => Str
		} ;

		Imp = {s : Polarity => Number => Str} ;


		--2 Questions and interrogatives

		QCl = {s : Tense => Anteriority => Polarity => QForm => Str} ; -- is Agr needed?

		-- IP depend on gender, or should it be fixed and other things depending on them (i don think so)?
		IP = {s : Str} ;

		IComp = {s : Str} ;

		IDet = {s : Str} ;	

		IQuant = {s : Str} ;


		--2 Relative clauses and pronouns

		RCl = {s : Tense => Anteriority => Polarity => Order => Agr => Str} ;

		RP = {s : Str} ;


		--2 Verb phrases

		VP = ResIce.VP ;

		Comp = {s : Agr => Str} ; 

		VPSlash = ResIce.VP ** {
			c2 : Preposition ;
			n  : Agr => Str
		} ;	


		--2 Adjectival phrases

		AP = {s : Number => Gender => Declension => Case => Str} ;


		--2 Nouns and noun phrases

		CN = {
			s : Number => Species => Declension => Case => Str ;
			g : Gender
		} ;

		NP =  ResIce.NP ;

		Pron = ResIce.Pron ;

		Det = {
			s : Gender => Case => Str ;
			n : Number ; 
			b : ResIce.Species ; 
			d : ResIce.Declension ;
			isPre : Bool
		} ;

		Predet = {
			s : Number => Gender => Case => Str
		} ;

		Quant = {
			s : Number => Gender => Case => Str ;
			b : ResIce.Species ; -- for nouns, indication if the suffixed article is used or not.
			d : ResIce.Declension ; -- for adjectives, indication if the weak or strong form of the adjective is used.
			isPre : Bool
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

		Ord = {
			s : Number => Gender => Case => Str
		} ; 

		DAP = {
			s : Gender => Case => Str ;
			n : Number ;
			b : ResIce.Species ;
			d : ResIce.Declension
		} ;


		--2 Numerals

		Numeral = {s : Str} ;


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
		N2 = ResIce.N ** {c2 : Str} ;
		N3 = ResIce.N ** {c2,c3 : Str} ;
		PN = {s : Case => Str ; g : Gender} ;
}
