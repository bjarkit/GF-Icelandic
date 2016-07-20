concrete CatIce of Cat = CommonX ** open ResIce, Prelude in {

  flags optimize=all_subs ;

	lincat

		-- Sentence

		S = {s : Str} ;

		RS = {
			s : Agr => Str ;
			c : Case
		} ;

		Cl = ResIce.Cl ;

		Imp = {s : Polarity => Number => Str} ;

		-- Relative

		RCl = {
			s : Tense => Anteriority => Polarity => Order => Agr => Str
		} ;

		-- Verb

		VP = ResIce.VP ;

		Comp = {s : Agr => Str} ; 

		VPSlash = ResIce.VP ** {
			c2 : Preposition ;
			n  : Agr => Str
		} ;	

		-- Adjective

		AP = { s : Number => Gender => Declension => Case => Str } ;

		-- Noun

		NP =  ResIce.NP ;
		
		CN = {
			s : Number => Species => Declension => Case => Str ;
			g : Gender
		} ;

		Det = {
			s : Gender => Case => Str ;
			n : Number ; 
			b : ResIce.Species ; 
			d : ResIce.Declension ;
			isPre : Bool
		} ;

		Num  = {
			s : Gender => Case => Str ; 
			n : Number ; 
			hasCard : Bool
		} ;

		Numeral = { s : Str } ;

		Ord = {
			s : Number => Gender => Case => Str
		} ; 

		Card = {
			s : Gender => Case => Str ; 
			n : Number
		} ;

		Pron = ResIce.Pron ;

		Quant = {
			s : Number => Gender => Case => Str ; 
			b : ResIce.Species ; -- for nouns, indication if the suffixed article is used or not.
			d : ResIce.Declension ; -- for adjectives, indication if the weak or strong form of the adjective is used.
			isPre : Bool
		} ;

		-- Structural
		Conj = {s1,s2 : Str ; n : Number} ;
		Prep = ResIce.Preposition ;

		-- Open lexical classes, e.g. Lexicon

		V, VS, VQ, VA = ResIce.V;

		VV, V2, V2A, V2S, V2Q = ResIce.V ** {c2 : Preposition} ;

		V3, V2V = ResIce.V ** {c2,c3 : Preposition} ;

		A = ResIce.A ;
		A2 = ResIce.A ** {c2 : Str} ;

		N = ResIce.N ;

		N2 = ResIce.N ** {c2 : Str} ;

		N3 = ResIce.N ** {c2,c3 : Str} ;

		PN = {s : Case => Str ; g : Gender} ;
}
