concrete CatIce of Cat = CommonX - [Pol] ** open ResIce, Prelude in {

  flags optimize=all_subs ;

	lincat
		Pol = {s : Str ; b : Bool} ;

		-- Tensed/Untensed

		S = {s : Str} ;
		RS = {
			s : Agr => Str ;
			c : Case
		} ;

		-- Sentence

		Cl = {s : ResIce.Tense => Bool => Str} ;

		-- Verb

		VP = ResIce.VP ;
		Comp = {s : Agr => Str} ; 

		-- Adjective

		AP = ResIce.A ;

		-- Noun

		NP =  ResIce.NP ;
		
		CN = {
			noun : Number => Species => Case => Str ;
			adj :  Number => Case => Declension => Str ;
			g : Gender ;
			isPre : Bool
		} ;

		Det = {
			s : Gender => Case => Str ;
			n : Number ; 
			b : ResIce.Species ; 
			d : ResIce.Declension
		} ;

		Num  = {
			s : Gender => Case => Str ; 
			n : Number ; 
			hasCard : Bool
		} ;

		Ord = {
			s : Case => Str
		} ; 

		Card = {
			s : Gender => Case => Str ; 
			n : Number
		} ;

		Pron = {
			s : Case => Str ; 
			a : Agr
		} ;

		Quant = {
			s : Number => Gender => Case => Str ; 
			b : ResIce.Species ; -- for nouns, indication if the suffixed article is used or not.
			d : ResIce.Declension -- for adjectives, indication if the weak or strong form of the adjective is used.
		} ;

		-- Structural
		Conj = {s : Str ; n : Number} ;

		-- Open lexical classes, e.g. Lexicon

		V = ResIce.V;

		V2 = V ** {c : Case} ;

		A = ResIce.A ;

		N = ResIce.N ;

		N2 = ResIce.N ** {c2 : Str} ;

		PN = {s : Case => Str ; g : Gender} ;
}
