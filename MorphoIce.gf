--# -path=.:../../prelude

--1 A Simple Icelandic Resource Morphology

-- This resource morphology contains definitions needed in the resource
-- syntax. To build a lexicon, it is better to use $ParadigmsIce$, which
-- gives a higher-level access to this module.

resource MorphoIce = open Prelude, (Predef=Predef), ResIce in {

	flags optimize=all ;

	oper 
		--2 Pronouns

		mkPron : (ég,mig,mér,mín : Str) -> Gender -> Number -> Person -> {s : Case => Str ; a : Agr} =
			\ég,mig,mér,mín,g,n,p -> {
			s = table {
				Nom => ég ;
				Acc => mig ;
				Dat => mér ;
				Gen => mín
				} ;
			a = Ag g n p ;
		} ;

} ;

