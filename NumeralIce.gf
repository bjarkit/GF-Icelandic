concrete NumeralIce of Numeral = CatIce [Numeral,Digits] ** open Prelude, ResIce in {

	param
		DForm = unit | teen | ten | neuter ;
		NGen = com | neut ; 
		Size = sg | less10 | pl ;
		CardOrd = NOrd Number Gender Case ;

	lincat
		Numeral 		= { s : Str } ;
		Digit			= LinDigit ;
		lincat Sub10 		= {s : DForm => Str ; size : Size} ;
		lincat Sub100 		= {s : NGen => Str ; size : Size} ;
		lincat Sub1000 		= {s : NGen => Str ; size : Size} ;
		lincat Sub1000000 	= { s : Str } ;

	lin 
		num x = x ;
		n2 = mkNum "tveir" "tólf" "tuttugu" "tvö" ;
		n3 = mkNum "Þrír" "Þréttán" "Þrjátíu" "Þrjú" ;
		n4 = mkNum "fjórir" "fjórtán" "fjörutíu" "fjögur" ;
		n5 = regNum "fimm" ;
		n6 = regNum "sex" ;
		n7 = mkNum "sjö" "sautján" "sjötíu" "sjö" ; 
		n8 = mkNum "átta" "átján" "áttíu" "átta" ;
		n9 = mkNum "níu" "nítján" "níutíu" "níu" ;

		pot01 = {s = table {f => "einn"} ; size = sg } ;
		pot0 d = {s = d.s ; size = less10 } ;
		pot110 = {s = table {_ => "tíu"} ; size = pl } ;-- ss "tíu" FIXME I have no Idea why ss does not work, but doing it like this does.
		pot111 = {s = table {_ => "ellefu"} ; size = pl } ;--ss "ellefu" FIXME same as for pot110
		pot1to19 d = {s = table {_=> (d.s ! teen)} ; size = pl } ; -- ss (d.s ! teen) ;
		pot0as1 n = {s = table {com => n.s ! unit ; neut => n.s ! neuter } ; size = n.size } ;
		pot1 d = {s = table {_ => d.s ! ten } ; size = pl};
		pot1plus d e = {s = table {
					com => d.s ! ten ++ "og" ++ e.s ! unit ; 
					neut => d.s ! ten ++ "og" ++ e.s ! neuter
				}; 
				size = pl
		} ;
		pot1as2 n = n ;
		pot2 d = {s = table {_ => omitsg (d.s ! neuter) d.size ++ "hundrað" } ; size = pl} ; 
		pot2plus d e = {s = table {f => omitsg (d.s ! neuter) d.size ++ "hundrað" ++ (maybeog) e.size ++ e.s ! f} ; size = pl} ; 

		pot2as3 n = {s = n.s ! com } ;
		pot3 n = {s = omitsg (n.s ! neut) n.size ++ "Þúsund"} ;
		pot3plus n m = {s = omitsg (n.s ! neut) n.size ++ "Þúsund" ++ (maybeog m.size) ++ m.s ! com} ;

	oper
		LinDigit = {s : DForm => Str} ;
	
		cardOrd : Str -> { s : CardOrd => Str } = \sjötti -> {s = table {
				NOrd Sg Masc Nom	=> sjötti ;
				NOrd Sg Masc _		=> (init sjötti) + "a" ;
				NOrd Sg Fem Nom 	=> (init sjötti) + "a" ;
				NOrd Sg Fem _		=> (init sjötti) + "u" ;
				NOrd Sg Neutr _		=> (init sjötti) + "a" ;
				NOrd Pl _ _		=> (init sjötti) + "u" 
			}
		};

		mkNum : (_,_,_,_ : Str) -> LinDigit = 
			\two,twelve,twenty,tvo -> 
				{s = table {unit => two ; teen => twelve ; ten => twenty ; neuter => tvo}} ;
		regNum : Str -> LinDigit = 
			\fimm -> mkNum fimm (fimm + "tán") (fimm + "tíu") fimm;

		ss : Str -> {s : NGen => Str ; size : Size} = \s -> {s = table {_ => s } ; size = pl};

		maybeog : Size -> Str = \sz -> table {pl => [] ; _ => "og" } ! sz ;  
		omitsg : Str -> Size -> Str = \s -> \sz -> table {sg => [] ; _ => s } ! sz ;

}
