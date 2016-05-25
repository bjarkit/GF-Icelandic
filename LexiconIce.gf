--# -path=.:prelude

concrete LexiconIce of Lexicon = CatIce **
  open ParadigmsIce, IrregIce, Prelude in {

	lin
		arrive_V = mkV "koma" "kem" "kemur" "kemur" "komum" "komið" "koma" "kom" "komst" "kom" "komum" "komuð" "komu" "komið";
		green_A = mkA "grænn" "grænan" "grænum" "grænans" 
			"græn" "græna" "grænni" "grænnar" 
			"grænt" "grænt" "grænu" "græns" 
			"grænir" "græna" "grænum" "grænna" 
			"grænar" "grænar" "grænum" "grænna" 
			"græn" "græn" "grænum" "grænna" 
			"græni" "græna" 
			"græna" "grænu" 
			"græna" "grænu" 
			"grænni" "grænna" "grænni"
			"grænastur" "grænastan" "grænustum" "grænasts"
			"grænust" "grænasta" "grænastri" "grænstrar"
			"grænast" "grænast" "grænsustu" "grænasts"
			"grænastir" "grænasta" "grænustum" "grænastra"
			"grænastar" "grænastar" "grænustum" "grænastra"
			"grænust" "grænust" "grænustum" "grænastra"
			"grænasti" "grænasta"
			"grænasta" "grænustu"
			"grænasta" "grænstu" ;
		house_N = mkN "hús" "hús" neuter;
		love_V2 = mkV2 "elska" "elska" "elskar" "elskar" "elskum" "elskið" "elska" "elskaði" "elskaðir" "elskaði" "elskuðum" "elskuðuð" "elskuðu" "elskað" accusative;
		man_N = mkN "maður" "mann" "manni" "manns" 
			"maðurinn" "manninn" "manninum" "mannsins" 
			"menn" "menn" "mönnum" "manna" 
			"mennirnir" "mennina" "mönnunum" "mannanna" masculine ;
		tree_N = mkN "tré" "tré" "tré" "trés" 
			"tréð" "tréð" "trénu" "trésins"
			"tré" "tré" "trjáum" "trjáa" 
			"trén" "trén" "trjánum" "trjánna" neuter;
		walk_V = mkV "ganga" "geng" "gengur" "gengur" "göngum" "gangið" "ganga" "gekk" "gekkst" "gekk" "gengum" "genguð" "gengu" "gengið";
		woman_N = mkN "kona" "konu" "konu" "konu" 
			"konan" "konuna" "konunni" "konunnar"
			"konur" "konur" "konum" "kvenna" 
			"konurnar" "konurnar" "konunum" "kvennanna" feminine;

} ;
