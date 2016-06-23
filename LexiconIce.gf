--# -path=.:prelude

concrete LexiconIce of Lexicon = CatIce **
  open ParadigmsIce, IrregIce, Prelude in {

	lin
		already_Adv = mkAdv "þegar" ;
		arrive_V = mkV "koma" "kem" "kemur" "kemur" "komum" "komið" "koma" "kom" "komst" "kom" "komum" "komuð" "komu" 
			"komi" "komir" "komi" "komum" "komið" "komi" "kæmi" "kæmir" "kæmi" "kæmum" "kæmuð" "kæmu"
			"komdu" "komið" "komandi" "kominn" "kominn" "komnum" "komins" "komin" "komna" "kominni" "kominnar" "komið" "komið" "komnu" "komins"
			"komnir" "komna" "komnum" "kominna" "komnar" "komnar" "komnum" "kominna" "komin" "komin" "komnum" "kominna" "komni" "komna" "komna" 
			"komnu" "komna" "komnu" "komið" ;
		boat_N = mkN "bátur" "bátar" masculine;
		boy_N = mkN "strákur" "strákar" masculine ;
		cheese_N = mkN "ostur" "ostar" masculine ;
		city_N = mkN "borg" "borgir" feminine ;
		cold_A = mkA "kaldur" "köld" ;
		computer_N = mkN "tölva" "tölvur" feminine ;
		dirty_A = mkA "skítugur" "skítug" ;
		dog_N = mkN "hundur" "hundar" masculine ;
		door_N = mkN "hurð" "hurðir" feminine ;
		factory_N = mkN "verksmiðja" "verksmiðjur" feminine ;
		far_Adv = mkAdv "langt" ;
		fish_N = mkN "fiskur" "fiskar" masculine ;
		floor_N = mkN "gólf" "gólf" neuter ;
		garden_N = mkN "garður" "garðar" masculine ;
		gold_N = mkN "gull" "gull" neuter ;
		--hill_N = mkN "hæð" "hæðar" feminine ; error -> Internal error in GeneratePMCFC: evalTerm ("hæð")
		horse_N = mkN "hestur" "hestar" masculine ;
		hot_A = mkA "heitur" "heit" ;
		house_N = mkN "hús" "hús" neuter ;
		important_A = mkA "mikilvægur" "mikilvæg" ;
		iron_N = mkN "járn" "járn" neuter ;
		--king_N = mkN "konungur" "konung" masculine ; error -> Internal error in GeneratePMCFC: evalTerm ("hæð")
		leather_N = mkN "leður" "leður" neuter ;
--		love_V2 = mkV2 "elska" "elska" "elskar" "elskar" "elskum" "elskið" "elska" "elskaði" "elskaðir" "elskaði" "elskuðum" "elskuðuð" "elskuðu" 
--			"elski" "elskir" "elski" "elskum" "elskið" "elski" "elskaði" "elskaðir" "elskaði" "elskuðum" "elskuðuð" "elskuðu"
--			"elskaður" "elskið" "elskandi" "elskaður" "elskað" accusative;

		man_N = mkN "maður" "mann" "manni" "manns" 
			"maðurinn" "manninn" "manninum" "mannsins" 
			"menn" "menn" "mönnum" "manna" 
			"mennirnir" "mennina" "mönnunum" "mannanna" masculine ;
		now_Adv = mkAdv "núna" ;
		today_Adv = mkAdv "í dag" ;
		tree_N = mkN "tré" "tré" "tré" "trés" 
			"tréð" "tréð" "trénu" "trésins"
			"tré" "tré" "trjáum" "trjáa" 
			"trén" "trén" "trjánum" "trjánna" neuter;
--		walk_V = mkV "ganga" "geng" "gengur" "gengur" "göngum" "gangið" "ganga" "gekk" "gekkst" "gekk" "gengum" "genguð" "gengu" "gengið";
		woman_N = mkN "kona" "konu" "konu" "konu" 
			"konan" "konuna" "konunni" "konunnar"
			"konur" "konur" "konum" "kvenna" 
			"konurnar" "konurnar" "konunum" "kvennanna" feminine;

} ;
