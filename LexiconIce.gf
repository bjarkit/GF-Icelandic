--# -path=.:prelude

concrete LexiconIce of Lexicon = CatIce **
  open ParadigmsIce, IrregIce, Prelude in {

	lin
		-- New paradigms (WORK IN PROGRESS)
		bank_N = mkN "höfuð" neuter ;
		--bank_N = mkN "banki" masculine ;
		boss_N = mkN "stjóri" masculine ;
		cap_N = mkN "húfa" feminine ;
		cat_N = mkN "kisa" feminine ;
		computer_N = mkN "tölva" feminine ;
		coat_N = mkN "kápa" feminine ;
		cousin_N = mkN "frændi" masculine ; -- frænka is the feminine equivalent
		factory_N = mkN "verksmiðja" feminine ;
		glove_N = mkN "hanski" masculine ;
		lamp_N = mkN "lampi" masculine ;
		oil_N = mkN "olía" feminine ;
		pen_N = mkN "penni" masculine ;
		planet_N = mkN "pláneta" feminine ;
		school_N = mkN "skóli" masculine ;
		shirt_N = mkN "skyrta" feminine ;
		student_N = mkN "nemandi" masculine ;
		teacher_N = mkN "kennari" masculine ;
		university_N = mkN "háskóli" masculine ;
		window_N = mkN "gluggi" masculine ;
		fat_N = mkN "fita" feminine ;
		fog_N = mkN "þoka" feminine ;
		tail_N = mkN "rófa" feminine ;
		rule_N = mkN "regla" feminine ;
		apple_N = mkN "epli" neuter ;
		baby_N = mkN "barn" neuter ;
		bike_N = mkN "hjól" neuter ;
		boot_N = mkN "sígvél" neuter ;
		bread_N = mkN "brauð" neuter ;
		butter_N = mkN "smjör" neuter ;
		carpet_N = mkN "teppi" neuter ;
		ceiling_N = mkN "loft" neuter ;
		child_N = mkN "barn" neuter ; -- N2 !
		country_N = mkN "land" neuter ;
		floor_N = mkN "gólf" neuter ;
		house_N = mkN "hús" neuter ;
		lake_N = mkN "vatn" neuter ;
		meat_N = mkN "kjöt" neuter ;
		moon_N = mkN "tungl" neuter ;
		mountain_N = mkN "fjall" neuter ;
		newspaper_N = mkN "dagblað" neuter ;
		plastic_N = mkN "plat" neuter ;
		radio_N = mkN "útvarp" neuter ;
		religion_N = mkN "trúarbragð" neuter ; -- FIXME - not correctly declined
		roof_N = mkN "þak" neuter ;
		rubber_N = mkN "gúmmí" neuter ; -- only singular
		ship_N = mkN "skip" neuter ;
		silver_N = mkN "silfur" neuter ; -- only singular
		steel_N = mkN "stál" neuter ;
		table_N = mkN "borð" neuter ;
		television_N = mkN "sjónvarp" neuter ;
		village_N = mkN "þorp" neuter ;
		war_N = mkN "stríð" neuter ;
		water_N = mkN "vatn" neuter ;
		wine_N = mkN "vín" neuter ;
		song_N = mkN "lag" neuter ;
		--number_N = mkN "númer" neuter depends on context, e.g., .. is number 1/.. er númer 1 and .. the number 1/.. talan 1
		animal_N = mkN "dýr" neuter ;
		back_N = mkN "bak" neuter ;
		blood_N = mkN "blóð" neuter ;
		bone_N = mkN "bein" neuter ;
		breast_N = mkN "brjóst" neuter ;
		dust_N = mkN "ryk" neuter ; -- only in singular
		ear_N = mkN "eyra" neuter ;
		egg_N = mkN "egg" neuter ;  -- FIXME - not correctly decline - should go like kyn - kynja i.e. egg - eggja
		eye_N = mkN "auga" neuter ;
		flower_N = mkN "blóm" neuter ;
		grass_N = mkN "gras" neuter ;
		hair_N = mkN "hár" neuter ;
		heart_N = mkN "hjarta" neuter ;
		horn_N = mkN "horn" neuter ;
		-- knee_N = mkN "hné" neuter ; -- FIXME - not currenctly pattern matched - only 2-3 words that go like hné (including hné :) )
		leaf_N = mkN "lauf" neuter ;
		name_N = mkN "nafn" neuter ;
		-- nose_N = mkN "nef" neuter ; -- FIXME - not correctly declined - should go like kyn - kynja, i.e. nef - nefja
		rope_N = mkN "reipi" neuter ;
		salt_N = mkN "salt" neuter ;
		seed_N = mkN "fræ" neuter ;
		stick_N = mkN "prik" neuter ;
		year_N = mkN "ár" neuter ;
		language_N = mkN "tungumál" neuter ;
		gold_N = mkN "gull" neuter ;
		iron_N = mkN "járn" neuter ;
		leather_N = mkN "leður" neuter ;

		-- Old paradigms
		already_Adv = mkAdv "þegar" ;
		arrive_V = mkV "koma" "kem" "kemur" "kemur" "komum" "komið" "koma" "kom" "komst" "kom" "komum" "komuð" "komu" 
			"komi" "komir" "komi" "komum" "komið" "komi" "kæmi" "kæmir" "kæmi" "kæmum" "kæmuð" "kæmu"
			"komdu" "komið" "komandi" "kominn" "kominn" "komnum" "komins" "komin" "komna" "kominni" "kominnar" "komið" "komið" "komnu" "komins"
			"komnir" "komna" "komnum" "kominna" "komnar" "komnar" "komnum" "kominna" "komin" "komin" "komnum" "kominna" "komni" "komna" "komna" 
			"komnu" "komna" "komnu" "komið" ;
		cold_A = mkA "kaldur" "köld" ;
		dirty_A = mkA "skítugur" "skítug" ;
		far_Adv = mkAdv "langt" ;
		fly_V = mkV "fljúga" "flýg" "flýgur" "flýgur" "fljúgum" "fljúgið" "fljúga" "flaug1" "flaugst" "flaug" "flugum" "fluguð" "flugu" 
			"fljúgi" "fljúgir" "fljúgi" "fljúgum" "fljúgið" "fljúgi" "flygi" "flygir" "flygi" "flygjum" "flygjuð" "flygju" 
			"fljúgðu" "fljúgið" "fljúgandi" "floginn" "floginn" "flognum" "flogins" "flogin" "flogna" "floginni" "floginnar" 
			"flogið" "flogið" "flognu" "flogins" "flognir" "flogna" "flognum" "floginna" 
			"flognar" "flognar" "flognum" "floginna" "flogin" "flogin" "flognum" "floginna" 
			"flogni" "flogna" "flogna" "flognu" "flogna" "flognu" "flogið" ;
		hot_A = mkA "heitur" "heit" ;
		important_A = mkA "mikilvægur" "mikilvæg" ;
		john_PN = mkPN "Jón" masculine ;
--		love_V2 = mkV2 "elska" "elska" "elskar" "elskar" "elskum" "elskið" "elska" "elskaði" "elskaðir" "elskaði" "elskuðum" "elskuðuð" "elskuðu" 
--			"elski" "elskir" "elski" "elskum" "elskið" "elski" "elskaði" "elskaðir" "elskaði" "elskuðum" "elskuðuð" "elskuðu"
--			"elskaður" "elskið" "elskandi" "elskaður" "elskað" accusative;

		now_Adv = mkAdv "núna" ;
		today_Adv = mkAdv "í dag" ;
--		walk_V = mkV "ganga" "geng" "gengur" "gengur" "göngum" "gangið" "ganga" "gekk" "gekkst" "gekk" "gengum" "genguð" "gengu" "gengið";
} ;
