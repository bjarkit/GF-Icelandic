--# -path=.:prelude

concrete LexiconIce of Lexicon = CatIce **
  open ParadigmsIce, IrregIce, Prelude in {

	lin
		bank_N = mkN "banki" masculine ;
		boss_N = mkN "stjóri" masculine ;
		cousin_N = mkN "frændi" masculine ; -- frænka is the feminine equivalent
		glove_N = mkN "hanski" masculine ;
		lamp_N = mkN "lampi" masculine ;
		pen_N = mkN "penni" masculine ;
		school_N = mkN "skóli" masculine ;
		student_N = mkN "nemandi" masculine ;
		teacher_N = mkN "kennari" masculine ;
		university_N = mkN "háskóli" masculine ;
		window_N = mkN "gluggi" masculine ;
		boat_N = mkN "bátur" masculine ;
		boy_N = mkN "strákur" masculine ;
		cheese_N = mkN "ostur" masculine ;
		dog_N = mkN "hundur" masculine ;
		fish_N = mkN "fiskur" masculine ;
		garden_N = mkN "garður"  masculine ;
		glove_N = mkN "hanski" masculine ;
		hat_N = mkN "hattur" masculine ;
		horse_N = mkN "hestur" masculine ;
		king_N = mkN "konungur" masculine ;
		priest_N = mkN "prestur" masculine ;
		snake_N = mkN "snákur" masculine ;
		sock_N = mkN "sokkur" masculine ;
		belly_N = mkN "magi" masculine ;
		fire_N = mkN "eldur" "eldar" masculine ;
		chair_N = mkN "stóll" masculine ;
		car_N = mkN "bíll" masculine ;
		--stone_N = mkN "steinn" masculine ;
		--rock_N = mkN "klettur" masculine ;
		mouth_N = mkN "munnur" masculine ;
		sand_N = mkN "sandur" masculine ;
		wind_N = mkN "vindur" masculine ;
		worm_N = mkN "ormur" masculine ;
{-
		beer_N = regN "beer" ;
		bird_N = regN "bird" ;
		brother_N2 = mkN2 (mkN masculine (mkN "brother")) (mkPrep "of") ;
		cat_N = regN "cat" ;
		doctor_N = mkN human (regN "doctor") ;
		enemy_N = regN "enemy" ;
		father_N2 = mkN2 (mkN masculine (mkN "father")) (mkPrep "of") ;
		fridge_N = regN "fridge" ;
		friend_N = mkN human (regN "friend") ;
		fruit_N = mkN "fruit" "fruit" ; --- was: fruit, fruits before 7/12/2012
		industry_N = regN "industry" ;
		man_N = mkN masculine (mk2N "man" "men") ;
		paper_N = regN "paper" ;
		peace_N = regN "peace" ;
		policeman_N = mkN masculine (mkN "policeman" "policemen") ;
		restaurant_N = regN "restaurant" ;
		sea_N = regN "sea" ;
		shoe_N = regN "shoe" ;
		wood_N = regN "wood" ;
		bark_N = regN "bark" ;
		day_N = regN "day" ;
		foot_N = mk2N "foot" "feet" ;
		forest_N = regN "forest" ;
		head_N = regN "head" ;
		husband_N = mkN masculine (regN "husband") ;
		ice_N = regN "ice" ;
		leg_N = regN "leg" ;
		neck_N = regN "neck" ;
		road_N = regN "road" ;
		sky_N = regN "sky" ;
		smoke_N = regN "smoke" ;
		snow_N = regN "snow" ;
		wing_N = regN "wing" ;
-}
		cap_N = mkN "húfa" feminine ;
		cat_N = mkN "kisa" feminine ;
		factory_N = mkN "verksmiðja" feminine ;
		fat_N = mkN "fita" feminine ;
		fog_N = mkN "þoka" feminine ;
		tail_N = mkN "rófa" feminine ;
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
		religion_N = mkN "trúarbragð" neuter ; -- plural only
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


		airplane_N = mkN "flugvél" "flugvélar" feminine ;
		apartment_N = mkN "íbúð" "íbúðir" feminine ;
		art_N = mkN "list" "listir" feminine ;
		camera_N = mkN "myndavél" "myndavélar" feminine ;
		church_N = mkN "kirkja" feminine ;
		city_N = mkN "borg" "borgir" feminine ;
		coat_N = mkN "kápa" feminine ;
		computer_N = mkN "tölva" feminine ;
		door_N = mkN "hurð" "hurðir" feminine ;
		girl_N = mkNPlGen "stelpa" feminine ;
		harbour_N = mkN "höfn" "hafnir" feminine ;
		hill_N = mkN "hæð" "hæðir" feminine ;
		love_N = mkN "ást" "ástir" feminine ;
		oil_N = mkN "olía" feminine ;
		planet_N = mkN "pláneta" feminine ;
		queen_N = mkN "drottning" feminine ;
		shirt_N = mkN "skyrta" feminine ;
		shop_N = mkN "búð" "búðir" feminine ;
		sister_N = mkN "systir" "systur" feminine ;
		star_N = mkN "stjarna" feminine ;
		stove_N = mkN "eldavél" "eldavélar" feminine ;
		sun_N = mkN "sól" "sólir" feminine ;
		train_N = mkN "lest" "lestir" feminine ;
		earth_N = mkN "jörð" "jarðir" feminine ;
		feather_N = mkN "fjöður" "fjaðrir" feminine ;
		fog_N = mkN "þoka" feminine ;
		guts_N = mkN "görn" "garnir" feminine ;
		liver_N = mkN "lifur" "lifrar" feminine ;
		person_N = mkN "persóna" feminine ;
		rain_N = mkN "rigning" feminine ;
		skin_N = mkN "húð" "húðir" feminine ;
		tongue_N = mkNPlGen "tunga" feminine ;
		rule_N = mkN "regla" feminine ;
		question_N = mkN "spurning" feminine ;
		reason_N = mkNPlGen "ástæða" feminine ;
		river_N = mkN "á" "ár" feminine ;
		louse_N = mkN "lús" "lýs" feminine ;
		book_N = mkN "bók" "bókar" "bækur" feminine ;
		--night_N = mkN "nótt" "nætur" "nætur" feminine ;
		root_N = mkN "rót" "rótar" "rætur" feminine ;
		factory = mkN "verksmiðja" feminine ;
		fingernail_N = mkN "nögl" "naglar" "neglur" feminine ;
		sheep_N = mkN "kind" "kindar" "kindur" feminine ;
		tooth_N = mkN "tönn" "tannar" "tennur" feminine ;
{-
		cow_N = regN "cow" ;
		distance_N3 = mkN3 (regN "distance") fromP toP ;
		factory_N = regN "factory" ;
		milk_N = regN "milk" ;
		mother_N2 = mkN2 (mkN feminine (mkN "mother")) (mkPrep "of") ;
		music_N = regN "music" ;
		woman_N = mkN feminine (mk2N "woman" "women") ;
		ashes_N = regN "ash" ; -- FIXME: plural only?
		fat_N = regN "fat" ;
		hand_N = regN "hand" ;
		wife_N = mkN feminine (mk2N "wife" "wives") ;
		grammar_N = regN "grammar" ;
-}

		bad_A = mkA "vondur" "vond" "verri" ;
		beautiful_A = mkA "fallegur" ;
		big_A = mkA "stór" "stór" "stærri" ;
		black_A = mkA "svartur" ;
		blue_A = mkA "blár" "blá" ;
		broad_A = mkA "breiður" ;
		brown_A = mkA "brúnn" ;
		clean_A = mkA "hreinn" ;
		clever_A = mkA "klár" "klár" ;
		cold_A = mkA "kaldur" ;
		dirty_A = mkA "skítugur" ;
		--easy_A2V = mkA2V (regA "easy") forP ;
		empty_A = mkA "tómur" ;
		--fun_AV = mkAV (regA "fun") ;
		good_A = mkA "góður" "góð" "betri" ;
		green_A = mkA "grænn" ;
		hot_A = mkA "heitur" ;
		important_A = mkA "mikilvægur" ;
		long_A = mkA "langur" ;
		--married_A2 = mkA2 (regA "married") toP ;
		narrow_A = mkA "þröngur" ;
		new_A = mkA "nýr" "ný" ;
		--old_A = mkA "gamall"
		--probable_AS = mkAS (regA "probable") ;
		red_A = mkA "rauður" ;
		short_A = mkA "stuttur" "stutt" "syttri" ;
		small_A = mkA "lítill" "lítil" "minni" ;
		stupid_A = mkA "heimskur" ;
		thick_A = mkA "þykkur" ;
		thin_A = mkA "þunnur" "þunn" "þynnri" ;
		ugly_A = mkA "ljótur" ;
		warm_A = mkA "heitur" ;
		white_A = mkA "hvítur" ;
		yellow_A = mkA "gulur" ;
		young_A = mkA "ungur" "ung" "yngri" ;
		correct_A = mkA "réttur" ;
		dry_A = mkA "þurr" "þurr" ;
		dull_A = mkA "leðinilegur" ;
		full_A = mkA "fullur" ;
		heavy_A = mkA "þungur" "þung" "þyngri" ;
		near_A = mkA "nálægur" ;
		rotten_A = mkA "rotinn" ;
		round_A = mkA "kringlóttur" ;
		sharp_A = mkA "beittur" ;
		smooth_A = mkA "sléttur" ;
		straight_A = mkA "beinn" ;
		wet_A = mkA "blautur" ;
		wide_A = mkA "víður" ;
		--  other_A = regA "other" ;
		ready_A = mkA "tilbúinn" ;
		uncertain_A = mkA "óviss" "óviss" "óvissari" ;

		-- Old paradigms
		already_Adv = mkAdv "þegar" ;
		arrive_V = mkV "koma" "kem" "kemur" "kemur" "komum" "komið" "koma" "kom" "komst" "kom" "komum" "komuð" "komu" 
			"komi" "komir" "komi" "komum" "komið" "komi" "kæmi" "kæmir" "kæmi" "kæmum" "kæmuð" "kæmu"
			"komdu" "komið" "komandi" "kominn" "kominn" "komnum" "komins" "komin" "komna" "kominni" "kominnar" "komið" "komið" "komnu" "komins"
			"komnir" "komna" "komnum" "kominna" "komnar" "komnar" "komnum" "kominna" "komin" "komin" "komnum" "kominna" "komni" "komna" "komna" 
			"komnu" "komna" "komnu" "komið" ;
		far_Adv = mkAdv "langt" ;
		fly_V = mkV "fljúga" "flýg" "flýgur" "flýgur" "fljúgum" "fljúgið" "fljúga" "flaug1" "flaugst" "flaug" "flugum" "fluguð" "flugu" 
			"fljúgi" "fljúgir" "fljúgi" "fljúgum" "fljúgið" "fljúgi" "flygi" "flygir" "flygi" "flygjum" "flygjuð" "flygju" 
			"fljúgðu" "fljúgið" "fljúgandi" "floginn" "floginn" "flognum" "flogins" "flogin" "flogna" "floginni" "floginnar" 
			"flogið" "flogið" "flognu" "flogins" "flognir" "flogna" "flognum" "floginna" 
			"flognar" "flognar" "flognum" "floginna" "flogin" "flogin" "flognum" "floginna" 
			"flogni" "flogna" "flogna" "flognu" "flogna" "flognu" "flogið" ;
		john_PN = mkPN "Jón" masculine ;

		now_Adv = mkAdv "núna" ;
		today_Adv = mkAdv "í dag" ;

		
		live_V = mkV "lifa" ;
		stop_V = mkV "stansa" "stansa" "stansaði" "stansaður" ;
		jump_V = mkV "hoppa" "hoppa" ;
		breathe_V = mkV "anda" "anda" "andaði" "andaður"; -- some bug somewhere...
		sew_V = mkV "sauma" "sauma" "saumaði" "saumaður" ;
		smell_V = mkV "þefa" "þefa" "þefaði" "þefaður" ;
		spit_V = mkV "hrækja" ;
		swell_V = mkV "bólgna" "bólgna" "bólgnaði" "bólgnaður" ;
		swim_V = mkV "synda" ;
		think_V = mkV "hugsa" "hugsa" "hugsaði" "hugsaður" ;
		vomit_V = mkV "æla" ;
{-
		come_V = (irregV "come" "came" "come") ;
		die_V = (regV "die") ;
		go_V = mk5V "go" "goes" "went" "gone" "going" ;
		run_V = (irregDuplV "run" "ran" "run") ;
		sleep_V = (irregV "sleep" "slept" "slept") ;
		travel_V = (regDuplV "travel") ; -- tricky! so called middle or st-verb
		walk_V = (regV "walk") ;
		blow_V = IrregIce.blow_V ;
		burn_V = IrregIce.burn_V ;
		dig_V = IrregIce.dig_V ;
		fall_V = IrregIce.fall_V ;
		float_V = regV "float" ;
		flow_V = regV "flow" ;
		fly_V = IrregIce.fly_V ;
		freeze_V = IrregIce.freeze_V ;
		give_V3 = mkV3 give_V noPrep noPrep ;
		laugh_V = regV "laugh" ;
		lie_V = IrregIce.lie_V ;
		play_V = regV "play" ;
		sing_V = IrregIce.sing_V ;
		sit_V = IrregIce.sit_V ;
		stand_V = IrregIce.stand_V ;
		turn_V = regV "turn" ;
-} 
} ;
