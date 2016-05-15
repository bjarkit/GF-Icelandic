concrete VerbIce of Verb = CatIce ** open ResIce, Prelude in {

--  flags optimize=all_subs ;

-- lin
		-- ComplV2 is split into ComplSlash and SlashV2V
		--Build a verb phrase from a two-place verb and a noun phrase (= the object):
		--V2 -> NP -> VP
--		ComplV2 v2 np = {
--			s = v2;
--			obj = \\_ => np.s ! v2.c
--		};
--
--		-- CompAP is split into UseComp and CompAP
--		--Build a verb phrase from an adjective phrase, using the verb 'að vera' ("stór" --> "er stór"):
--		--AP -> VP
--		CompAP ap = { 
--			s = mkVerb "vera" "er" "ert" "er" "erum" "eruð" "eru" "var" "varst" "var" "vorum" "voruð" "voru" "verið";
--			-- As far as I know, the verb "að vera" ("to be") always inflects the object into the Nominative case.
--			-- And the Srong being the 'unmarked' declension is dominant here.
--			obj = \\a => case a of {
--				Ag g n _ => ap.s ! n ! g ! Nom ! Strong
--			}
--		};

}
