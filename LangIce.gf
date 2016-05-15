--# -path=.:../abstract:../common:../api

concrete LangIce of Lang = 
  GrammarIce,
  Test -- LexiconIce
  ,ConstructionIce
  ** {} ;
