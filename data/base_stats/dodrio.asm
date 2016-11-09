	db DODRIO ; 085

	db  60, 110,  70, 110,  60,  60
	;   hp  atk  def  spd  sat  sdf

	db NORMAL, FLYING
	db 45 ; catch rate
	db 158 ; base exp
	db NO_ITEM ; item 1
	db SHARP_BEAK ; item 2
	db 127 ; gender
	db 20 ; step cycles to hatch
	dn 7, 7 ; frontpic dimensions
	db RUN_AWAY ; ability 1
	db EARLY_BIRD ; ability 2
	db TANGLED_FEET ; hidden ability
	db MEDIUM_FAST ; growth rate
	dn AVIAN, AVIAN ; egg groups

	; ev_yield
	ev_yield   0,   2,   0,   0,   0,   0
	;         hp, atk, def, spd, sat, sdf

	; tmhm
	tmhm CURSE, TOXIC, SWORDS_DANCE, HIDDEN_POWER, SUNNY_DAY, HYPER_BEAM, PROTECT, RETURN, MUD_SLAP, DOUBLE_TEAM, SWIFT, AERIAL_ACE, REST, ATTRACT, THIEF, STEEL_WING, ENDURE, GIGA_IMPACT, FLY, BODY_SLAM, DOUBLE_EDGE, SLEEP_TALK, SUBSTITUTE, SWAGGER
	; end
