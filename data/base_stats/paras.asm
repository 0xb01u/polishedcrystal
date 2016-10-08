	db PARAS ; 046

	db  35,  70,  55,  25,  45,  55
	;   hp  atk  def  spd  sat  sdf

	db BUG, GRASS
	db 190 ; catch rate
	db 70 ; base exp
	db TINYMUSHROOM ; item 1
	db BIG_MUSHROOM ; item 2
	db 127 ; gender
	db 20 ; step cycles to hatch
	dn 5, 5 ; frontpic dimensions
	db EFFECT_SPORE ; ability 1
	db DRY_SKIN ; ability 2
	db DAMP ; hidden ability
	db MEDIUM_FAST ; growth rate
	dn INSECT, PLANT ; egg groups

	; ev_yield
	ev_yield   0,   1,   0,   0,   0,   0
	;         hp, atk, def, spd, sat, sdf

	; tmhm
	tmhm CURSE, TOXIC, SWORDS_DANCE, HIDDEN_POWER, SUNNY_DAY, HONE_CLAWS, LIGHT_SCREEN, PROTECT, GIGA_DRAIN, SOLAR_BEAM, RETURN, DIG, DOUBLE_TEAM, SLUDGE_BOMB, AERIAL_ACE, REST, ATTRACT, THIEF, FURY_CUTTER, SUBSTITUTE, BODY_SLAM, ENERGY_BALL, FALSE_SWIPE, X_SCISSOR, ENDURE, CUT, FLASH, ROCK_SMASH, COUNTER, DOUBLE_EDGE, SEED_BOMB, SLEEP_TALK, SWAGGER
	; end
