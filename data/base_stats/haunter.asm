	db HAUNTER ; 093

	db  45,  50,  45,  95, 115,  55
	;   hp  atk  def  spd  sat  sdf

	db GHOST, POISON
	db 90 ; catch rate
	db 126 ; base exp
	db NO_ITEM ; item 1
	db SPELL_TAG ; item 2
	db 127 ; gender
	db 20 ; step cycles to hatch
	dn 6, 6 ; frontpic dimensions
	db LEVITATE ; ability 1
	db LEVITATE ; ability 2
	db LEVITATE ; hidden ability
	db MEDIUM_SLOW ; growth rate
	dn AMORPHOUS, AMORPHOUS ; egg groups

	; ev_yield
	ev_yield   0,   0,   0,   0,   2,   0
	;         hp, atk, def, spd, sat, sdf

	; tmhm
	tmhm CURSE, TOXIC, HIDDEN_POWER, SUNNY_DAY, PROTECT, RAIN_DANCE, GIGA_DRAIN, THUNDERBOLT, THUNDER, RETURN, PSYCHIC, SHADOW_BALL, DOUBLE_TEAM, SLUDGE_BOMB, REST, ATTRACT, THIEF, SUBSTITUTE, ENERGY_BALL, DARK_PULSE, ENDURE, DAZZLINGLEAM, WILL_O_WISP, POISON_JAB, EXPLOSION, SHADOW_CLAW, DREAM_EATER, FIRE_PUNCH, ICE_PUNCH, ICY_WIND, SLEEP_TALK, SWAGGER, THUNDERPUNCH, ZAP_CANNON
	; end
