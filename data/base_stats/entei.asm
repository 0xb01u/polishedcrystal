	db ENTEI ; 244

	db 115, 115,  85, 100,  90,  75
	;   hp  atk  def  spd  sat  sdf

	db FIRE, FIRE
	db 3 ; catch rate
	db 217 ; base exp
	db NO_ITEM ; item 1
	db NO_ITEM ; item 2
	db 255 ; gender
	db 80 ; step cycles to hatch
	dn 7, 7 ; frontpic dimensions
	db PRESSURE ; ability 1
if DEF(FAITHFUL)
	db PRESSURE ; ability 2
	db INNER_FOCUS ; hidden ability
else
	db INTIMIDATE ; ability 2
	db FLASH_FIRE ; hidden ability
endc
	db SLOW ; growth rate
	dn NO_EGGS, NO_EGGS ; egg groups

	; ev_yield
	ev_yield   1,   2,   0,   0,   0,   0
	;         hp, atk, def, spd, sat, sdf

	; tmhm
	tmhm CURSE, CALM_MIND, ROAR, TOXIC, HIDDEN_POWER, SUNNY_DAY, HYPER_BEAM, PROTECT, RAIN_DANCE, SOLAR_BEAM, IRON_TAIL, EARTHQUAKE, RETURN, DIG, SHADOW_BALL, MUD_SLAP, DOUBLE_TEAM, REFLECT, FLAMETHROWER, SANDSTORM, FIRE_BLAST, SWIFT, STONE_EDGE, REST, ROCK_SMASH, ENDURE, WILL_O_WISP, GIGA_IMPACT, FLASH, CUT, STRENGTH, BODY_SLAM, DOUBLE_EDGE, HEADBUTT, IRON_HEAD, SLEEP_TALK, SUBSTITUTE, SWAGGER
	; end
