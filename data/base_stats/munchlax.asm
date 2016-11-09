	db MUNCHLAX ; 446

	db 135,  85,  45,   5,  40,  85
	;   hp  atk  def  spd  sat  sdf

	db NORMAL, NORMAL
	db 50 ; catch rate
	db 94 ; base exp
	db NO_ITEM ; item 1
	db LEFTOVERS ; item 2
	db 31 ; gender
	db 40 ; step cycles to hatch
	dn 6, 6 ; frontpic dimensions
	db PICKUP ; ability 1
	db THICK_FAT ; ability 2
	db PICKUP ; hidden ability
	db SLOW ; growth rate
	dn NO_EGGS, NO_EGGS ; egg groups

	; ev_yield
	ev_yield   1,   0,   0,   0,   0,   0
	;         hp, atk, def, spd, sat, sdf

	; tmhm
	tmhm CALM_MIND, TOXIC, HIDDEN_POWER, SUNNY_DAY, ICE_BEAM, BLIZZARD, PROTECT, RAIN_DANCE, SOLAR_BEAM, THUNDERBOLT, THUNDER, EARTHQUAKE, RETURN, PSYCHIC, SHADOW_BALL, MUD_SLAP, DOUBLE_TEAM, FLAMETHROWER, SANDSTORM, FIRE_BLAST, REST, ATTRACT, ROCK_SLIDE, ROCK_SMASH, ENDURE, SURF, STRENGTH, WHIRLPOOL, BODY_SLAM, DEFENSE_CURL, FIRE_PUNCH, HEADBUTT, HYPER_VOICE, ICE_PUNCH, ICY_WIND, ROLLOUT, SEED_BOMB, SLEEP_TALK, SUBSTITUTE, SWAGGER, THUNDERPUNCH, WATER_PULSE, ZEN_HEADBUTT
	; end
