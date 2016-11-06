	db GLACEON ; 253

	db  65,  60, 110,  65, 130,  95
	;   hp  atk  def  spd  sat  sdf

	db ICE, ICE
	db 45 ; catch rate
	db 196 ; base exp
	db NO_ITEM ; item 1
	db NO_ITEM ; item 2
	db 31 ; gender
	db 35 ; step cycles to hatch
	dn 7, 7 ; frontpic dimensions
	db SNOW_CLOAK ; ability 1
if DEF(FAITHFUL)
	db SNOW_CLOAK ; ability 2
else
	db SNOW_WARNING ; ability 2
endc
	db ICE_BODY ; hidden ability
	db MEDIUM_FAST ; growth rate
	dn FIELD, FIELD ; egg groups

	; ev_yield
	ev_yield   0,   0,   0,   0,   2,   0
	;         hp, atk, def, spd, sat, sdf

	; tmhm
	tmhm ROAR, TOXIC, HAIL, HIDDEN_POWER, SUNNY_DAY, ICE_BEAM, BLIZZARD, HYPER_BEAM, PROTECT, RAIN_DANCE, IRON_TAIL, RETURN, DIG, SHADOW_BALL, MUD_SLAP, DOUBLE_TEAM, SWIFT, AVALANCHE, REST, ATTRACT, SUBSTITUTE, ENDURE, STRENGTH, ROCK_SMASH, AQUA_TAIL, DOUBLE_EDGE, HEADBUTT, HYPER_VOICE, ICY_WIND, SLEEP_TALK, SWAGGER, WATER_PULSE
	; end
