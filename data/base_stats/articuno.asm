	db ARTICUNO ; 144

	db  90,  85, 100,  85,  95, 125
	;   hp  atk  def  spd  sat  sdf

	db ICE, FLYING
	db 3 ; catch rate
	db 215 ; base exp
	db LUM_BERRY ; item 1
	db LUM_BERRY ; item 2
	db 255 ; gender
	db 80 ; step cycles to hatch
	dn 7, 7 ; frontpic dimensions
	db PRESSURE ; ability 1
if DEF(FAITHFUL)
	db PRESSURE ; ability 2
else
	db SNOW_WARNING ; ability 2
endc
	db SNOW_CLOAK ; hidden ability
	db SLOW ; growth rate
	dn NO_EGGS, NO_EGGS ; egg groups

	; ev_yield
	ev_yield   0,   0,   0,   0,   0,   3
	;         hp, atk, def, spd, sat, sdf

	; tmhm
	tmhm CURSE, ROAR, TOXIC, HAIL, HIDDEN_POWER, SUNNY_DAY, ICE_BEAM, BLIZZARD, HYPER_BEAM, PROTECT, RAIN_DANCE, RETURN, MUD_SLAP, DOUBLE_TEAM, REFLECT, SANDSTORM, SWIFT, AERIAL_ACE, AVALANCHE, REST, STEEL_WING, SUBSTITUTE, ENDURE, FLY, ROCK_SMASH, DOUBLE_EDGE, ICY_WIND, SLEEP_TALK, SWAGGER, WATER_PULSE
	; end
