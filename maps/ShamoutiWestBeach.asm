const_value set 2

ShamoutiWestBeach_MapScriptHeader:
.MapTriggers:
	db 0

.MapCallbacks:
	db 0

ShamoutiWestBeach_MapEventHeader:
	; filler
	db 0, 0

.Warps:
	db 2
	warp_def $d, $f, 1, SHAMOUTI_WEST_BEACH_SHACK
	warp_def $5, $8, 2, SHAMOUTI_TUNNEL

.XYTriggers:
	db 0

.Signposts:
	db 0

.PersonEvents:
	db 0
