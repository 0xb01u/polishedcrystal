const_value set 0

ShamoutiWestBeachShack_MapScriptHeader:
.MapTriggers:
	db 0

.MapCallbacks:
	db 0

ShamoutiWestBeachShack_MapEventHeader:
	; filler
	db 0, 0

.Warps:
	db 2
	warp_def $7, $2, 1, SHAMOUTI_WEST_BEACH
	warp_def $7, $3, 1, SHAMOUTI_WEST_BEACH

.XYTriggers:
	db 0

.Signposts:
	db 0

.PersonEvents:
	db 0
