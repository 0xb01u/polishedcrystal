NUM_INITIAL_OPTIONS EQU 8

SetInitialOptions:
	ld a, $10
	ld [wMusicFade], a

	xor a ; MUSIC_NONE
	ld [wMusicFadeIDLo], a
	ld [wMusicFadeIDHi], a
	ld c, 8
	call DelayFrames

	call ClearBGPalettes
	call LoadFontsExtra

	hlcoord 0, 0
	ld bc, SCREEN_HEIGHT * SCREEN_WIDTH
	xor a
	call ByteFill

	hlcoord 0, 0, wAttrMap
	ld bc, SCREEN_HEIGHT * SCREEN_WIDTH
	xor a
	call ByteFill

	ld hl, .BGPalette
	ld de, wUnknBGPals
	ld bc, 1 palettes
	ld a, $5
	call FarCopyWRAM

	ld de, .BGTiles
	ld hl, vTiles2 tile $00
	lb bc, BANK(.BGTiles), 3
	call Get2bpp

	farcall ApplyPals

	call ApplyAttrAndTilemapInVBlank
	call SetPalettes

	ld hl, .InitialOptionsText
	call PrintText

	ld hl, hInMenu
	ld a, [hl]
	push af
	ld [hl], $1

;	call ClearBGPalettes

	hlcoord 0, 0
	ld a, " "
	ld bc, SCREEN_WIDTH * SCREEN_HEIGHT
	call ByteFill

	hlcoord 0, 0
	ld a, $01 ; left
	ld bc, SCREEN_WIDTH - 2
	ld d, SCREEN_HEIGHT
.edge_loop
	ld [hli], a
	inc a ; right
	add hl, bc
	ld [hli], a
	dec a ; left
	dec d
	jr nz, .edge_loop

	hlcoord 2, 0
	ld de, .InitialOptionsString
	call PlaceString

;	ld a, CGB_DIPLOMA
;	call GetCGBLayout
;	call SetPalettes

	xor a
	ld [wJumptableIndex], a
	ld [hJoyPressed], a
	ld c, NUM_INITIAL_OPTIONS
.print_text_loop ; this next will display the settings of each option when the menu is opened
	push bc
	xor a
	ld [hJoyLast], a
	call GetInitialOptionPointer
	pop bc
	ld hl, wJumptableIndex
	inc [hl]
	dec c
	jr nz, .print_text_loop

	xor a
	ld [wJumptableIndex], a
	inc a
	ld [hBGMapMode], a
	call ApplyTilemapInVBlank

.joypad_loop
	call JoyTextDelay
	ld a, [hJoyPressed]
	and START | B_BUTTON
	jr nz, .ExitOptions
	call InitialOptionsControl
	jr c, .dpad
	call GetInitialOptionPointer
	jr c, .ExitOptions

.dpad
	call InitialOptions_UpdateCursorPosition
	ld c, 3
	call DelayFrames
	jr .joypad_loop

.ExitOptions:
	ld hl, wInitialOptions2
	res RESET_INIT_OPTS, [hl]
	ld de, SFX_TRANSACTION
	call PlaySFX
	call WaitSFX
	pop af
	ld [hInMenu], a
	ret

.InitialOptionsText:
	text_jump _InitialOptionsText
	db "@"

.BGPalette:
if !DEF(MONOCHROME)
	RGB 31, 31, 31
	RGB 09, 30, 31
	RGB 01, 11, 31
	RGB 00, 00, 00
else
	MONOCHROME_RGB_FOUR
endc

.BGTiles:
INCBIN "gfx/new_game/init_bg.2bpp"

.InitialOptionsString:
	db "Natures<LNBRK>"
	db "            :<LNBRK>"
	db "Abilities<LNBRK>"
	db "            :<LNBRK>"
	db "Phys/Spcl split<LNBRK>"
	db "            :<LNBRK>"
	db "Exp. scaling<LNBRK>"
	db "            :<LNBRK>"
	db "IVs vary colors<LNBRK>"
	db "            :<LNBRK>"
	db "Perfect stats<LNBRK>"
	db "            :<LNBRK>"
	db "Traded #mon<LNBRK>"
	db "treat you as OT<LNBRK>"
	db "            :<LNBRK>"
	db "Nuzlocke mode<LNBRK>"
	db "            :<LNBRK>"
	db "Done@"

GetInitialOptionPointer:
	ld a, [wJumptableIndex] ; load the cursor position to a
	ld e, a ; copy it to de
	ld d, 0
	ld hl, .Pointers
	add hl, de
	add hl, de
	ld a, [hli]
	ld h, [hl]
	ld l, a
	jp hl ; jump to the code of the current highlighted item

.Pointers:
	dw InitialOptions_Natures
	dw InitialOptions_Abilities
	dw InitialOptions_PSS
	dw InitialOptions_ExpScaling
	dw InitialOptions_ColorVariation
	dw InitialOptions_PerfectIVs
	dw InitialOptions_TradedMon
	dw InitialOptions_NuzlockeMode
	dw InitialOptions_Done

InitialOptions_Natures:
	ld hl, wInitialOptions
	ld a, [hJoyPressed]
	and D_LEFT | D_RIGHT | A_BUTTON
	jr nz, .Toggle
	bit NATURES_OPT, [hl]
	jr z, .SetNo
	jr .SetYes
.Toggle
	bit NATURES_OPT, [hl]
	jr z, .SetYes
.SetNo:
	res NATURES_OPT, [hl]
	ld de, NoString
	jr .Display
.SetYes:
	set NATURES_OPT, [hl]
	ld de, YesString
.Display:
	hlcoord 15, 1
	call PlaceString
	and a
	ret

InitialOptions_Abilities:
	ld hl, wInitialOptions
	ld a, [hJoyPressed]
	and D_LEFT | D_RIGHT | A_BUTTON
	jr nz, .Toggle
	bit ABILITIES_OPT, [hl]
	jr z, .SetNo
	jr .SetYes
.Toggle
	bit ABILITIES_OPT, [hl]
	jr z, .SetYes
.SetNo:
	res ABILITIES_OPT, [hl]
	ld de, NoString
	jr .Display
.SetYes:
	set ABILITIES_OPT, [hl]
	ld de, YesString
.Display:
	hlcoord 15, 3
	call PlaceString
	and a
	ret

InitialOptions_PSS:
	ld hl, wInitialOptions
	ld a, [hJoyPressed]
	and D_LEFT | D_RIGHT | A_BUTTON
	jr nz, .Toggle
	bit PSS_OPT, [hl]
	jr z, .SetNo
	jr .SetYes
.Toggle
	bit PSS_OPT, [hl]
	jr z, .SetYes
.SetNo:
	res PSS_OPT, [hl]
	ld de, NoString
	jr .Display
.SetYes:
	set PSS_OPT, [hl]
	ld de, YesString
.Display:
	hlcoord 15, 5
	call PlaceString
	and a
	ret

InitialOptions_ExpScaling:
	ld hl, wInitialOptions
	ld a, [hJoyPressed]
	and D_LEFT | D_RIGHT | A_BUTTON
	jr nz, .Toggle
	bit SCALED_EXP, [hl]
	jr z, .SetNo
	jr .SetYes
.Toggle
	bit SCALED_EXP, [hl]
	jr z, .SetYes
.SetNo:
	res SCALED_EXP, [hl]
	ld de, NoString
	jr .Display
.SetYes:
	set SCALED_EXP, [hl]
	ld de, YesString
.Display:
	hlcoord 15, 7
	call PlaceString
	and a
	ret

InitialOptions_ColorVariation:
	ld hl, wInitialOptions
	ld a, [hJoyPressed]
	and D_LEFT | D_RIGHT | A_BUTTON
	jr nz, .Toggle
	bit COLOR_VARY_OPT, [hl]
	jr z, .SetNo
	jr .SetYes
.Toggle
	bit COLOR_VARY_OPT, [hl]
	jr z, .SetYes
.SetNo:
	res COLOR_VARY_OPT, [hl]
	ld de, NoString
	jr .Display
.SetYes:
	set COLOR_VARY_OPT, [hl]
	ld de, YesString
.Display:
	hlcoord 15, 9
	call PlaceString
	and a
	ret

InitialOptions_PerfectIVs:
	ld hl, wInitialOptions
	ld a, [hJoyPressed]
	and D_LEFT | D_RIGHT | A_BUTTON
	jr nz, .Toggle
	bit PERFECT_IVS_OPT, [hl]
	jr z, .SetNo
	jr .SetYes
.Toggle
	bit PERFECT_IVS_OPT, [hl]
	jr z, .SetYes
.SetNo:
	res PERFECT_IVS_OPT, [hl]
	ld de, NoString
	jr .Display
.SetYes:
	set PERFECT_IVS_OPT, [hl]
	ld de, YesString
.Display:
	hlcoord 15, 11
	call PlaceString
	and a
	ret

InitialOptions_TradedMon:
	ld hl, wInitialOptions
	ld a, [hJoyPressed]
	and D_LEFT | D_RIGHT | A_BUTTON
	jr nz, .Toggle
	bit TRADED_AS_OT_OPT, [hl]
	jr z, .SetNo
	jr .SetYes
.Toggle
	bit TRADED_AS_OT_OPT, [hl]
	jr z, .SetYes
.SetNo:
	res TRADED_AS_OT_OPT, [hl]
	ld de, NoString
	jr .Display
.SetYes:
	set TRADED_AS_OT_OPT, [hl]
	ld de, YesString
.Display:
	hlcoord 15, 14
	call PlaceString
	and a
	ret

InitialOptions_NuzlockeMode:
	ld hl, wInitialOptions
	ld a, [hJoyPressed]
	and D_LEFT | D_RIGHT | A_BUTTON
	jr nz, .Toggle
	bit NUZLOCKE_MODE, [hl]
	jr z, .SetNo
	jr .SetYes
.Toggle
	bit NUZLOCKE_MODE, [hl]
	jr z, .SetYes
.SetNo:
	res NUZLOCKE_MODE, [hl]
	ld de, NoString
	jr .Display
.SetYes:
	set NUZLOCKE_MODE, [hl]
	ld de, YesString
.Display:
	hlcoord 15, 16
	call PlaceString
	and a
	ret

InitialOptions_Done:
	ld hl, wInitialOptions2
	res RESET_INIT_OPTS, [hl]
	ld a, [hJoyPressed]
	and A_BUTTON
	jr nz, .Exit
	and a
	ret

.Exit:
	scf
	ret

NoString:
	db "No @"
YesString:
	db "Yes@"

InitialOptionsControl:
	ld hl, wJumptableIndex
	ld a, [hJoyLast]
	cp D_DOWN
	jr z, .DownPressed
	cp D_UP
	jr z, .UpPressed
	and a
	ret

.DownPressed:
	ld a, [hl] ; load the cursor position to a
	cp NUM_INITIAL_OPTIONS
	jr nz, .Increase
	ld [hl], -1
.Increase:
	inc [hl]
	scf
	ret

.UpPressed:
	ld a, [hl]
	and a
	jr nz, .Decrease
	ld [hl], NUM_INITIAL_OPTIONS + 1
.Decrease:
	dec [hl]
	scf
	ret

InitialOptions_UpdateCursorPosition:
	hlcoord 1, 0
	ld de, SCREEN_WIDTH
	ld c, SCREEN_HEIGHT
.loop
	ld [hl], " "
	add hl, de
	dec c
	jr nz, .loop
	ld hl, .InitialOptions_CursorPositions
	ld a, [wJumptableIndex]
	ld c, a
	ld b, 0
	add hl, bc
	ld a, [hl]
	; hlcoord 1, a
	ld hl, wTileMap
	ld bc, SCREEN_WIDTH
	rst AddNTimes
	inc hl
	ld [hl], "▶"
	ret

.InitialOptions_CursorPositions:
	db 0, 2, 4, 6, 8, 10, 12, 15, 17
