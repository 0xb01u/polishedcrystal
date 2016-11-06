SetEarlyGameOptions:
	call InitEarlyGameOptionsScreen
	call LoadEarlyGameOptionsScreenPal
	call LoadEarlyGameOptionsScreenGFX
	call WaitBGMap2
	call SetPalettes
	ld hl, EarlyGameOptionsText
	call PrintText
	call EarlyGameOptionsMenu
	ret

EarlyGameOptionsText:
	text_jump _EarlyGameOptionsText
	db "@"

InitEarlyGameOptionsScreen: ; 48e14 (12:4e14)
	ld a, $10
	ld [MusicFade], a
	ld a, MUSIC_NONE
	ld [MusicFadeIDLo], a
	ld a, $0
	ld [MusicFadeIDHi], a
	ld c, 8
	call DelayFrames
	call ClearBGPalettes
	call LoadFontsExtra
	hlcoord 0, 0
	ld bc, SCREEN_HEIGHT * SCREEN_WIDTH
	ld a, $0
	call ByteFill
	hlcoord 0, 0, AttrMap
	ld bc, SCREEN_HEIGHT * SCREEN_WIDTH
	xor a
	call ByteFill
	ret

LoadEarlyGameOptionsScreenPal: ; 48e47 (12:4e47)
	ld hl, .Palette
	ld de, UnknBGPals
	ld bc, 1 palettes
	ld a, $5
	call FarCopyWRAM
	farcall ApplyPals
	ret
; 48e5c (12:4e5c)

.Palette: ; 48e5c
	RGB 31, 31, 31
	RGB 09, 30, 31
	RGB 01, 11, 31
	RGB 00, 00, 00
; 48e64

LoadEarlyGameOptionsScreenGFX: ; 48e64 (12:4e64)
	ld de, .LightBlueTile
	ld hl, VTiles2 tile $00
	lb bc, BANK(.LightBlueTile), 1
	call Get2bpp
	ret
; 48e71 (12:4e71)

.LightBlueTile: ; 48e71
INCBIN "gfx/intro/init_bg.2bpp"

EarlyGameOptionsMenu:
	ld hl, hInMenu
	ld a, [hl]
	push af
	ld [hl], $1
	call ClearBGPalettes
	hlcoord 0, 0
	ld b, 16
	ld c, 18
	call TextBox
	hlcoord 2, 2
	ld de, StringEarlyGameOptions
	call PlaceString
	ld b, SCGB_08
	call GetSGBLayout
	call SetPalettes
	xor a
	ld [wJumptableIndex], a
	ld [hJoyPressed], a
	ld c, $4 ; number of items on the menu minus 1 (for cancel)
.print_text_loop ; this next will display the settings of each option when the menu is opened
	push bc
	xor a
	ld [hJoyLast], a
	call GetEarlyGameOptionPointer
	pop bc
	ld hl, wJumptableIndex
	inc [hl]
	dec c
	jr nz, .print_text_loop
	xor a
	ld [wJumptableIndex], a
	inc a
	ld [hBGMapMode], a
	call WaitBGMap

.joypad_loop
	call JoyTextDelay
	ld a, [hJoyPressed]
	and START | B_BUTTON
	jr nz, .ExitOptions
	call EarlyGameOptionsControl
	jr c, .dpad
	call GetEarlyGameOptionPointer
	jr c, .ExitOptions

.dpad
	call EarlyGameOptions_UpdateCursorPosition
	ld c, 3
	call DelayFrames
	jr .joypad_loop

.ExitOptions:
	ld de, SFX_TRANSACTION
	call PlaySFX
	call WaitSFX
	pop af
	ld [hInMenu], a
	ret
; e4241

StringEarlyGameOptions:
	db "Natures<LNBRK>"
	db "            :<LNBRK>"
	db "<LNBRK>"
	db "Abilities<LNBRK>"
	db "            :<LNBRK>"
	db "<LNBRK>"
	db "Traded #mon<LNBRK>"
	db "grow faster and<LNBRK>"
	db "can disobey<LNBRK>"
	db "            :<LNBRK>"
	db "<LNBRK>"
	db "Nuzlocke mode<LNBRK>"
	db "            :<LNBRK>"
	db "<LNBRK>"
	db "Done@"

GetEarlyGameOptionPointer: ; e42d6
	ld a, [wJumptableIndex] ; load the cursor position to a
	ld e, a ; copy it to de
	ld d, 0
	ld hl, .Pointers
	add hl, de
	add hl, de
	ld a, [hli]
	ld h, [hl]
	ld l, a
	jp [hl] ; jump to the code of the current highlighted item
; e42e5

.Pointers:
	dw EarlyGameOptions_Natures
	dw EarlyGameOptions_Abilities
	dw EarlyGameOptions_TradedMon
	dw EarlyGameOptions_NuzlockeMode
	dw EarlyGameOptions_Done

EarlyGameOptions_Natures:
	ld hl, EarlyGameOptions
	ld a, [hJoyPressed]
	and D_LEFT | D_RIGHT
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
	hlcoord 15, 3
	call PlaceString
	and a
	ret

EarlyGameOptions_Abilities:
	ld hl, EarlyGameOptions
	ld a, [hJoyPressed]
	and D_LEFT | D_RIGHT
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
	hlcoord 15, 6
	call PlaceString
	and a
	ret

EarlyGameOptions_TradedMon:
	ld hl, EarlyGameOptions
	ld a, [hJoyPressed]
	and D_LEFT | D_RIGHT
	jr nz, .Toggle
	bit TRADED_BEHAVIOR, [hl]
	jr z, .SetNo
	jr .SetYes
.Toggle
	bit TRADED_BEHAVIOR, [hl]
	jr z, .SetYes
.SetNo:
	res TRADED_BEHAVIOR, [hl]
	ld de, NoString
	jr .Display
.SetYes:
	set TRADED_BEHAVIOR, [hl]
	ld de, YesString
.Display:
	hlcoord 15, 11
	call PlaceString
	and a
	ret

EarlyGameOptions_NuzlockeMode:
	ld hl, EarlyGameOptions
	ld a, [hJoyPressed]
	and D_LEFT | D_RIGHT
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
	hlcoord 15, 14
	call PlaceString
	and a
	ret

EarlyGameOptions_Done:
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

EarlyGameOptionsControl: ; e452a
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
	cp $4 ; maximum number of items in option menu
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
	ld [hl], $5 ; number of option items + 1
.Decrease:
	dec [hl]
	scf
	ret
; e455c

EarlyGameOptions_UpdateCursorPosition: ; e455c
	hlcoord 1, 1
	ld de, SCREEN_WIDTH
	ld c, $10
.loop
	ld [hl], " "
	add hl, de
	dec c
	jr nz, .loop
	ld hl, .EarlyGameOptions_CursorPositions
	ld a, [wJumptableIndex]
	ld c, a
	ld b, 0
	add hl, bc
	ld a, [hl]
	; hlcoord 1, a
	ld hl, TileMap
	ld bc, SCREEN_WIDTH
	call AddNTimes
	inc hl
	ld [hl], "▶"
	ret
; e4579

.EarlyGameOptions_CursorPositions:
	db 2, 5, 8, 13, 16
