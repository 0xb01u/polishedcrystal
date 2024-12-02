INCLUDE "data/mon_menu.asm"

MonSubmenu:
	xor a
	ldh [hBGMapMode], a
	call GetMonSubmenuItems
	farcall FreezeMonIcons
	ld hl, .MenuDataHeader
	call LoadMenuHeader
	call .GetTopCoord
	call PopulateMonMenu

	ld a, 1
	ldh [hBGMapMode], a
	call MonMenuLoop
	ld [wMenuSelection], a

	jmp ExitMenu

.MenuDataHeader:
	db $40 ; tile backup
	db 00, 06 ; start coords
	db 17, 19 ; end coords
	dw 0
	db 1 ; default option

.GetTopCoord:
; TopCoord = 1 + BottomCoord - 2 * (NumSubmenuItems + 1)
	ld a, [wBuffer1]
	inc a
	add a
	ld b, a
	ld a, [wMenuBorderBottomCoord]
	sub b
	inc a
	ld [wMenuBorderTopCoord], a
	jmp MenuBox

MonMenuLoop:
.loop
	ld a, $a0 ; flags
	ld [wMenuDataFlags], a
	ld a, [wBuffer1] ; items
	ld [wMenuDataItems], a
	call InitVerticalMenuCursor
	ld hl, w2DMenuFlags1
	set 6, [hl]
	call DoMenuJoypadLoop
	ld de, SFX_READ_TEXT_2
	call PlaySFX
	ldh a, [hJoyPressed]
	bit 0, a ; A
	jr nz, .select
	bit 1, a ; B
	jr z, .loop
	ld a, MONMENUITEM_CANCEL
	ret

.select
	ld a, [wMenuCursorY]
	dec a
	ld c, a
	ld b, 0
	ld hl, wBuffer2
	add hl, bc
	ld a, [hl]
	ret

PopulateMonMenu:
	call MenuBoxCoord2Tile
	ld bc, $2a ; 42
	add hl, bc
	ld de, wBuffer2
.loop
	ld a, [de]
	inc de
	cp -1
	ret z
	push de
	push hl
	call GetMonMenuString
	pop hl
	rst PlaceString
	ld bc, $28 ; 40
	add hl, bc
	pop de
	jr .loop

GetMonMenuString:
	ld hl, MonMenuOptions + 1
	ld de, 3
	call IsInArray
	dec hl
	ld a, [hli]
	cp MONMENU_MENUOPTION
	jr z, .NotMove
	inc hl
	ld a, [hl]
	ld [wNamedObjectIndex], a
	jmp GetMoveName

.NotMove:
	inc hl
	ld a, [hl]
	dec a
	ld hl, MonMenuOptionStrings
	call GetNthString
	ld d, h
	ld e, l
	ret

GetMonSubmenuItems:
	call ResetMonSubmenu
	ld a, MON_IS_EGG
	call GetPartyParamLocation
	bit MON_IS_EGG_F, [hl]
	jr nz, .egg
	ld a, [wLinkMode]
	and a
	jr nz, .skip_moves
	ld a, MON_MOVES
	call GetPartyParamLocation
	ld d, h
	ld e, l
	ld b, 0 ; b stores the amount of moves displayed.
	ld c, NUM_MOVES
.loop
	push bc
	push de
	ld a, [de]
	and a
	jr z, .next
	push hl
	call IsFieldMove
	pop hl
	jr nc, .next
	pop de
	pop bc
	inc b ; increase move displayed count.
	push bc
	push de
	call AddMonMenuItem
.next
	pop de
	inc de
	pop bc
	dec c
	jr nz, .loop

	call AddLearnableTMHMItems

.skip_moves
	ld a, MONMENUITEM_STATS
	call AddMonMenuItem
	ld a, MONMENUITEM_SWITCH
	call AddMonMenuItem
	ld a, MONMENUITEM_MOVE
	call AddMonMenuItem
	ld a, [wLinkMode]
	and a
	jr nz, .skip2
	push hl
	ld a, MON_ITEM
	call GetPartyParamLocation
	ld d, [hl]
	call ItemIsMail ; set carry if mail
	pop hl
	; a = carry ? MONMENUITEM_MAIL : MONMENUITEM_ITEM
	sbc a
	and MONMENUITEM_MAIL - MONMENUITEM_ITEM
	add MONMENUITEM_ITEM
	call AddMonMenuItem

.skip2
	ld a, [wBuffer1]
	cp NUM_MONMENU_ITEMS
	jr z, TerminateMonSubmenu
	ld a, MONMENUITEM_CANCEL
	call AddMonMenuItem
	jr TerminateMonSubmenu

.egg
	ld a, MONMENUITEM_STATS
	call AddMonMenuItem
	ld a, MONMENUITEM_SWITCH
	call AddMonMenuItem
	ld a, MONMENUITEM_CANCEL
	call AddMonMenuItem
	jr TerminateMonSubmenu

IsFieldMove:
	ld b, a
	ld hl, MonMenuOptions
.next
	ld a, [hli]
	cp -1
	ret z
	cp MONMENU_MENUOPTION
	ret z
	ld d, [hl]
	inc hl
	ld a, [hli]
	cp b
	jr nz, .next
	ld a, d
	scf
	ret

ResetMonSubmenu:
	xor a
	ld [wBuffer1], a
	ld hl, wBuffer2
	ld bc, NUM_MONMENU_ITEMS + 1
	rst ByteFill
	ret

TerminateMonSubmenu:
	ld a, [wBuffer1]
	ld e, a
	ld d, $0
	ld hl, wBuffer2
	add hl, de
	ld [hl], -1
	ret

AddMonMenuItem:
	push hl
	push de
	push af
	ld a, [wBuffer1]
	ld e, a
	inc a
	ld [wBuffer1], a
	ld d, $0
	ld hl, wBuffer2
	add hl, de
	pop af
	ld [hl], a
	pop de
	pop hl
	ret

AddLearnableTMHMItems:
	; The menu has a hard limit on the number of options it can display.
	; (The game will crash if it is exceeded.)
	; As a compromise, only show in the menu the field moves that are not
	; triggered by an overworld event.
	; Also, do not add options past the 4 moves limit.
	ld a, b
	cp 4
	jr nz, .cut
	ret
	; CUT:
.cut
	; FLY:
.fly
	; Check the mon can learn fly:
	ld a, FLY
	ld [wPutativeTMHMMove], a
	predef CanLearnTMHMMove
	ld a, c
	and a
	jr z, .surf
	; Check the mon does not know fly:
	ld a, MON_MOVES
	call GetPartyParamLocation
	ld d, h
	ld e, l
	ld c, NUM_MOVES
.loop_fly
	ld a, [de]
	and a
	jr z, .check_item_fly
	cp FLY
	jr z, .surf
	inc de
	dec c
	jr nz, .loop_fly
.check_item_fly
	; Check has the HM in the bag:
	push bc
	ld a, FLY
	call CheckTMHM
	pop bc
	jr nc, .surf
	; Add option:
	ld a, MONMENUITEM_FLY
	call AddMonMenuItem
	inc b
	ld a, b
	cp 4
	jr z, .end
	; SURF:
.surf
	; STRENGTH:
.strength
	; FLASH:
.flash
	; Check the mon can learn flash:
	ld a, FLASH
	ld [wPutativeTMHMMove], a
	predef CanLearnTMHMMove
	ld a, c
	and a
	jr z, .waterfall
	; Check the mon does not know flash:
	ld a, MON_MOVES
	call GetPartyParamLocation
	ld d, h
	ld e, l
	ld c, NUM_MOVES
.loop_flash
	ld a, [de]
	and a
	jr z, .check_item_flash
	cp FLASH
	jr z, .waterfall
	inc de
	dec c
	jr nz, .loop_flash
.check_item_flash
	; Check has the HM in the bag:
	push bc
	ld a, FLASH
	call CheckTMHM
	pop bc
	jr nc, .dig
	; Add option:
	ld a, MONMENUITEM_FLASH
	call AddMonMenuItem
	ld a, b
	cp 4
	jr z, .end
	; WATERFALL:
.waterfall
	; WHIRLPOOL:
.whirlpool
	; DIG:
.dig
	; Check the mon can learn dig:
	ld a, DIG
	ld [wPutativeTMHMMove], a
	predef CanLearnTMHMMove
	ld a, c
	and a
	jr z, .teleport
	; Check the mon does not know dig:
	ld a, MON_MOVES
	call GetPartyParamLocation
	ld d, h
	ld e, l
	ld c, NUM_MOVES
.loop_dig
	ld a, [de]
	and a
	jr z, .check_item_dig
	cp DIG
	jr z, .teleport
	inc de
	dec c
	jr nz, .loop_dig
.check_item_dig
	; Check has the TM in the bag:
	push bc
	ld a, DIG
	call CheckTMHM
	pop bc
	jr nc, .teleport
	; Add option:
	ld a, MONMENUITEM_DIG
	call AddMonMenuItem
	ld a, b
	cp 4
	jr z, .end
	; TELEPORT:
.teleport ; Omitted to make things interesting
	; FRESH_SNACK:
.fresh_snack ; Omitted to make things interesting.
	; HEADBUTT:
.headbutt
	; ROCK_SMASH:
.rocksmash
.end
	ret
