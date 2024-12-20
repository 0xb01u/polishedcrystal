; Data tables that vary for forms list normal species data up to 255 (EGG),
; then continue with entries for these species+form combinations.

CosmeticSpeciesAndFormTable:
	table_width 2, CosmeticSpeciesAndFormTable
	dp UNOWN,     UNOWN_B_FORM
	dp UNOWN,     UNOWN_C_FORM
	dp UNOWN,     UNOWN_D_FORM
	dp UNOWN,     UNOWN_E_FORM
	dp UNOWN,     UNOWN_F_FORM
	dp UNOWN,     UNOWN_G_FORM
	dp UNOWN,     UNOWN_H_FORM
	dp UNOWN,     UNOWN_I_FORM
	dp UNOWN,     UNOWN_J_FORM
	dp UNOWN,     UNOWN_K_FORM
	dp UNOWN,     UNOWN_L_FORM
	dp UNOWN,     UNOWN_M_FORM
	dp UNOWN,     UNOWN_N_FORM
	dp UNOWN,     UNOWN_O_FORM
	dp UNOWN,     UNOWN_P_FORM
	dp UNOWN,     UNOWN_Q_FORM
	dp UNOWN,     UNOWN_R_FORM
	dp UNOWN,     UNOWN_S_FORM
	dp UNOWN,     UNOWN_T_FORM
	dp UNOWN,     UNOWN_U_FORM
	dp UNOWN,     UNOWN_V_FORM
	dp UNOWN,     UNOWN_W_FORM
	dp UNOWN,     UNOWN_X_FORM
	dp UNOWN,     UNOWN_Y_FORM
	dp UNOWN,     UNOWN_Z_FORM
	dp UNOWN,     UNOWN_EXCLAMATION_FORM
	dp UNOWN,     UNOWN_QUESTION_FORM
	dp ARBOK,     ARBOK_KANTO_FORM
	dp ARBOK,     ARBOK_KOGA_FORM
	dp ARBOK,     ARBOK_AGATHA_FORM
	dp ARBOK,     ARBOK_ARIANA_FORM
	dp PIKACHU,   PIKACHU_FLY_FORM
	dp PIKACHU,   PIKACHU_SURF_FORM
	dp PIKACHU,   PIKACHU_RED_FORM
	dp PIKACHU,   PIKACHU_YELLOW_FORM
	dp PIKACHU,   PIKACHU_SPARK_FORM
	dp PICHU,     PICHU_SPIKY_EARED_FORM
	dp MAGIKARP,  MAGIKARP_SKELLY_FORM
	dp MAGIKARP,  MAGIKARP_CALICO1_FORM
	dp MAGIKARP,  MAGIKARP_CALICO2_FORM
	dp MAGIKARP,  MAGIKARP_CALICO3_FORM
	dp MAGIKARP,  MAGIKARP_TWO_TONE_FORM
	dp MAGIKARP,  MAGIKARP_ORCA_FORM
	dp MAGIKARP,  MAGIKARP_DAPPLES_FORM
	dp MAGIKARP,  MAGIKARP_TIGER_FORM
	dp MAGIKARP,  MAGIKARP_ZEBRA_FORM
	dp MAGIKARP,  MAGIKARP_STRIPE_FORM
	dp MAGIKARP,  MAGIKARP_BUBBLES_FORM
	dp MAGIKARP,  MAGIKARP_FOREHEAD_FORM
	dp MAGIKARP,  MAGIKARP_MASK_FORM
	dp MAGIKARP,  MAGIKARP_SAUCY_FORM
	dp MAGIKARP,  MAGIKARP_RAINDROP_FORM
	assert_table_length NUM_COSMETIC_FORMS
	; fallthrough

VariantSpeciesAndFormTable:
	table_width 2, VariantSpeciesAndFormTable
	dp GYARADOS,  GYARADOS_RED_FORM
	dp MEWTWO,    MEWTWO_ARMORED_FORM
	dp RATTATA,   ALOLAN_FORM
	dp RATICATE,  ALOLAN_FORM
	dp SANDSHREW, ALOLAN_FORM
	dp SANDSLASH, ALOLAN_FORM
	dp VULPIX,    ALOLAN_FORM
	dp NINETALES, ALOLAN_FORM
	dp DIGLETT,   ALOLAN_FORM
	dp DUGTRIO,   ALOLAN_FORM
	dp MEOWTH,    ALOLAN_FORM
	dp PERSIAN,   ALOLAN_FORM
	dp GEODUDE,   ALOLAN_FORM
	dp GRAVELER,  ALOLAN_FORM
	dp GOLEM,     ALOLAN_FORM
	dp GRIMER,    ALOLAN_FORM
	dp MUK,       ALOLAN_FORM
	dp RAICHU,    ALOLAN_FORM
	dp EXEGGUTOR, ALOLAN_FORM
	dp MAROWAK,   ALOLAN_FORM
	dp PONYTA,    GALARIAN_FORM
	dp RAPIDASH,  GALARIAN_FORM
	dp SLOWPOKE,  GALARIAN_FORM
	dp SLOWBRO,   GALARIAN_FORM
	dp SLOWKING,  GALARIAN_FORM
	dp WEEZING,   GALARIAN_FORM
	dp ARTICUNO,  GALARIAN_FORM
	dp ZAPDOS,    GALARIAN_FORM
	dp MOLTRES,   GALARIAN_FORM
	dp MEW,       MEW_ARMORED_FORM
	assert_table_length NUM_VARIANT_FORMS
	; fallthrough

ExtSpeciesTable:
; For species after index 254. Just a simple ordered table.
; We can't just convert directly, that results in problems with formes, even if
; the extspecies doesn't have a form on its own.
; TODO: maybe convert directly anyway by splitting the tables up for
; optimization reasons? This would only really be relevant for the pokedex.
	table_width 2, ExtSpeciesTable
	assert_table_length NUM_EXT_SPECIES

	db 0 ; end
