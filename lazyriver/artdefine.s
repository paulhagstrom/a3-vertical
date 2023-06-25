; lazyriver
; Graphic asset definitions
;
; I don't want to mess with drawing programs, so the pixels are all defined in here

; Art is 7x8 pixels, 7 pixels are represented in 4 bytes
; The Apple /// graphics mode smears them across 4 bytes using only
; 7 bits per byte, but in here they are defined "sensibly"
; with one nibble per pixel.  The routines in artinit.s handle
; conversion from the sensible definition format to the bonkers A3 format.

; definitions of graphics colors
; can use in bytes like
; .byte cLB * 16 + cMg
cbk = %0000     ; Black
cMg = %0001     ; Magenta
cDB = %0010     ; Dark blue
cPr = %0011     ; Purple
cGn = %0100     ; Dark green
cGy = %0101     ; Grey1
cMB = %0110     ; Medium blue
cLB = %0111     ; Light blue
cBn = %1000     ; Brown
cOr = %1001     ; Orange
cGY = %1010     ; Grey2 <- mask color
cPk = %1011     ; Pink
cGN = %1100     ; Green
cYw = %1101     ; Yellow
cAq = %1110     ; Aqua
cWh = %1111     ; White
c__ = cGY       ; transparent mask color is Grey2
c_x = cGy       ; translucent mask color is Grey1

; ca65 macro to make entering/editing/reading graphics a little easier
; pushes 7 pixels into 4 bytes
.macro  tile    arg1, arg2, arg3, arg4, arg5, arg6, arg7
        .byte   arg1 * 16 + arg2, arg3 * 16 + arg4, arg5 * 16 + arg6, arg7 * 16
.endmacro

; Map graphics

; tile names

C_LAND_A    = $00         ; land type 1
C_LAND_B    = $01         ; land type 2
C_LAND_C    = $02         ; land type 3
C_LAND_D    = $03         ; land type 4
C_WATER_A   = $04         ; water type 1
C_WATER_B   = $05         ; water type 2
C_WATER_C   = $06         ; water type 3
C_WATER_D   = $07         ; water type 4

MapTiles:
; Land 1
            tile    cGn, cGn, cGn, cGn, cGn, cGN, cGn
            tile    cGn, cGn, cGn, cGn, cGn, cGn, cGn
            tile    cGn, cGn, cGn, cGn, cGn, cGn, cGn
            tile    cGn, cGn, cGn, cPk, cGn, cYw, cGn
            tile    cGn, cGn, cGn, cGn, cYw, cYw, cYw
            tile    cGn, cGn, cWh, cGn, cGn, cYw, cGn
            tile    cGn, cWh, cWh, cWh, cGn, cGn, cGn
            tile    cGn, cGn, cWh, cGn, cGn, cGn, cGn
; Land 2
            tile    cGn, cGn, cGn, cGn, cGn, cGn, cGn
            tile    cGn, cGn, cGn, cGN, cGn, cGn, cGn
            tile    cGn, cGn, cGn, cGn, cGn, cGn, cYw
            tile    cGn, cGn, cGn, cPk, cBn, cGn, cGn
            tile    cGn, cGn, cGn, cGn, cGn, cGN, cGn
            tile    cGn, cGn, cGn, cGn, cYw, cGn, cGn
            tile    cGn, cGn, cBn, cGn, cGn, cGn, cGn
            tile    cGn, cGn, cGn, cGn, cGn, cGn, cGn
; Land 3
            tile    cGn, cGn, cGn, cGn, cGn, cGn, cGn
            tile    cGn, cGn, cGn, cGn, cGn, cGn, cGn
            tile    cGn, cGn, cBn, cBn, cGn, cGn, cGn
            tile    cGN, cGn, cBn, cBn, cGn, cGn, cGn
            tile    cGn, cGn, cGn, cGn, cGn, cGn, cGn
            tile    cGn, cGn, cOr, cGn, cGn, cGn, cGn
            tile    cGn, cGN, cGn, cGn, cGn, cGn, cGn
            tile    cGn, cGn, cGn, cGn, cGn, cGn, cGn
; Land 4
            tile    cGn, cGn, cGn, cGn, cGn, cGn, cGn
            tile    cGn, cGn, cGN, cGn, cGn, cGn, cGn
            tile    cGn, cGN, cGN, cGN, cGN, cGn, cGn
            tile    cGn, cGN, cGn, cGN, cGN, cGn, cGn
            tile    cGn, cGn, cGN, cGN, cGn, cGn, cGn
            tile    cGn, cGn, cGn, cGn, cGn, cGn, cGn
            tile    cGn, cGn, cGn, cGn, cGn, cBn, cGn
            tile    cGn, cGn, cGn, cGn, cGn, cGn, cGn

; Water 1
            tile    cDB, cDB, cDB, cDB, cDB, cDB, cDB
            tile    cDB, cDB, cDB, cDB, cDB, cDB, cDB
            tile    cDB, cDB, cDB, cDB, cAq, cDB, cDB
            tile    cMB, cDB, cDB, cDB, cDB, cDB, cDB
            tile    cDB, cMB, cDB, cDB, cDB, cDB, cDB
            tile    cDB, cDB, cDB, cMB, cDB, cDB, cDB
            tile    cAq, cDB, cDB, cDB, cDB, cDB, cDB
            tile    cDB, cDB, cDB, cDB, cDB, cDB, cDB
; Water 2
            tile    cDB, cDB, cDB, cDB, cDB, cDB, cDB
            tile    cDB, cDB, cDB, cDB, cDB, cDB, cDB
            tile    cDB, cAq, cDB, cDB, cLB, cDB, cDB
            tile    cMB, cDB, cDB, cDB, cDB, cDB, cDB
            tile    cDB, cDB, cDB, cMB, cDB, cDB, cDB
            tile    cDB, cDB, cDB, cDB, cDB, cDB, cDB
            tile    cAq, cDB, cDB, cDB, cDB, cDB, cDB
            tile    cDB, cDB, cDB, cDB, cDB, cDB, cDB
; Water 3
            tile    cDB, cDB, cDB, cDB, cDB, cDB, cDB
            tile    cDB, cDB, cLB, cDB, cDB, cDB, cDB
            tile    cDB, cDB, cDB, cDB, cDB, cDB, cDB
            tile    cDB, cDB, cDB, cMB, cLB, cDB, cDB
            tile    cMB, cDB, cDB, cDB, cDB, cDB, cDB
            tile    cDB, cDB, cDB, cDB, cDB, cDB, cDB
            tile    cAq, cDB, cAq, cDB, cDB, cDB, cDB
            tile    cDB, cDB, cDB, cDB, cDB, cDB, cDB
; Water 4
            tile    cDB, cDB, cDB, cDB, cDB, cDB, cDB
            tile    cDB, cDB, cLB, cDB, cDB, cDB, cAq
            tile    cDB, cDB, cDB, cDB, cDB, cDB, cDB
            tile    cDB, cDB, cDB, cAq, cDB, cDB, cDB
            tile    cMB, cDB, cDB, cDB, cDB, cDB, cDB
            tile    cDB, cDB, cDB, cDB, cDB, cDB, cLB
            tile    cDB, cDB, cDB, cDB, cDB, cDB, cDB
            tile    cDB, cDB, cDB, cDB, cDB, cDB, cDB

; Sprite graphics
; Anything that is in the mask color (Grey2, c__) will be transparent.
; Anything that is in the translucent color (Grey1, c_x) will be filtered.
; each sprite definition is 32 ($20) bytes

; if a sprite contains c__, that pixel is transparent
; if a sprite contains c_x, that pixel is "translucent" (filtered)
;   "translucent" means it is the background OR %1000 (defined in zeroclear)
;   black, brown    -> brown        darkgreen, green    -> green
;   magenta, orange -> orange       yellow              -> yellow
;   darkblue        -> grey2        medblue, aqua       -> aqua
;   purple, pink    -> pink         lightblue, white    -> white

NumSprites = 10     ; # of sprite-frames to process (1-based) (2 per sprite)

; sprite names

S_PLAYER    = $04

Sprites:
; Log 1A
            tile    c__, c__, c__, c__, c__, c__, c__
            tile    c__, c__, cBn, c__, cAq, c__, c__
            tile    c__, cOr, cBn, cBn, cBn, cBn, cOr
            tile    cOr, cbk, cBn, cBn, cBn, cOr, cYw
            tile    cOr, cBn, cBn, cbk, cBn, cOr, cYw
            tile    c__, cOr, cBn, cBn, cBn, cBn, cOr
            tile    c__, c__, c__, c__, c__, c__, c__
            tile    c__, c__, c__, c__, c__, c__, c__
; Log 1B
            tile    c__, c__, c__, c__, cAq, c__, c__
            tile    c__, c__, cBn, cAq, c__, cAq, c__
            tile    c__, cOr, cBn, c__, cBn, cBn, cOr
            tile    cOr, cbk, cBn, cBn, cBn, cOr, cYw
            tile    cOr, cBn, cBn, cbk, cBn, cOr, cYw
            tile    c__, c__, c__, cBn, cBn, cBn, cOr
            tile    c__, cAq, c__, c__, c__, c__, c__
            tile    c__, c__, cAq, c__, c__, c__, c__
; Log 2A
            tile    c__, c__, c__, c__, c__, c__, c__
            tile    c__, c__, cBn, c__, cAq, c__, c__
            tile    c__, cOr, cBn, cBn, cBn, cBn, cOr
            tile    cOr, cbk, cPk, cBn, cBn, cOr, cYw
            tile    cOr, cBn, cBn, cbk, cBn, cOr, cYw
            tile    c__, cOr, cBn, cBn, cBn, cBn, cOr
            tile    c__, c__, c__, c__, c__, c__, c__
            tile    c__, c__, c__, c__, c__, c__, c__
; Log 2B
            tile    c__, c__, c__, c__, cAq, c__, c__
            tile    c__, c__, cBn, cAq, c__, cAq, c__
            tile    c__, cOr, cBn, c__, cBn, cBn, cOr
            tile    cOr, cbk, cPk, cBn, cBn, cOr, cYw
            tile    cOr, cBn, cBn, cbk, cBn, cOr, cYw
            tile    c__, c__, c__, cBn, cBn, cBn, cOr
            tile    c__, cAq, c__, c__, c__, c__, c__
            tile    c__, c__, cAq, c__, c__, c__, c__
; Log 3A
            tile    c__, c__, c__, c__, c__, c__, c__
            tile    c__, c__, cBn, c__, cAq, c__, c__
            tile    c__, cOr, cBn, cBn, cBn, cBn, cOr
            tile    cOr, cbk, cBn, cBn, cBn, cOr, cYw
            tile    cOr, cPk, cPk, cbk, cBn, cOr, cYw
            tile    c__, cOr, cBn, cBn, cBn, cBn, cOr
            tile    c__, c__, c__, c__, c__, c__, c__
            tile    c__, c__, c__, c__, c__, c__, c__
; Log 3B
            tile    c__, c__, c__, c__, cAq, c__, c__
            tile    c__, c__, cBn, cAq, c__, cAq, c__
            tile    c__, cOr, cBn, c__, cBn, cBn, cOr
            tile    cOr, cbk, cBn, cBn, cBn, cOr, cYw
            tile    cOr, cPk, cPk, cbk, cBn, cOr, cYw
            tile    c__, c__, c__, cBn, cBn, cBn, cOr
            tile    c__, cAq, c__, c__, c__, c__, c__
            tile    c__, c__, cAq, c__, c__, c__, c__
; Log 4A
            tile    c__, c__, c__, c__, c__, c__, c__
            tile    c__, c__, cBn, c__, cAq, c__, c__
            tile    c__, cOr, cBn, cBn, cBn, cBn, cOr
            tile    cOr, cbk, cBn, cBn, cPk, cOr, cYw
            tile    cOr, cBn, cBn, cbk, cPk, cOr, cYw
            tile    c__, cOr, cBn, cBn, cBn, cBn, cOr
            tile    c__, c__, c__, c__, c__, c__, c__
            tile    c__, c__, c__, c__, c__, c__, c__
; Log 4B
            tile    c__, c__, c__, c__, cAq, c__, c__
            tile    c__, c__, cBn, cAq, c__, cAq, c__
            tile    c__, cOr, cBn, c__, cBn, cBn, cOr
            tile    cOr, cbk, cBn, cBn, cPk, cOr, cYw
            tile    cOr, cBn, cBn, cbk, cPk, cOr, cYw
            tile    c__, c__, c__, cBn, cBn, cBn, cOr
            tile    c__, cAq, c__, c__, c__, c__, c__
            tile    c__, c__, cAq, c__, c__, c__, c__

; PlayerA
            tile    c__, c__, c__, cPk, c__, c__, c__
            tile    c__, c__, cPk, cMg, cPk, c__, c__
            tile    c__, cPk, cMg, cMg, cMg, cPk, c__
            tile    cPk, cMg, c_x, c_x, c_x, cMg, cPk
            tile    cPk, cMg, c_x, c_x, c_x, cMg, cPk
            tile    cPk, cMg, c_x, c_x, c_x, cMg, cPk
            tile    cPk, cOr, cMg, cMg, cMg, cOr, cPk
            tile    c__, cPk, cOr, cYw, cOr, cPk, c__
; PlayerB
            tile    c__, c__, c__, cYw, c__, c__, c__
            tile    c__, c__, cPk, cMg, cPk, c__, c__
            tile    c__, cPk, cMg, cMg, cMg, cPk, c__
            tile    cPk, cMg, c_x, c_x, c_x, cMg, cPk
            tile    cPk, cMg, c_x, c_x, c_x, cMg, cPk
            tile    cPk, cMg, c_x, c_x, c_x, cMg, cPk
            tile    cPk, cOr, cMg, cMg, cMg, cOr, cPk
            tile    c__, cPk, cPk, cPk, cPk, cPk, c__
