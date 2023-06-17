; lazyriver
; Graphic asset definitions and transforms
;
; I don't want to mess with drawing programs, so it is all defined in here
; Graphics will be transformed from the definitions in here to appropriate
; bytes in bank 2.
; Map occupies $0000-$13FF
; Map tile graphics: $1400-14FF (8 tile types)
; Each map tile is 7x8 = 32 bytes.
; 8 map tiles = $100 bytes, so should end at $14FF.
; Sprite graphics will start at $1500, and work like this:
;   $1500 - sprite 1 shift 0
;       $1540 - sprite 2 shift 0
;       $1580 - sprite 3 shift 0
;       $15C0 - sprite 4 shift 0
;   $1600 - sprite 1 shift 1
;   ...
;   $1B00 - sprite 1 shift 6
;   $1900 - sprite 1 mask shift 0
;   ...
;   $2200 - sprite 1 mask shift 7
; so, we get four sprites in from $1500-22FF.
; or, $E00 per 4 sprites.
; we can put sprites 5-8 at $2300-31FF
; and we can keep going.
; 1-4 $1500, 5-8 $2300, 9-12 $3100,
; 13-16 $3F00, 17-20 $4D00, 21-24 $5B00, 25-28 $6900 (ends at $77FF)
; We can fit 28 sprites this way.
; another option is to use 1-4 as animation variants
; with 4 frames per sprite, we can get 7 sprites.
; with 2 frames per sprite, we can get 14 sprites.

; tile names

C_LAND_A    = $00         ; land type 1
C_LAND_B    = $01         ; land type 2
C_LAND_C    = $02         ; land type 3
C_LAND_D    = $03         ; land type 4
C_WATER_A   = $04         ; water type 1
C_WATER_B   = $05         ; water type 2
C_WATER_C   = $06         ; water type 3
C_WATER_D   = $07         ; water type 4

; a slightly more intuitive way to look at this, with the
; pixels backwards but the bits in the right order

; |   4001      |   2001      |   4000      |   2000      |
; | | | | | | | | | | | | | | | | | | | | | | | | | | | | |
; |MSB       LSB|             |             |             | 
; |-- P6 -|-- P5 -|-- P4 -|-- P3 -|-- P2 -|-- P1 -|-- P0 -|

; what the manual says:
;
; |   2000      |   4000      |   2001      |   4001      |
; | | | | | | | | | | | | | | | | | | | | | | | | | | | | |
; |LSB       MSB|             |             |             | 
; |-- P0 -|-- P1 -|-- P2 -|-- P3 -|-- P4 -|-- P5 -|-- P6 -|
;
; Colors:
; 0000 0 black      0100 4 darkgreen    1000 8 brown    1100 C green
; 0001 1 magenta    0101 5 grey1        1001 9 orange   1101 D yellow
; 0010 2 darkblue   0110 6 medblue      1010 A grey2    1110 E aqua
; 0011 3 purple     0111 7 lightblue    1011 B pink     1111 F white

; TODO: plan here is to convert tile colors to proper 4-byte segments
; TODO: then, convert objects to pre-shifted 4-byte segments with masks

buildgfx:   lda #$82            ; bank 2
            sta ZPtrA + XByte
            lda #$14            ; start map tiles at $1400
            sta ZPtrA + 1
            ldy #$00
            sty ZPtrA
bgtile:     tya
            pha                 ; stash start index of tile line
            ; store definition line in ZPixByteA-ZpixByteD
            lda MapTiles, y
            sta ZPixByteA
            iny
            lda MapTiles, y
            sta ZPixByteB
            iny
            lda MapTiles, y
            sta ZPixByteC
            iny
            lda MapTiles, y
            sta ZPixByteD
            jsr xlatequad       ; translate into ZPixByteE-ZPixByteH
            pla                 ; restore start index of tile line
            tay
            lda ZPixByteE
            sta (ZPtrA), y
            iny
            lda ZPixByteF
            sta (ZPtrA), y
            iny
            lda ZPixByteG
            sta (ZPtrA), y
            iny
            lda ZPixByteH
            sta (ZPtrA), y
            iny                 ; move to next tile line
            bne bgtile          ; assumes exactly 8 tiles
            ; tiles done, now do sprites
            lda #$15            ; start sprite data at $1500
            sta ZPtrA + 1
            ldy #$00
bgsprite:   tya
            pha                 ; stash start index of sprite line
            ldx #$00            ; preshift offset
            
            ; store definition line in ZPixByteA-ZpixByteD
            lda Sprites, y
            sta ZPixByteA
            iny
            lda Sprites, y
            sta ZPixByteB
            iny
            lda Sprites, y
            sta ZPixByteC
            iny
            lda Sprites, y
            sta ZPixByteD
            jsr xlatequad       ; translate into ZPixByteE-ZPixByteH
            pla                 ; restore start index of tile line
            tay
            lda ZPixByteE
            sta (ZPtrA), y
            iny
            lda ZPixByteF
            sta (ZPtrA), y
            iny
            lda ZPixByteG
            sta (ZPtrA), y
            iny
            lda ZPixByteH
            sta (ZPtrA), y
            iny                 ; move to next tile line
            bne bgsprite        ; assumes exactly 8 tiles
            
            rts
            
; convert 4 bytes of sensibly encoded pixels into
; the bonkers-encoded pixels of Apple /// hires mode
; call by putting the color codes of pixels 1-7 in
; ZPixByteA [12], ZPixByteB [34], ZPixByteC [56], ZPixByteD [7-]
; and the return bytes will be ZPixByteE-ZPixByteH
xlatequad:
            lda ZPixByteD       ; pixel 6 (sensible, in high nibble)
            lsr                 ; shift so bit seven is clear
            sta ZPxScratch      ; stash
            lda ZPixByteC       ; pixels 4-5 (sensible)
            tay
            and #$0E            ; pixel 5 (sensible, partial)
            lsr                 ; shift down below pixel 6 (bonkers)
            ora ZPxScratch      ; combine pixels 5 and 6
            sta ZPixByteH       ; output
            tya
            and #$01            ; the bit we missed from pixel 5 (sensible)
            ror                 ; in carry
            ror                 ; in bit 8
            ror                 ; put it in bit 7
            sta ZPxScratch      ; stash
            tya
            and #$F0            ; pixel 4 (sensible)
            lsr
            lsr                 ; move it down to end at bit 6
            ora ZPxScratch      ; add in with pixel 5
            sta ZPxScratch      ; stash
            lda ZPixByteB       ; pixels 2-3 (sensible)
            tay
            and #$0C            ; pixel 3 (sensible, partial)
            lsr
            lsr                 ; move into lowest two bits
            ora ZPxScratch      ; add in with pixels 4-5
            sta ZPixByteG       ; output
            tya
            and #$07            ; pixel 3 (sensible, partial)
            ror                 ; bit 1 and carry
            ror                 ; carry and bit 8
            ror                 ; bit 8 and bit 7
            ror                 ; move into bits 6 and 7
            sta ZPxScratch      ; stash
            tya
            and #$F0            ; pixel 2 (sensible)
            lsr
            lsr
            lsr                 ; move down three bits
            ora ZPxScratch      ; add to pixel 3
            sta ZPxScratch      ; stash
            lda ZPixByteA       ; pixels 0-1 (sensible)
            tay
            and #$08            ; one bit of pixel 1
            lsr
            lsr
            lsr                 ; move to bit 1
            ora ZPxScratch      ; add in with pixels 2 and 3
            sta ZPixByteF       ; output
            tya
            and #$07            ; pixel 1 (sensible, partial)
            asl
            asl
            asl
            asl                 ; move to high nibble
            sta ZPxScratch      ; stash
            tya
            and #$F0            ; pixel 0 (sensible)
            lsr
            lsr
            lsr
            lsr                 ; move to low nibble
            ora ZPxScratch      ; add in with pixel 1
            sta ZPixByteE       ; output
            ; the 7 pixels are now computed and stored in ZPixByteE-H
            rts

; Art is 7x8 pixels, 7 pixels are represented in 4 bytes
; We will pre-shift these into proper A3 Hires bytes, so the definitions
; will be done using nibble-aligned colors, with the last nibble unused.

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

c__ = cGY       ; mask color is Grey2

; graphics macro to tile 7 pixels into 4 bytes for easier editing/reading
.macro  tile    arg1, arg2, arg3, arg4, arg5, arg6, arg7
        .byte   arg1 * 16 + arg2, arg3 * 16 + arg4, arg5 * 16 + arg6, arg7 * 16
.endmacro

; Map graphics
; TODO - later see if coastlines can join better to be smoother

MapTiles:
; Land 1
            tile    cGn, cGn, cGn, cGn, cGn, cGN, cGn
            tile    cGn, cGn, cGn, cGn, cGn, cGn, cGn
            tile    cGn, cGn, cGn, cGn, cGn, cGn, cGn
            tile    cGn, cGn, cGn, cPk, cGn, cYw, cGn
            tile    cGn, cGn, cGn, cGn, cYw, cYw, cYw
            tile    cGn, cGn, cGn, cGn, cGn, cYw, cGn
            tile    cGn, cGn, cGn, cGn, cGn, cGn, cGn
            tile    cGn, cGn, cGn, cGn, cGn, cGn, cGn
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

Sprites:
; Player - TODO - later have it bank depending on XV, so have 3 orientations
            
            tile    c__, c__, c__, cPk, c__, c__, c__
            tile    c__, c__, cPk, cMg, cPk, c__, c__
            tile    c__, cPk, cMg, cMg, cMg, cPk, c__
            tile    cPk, cMg, cMg, cMg, cMg, cMg, cPk
            tile    cPk, cMg, cLB, cLB, cLB, cMg, cPk
            tile    cPk, cMg, cLB, cLB, cLB, cMg, cPk
            tile    cPk, cOr, cMg, cMg, cMg, cMg, cPk
            tile    c__, cPk, cPk, cPk, cPk, cPk, c__
