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
; each sprite is 64 bytes of data (2 tiles wide) and 64 bytes of mask
; allowing for 30 sprites between $1500-$7EFF
; optimizing for drawing, mask and data will be interleaved, like:
; $1500 - sprite 1 shift 0 line 1 first half mask
; $1504 - sprite 1 shift 0 line 1 first half data
; $1508 - sprite 1 shift 0 line 1 second half mask
; $150C - sprite 1 shift 0 line 1 second half data
; $1510 - sprite 1 shift 0 line 2 first half mask
; $1514 - sprite 1 shift 0 line 2 first half data
; ...
; $1578 - sprite 1 shift 0 line 8 second half mask
; $157C - sprite 1 shift 0 line 8 second half data
; $1580 - sprite 2 shift 0 line 1 first half mask
; ...
; $15F8 - sprite 2 shift 0 line 8 second half data
; $1600 - sprite 1 shift 1 line 1 first half mask
; ...
; $16F8 - sprite 2 shift 1 line 8 second half data
; $1700 - sprite 1 shift 2 line 1 first half mask
; ...
; $1BF8 - sprite 2 shift 6 line 8 second half data
; $1C00 - sprite 3 shift 0 line 1 first half mask
; ...
; $22F8 - sprite 4 shift 6 line 8 second half data
; that is:
; sprite 1 is in first $80, sprite 2 is in second $80 of a page
; masks and data interleaved, 8 lines, 8 bytes across, interleaves every 4 bytes
; shift increase base address by $100
; full 7 shifts of 2 sprites fit in $700 bytes ($1500...$1C00)
; odd sprites start at pages:
; $15, $1C, $23, $2A, $31, $38, $3F, $46, $4D, $54, $5B, $62, $69, $70, $77
; we have room for 30 sprites, should we wish to use them.
; or 15 sprites with two animation variants
; or 10 sprites with three animation variants

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

buildgfx:   lda #$82            ; bank 2
            sta ZPtrA + XByte
            jsr buildtiles      ; transform the tile graphics
            jsr buildsprs       ; transform the sprite graphics
            rts

; build the tile graphics
buildtiles: lda #$14            ; start map tiles at $1400
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
            rts

; transform the sprite graphics            
buildsprs:  lda #$15            ; start sprite data at $1500
            sta ZPtrA + 1
            lda #$00
            sta ZPtrA
            lda #NumSprites     ; number of sprites to transform
            sta ZSprLeft
            lda #>Sprites
            sta CurrSprLn + 1
            lda #<Sprites
            sta CurrSprLn
bgsprites:  lda #$08
            sta ZSprLnsLeft
bgsprite:   jsr bgspreadln      ; read current line def (CurrSprLn) into workspace
            lda #$07            ; then do 7 shifts
            sta ZShiftsLeft
bgsprline:  jsr bgdoshift       ; write masks/data for this sprite line, this shift
            dec ZShiftsLeft
            beq bgsprldone      ; branch if all shifts done
            ; advance data pointer to next shift, this sprite, this line
            ; (back $10, ahead $100)
            lda ZPtrA
            sec
            sbc #$10
            sta ZPtrA
            inc ZPtrA + 1
            jsr bgshift         ; shift pixels in the workspace to the right
            jmp bgsprline
            ; we have done all shifts and masks for this sprite, one line
            ; now, move to the next line if there are more lines
bgsprldone: lda CurrSprLn       ; move pointer into sprite definition ahead
            clc
            adc #$04
            bcc :+
            inc CurrSprLn + 1
 :          dec ZSprLnsLeft
            beq bgsprdone       ; branch away if all lines are done
            ; ZPtr is just beyond the end of the sprite line data block
            ; next line is 6 pages back
            lda ZPtrA + 1       ; back up $600 (e.g., from $1B to $15)
            sec
            sbc #$06
            sta ZPtrA + 1
            jmp bgsprite
            ; we have done all lines for this sprite
            ; now, if there are more sprites, do the next one
bgsprdone:  dec ZSprLeft
            beq bgalldone
            ; move to the next sprite
            ; ZPtr is just beyond the end of the sprite data block
            ; next sprite is either $700 back (e.g., $1580) or here (e.g. $1C00)
            lda ZPtrA
            bmi :+              ; branch if we just did an odd sprite
            lda ZPtrA + 1       ; back up $700, we just did an even sprite
            sec
            sbc #$07
            sta ZPtrA + 1
            jmp bgsprite        ; go do this next sprite
            
:           jmp bgsprites
bgalldone:  rts

; shift all pixels in the 2-tile buffer to the right
; AB CD EF G- HI JK LM N- => 
; _A BC DE F- GH IJ KL M-
; 00 01 02 03 04 05 06 07 index
bgshift:    lda #$00
            sta ZPxScratch
            ldx #$06            ; last pixel does not survive
bgshiftb:   lda ZPixByteI, x
            tay
            and #$0F            ; second pixel
            asl
            asl
            asl
            asl
            ora ZPxScratch
            sta ZPixByteJ, x
            tya
            and #$F0            ; first pixel
            lsr
            lsr
            lsr
            lsr
            sta ZPxScratch      ; becomes second pixel here
            dex
            beq bgshiftd
            cpx #$03            ; at the juncture, special case
            beq bgshiftc
            bne bgshiftb        ; branch always
bgshiftc:   lda ZPixByteI, x    ; load lone pixel of first tile
            and #$F0
            ora ZPxScratch
            sta ZPixByteJ, x
            lda #$00
            sta ZPxScratch
            dex
            bne bgshiftb        ; branch always
bgshiftd:   lda #(c__ * 16)     ; shift in a transparent pixel on the left
            ora ZPxScratch
            sta ZPixByteI
            rts
            
; fill workspace for shift 0 of current sprite line
; assumes:
; - CurrSprLn points to current line in the sprite definition
            ; fill two-tile-wide line with sprite + 7 transparent pixels
bgspreadln: lda #(c__*16 + c__) ; two transparent pixels
            ldx #$03            ; make second 7 pixels transparent
bgsprfill:  sta ZPixByteM, x
            dex
            bpl bgsprfill
            ldy #$03            ; move sprite into first 7 pixels
CurrSprLn = *+1
bgspradd:   lda INLINEADDR, y
            sta ZPixByteI, y
            dey
            bpl bgspradd
            rts

; advance the data pointer to next group of 7
bgadvance:  lda ZPtrA
            clc
            adc #$04
            sta ZPtrA
            rts

; work on current shift
; assumes:
; - ZPixByteI-P hold current (shifted) pixels
; - ZPtrA points to storage location for this sprite masks/data
; ends with ZPtrA advanced by $10 (mask, data, mask, data)

bgdoshift:  ldy #$03            ; build mask for first half
bgmaska:    lda ZPixByteI, y
            jsr tomask
            sta ZPixByteA, y
            dey
            bpl bgmaska
            jsr xlatequad
            ldy #$03
bgmaskouta: lda ZPixByteE, y    ; write sprite mask data first half
            sta (ZPtrA), y
            dey
            bpl bgmaskouta
            jsr bgadvance       ; advance data pointer
            ldy #$03            ; translate data for first half
bgspra:     lda ZPixByteI, y
            sta ZPixByteA, y
            dey
            bpl bgspra
            jsr xlatequad
            ldy #$03
bgsprouta:  lda ZPixByteE, y    ; write sprite data first half
            sta (ZPtrA), y
            dey
            bpl bgsprouta
            jsr bgadvance       ; advance data pointer
            ldy #$03            ; build mask for second half
bgmaskb:    lda ZPixByteM, y
            jsr tomask
            sta ZPixByteA, y
            dey
            bpl bgmaskb
            jsr xlatequad
            ldy #$03
bgmaskoutb: lda ZPixByteE, y    ; write sprite mask data second half
            sta (ZPtrA), y
            dey
            bpl bgmaskoutb
            jsr bgadvance       ; advance data pointer
            ldy #$03            ; translate data for second half
bgsprb:     lda ZPixByteM, y
            sta ZPixByteA, y
            dey
            bpl bgsprb
            jsr xlatequad
            ldy #$03
bgsproutb:  lda ZPixByteE, y    ; write sprite data second half
            sta (ZPtrA), y
            dey
            bpl bgsproutb
            ; mask and data now transferred for one line
            jmp bgadvance       ; leave by advancing data pointer

; convert a sensibly-encoded byte into a mask
; color c__ -> 1111, others -> 0000

tomask:     pha
            and #$F0
            cmp #(c__ * 16)
            beq :+
            lda #$00
            beq :++
:           lda #$F0
:           sta ZPxScratch
            pla
            and #$0F
            cmp #c__
            beq :+
            lda #$00
            beq :++
:           lda #$0F
:           ora ZPxScratch
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
; each sprite definition is 32 ($20) bytes

NumSprites = 1

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
