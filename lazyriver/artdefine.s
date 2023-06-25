; lazyriver
; Graphic asset definitions and transforms
;
; I don't want to mess with drawing programs, so it is all defined in here
; Graphics will be transformed from the definitions in here to appropriate
; bytes in bank 1.
; Map occupies $0000-$13FF
; Map tile graphics: $1400-14FF (8 tile types)
; Each map tile is 7x8 = 32 bytes.
; Sprites: 2 tiles wide, 64 bytes of data, 64 bytes of mask, 7 shifts.  $700 bytes.
; Odd sprites at pages: 15, 1C, 23, 2A, 31, 38, 3F, 46, 4D, 54, 5B, 62, 69, 70, 77.
; That leaves room for 30 sprites between $1500-7EFF.
; Animated with 2 frames each leaves room for 15
; optimizing for drawing, masks, data, B bytes, and A bytes will all
; be separated so that they can be referred to by the same index.
; $1500 - sprite 1A shift 0 line 1 4 A bytes
; $1504 - sprite 1A shift 0 line 2 4 A bytes
; ...
; $1518 - sprite 1A shift 0 line 8 4 A bytes
; $1520 - sprite 1A shift 0 line 1 4 B bytes
; ...
; $1538 - sprite 1A shift 0 line 8 4 B bytes
; $1540 - sprite 1A shift 0 line 1 4 A mask bytes
; ...
; $1560 - sprite 1A shift 0 line 1 4 B mask bytes
; ...
; $1580 - sprite 1B shift 0 line 1 4 A bytes
; ...
; $15F8 - sprite 1B shift 0 line 8 4 B mask bytes
; $1600 - sprite 1A shift 1 line 1 4 A bytes
; ...
; $16F8 - sprite 1B shift 1 line 8 4 B mask bytes
; $1700 - sprite 1B shift 2 line 1 4 A bytes
; ...
; $1BF8 - sprite 1B shift 6 line 8 4 B mask bytes
; $1C00 - sprite 2A shift 0 line 1 4 A bytes
; ...
; $22F8 - sprite 2B shift 6 line 8 4 B mask bytes
; that is;
; sprite 1A is in first $80, sprite 1B is in second $80 of a page
; A data is from $00-1F, B data is from $20-3F
; A mask is from $40-5F, B mask is from $60-7F
; shift increases base address by $100

; tile names

C_LAND_A    = $00         ; land type 1
C_LAND_B    = $01         ; land type 2
C_LAND_C    = $02         ; land type 3
C_LAND_D    = $03         ; land type 4
C_WATER_A   = $04         ; water type 1
C_WATER_B   = $05         ; water type 2
C_WATER_C   = $06         ; water type 3
C_WATER_D   = $07         ; water type 4

; sprite names

S_PLAYER    = $04

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

buildgfx:   jsr buildtiles      ; transform the tile graphics
            jsr buildsprs       ; transform the sprite graphics
            rts

; build the tile graphics
; note that ZPtrSprA has been set to bank 1 in setmemory
buildtiles: lda #$14            ; start map tiles at $1400
            sta ZPtrSprA + 1
            ldy #$00
            sty ZPtrSprA
:           ldx #$00
:           lda MapTiles, y     ; copy sensible bytes into A-D
            sta ZPixByteA, x
            iny
            inx
            cpx #$04
            bne :-
            dey                 ; y ends past bytes, move back to last byte
            jsr xlatequad       ; A-D (sensible) => E-H (bonkers)
            ldx #$03
:           lda ZPixByteE, x    ; copy bonkers bytes into asset memory
            sta (ZPtrSprA), y
            dey
            dex
            bpl :-
            tya                 ; move to next line
            clc
            adc #$05            ; y ends before bytes, move past bytes
            tay
            bne :---            ; 8 tiles, 8 lines of 4 bytes each = 256
            rts
            
; transform the sprite graphics   
; note that ZPtrSprA has been set to bank 1 in setmemory         
buildsprs:  lda #$15            ; start sprite data at $1500
            sta ZPtrSprA + 1
            lda #$00
            sta ZPtrSprA
            lda #NumSprites     ; number of sprites to transform (1-based)
            sta ZSprLeft
            lda #>Sprites       ; put current sprite line at the beginning of definition
            sta CurrSprLn + 1
            lda #<Sprites
            sta CurrSprLn
bgsprite:   lda #$07            ; draw 8 lines
            sta ZSprLnsLeft
bgsprline:  jsr bgspreadln      ; read current line def (CurrSprLn) into workspace
            lda #$06            ; then do 7 shifts
            sta ZShiftsLeft
bgsprshift: jsr bgwrshift       ; write masks/data for this sprite line, this shift
            jsr bgshift         ; shift pixels in the workspace to the right
            inc ZPtrSprA + 1    ; advance to next shift for sprite/line (ahead $100)
            dec ZShiftsLeft
            bpl bgsprshift
            ; we have done all shifts and masks for this sprite, one line
            ; pointer is now just past last shift (start + $700)
            ; do next line in this sprite if more remain
            lda CurrSprLn       ; advance sprite definition pointer by 1 line
            clc
            adc #$04
            sta CurrSprLn
            bcc :+
            inc CurrSprLn + 1
:           lda ZPtrSprA        ; advance to next line in target space
            clc
            adc #$04
            sta ZPtrSprA
            lda ZPtrSprA + 1    ; back up $700 (e.g., from $1C to $15)
            sec
            sbc #$07
            sta ZPtrSprA + 1
            dec ZSprLnsLeft
            bpl bgsprline
            ; CurrSprLn is now pointing just past this sprite
            ; (to first line of next sprite, if there is another one)
            ; ZPtrSprA points either to start+$80 (correct) or to
            ; start (which needs to be [re=]advanced by $700)
            ; TODO - once this is shown to work, maybe optimize out the
            ; Duke of Yorking
            lda ZPtrSprA
            bmi :+
            lda ZPtrSprA + 1
            clc
            adc #$07
            sta ZPtrSprA + 1
:           dec ZSprLeft        ; NumSprites is 1-based
            bne bgsprite
            rts

; shift all pixels in the 2-tile buffer to the right
; pi pj pk pl pm pn po pp
; AB CD EF G- HI JK LM N- => 
; _A BC DE F- GH IJ KL M-
; 00 01 02 03 04 05 06 07 index
; a, x, y do not survive
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
            bmi bgshiftd
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
            ldy #$03            ; make second 7 pixels transparent
bgsprfill:  sta ZPixByteM, y
            dey
            bpl bgsprfill
            ldy #$03            ; move sprite into first 7 pixels
CurrSprLn = *+1
bgspradd:   lda INLINEADDR, y
            sta ZPixByteI, y
            dey
            bpl bgspradd
            rts

; work on current shift
; assumes:
; - ZPixByteI-P hold current (shifted) pixels
; - ZPtrSprA points to storage location for this sprite masks/data
; - ZPtrSprA etc. have been set to bank 1 in setmemory

bgwrshift:  lda ZPtrSprA        ; set up the other three pointers
            clc                 ; nothing below risks setting carry
            adc #$20
            sta ZPtrSprB
            adc #$20
            sta ZPtrMaskA
            adc #$20
            sta ZPtrMaskB
            lda ZPtrSprA + 1
            sta ZPtrSprB + 1
            sta ZPtrMaskA + 1
            sta ZPtrMaskB + 1
            ldy #$03            ; build mask for first half
bgmaska:    lda ZPixByteI, y
            jsr tomask
            sta ZPixByteA, y
            dey
            bpl bgmaska
            jsr xlatequad
            ldy #$00
            lda ZPixByteE
            sta (ZPtrMaskA), y
            lda ZPixByteF
            sta (ZPtrMaskB), y
            iny
            lda ZPixByteG
            sta (ZPtrMaskA), y
            lda ZPixByteH
            sta (ZPtrMaskB), y
            ldy #$03            ; build mask for second half
bgmaskb:    lda ZPixByteM, y
            jsr tomask
            sta ZPixByteA, y
            dey
            bpl bgmaskb
            jsr xlatequad
            ldy #$02
            lda ZPixByteE
            sta (ZPtrMaskA), y
            lda ZPixByteF
            sta (ZPtrMaskB), y
            iny
            lda ZPixByteG
            sta (ZPtrMaskA), y
            lda ZPixByteH
            sta (ZPtrMaskB), y
            ldy #$03            ; translate data for first half
bgspra:     lda ZPixByteI, y
            jsr zeroclear
            sta ZPixByteA, y
            dey
            bpl bgspra
            jsr xlatequad
            ldy #$00
            lda ZPixByteE
            sta (ZPtrSprA), y
            lda ZPixByteF
            sta (ZPtrSprB), y
            iny
            lda ZPixByteG
            sta (ZPtrSprA), y
            lda ZPixByteH
            sta (ZPtrSprB), y
            ldy #$03            ; translate data for second half
bgsprb:     lda ZPixByteM, y
            jsr zeroclear
            sta ZPixByteA, y
            dey
            bpl bgsprb
            jsr xlatequad
            ldy #$02
            lda ZPixByteE
            sta (ZPtrSprA), y
            lda ZPixByteF
            sta (ZPtrSprB), y
            iny
            lda ZPixByteG
            sta (ZPtrSprA), y
            lda ZPixByteH
            sta (ZPtrSprB), y
            rts

; convert a sensibly-encoded byte into a mask
; color c__ or c_x -> 1111, others -> 0000
; allows for transparent and translucent pixels

tomask:     pha
            and #$F0
            cmp #(c__ * 16)
            beq :+
            cmp #(c_x * 16)
            beq :+
            lda #$00
            beq :++
:           lda #$F0
:           sta ZPxScratch
            pla
            and #$0F
            cmp #c__
            beq :+
            cmp #c_x
            beq :+
            lda #$00
            beq :++
:           lda #$0F
:           ora ZPxScratch
            rts

; convert a sensibly-encoded byte into a ORable byte
; converting transparent pixels to 0000, translucent pixels to 0010
; color c__ -> 0000, c_x -> 0010, others -> unchanged

zeroclear:  pha
            and #$F0
            cmp #(c_x * 16)
            bne :+
            lda #%00010000
            bne :++
:           cmp #(c__ * 16)
            bne :+
            lda #$00
:           sta ZPxScratch
            pla
            and #$0F
            cmp #c_x
            bne :+
            lda #%00000001
            bne :++
:           cmp #c__
            bne :+
            lda #$00
:           ora ZPxScratch
            rts
            
; convert 4 bytes of sensibly encoded pixels into
; the bonkers-encoded pixels of Apple /// hires mode
; call by putting the color codes of pixels 1-7 in
; ZPixByteA [12], ZPixByteB [34], ZPixByteC [56], ZPixByteD [7-]
; and the return bytes will be ZPixByteE-ZPixByteH
; y is preserved, x and a are not
xlatequad:  lda ZPixByteD       ; pixel 6 (sensible, in high nibble)
            lsr                 ; shift so bit seven is clear
            sta ZPxScratch      ; stash
            lda ZPixByteC       ; pixels 4-5 (sensible)
            tax
            and #$0E            ; pixel 5 (sensible, partial)
            lsr                 ; shift down below pixel 6 (bonkers)
            ora ZPxScratch      ; combine pixels 5 and 6
            sta ZPixByteH       ; output
            txa
            and #$01            ; the bit we missed from pixel 5 (sensible)
            lsr                 ; in carry
            ror                 ; in bit 8
            ror                 ; put it in bit 7
            sta ZPxScratch      ; stash
            txa
            and #$F0            ; pixel 4 (sensible)
            lsr
            lsr                 ; move it down to end at bit 6
            ora ZPxScratch      ; add in with pixel 5
            sta ZPxScratch      ; stash
            lda ZPixByteB       ; pixels 2-3 (sensible)
            tax
            and #$0C            ; pixel 3 (sensible, partial)
            lsr
            lsr                 ; move into lowest two bits
            ora ZPxScratch      ; add in with pixels 4-5
            sta ZPixByteG       ; output
            txa
            and #$07            ; pixel 3 (sensible, partial)
            lsr                 ; bit 1 and carry
            ror                 ; carry and bit 8
            ror                 ; bit 8 and bit 7
            ror                 ; move into bits 6 and 7
            sta ZPxScratch      ; stash
            txa
            and #$F0            ; pixel 2 (sensible)
            lsr
            lsr
            lsr                 ; move down three bits
            ora ZPxScratch      ; add to pixel 3
            sta ZPxScratch      ; stash
            lda ZPixByteA       ; pixels 0-1 (sensible)
            tax
            and #$08            ; one bit of pixel 1
            lsr
            lsr
            lsr                 ; move to bit 1
            ora ZPxScratch      ; add in with pixels 2 and 3
            sta ZPixByteF       ; output
            txa
            and #$07            ; pixel 1 (sensible, partial)
            asl
            asl
            asl
            asl                 ; move to high nibble
            sta ZPxScratch      ; stash
            txa
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

c__ = cGY       ; transparent mask color is Grey2
c_x = cGy       ; translucent mask color is Grey1

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

NumSprites = 10     ; number of sprite-frames to process (2x num of sprites)

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
