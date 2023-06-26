; lazyriver
; Graphic asset transforms
;
; Routines to transform the graphics assets defined in artdefine.s into
; a usable format for drawing.
;
; These graphics live in bank 1 and are arranged as follows:
; (Map occupies bank 1 from $000-13FF)
; Graphics for the tiles are from $1400 to $14FF
;   8 tiles in sequence, first four are land, second four are water.
;   each tile is 7x8, every 7 pixels take 4 bytes, so each tile is $20 bytes.
; Sprite definitions are from $1500-7EFF (up to 15 sprites)
;   each sprite is two tiles wide (to cover movement between tiles)
;   each sprite has 7 shifts, each shift is 14x8, or 64 bytes
;   each shift also has a mask, also 64 bytes.
;   each sprite has two frames for animation, both frames together, 256 bytes.
;   Memory layout is (in groups of 8 bytes, representing 14 pixels)
;   $1500: [spr 0 pg A ln 0 shft 0 frm 0 data] ... [spr 0 pg A ln 7 shft 0 frm 0 data]
;   $1520: [spr 0 pg B ln 0 shft 0 frm 0 data] ... [spr 0 pg B ln 7 shft 0 frm 0 data]
;   $1540: [spr 0 pg A ln 0 shft 0 frm 0 mask] ... [spr 0 pg A ln 7 shft 0 frm 0 mask]
;   $1560: [spr 0 pg B ln 0 shft 0 frm 0 mask] ... [spr 0 pg B ln 7 shft 0 frm 0 mask]
;   $1580: [spr 0 pg A ln 0 shft 0 frm 1 data] ... [spr 0 pg A ln 7 shft 0 frm 1 data]
;   $15A0: [spr 0 pg B ln 0 shft 0 frm 1 data] ... [spr 0 pg B ln 7 shft 0 frm 1 data]
;   $15C0: [spr 0 pg A ln 0 shft 0 frm 1 mask] ... [spr 0 pg A ln 7 shft 0 frm 1 mask]
;   $15E0: [spr 0 pg B ln 0 shft 0 frm 1 mask] ... [spr 0 pg B ln 7 shft 0 frm 1 mask]
;   $1600: [spr 0 pg A ln 0 shft 1 frm 0 data] ... [spr 0 pg A ln 7 shft 1 frm 0 data]
;   ...
;   $1BE0: [spr 0 pg B ln 0 shft 6 frm 1 mask] ... [spr 0 pg B ln 7 shft 6 frm 1 mask]
;   $1C00: [spr 1 pg A ln 0 shft 0 frm 0 data] ... [spr 1 pg B ln 7 shft 0 frm 0 mask]
;   ...
;   $22E0: [spr 1 pg B ln 0 shft 6 frm 1 mask] ... [spr 1 pg B ln 7 shft 6 frm 1 mask]
;   $2300: [spr 2 pg A ln 0 shft 0 frm 0 data] ... [spr 1 pg B ln 7 shft 0 frm 0 mask] ...
;   $2A00: [spr 3 pg A ln 0 shft 0 frm 0 data] ... [spr 1 pg B ln 7 shft 0 frm 0 mask] ...
;   $3100: [spr 4 pg A ln 0 shft 0 frm 0 data] ... [spr 1 pg B ln 7 shft 0 frm 0 mask] ...
;   ...
;   $7700: [spr 14 pg A ln 0 shft 0 frm 0 data] ... [spr 14 pg B ln 7 shft 0 frm 0 mask] ...
;   That is: If sprite start is at $XX00
;   Data page A starts at $XX00 + (shift * $100) + (frame * $80)
;   Data page B starts at $XX00 + (shift * $100) + (frame * $80) + $20
;   Mask page A starts at $XX00 + (shift * $100) + (frame * $80) + $40
;   Mask page B starts at $XX00 + (shift * $100) + (frame * $80) + $60

; Apple 3 hires graphics format is as follows.  The pixels are shown
; "backwards" so that the most significant bits in a byte can be
; on the left.  Pixels 0, 2, 4, and 6 are entirely contained within
; a single byte, and the other pixels are spread across the bits
; of two bytes.  Only 7 bits are used per byte, hence the weirdness.

; |   4001      |   2001      |   4000      |   2000      |
; | | | | | | | | | | | | | | | | | | | | | | | | | | | | |
; |MSB       LSB|             |             |             | 
; |-- P6 -|-- P5 -|-- P4 -|-- P3 -|-- P2 -|-- P1 -|-- P0 -|
;
; Colors:
; 0000 0 black      0100 4 darkgreen    1000 8 brown    1100 C green
; 0001 1 magenta    0101 5 grey1        1001 9 orange   1101 D yellow
; 0010 2 darkblue   0110 6 medblue      1010 A grey2    1110 E aqua
; 0011 3 purple     0111 7 lightblue    1011 B pink     1111 F white
;
; so, to put colors 0-6 in the first six pixels in order, you would have
; 0100 0101 0100 0011 0010 0001 0000 (pixels from 6 to 0 in that order)
; which gets chopped in to 7 bit chunks
; 0100 010 | 1 0100 00 | 11 0010 0 | 001 0000
; or (with hi bit zero, 7 bits per interleaved pairs of bytes):
; 4001: %00100010   2001: %01010000     4000: %01100100     2000: %00010000

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
            lda #>Sprites       ; put current sprite line at the beginning of definition
            sta CurrSprLn + 1
            lda #<Sprites
            sta CurrSprLn
            lda #NumSprites     ; number of sprites to transform (1-based)
            sta ZSprLeft
bgsprite:   lda #$07            ; draw 8 lines in the frame
            sta ZSprLnsLeft
bgsprline:  jsr bgspreadln      ; read current line def (CurrSprLn) into workspace
            lda #$06            ; then do 7 shifts
            sta ZShiftsLeft
bgsprshift: jsr bgwrshift       ; write masks/data for this sprite line, this shift
            jsr bgshift         ; shift pixels in the workspace to the right
            inc ZPtrSprA + 1    ; advance to next shift for sprite/line (ahead $100)
            dec ZShiftsLeft
            bpl bgsprshift
            ; we have done all shifts and masks for one line
            ; target pointer is now just past last shift (start + $700) for that line
            ; move definition pointer to next line in this sprite definition
            ; move target pointer to next line (+4), and back to first shift (-$700)
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
            lda ZPtrSprA + 1    ; back up $700 to first shift (e.g., from $1C to $15)
            sec
            sbc #$07
            sta ZPtrSprA + 1
            dec ZSprLnsLeft
            bpl bgsprline
            ; we have now done all lines for this sprite, one frame
            ; definition pointer now points at next frame/sprite start
            ; if it is pointing at the second frame, targetL will be $80
            ; if we already did the second frame it will have wrapped back to
            ; the start
            lda ZPtrSprA
            bmi bgsprite        ; go back and do second frame
            ; target pointer wrapped around such that it has returned to start
            ; so advance $700 to put it at the beginning of the next sprite.
            lda ZPtrSprA + 1    ; advance $700 to next sprite
            clc
            adc #$07
            sta ZPtrSprA + 1
            dec ZSprLeft        ; NumSprites is 1-based
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
; color c__ -> 0000, c_x -> 1000, others -> unchanged

zeroclear:  pha
            and #$F0
            cmp #(c_x * 16)
            bne :+
            lda #%10000000
            bne :++
:           cmp #(c__ * 16)
            bne :+
            lda #$00
:           sta ZPxScratch
            pla
            and #$0F
            cmp #c_x
            bne :+
            lda #%00001000
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
