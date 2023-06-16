; lazyriver
; Graphic asset definitions and transforms
;
; I don't want to mess with drawing programs, so it is all defined in here
; Graphics will be transformed from the definitions in here to appropriate
; bytes in bank 2.
; Map graphics will start at $3400 (just above the map).
; Each map tile is 7x8 = 32 bytes.
; 8 map tiles = $100 bytes, so should end at $34FF.
; Object graphics will start at $3500 (just above map tiles)

; tile names

C_LAND_A    = $00         ; land type 1
C_LAND_B    = $01         ; land type 2
C_LAND_C    = $02         ; land type 3
C_LAND_D    = $03         ; land type 4
C_WATER_A   = $04         ; water type 1
C_WATER_B   = $05         ; water type 2
C_WATER_C   = $06         ; water type 3
C_WATER_D   = $07         ; water type 4

; |   2000      |   4000      |   2001      |   4001      |
; | | | | | | | | | | | | | | | | | | | | | | | | | | | | |
; |LSB       MSB|             |             |             | 
; |-- P1 -|-- P2 -|-- P3 -|-- P4 -|-- P5 -|-- P6 -|-- P7 -|
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
            lda #$34            ; start map tiles at $3400
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
            jsr xlatequad       ; trasnlate into ZPixByteE-ZPixByteH
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
            
            rts
            
; convert 4 bytes of sensibly encoded pixels into
; the bonkers-encoded pixels of Apple /// hires mode
; call by putting the color codes of pixels 1-7 in
; ZPixByteA [12], ZPixByteB [34], ZPixByteC [56], ZPixByteD [7-]
; and the return bytes will be ZPixByteE-ZPixByteH
xlatequad:
            lda ZPixByteA       ; pixels 0-1
            tay                 ; remember for later (need pixel 1)
            and #$7F
            sta ZPixByteE       ; output byte
            ; byte 1 (byte 0 page 2): -3322221 [0+1+1] 21/8421/8
            tya                 ; recall color of pixel 1
            asl                 ; move hi bit of pixel 1 color
            rol                 ; into lo bit of byte 1
            and #$01
            sta ZPxScratch      ; stash bit of pixel 1
            lda ZPixByteB       ; pixels 2-3
            tay                 ; remember for later (need pixel 3)
            asl                 ; move pixel 2's and 3's bits up
            and #%011111110     ; and chop off the two hi bits of pixel 3
            ora ZPxScratch      ; and then add pixel 1's last bit in
            sta ZPixByteF       ; output byte
            ; byte 2 (byte 1 page 1): -5444433 [1+2+2] 1/8421/84
            tya                 ; recall color of pixel 3
            asl
            rol
            rol                 ; put pixel 3's hi 2 bits in low bits
            and #$03            ; isolate the pixel 3 color's higher two bits
            sta ZPxScratch      ; and stash them
            lda ZPixByteC       ; pixels 4-5
            tay                 ; remember for later
            asl                 ; shift them up
            asl
            ora ZPxScratch      ; add in pixel 3's hi 2 bits
            and #$7F            ; chop off the msb
            sta ZPixByteG       ; output byte
            ; byte 3 (byte 1 page 2): -6666555 [2+3] 8421/842
            tya                 ; recall color of pixel 5
            asl                 ; move higher 3 bits of pixel 5 into low 3 bits
            rol
            rol
            rol
            and #$07
            sta ZPxScratch
            lda ZPixByteD       ; pixel 6 [in high nibble]
            lsr                 ; move pixel 6 right one
            ora ZPxScratch      ; and add in pixel 5's bits
            sta ZPixByteH       ; output byte
            ; the 7 pixels are now computed and stored in ZPixByteE-H
            rts

; Art is 7x8 pixels, 7 pixels are represented in 4 bytes
; We will pre-shift these into proper A3 Hires bytes, so the definitions
; will be done using nibble-aligned colors, with the last nibble unused.

; definitions of graphics colors
; can use in bytes like
; .byte cLB * 16 + cMg
cbk = %0000  ; Black
cMg = %0001  ; Magenta
cDB = %0010  ; Dark blue
cPr = %0011  ; Purple
cGn = %0100  ; Dark green
cGy = %0101  ; Grey1
cMB = %0110  ; Medium blue
cLB = %0111  ; Light blue
cBn = %1000  ; Brown
cOr = %1001  ; Orange
cGY = %1010  ; Grey2
cPk = %1011  ; Pink
cGN = %1100  ; Green
cYw = %1101  ; Yellow
cAq = %1110  ; Aqua
cWh = %1111  ; White

; graphics macro to tile 7 pixels into 4 bytes for easier editing/reading
.macro  tile    arg1, arg2, arg3, arg4, arg5, arg6, arg7
        .byte   arg2 * 16 + arg1, arg4 * 16 + arg3, arg6 * 16 + arg5, arg7 * 16
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

; Object graphics
; These have both a 1-bit mask and a pixel array
; Mask is the lower 7 bits of the mask byte

Sprites:
; Player - TODO - later have it bank depending on XV, so have 3 orientations
            .byte   %00001000
            .byte   %00011100
            .byte   %00111110
            .byte   %01111111
            .byte   %01111111
            .byte   %01111111
            .byte   %01111111
            .byte   %00111110
            
            tile    cbk, cbk, cbk, cPk, cbk, cbk, cbk
            tile    cbk, cbk, cPk, cMg, cPk, cbk, cbk
            tile    cbk, cPk, cMg, cMg, cMg, cPk, cbk
            tile    cPk, cMg, cMg, cMg, cMg, cMg, cPk
            tile    cPk, cMg, cLB, cLB, cLB, cMg, cPk
            tile    cPk, cMg, cLB, cLB, cLB, cMg, cPk
            tile    cPk, cOr, cMg, cMg, cMg, cMg, cPk
            tile    cbk, cPk, cPk, cPk, cPk, cPk, cbk
