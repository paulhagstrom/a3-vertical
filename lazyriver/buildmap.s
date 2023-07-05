; lazyriver
; map construction

; builds a river with a somewhat random edge
; map has 256 tiles down, build from the bottom up
; (since it starts wide and will probably get narrow further on)

; Map data and tile and sprite graphics data are in bank 1
; Element tracking variables and background caches are in bank 2
; Map data lives from $0000-13FF (bank 1)
; Tile data from $1400-14FF (bank 1)
; Sprite data from $1500-7EFF (bank 1)
; Element tracking variables from $300-AFF (bank 2)
; Sprite background cache data from $1000-4FFF (bank 2)
; have space for 128 sprite caches, needs 64 bytes per sprite, one per page
; Sound effect data from $5000-7EFF (bank 2)
; In the map:
; Each byte represents a 7x8 tile (screen tiles 20x24)
; Each line fills the screen, but not more, so there are 20 tiles across.
; With 256 lines, this means we have 5120 ($1400) bytes.

; Map byte holds flow vector and tile type
; tile type (3 bits) is one of 8 (four water, four land)
; y velocity (2 bits) is always one direction, four speeds including stopped, 
; x velocity (3 bits) is from 0-6, speed + 3, representing -3 to +3
; 8     7   6     5     4       3 2 1
; -------   -------------       -----
; yvel      xvel                tile type
; 00=land   010=left slow -1    100 = water 1
; 01=slow   000=left fast -3    111 = water 4
; 10=med    100=right slow +1   000 = land 1
; 11=fast   110=right fast +3   011 = land 4
;
; tile is water: AND #$04 is nonzero
; use flow only for water tiles (land will have a Xflow of -3)

TileLand:   .byte   C_LAND_A, C_LAND_B, C_LAND_C, C_LAND_D
TileWater:  .byte   C_WATER_A, C_WATER_B, C_WATER_C, C_WATER_D
ShoreL:     .byte 0         ; tile X-coordinate of left shore (first water)
ShoreR:     .byte 0         ; tile X-coordinate of right shore (first land)
ShoreLV:    .byte 0         ; left shoreline velocity
ShoreRV:    .byte 0         ; right shoreline velocity
ProxL:      .byte 0         ; tile at which we are 2 from left shore
ProxR:      .byte 0         ; countdown to 2 from right shore
MapLine:    .byte 0         ; current map line being built

            ; fill in the map start address lookup table
buildmap:   ldx #$00        ; high byte of map address
            ldy #$00        ; line of map we are on
            tya             ; coincidentally, low byte of map address
bmidxmap:   sta MapLineL, y
            pha
            txa
            sta MapLineH, y
            pla
            iny
            beq bmidxdone   ; wrapped around, we're done
            clc
            adc #$14        ; 20 tiles per line
            bcc bmidxmap
            inx
            bne bmidxmap    ; branch always
bmidxdone:  dey             ; start at line $FF and build toward 0
            ; pick random shore start points, start "velocity" as straight up
            ldx Seed
            inc Seed
            lda Random, x
            and #$07        ; limit left shore start to tiles 1-8
            clc             ; ShoreL is first water, land is one to its left
            adc #$01        ; so be sure there is at least one land tile
            sta ShoreL
            inx
            inc Seed
            lda Random, x
            and #$07        ; limit right shore start to tiles 12-19
            sta ShoreR      ; shore R is first land
            lda #19         ; last tile on the right
            sec
            sbc ShoreR
            sta ShoreR
            lda #$00        ; start shore velocity at 0 (straight up)
            sta ShoreLV
            sta ShoreRV
            ; fill the current line
            ; Y will (still) hold MapLine (255) at the beginning
bmmapline:  sty MapLine     ; put map line base address in ZMapPtr
            lda MapLineH, y
            sta ZMapPtr + 1
            lda MapLineL, y
            sta ZMapPtr
            lda #$03        ; water tile countdown for right shore current
            sta ProxR
            lda ShoreL      
            clc
            adc #$03
            sta ProxL       ; tile to trigger left shore current
            lda ShoreR      ; compute present width
            sec
            sbc ShoreL
            sta ZWidth
            ldy #$13        ; right to left, start at last tile in the line
bmscan:     ldx Seed        ; pick a random tile of four options
            inc Seed
            lda Random, x
            and #$03
            tax
            cpy ShoreR          ; are we on the right bank?
            bcs bmland          ; branch away if we are on the right bank, land
            cpy ShoreL          ; are we on the left bank?
            bcs bmwater         ; branch away if we are in the water
bmland:     lda TileLand, x     ; load the land tile
            bpl bmstore         ; branch always to store the map value
bmwater:    lda TileWater, x    ; otherwise, we're in the water, load the water tile
            sta ZPxScratch      ; start building the map byte, starting with tile type
            dec ProxR           ; are we within 2 tiles of the right shore?
            bmi :+              ; branch away if we're further from the right shore
            lda #<-2            ; sent it away from the shore at -2 x velocity
            bpl bmxflowz        ; unless it is positive (widening)
            bmi bmxflow         ; branch always
:           cpx ProxL           ; are we within 2 tiles of the left shore?
            bcs bmxflowz        ; branch away if we're further away from left shore
            lda #2              ; send it away from the shore at +2 x velocity
            bpl bmxflow         ; unless it is negative (widening)
bmxflowz:   lda #$00
bmxflow:    clc                 ; add three to x velocity because it is
            adc #$03            ; simplest to work with positive numbers
            asl                 ; move the x flow bits into position
            asl
            asl
            ora ZPxScratch      ; add the x flow bits to the tile type
            sta ZPxScratch
            lda ZWidth          ; work out y flow speed
            cmp #$08            ; narrow, fast water
            bcs :+
            lda #%11000000      ; 3 is fast
            bne bmyflow         ; branch always
:           cmp #$0E            ; middle width, speedy water
            bcs :+
            lda #%10000000      ; 2 is speedy but not fast
            bne bmyflow         ; branch always
:           lda #%01000000      ; 1 is slowish speed
bmyflow:    ora ZPxScratch      ; add the y flow bits to other bits
bmstore:    sta (ZMapPtr), y
            dey                 ; move to next map tile
            bpl bmscan
            ; make the shoreline wander, if we're not already too narrow
            lda ZWidth
            cmp #$06        ; if shore edges are at least 6 tiles apart, wander
            bcs bmlwander
            lda #<-1        ; otherwise set shore velocity to diverge (widen)
            sta ShoreLV
            lda #1
            sta ShoreRV
            bne bmaddvel    ; branch always
bmlwander:  ; far enough apart to let shores wander
            ldy ShoreLV
            jsr bmwander
            sty ShoreLV
            ldy ShoreRV
            jsr bmwander
            sty ShoreRV
            ; add shore velocity to shore
bmaddvel:   lda ShoreL
            clc
            adc ShoreLV
            beq :+          ; if we hit the left side, come back out
            bpl bmlok
:           lda #$01        ; we ran off the left side, turn velocity inward
            sta ShoreLV
            lda #$01        ; keep the shore on the left edge of the screen
bmlok:      sta ShoreL
            lda ShoreR
            clc
            adc ShoreRV
            cmp #19
            bcc bmrok
            lda #<-1        ; we ran off the right side, turn velocity inward
            sta ShoreRV
            lda #19         ; keep the shore on the right edge of the screen
bmrok:      sta ShoreR
            ldy MapLine
            beq bmdone      ; branch away if we have done 256 map lines
            dey
            jmp bmmapline   ; otherwise go do the next one
            ; the shores are now done
bmdone:     rts

; wander a shoreline - velocity in Y, returns new velocity in Y
bmwander:   ; constrain velocity -- if it is 2 pull back to 1, either direction
            cpy #<-2
            beq bmwplus
            cpy #2
            beq bmwminus
            ldx Seed
            inc Seed
            lda Random, x   ; change velocity? y/n
            bmi bmwdone     ; no, branch away
            inx             ; yes, pick a direction
            inc Seed
            lda Random, x   ; which direction?
            bpl bmwplus
bmwminus:   dey
            rts
bmwplus:    iny
bmwdone:    rts
