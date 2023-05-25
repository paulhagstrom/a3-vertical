; lazyriver
; map construction

; Map data lives in bank 2, from $2000-5FFF
; Each byte represents a 7x8 tile (screen tiles 20x24)
; Element tracking variables are from $300-AFF.
; 
; builds a river with a somewhat random edge

TileLand:   .byte   C_LAND_A, C_LAND_B, C_LAND_C, C_LAND_D
TileWater:  .byte   C_WATER_A, C_WATER_B, C_WATER_C, C_WATER_D
ShoreL:     .byte 0
ShoreR:     .byte 0
ShoreLV:    .byte 0
ShoreRV:    .byte 0
MapLine:    .byte 0

buildmap:   
            ; pick random shore start points, start "velocity" as straight down
            ldx Seed
            lda Random, x
            and #$07        ; limit left shore start to first 7 tiles
            sta ShoreL
            inx
            lda Random, x
            stx Seed
            and #$07        ; limit right short start to last 7 tiles
            sta ShoreR
            lda #19         ; last tile on the right
            sec
            sbc ShoreR
            sta ShoreR
            lda #$00        ; start shore velocity at 0 (straight down)
            sta ShoreLV
            sta ShoreRV
            sta MapLine     ; set current map line to 0
            sta ZPtrA       ; set ZPtrA to $2000 of bank 2, top line of map
            lda #$20
            sta ZPtrA + 1
            lda #$82
            sta ZPtrA + XByte
            ; fill the current line
            ; land from left to shore left
bmmapline:  ldy ShoreL
bmlandl:    ldx Seed
            lda Random, x
            and #$03        ; pick one of four land tiles
            inc Seed
            tax
            lda TileLand, x
            sta (ZPtrA), y
            dey
            bpl bmlandl
            ; water from shore left to shore right
            ldy ShoreR
bmwater:    ldx Seed
            lda Random, x
            and #$03        ; pick one of four water tiles
            inc Seed
            tax
            lda TileWater, x
            sta (ZPtrA), y
            dey
            cpy ShoreL
            bne bmwater
            ; land from shore right to right
            ldy #19         ; last tile on the right
bmlandr:    ldx Seed
            lda Random, x
            and #$03        ; pick one of four land tiles
            inc Seed
            tax
            lda TileLand, x
            sta (ZPtrA), y
            dey
            cpy ShoreR
            bne bmlandr
            ; make the shoreline wander
            ; check to see if we're already narrow
            lda ShoreR
            sec
            sbc ShoreL
            cmp #$04        ; if shore edges are at least 4 tiles apart, wander
            bcc bmlwander
            lda #<-1         ; otherwise set shore velocity to diverge (widen)
            sta ShoreLV
            lda #1
            sta ShoreRV
            bne bmaddvel
bmlwander:  lda ShoreLV     ; far enough apart to let the shores wander
            cmp #<-2
            beq bmlplus     ; if LV is already -2, bring it back to -1
            cmp #2
            beq bmlminus    ; if LV is already 2, bring it back to 1
            ; move the left shore
            ldx Seed        ; otherwise randomly increase or decrease LV
            inc Seed
            lda Random, x   ; change velocity? y/n
            bmi bmrwander   ; branch away if no
            inx
            inc Seed
            lda Random, x
            bpl bmlplus
bmlminus:   dec ShoreLV
            jmp bmrwander
bmlplus:    inc ShoreLV
bmrwander:  lda ShoreRV
            cmp #<-2
            beq bmrplus     ; if RV is already -2, bring it back to -1
            cmp #2
            beq bmrminus    ; if RV is already 2, bring it back to 1
            ; move the right shore
            ldx Seed        ; otherwise randomly increase or decrease RV
            inc Seed
            lda Random, x   ; change velocity? y/n
            bmi bmaddvel    ; branch away if no
            inx
            inc Seed
            lda Random, x   
            bpl bmrplus
bmrminus:   dec ShoreRV
            jmp bmaddvel
bmrplus:    inc ShoreRV
            ; add shore velocity to shore
bmaddvel:   lda ShoreL
            clc
            adc ShoreLV
            bpl bmlok
            lda #$01        ; we ran off the left side, turn velocity inward
            sta ShoreLV
            lda #$00        ; keep the shore on the left edge of the screen
bmlok:      sta ShoreL
            lda ShoreR
            clc
            adc ShoreRV
            cmp #20
            bcc bmrok
            lda #<-1        ; we ran off the right side, turn velocity inward
            sta ShoreRV
            lda #19         ; keep the shore on the right edge of the screen
bmrok:      sta ShoreR
            ; save the current line's map start and move to the next line
            ldx MapLine
            lda ZPtrA
            sta MapBaseL, x
            clc
            adc #20
            sta ZPtrA
            lda ZPtrA + 1
            sta MapBaseH, x
            adc #0
            sta ZPtrA + 1
            inc MapLine
            beq bmdone   ; branch away if we have done 256 map lines
            jmp bmmapline   ; otherwise go do the next one
            ; the shores are now done
bmdone:
            rts
