; logdrive
; map construction and object placement

; Map data lives in bank 2, from $2000-5FFF
; Each byte represents a 7x8 tile (screen tiles 20x24)
; Element tracking variables are from $300-AFF.
; 
; builds a river with a somewhat random edge
; then places some islands and logs

TileLand:   .byte   C_LAND_A, C_LAND_B, C_LAND_C, C_LAND_D
TileWater:  .byte   C_WATER_A, C_WATER_B, C_WATER_C, C_WATER_D
ShoreL:     .byte 0
ShoreR:     .byte 0
ShoreLV:    .byte 0
ShoreRV:    .byte 0
MapLine:    .byte 0
MapIslands: .byte 0
LogTotal:   .byte 0
LogCurrent: .byte 0

buildmap:   
            ; Start by initializing memory pointers for location tracking.
            ; main game elements are logs, of which there is capacity for 256
            ; LogX = 300, LogY = 400, LogXV = 500, LogYV = 600
            ; LogType = 700, LogTick = 800, LogAnim = 900, LogPeriod = A00
            lda #$00
            ldx #$03
            sta ZLogX
            stx ZLogX + 1
            inx
            sta ZLogY
            stx ZLogY + 1
            inx
            sta ZLogXV
            stx ZLogXV + 1
            inx
            sta ZLogYV
            stx ZLogYV + 1
            inx
            sta ZLogType
            stx ZLogType + 1
            inx
            sta ZLogTick
            stx ZLogTick + 1
            inx
            sta ZLogAnim
            stx ZLogAnim + 1
            inx
            sta ZLogPeriod
            stx ZLogPeriod + 1
            lda #$82        ; bank 2
            sta ZLogX + XByte
            sta ZLogY + XByte
            sta ZLogXV + XByte
            sta ZLogYV + XByte
            sta ZLogType + XByte
            sta ZLogTick + XByte
            sta ZLogAnim + XByte
            ; now start building the map.
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
            beq bmislands   ; branch away if we have done 256 map lines
            jmp bmmapline   ; otherwise go do the next one
            ; the shores are now done
            ; add in some islands in the middle
bmislands:  lda #10         ; for now, add 10 islands
            sta MapIslands
bmisloop:   ldx Seed
            lda Random, x
            sta MapLine
            inx
            lda Random, x
            inx
            stx Seed
            and #$07        ; start island in one of 8 positions (middle third)
            clc
            adc #$06        ; start 6 in, so x coord can be from 6 to 13
            tay
            ; for now just put a dot, but an island should be really just another shoreline
            ldx MapLine
            lda MapBaseL, x
            sta ZPtrA
            lda MapBaseH, x
            sta ZPtrA + 1
            ldx Seed
            inc Seed
            lda Random, x
            and #$03        ; random land type
            tax
            lda TileLand, x
            sta (ZPtrA), y
            dec MapIslands
            bpl bmislloop
            ; we have now drawn the shores and the islands.
            ; time to place the logs
            ; which until I do islands better is really just like placing islands
            lda #$10        ; place 16 logs for now
            sta LogTotal
            tay
bmlogloop:  ldx Seed
            lda Random, x
            sta (ZLogY), y
            inx
            lda Random, x
            and #$0F        ; start log in one of 15 positions
            clc
            adc #$02
            sta (ZLogX), y
            inx
            lda Random, x
            and #$81        ; this is not ideal
            sta (ZLogXV), y
            lda #$01
            sta (ZLogYV), y
            lda #$00
            sta (ZLogTick), y
            sta (ZLogAnim), y
            ; TODO - later allow for different log types, for now just the one
            sta (ZLogType), y
            inx
            lda Random, x
            and #$03
            sta (ZLogPeriod), y
            inx
            stx Seed
            ; TODO - check to see if it as least in the water
            dey
            bpl bmlogloop
            
            ; TODO - maybe evaluate each tile for how wide a waterway it is
            ; this can be used to compute current velocity.
            ; that is: logs in narrow waterways move faster vertically
            ; note that it may not be the same across a whole line if there is an island
            ; and, actually, as one approaches an island, there might be a pull toward
            ; the smaller one because it is faster.  So maybe each tile has a current vector
            ; and logs follow the current vectors plus maybe a little bit of wobble.
            ; so a log has a velocity and the current vector adds to it
            ; a log hitting another log will stop but transfer its velocity to the other log
            ; unless that log is immobile already.
            ; a log that hits a shore will stop until the current picks it up
            ; but will only start moving if the velocity takes it away from the shore
            ; you can push a log by bumping into it, maybe also send explosives in
            ; do logs move when you can't see them? They should. But you only need to draw
            ; the ones you can see.  So there will be a lot of movement computation to do.
            ; this is turning out not to be a simple game.
            ; maybe I should do a one-step simpler game where you just drive down a river
            ; without islands or logs but just scrolling and placing one (player) object
            
            rts