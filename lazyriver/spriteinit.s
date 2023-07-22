; lazyriver
; initialize sprites

SprPlayer   = $7F           ; sprite number for player

LogsLeft:   .byte 0         ; logs left to place

; now place logs
spriteinit:
            ; Start by initializing memory pointers for sprite tracking.
            ; there is a capacity for 128 sprites
            ldx #$03            ; $300
            lda #$00
            sta ZSprX
            stx ZSprX + 1
            lda #$80
            sta ZSprY
            stx ZSprY + 1
            inx                 ; $400
            lda #$00
            sta ZSprXOff
            stx ZSprXOff + 1
            lda #$80
            sta ZSprYOff
            stx ZSprYOff + 1
            inx                 ; $500
            lda #$00
            sta ZSprXV
            stx ZSprXV + 1
            lda #$80
            sta ZSprYV
            stx ZSprYV + 1
            inx                 ; $600
            lda #$00
            sta ZSprType
            stx ZSprType + 1
            lda #$80
            sta ZSprTick
            stx ZSprTick + 1
            inx                 ; $700
            lda #$00
            sta ZSprAnim
            stx ZSprAnim + 1
            lda #$80
            sta ZSprPeriod
            stx ZSprPeriod + 1
            inx                 ; $800
            lda #$00
            sta ZSprDrXOne
            stx ZSprDrXOne + 1
            lda #$80
            sta ZSprDrYOne
            stx ZSprDrYOne + 1
            inx                 ; $900
            lda #$00
            sta ZSprDrXTwo
            stx ZSprDrXTwo + 1
            lda #$80
            sta ZSprDrYTwo
            stx ZSprDrYTwo + 1
            inx                 ; $A00
            lda #$00
            sta ZSprBgL
            stx ZSprBgL + 1
            lda #$80
            sta ZSprBgH
            stx ZSprBgH + 1
            inx                 ; $B00
            lda #$00
            sta ZSprSprH
            stx ZSprSprH + 1
            lda #$80            ; TODO - remove
            sta ZSprDelay
            stx ZSprDelay + 1
            inx                 ; $C00
            lda #$00
            sta ZPrevX
            stx ZPrevX + 1
            lda #$80
            sta ZPrevY
            stx ZPrevY + 1
            inx                 ; $D00
            lda #$00
            sta ZPrevXOff
            stx ZPrevXOff + 1
            lda #$80
            sta ZPrevYOff
            stx ZPrevYOff + 1
            inx                 ; $E00
            lda #$00
            sta ZSprCollH
            stx ZSprCollH + 1
            lda #$80
            sta ZSprCollL
            stx ZSprCollL + 1
            inx                 ; $F00
            lda #$00            ; TODO - remove
            sta ZSprMvTick
            stx ZSprMvTick + 1
            lda #$80
            sta ZSprColChk
            stx ZSprColChk + 1
            lda #$82            ; bank 2
            sta ZSprX + XByte
            sta ZSprY + XByte
            sta ZSprXOff + XByte
            sta ZSprYOff + XByte
            sta ZSprXV + XByte
            sta ZSprYV + XByte
            sta ZSprType + XByte
            sta ZSprTick + XByte
            sta ZSprAnim + XByte
            sta ZSprPeriod + XByte
            sta ZSprDrXOne + XByte
            sta ZSprDrYOne + XByte
            sta ZSprDrXTwo + XByte
            sta ZSprDrYTwo + XByte
            sta ZSprBgL + XByte
            sta ZSprBgH + XByte
            sta ZSprSprH + XByte
            sta ZSprDelay + XByte
            sta ZSprMvTick + XByte
            sta ZPrevX + XByte
            sta ZPrevY + XByte
            sta ZPrevXOff + XByte
            sta ZPrevYOff + XByte
            sta ZSprCollH + XByte
            sta ZSprCollL + XByte
            sta ZSprColChk + XByte

            ; place some logs
            lda NumLogs
            sta LogsLeft            ; 0-based, must have at least one log
            
placelog:   ldy Seed                ; pick a map row
            inc Seed
            lda Random, y
            and #%00001111          ; in the last 16 rows
            clc
            adc #239
            tax
            lda MapLineL, x
            sta ZMapPtr
            lda MapLineH, x
            sta ZMapPtr + 1
            iny                     ; pick an x-coordinate
            inc Seed
            lda Random, y
            and #%00001111          ; between 2 and 17
            clc
            adc #$02
            tay
:           lda (ZMapPtr), y
            and #%00001000          ; is it water?
            bne :+                  ; branch away if it is water
            dey                     ; search this row to the left to find water
            bne :-
            beq placelog            ; branch always; didn't find water, pick a new spot
:           tya
            ldy LogsLeft            ; this is the sprite number
            sta (ZSprX), y
            txa                     ; x still holds the map row
            sta (ZSprY), y
            ldx Seed                ; pick a start frame
            inc Seed
            lda Random, x
            and #$01                ; between 0 and 1
            sta (ZSprAnim), y
            inx                     ; pick a movement delay (speed control)
            inc Seed
            lda Random, x
            and #$03                ; between 0 and 3
            clc
            adc #$02                ; well, between 2 and 5
            lda #$00                ; well, actually, just zero
            sta (ZSprDelay), y
            inx                     ; pick a animation period
            inc Seed
            lda Random, x
            and #$03                ; between 0 and 3
            clc
            adc #$05                ; well, between 5 and 8
            sta (ZSprPeriod), y
            inx                     ; pick a log type
            inc Seed
            lda Random, x
            and #$03                ; between 0 and 3
            jsr sprfinish           ; fill in the rest of the sprite variables
            
            dec LogsLeft
            bmi bmlogsdone
            jmp placelog

            ; place the player
bmlogsdone: ldy #SprPlayer          ; player sprite
            lda #3                  ; on the left side
            sta (ZSprX), y
            lda #250                ; near the bottom of the map
            sta (ZSprY), y
            lda #02                 ; animate every 3 frames
            sta (ZSprPeriod), y
            lda #$00                ; start on frame 0
            sta (ZSprAnim), y
            lda #S_PLAYER           ; sprite type
            jsr sprfinish           ; fill in the rest of the easy variables
            
            ; now done placing logs and player
            

            ; TODO - and add collision detection that can stop a log if it hits something.
            ; TODO - idea would be that touching something (bank, log, player)
            ; TODO - induces friction.  Player can bang into a log.
            rts

; finish the sprite after most of the definition was filled in
; sets SprH, zeros XV, YV ,YOff, Tick, marks as undrawn, computes BgL/H
; common code between logs and player
; enter with A being the sprite type and y being the sprite number
sprfinish:  sta (ZSprType), y       ; A=sprite type
            asl
            asl
            asl                     ; x 8
            sec
            sbc (ZSprType), y       ; minus sprite type (offset into data = type x 7)
            clc
            adc #$15                ; add to overall sprite data start ($1500)
            sta (ZSprSprH), y
            lda (ZSprType), y       ; recall sprite type (to find collision masks)
            and #$01                ; it sprite even or odd?
            lsr
            ror
            ror                     ; even starts at $00, odd starts at $40
            sta (ZSprCollL), y
            lda (ZSprType), y       ; recall sprite type (to find collision masks)
            lsr                     ; int(sprite/2)
            sta (ZSprCollH), y
            asl
            asl
            asl                     ; 8 * int(sprite/2)
            sec
            sbc (ZSprCollH), y      ; minus int(sprite/2) (making 7*int(sprite/2))
            clc
            adc #$59                ; collision mask base is $5900
            sta (ZSprCollH), y
            lda #$00
            sta (ZSprXV), y         ; x velocity
            sta (ZSprYV), y         ; y velocity
            sta (ZSprXOff), y       ; x offset from tile edge
            sta (ZSprYOff), y       ; y offset from tile edge
            sta (ZSprTick), y       ; current animation tick
            sta (ZSprMvTick), y     ; current movement tick
            sta (ZPrevX), y         ; previous X
            sta (ZPrevY), y         ; previous Y
            sta (ZPrevXOff), y      ; previous X offset
            sta (ZPrevYOff), y      ; previous Y offset
            lda #$FF
            sta (ZSprDrXOne), y     ; mark as undrawn
            sta (ZSprDrXTwo), y     ; mark as undrawn
            ; compute cache address (bank 2 from $1000)
            ; should be $1000 + int(sprite / 4) * $100 + (sprite % 4) * $40
            ; this will run from $1000-2FFF, and for page 2, add $2000 ($3000-4FFF)
            ; (takes $40 bytes to cache two tiles, 8 bytes by 8 lines)
            ; (internally, first $20 are A bytes, second $20 are B bytes)
            tya
            and #$03                ; sprite number mod 4
            lsr
            ror
            ror                     ; x $40 (low byte of cache address)
            sta (ZSprBgL), y
            tya
            lsr
            lsr                     ; int( sprite number / 4 )
            clc
            adc #$10                
            sta (ZSprBgH), y        ; high byte of A page 1, e.g., $10
            rts
