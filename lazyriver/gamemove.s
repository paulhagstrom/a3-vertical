; lazyriver
; movement processing

domove:     lda ShownPage
            eor #$01            ; focus on nondisplayed page (starts in sync with displayed)
            and #$01
            tax                 ; x is 0 if we are not looking at page 1, 1 if page 2
            ; move ground
            lda GroundVel
            beq dmgndstay       ; branch if not moving vertically
            bmi dmgnddown       ; branch if ground down, offset decreasing
            ; ground scrolls up, offset increasing
            lda #$01            ; map will be scrolling up, adding to offset
            ldy PgOneTop, x     ; check to see if we are at the bottom
            cpy #231            ; last possible top row?
            bne dmgndmove       ; no, so proceed
            ldy PgOneOff, x     ; yes, last possible top row
            cpy #$07            ; last possible offset?
            bcs dmgndstay       ; if at the very bottom, do not move
            bcc dmgndmove       ; branch always - otherwise, move
            ; ground scrolls down, offset decreasing
dmgnddown:  lda #$FF            ; map will be scrolling down, subtracting from offset
            ldy PgOneTop, x     ; check to see if we are at the top
            bne dmgndmove       ; if not at top map line, up is for sure ok
            ldy PgOneOff, x     ; in top map line, at top offset?
            beq dmgndstay       ; if at the very top, do not move
dmgndmove:  sta NeedScroll      ; 0=stop, neg=map down/dec off, pos=map up/inc off
dmgndstay:
            ; move player
            lda VelocityY
            beq dmheronov       ; branch if not moving vertically
            bmi dmherovup       ; branch if hero moving up
            ; hero moving down
            lda PlayerY
            clc
            adc VelocityY
            cmp #185            ; are we as low as we can be?
            bcs dmheronov       ; branch away if we can't move down
            sta PlayerY         ; move down
            bne dmheronov
            ; hero moving up
dmherovup:  lda PlayerY
            clc
            adc VelocityY
            cmp #$09
            bcc dmheronov       ; branch away if we can't move up
            sta PlayerY         ; move up
            ; check horizontal movement
dmheronov:  lda VelocityX
            beq dmheronoh       ; branch if not moving horizontally
            bmi dmheroleft      ; branch if hero moving left
            ; hero moving right
            lda PlayerXOff
            clc
            adc VelocityX
            cmp #$07
            bcc dmherorok
            sec
            sbc #$07
            ldy PlayerX
            cpy #18
            bcs dmheronoh
            inc PlayerX
dmherorok:  sta PlayerXOff
            jmp dmheronoh
            ; hero moving left
dmheroleft: lda PlayerXOff
            clc
            adc VelocityX
            bpl dmherolok
            clc
            adc #$07
            ldy PlayerX
            beq dmheronoh
            dec PlayerX
dmherolok:  sta PlayerXOff
dmheronoh:
            ; move logs
            ldy NumLogs
            sty ZCurrSpr
            ; animate sprite (alternate frames)
            dec (ZSprTick), y
            bpl :+              ; not time to switch frames yet
            lda (ZSprPeriod), y ; reset tick timer
            sta (ZSprTick), y
            lda (ZSprAnim), y   ; switch frames
            eor #$01
            sta (ZSprAnim), y
            ; retrieve flow vector from map tile this sprite starts on
:           lda (ZSprY), y      ; find map line
            tax
            lda (ZSprX), y      ; find map column
            tay
            lda MapLineL, x
            sta ZMapPtr
            lda MapLineH, x
            sta ZMapPtr + 1
            lda (ZMapPtr), y    ; load map data
            sta ZMapTmp
            and #%11000000      ; y flow bits
            asl
            rol
            rol                 ; y flow speed is 0-3
            eor #$FF            ; invert
            adc #$01            ; carry known to be clear
            sta ZYFlow          ; y flow is between -3 and 0
            lda ZMapTmp         ; convert 3-bit signed to 7-bit signed
            and #%00100000      ; x flow bits sign bit
            beq :+
            lda #%11111100
:           sta ZXFlow
            lda ZMapTmp
            and #%00011000      ; x flow bits without sign bit
            lsr
            lsr
            lsr
            ora ZXFlow
            sta ZXFlow          ; x flow speed is -3 to +3
            ldy ZCurrSpr
            ; update velocity based on flow vector
            lda (ZSprYV), y
            sec
            sbc ZYFlow          ; YOU ARE HERE AND MATH IS HARD
            beq :++             ; y velocity already matches flow speed
            bcs :+              ; y velocity is more positive than flow speed
            inc (ZSprYV), y     ; increase Y velocity toward flow speed
            jmp :++
:           dec (ZSprYV), y     ; decrease Y velocity toward flow speed
:           lda (ZSprXV), y
            cmp ZXFlow
            beq :++             ; x velocity already matches flow speed
            bcs :+              ; x velocity is more positive than flow speed
            inc (ZSprYV), y     ; increase X velocity toward flow speed
            jmp :++
:           dec (ZSprYV), y     ; decrease X velocity toward flow speed
            ; attempt to move the sprite according to its velocity vector
:           lda (ZSprXOff), y
            clc
            adc (ZSprXV), y
            bmi :+              ; branch away if offset wrapped moving leftward
            cmp #$07
            bcc :+              ; branch away if offset wrapped moving rightward
            bcs :+              ; branch away if offset did not wrap
            
            
dmdone:     rts
