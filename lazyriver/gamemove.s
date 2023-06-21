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
            ; move hero
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
            ; TODO - check for collision
            ; RODO - move logs
dmdone:     rts
