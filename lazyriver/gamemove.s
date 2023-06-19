; lazyriver
; movement processing

domove:     ; hero is stationary on screen, Y only moves map
            lda ShownPage
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
            cmp #183           ; are we as low as we can be?
            bcs dmheronov       ; branch away if we can't move down
            inc PlayerY         ; move down
            bne dmheronov
            ; hero moving up
dmherovup:  lda PlayerY
            beq dmheronov       ; branch away if we can't move up
            dec PlayerY         ; move up
            ; check horizontal movement
dmheronov:  lda VelocityX
            beq dmheronoh       ; branch if not moving horizontally
            bmi dmheroleft      ; branch if hero moving left
            ; hero moving right
            lda PlayerX
            cmp #18
            bcc dmherorok       ; at least a tile away from the edge, ok
            lda PlayerXOff
            cmp #$06
            beq dmheronoh       ; branch if we can't move right
dmherorok:  inc PlayerXOff
            lda PlayerXOff
            cmp #$07
            bcc :+
            lda #$00
            sta PlayerXOff
            inc PlayerX
:           jmp dmheronoh
            ; hero moving left
dmheroleft: lda PlayerX
            bne dmherolok
            lda PlayerXOff
            beq dmheronoh       ; branch if we can't move left
dmherolok:  dec PlayerXOff
            bpl dmheronoh
            lda #$06
            sta PlayerXOff
            dec PlayerX
dmheronoh:
            ; TODO - check for collision
            ; RODO - move logs
dmdone:     rts
