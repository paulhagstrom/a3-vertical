; lazyriver
; movement processing

domove:     ; TODO - move logs
            ; hero is stationary on screen, Y only moves map
            ; TODO - allow hero to move vertically too
            lda VelocityY
            beq dmherostay      ; branch if not moving vertically
            bmi dmheroup        ; branch if hero moving up, map scrolling down
            ; hero moving down (map scrolls up, top indicators increase)
            lda #$01            ; map will be scrolling up, adding to offset
            ldy TopRow          ; check to see if we are at the bottom
            cpy #231            ; last possible top row?
            bne dmheromove      ; no, so proceed
            ldy TopOff          ; yes, last possible top row
            cpy #$07            ; last possible offset?
            bcs dmherostay      ; if at the very bottom, do not move
            bcc dmheromove      ; otherwise, move
            ; hero moving up (map scrolls down, top indicators decrease)
dmheroup:   lda #$FF            ; map will be scrolling down, subtracting from offset
            ldy TopRow          ; check to see if we are at the top
            bne dmheromove      ; if not at top map line, up is for sure ok
            ldy TopOff          ; in top map line, at top offset?
            beq dmherostay      ; if at the very top, do not move
dmheromove: sta NeedScroll      ; 0=stop, neg=map down/dec off, pos=map up/inc off
dmherostay: ; TODO - allow hero to move horizontally too
            ; TODO - check for collision
dmdone:     rts
