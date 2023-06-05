; lazyriver
; movement processing

domove:     ; TODO - move logs
            ; hero is stationary on screen, Y only moves map
            ; TODO - allow hero to move vertically too
            lda VelocityY
            beq dmherostay      ; branch if not moving vertically
            bmi dmheroup        ; branch if moving up toward top of map
            ; moving down toward bottom of map
            ldy MapTop          ; check to see if we are at the bottom
            cpy #231            ; last possible maptop?
            bne dmherodnok
            ldy MapOff          ; last possible offset?
            cpy #$07
            bcs dmherostay      ; if at the very bottom, do not move
dmherodnok: sta NeedScroll
            bpl dmdone          ; branch always
dmheroup:   ldy MapTop          ; check to see if we are at the top
            bne dmheroupok      ; if not at top map line, up is for sure ok
            ldy MapOff          ; in top map line, at top offset?
            beq dmherostay      ; if at the very top, do not move
dmheroupok: sta NeedScroll
dmherostay: ; TODO - allow hero to move horizontally too
            ; TODO - check for collision
dmdone:     rts
