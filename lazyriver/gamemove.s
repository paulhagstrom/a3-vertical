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
            ldy #127
            jsr ticksprite
            ; do not subject the player to the flow vectors or shore collisions
            jsr movesprite
            jsr collsprite
            jsr spriteupd
            ; move logs
            ldy NumLogs
dmmovelog:  jsr ticksprite
            jsr flowsprite
            jsr movesprite
            jsr collshore
            jsr collsprite
            jsr spriteupd
            ; it is known that ticksprite saves y in ZCurrSpr
            ldy ZCurrSpr
            dey
            bpl dmmovelog
            
            rts

; tick animation for a single sprite
; enter with Y holding the sprite number
ticksprite: sty ZCurrSpr
            lda (ZSprY), y      ; set ZOldY with map row
            sta ZOldY
            lda (ZSprX), y      ; set ZOldX with map column
            sta ZOldX
            rts
            lda (ZSprTick), y   ; decrease tick
            sec
            sbc #$01
            sta (ZSprTick), y
            bpl :+              ; not time to switch frames yet
            lda (ZSprPeriod), y ; reset tick timer
            sta (ZSprTick), y
            lda (ZSprAnim), y   ; switch frames
            eor #$01
            sta (ZSprAnim), y
:           rts

; update sprite's velocity based on flow vector of the tile it is in
; assumes ZCurrSpr hold the sprite number (after ticksprite)
flowsprite: ldx ZOldY           ; find map line
            ldy ZOldX           ; find map column
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
            sbc ZYFlow
            beq logcheckxv      ; y velocity already matches flow speed
            bvc :+
            eor #$80
:           bmi :+              ; branch if y vel is more negative than flow speed
            lda (ZSprYV), y     ; decrease Y velocity toward flow speed
            sec
            sbc #$01
            sta (ZSprYV), y
            jmp logcheckxv
:           lda (ZSprYV), y     ; increase Y velocity toward flow speed
            clc
            adc #$01
            sta (ZSprYV), y
logcheckxv: lda (ZSprXV), y
            sec
            sbc ZXFlow
            beq flowdone       ; x velocity already matches flow speed
            bvc :+
            eor #$80
:           bmi :+              ; branch if x vel is more negative than flow speed
            lda (ZSprYV), y     ; decrease X velocity toward flow speed
            sec
            sbc #$01
            sta (ZSprYV), y
            jmp flowdone
:           lda (ZSprYV), y     ; increase X velocity toward flow speed
            clc
            adc #$01
            sta (ZSprYV), y
flowdone:   rts

; attempt to move the sprite according to its velocity vector
; assumes y and ZCurrSpr hold the sprite number (after ticksprite)

movesprite: lda (ZSprXV), y
            bmi loggoleft       ; branch if moving left
            clc                 ; moving right (or stationary), add to x offset
            adc (ZSprXOff), y
            cmp #$07
            bcc hjustoff        ; branch if still within the same tile
            sbc #$07            ; wrap offset (carry known to be set)
            sta ZNewXOff
            lda ZOldX
            cmp #18
            bcc :+              ; branch if we can move further right
            sta ZNewX           ; stay in the same place
            lda #$06            ; stop at offset 6
            sta ZNewXOff
            bne logvert         ; branch always
:           adc #$01            ; we can move right, inc X (carry known to be clear)
            sta ZNewX
            bne logvert         ; branch always
hjustoff:   sta ZNewXOff
            lda ZOldX
            sta ZNewX
            jmp logvert
loggoleft:  clc                 ; log is moving left
            adc (ZSprXOff), y
            bpl hjustoff        ; branch if still within the same tile
            clc
            adc #$07            ; wrap offset
            sta ZNewXOff
            lda ZOldX
            cmp #$01
            bcs :+              ; branch if we can move further left
            sta ZNewX           ; stay in the same place
            lda #$00            ; stop at offset 0
            sta ZNewXOff
            beq logvert         ; branch always
:           sbc #$01            ; carry is known to be set, decrease X
            sta ZNewX
logvert:    
            lda (ZSprYV), y
            bmi loggoup         ; branch if moving up
            clc                 ; moving down, add to y offset
            adc (ZSprYOff), y
            cmp #$08
            bcc vjustoff        ; branch if still within the same tile
            and #$07            ; wrap offset
            sta ZNewYOff
            lda ZOldY
            cmp #254
            bcc :+              ; branch if we can move further down
            sta ZNewY           ; stay in the same place
            lda #$07            ; stop at offset 7
            sta ZNewYOff
            bne mvsprdone       ; branch always
:           adc #$01            ; we can move down, inc Y (carry known to be clear)
            sta ZNewY
            bne mvsprdone       ; branch always
vjustoff:   sta ZNewYOff
            lda ZOldY
            sta ZNewY
            jmp mvsprdone
loggoup:    clc                 ; log is moving up
            adc (ZSprYOff), y
            bpl vjustoff        ; branch if still within the same tile
            and #$07            ; wrap offset
            sta ZNewYOff
            lda ZOldY
            cmp #$01
            bcs :+              ; branch if we can move further up
            sta ZNewY           ; stay in the same place
            lda #$00            ; stop at offset 0
            sta ZNewYOff
            beq mvsprdone       ; branch always
:           sbc #$01            ; carry is known to be set, decrease Y
            sta ZNewY
mvsprdone:  rts

            ; ZNewX, ZNewXOff, ZNewY, ZNewYoff now hold the proposed new position
            ; now we need to check to see if the new position is blocked
            ; two things to check: shoreline and other sprites
            ; to check shoreline, we can basically check the map for whether
            ; ZNewX, ZNewY is on land.  If it is, we stop velocity and do not
            ; move.  This could make a log stop slightly short of shore, though.
            ; flow should get it moving again.  Want to allow it to move, e.g., in X
            ; direction even if it can't move in Y direction.

; Check collision with the shoreline and stop (possibly in one direction)
; if the sprite hits it.
; assumes that ZCurrSpr is set to the current sprite
collshore:  ldx ZNewY
            ldy ZNewX
            lda MapLineL, x
            sta ZMapPtr
            lda MapLineH, x
            sta ZMapPtr + 1
            lda (ZMapPtr), y    ; load map data
            and #$04            ; check water bit
            bne shoredone       ; branch away if movement lands in water, on to sprite check
            ; desired movement failed, check if moving just in Y direction would work
            ldy ZOldX
            lda (ZMapPtr), y    ; load map data
            and #$04            ; check water bit
            clc                 ; tell logshyok block to only zero X
            bne logshyok        ; branch away if vertical move lands in water
            ; moving just vertically fails, check if moving just in X direction works
            ldy ZNewX
            ldx ZOldY
            lda MapLineL, x
            sta ZMapPtr
            lda MapLineH, x
            sta ZMapPtr + 1
            lda (ZMapPtr), y    ; load map data
            and #$04            ; check water bit
            bne logshxok        ; branch away if vertical move lands in water
            ; no movement worked, so zero out velocity and stay here.
            sec                 ; tell following block to zero both X and Y
            ; shore impedes horizontal movement only
logshyok:   ldy ZCurrSpr
            lda #$00
            sta (ZSprXV), y
            lda ZOldX           ; propose that X not move
            sta ZNewX
            lda (ZSprXOff), y
            sta ZNewXOff
            bcc shoredone       ; jump to rts if we only zero X
            ; shore impedes vertical movement only
logshxok:   ldy ZCurrSpr
            lda #$00
            sta (ZSprYV), y
            lda ZOldY           ; propose that Y not move
            sta ZNewY
            lda (ZSprYOff), y
            sta ZNewYOff        ; fall through to rts
shoredone:  rts

; check if this sprite collides with other sprites
; we are here only if movement didn't get blocked by a shore already
; for checking against sprites, it is going to be more elaborate.
; may require checking each sprite against all others.
; for each sprite A
; for each sprite B
; if masks cannot overlap, skip ahead
; AND through the area of sprite overlap, if anything is nonzero, collision
; some delicate math to determine these "overlap" conditions
collsprite: rts

; update the sprite's coordinates based on what survived of the proposal
; assumes ZCurrSpr is set to current sprite number
spriteupd:  ldy ZCurrSpr
            lda ZNewX
            sta (ZSprX), y
            lda ZNewY
            sta (ZSprY), y
            lda ZNewXOff
            sta (ZSprXOff), y
            lda ZNewYOff
            sta (ZSprYOff), y
            rts
