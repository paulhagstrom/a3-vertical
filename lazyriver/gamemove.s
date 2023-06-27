; lazyriver
; movement processing

domove:     lda ShownPage
            eor #$01            ; focus on nondisplayed page (starts in sync with displayed)
            and #$01
            tax                 ; x is 0 if we are not looking at page 1, 1 if page 2
            ; move ground
            lda GroundVel
            beq dmplayer        ; branch if not moving vertically
            bmi dmgnddown       ; branch if ground down, offset decreasing
            ; ground scrolls up, offset increasing
            lda #$01            ; map will be scrolling up, adding to offset
            ldy PgOneTop, x     ; check to see if we are at the bottom
            cpy #231            ; last possible top row?
            bne dmgndmove       ; no, so proceed
            ldy PgOneOff, x     ; yes, last possible top row
            cpy #$07            ; last possible offset?
            bcs dmplayer        ; if at the very bottom, do not move
            bcc dmgndmove       ; branch always - otherwise, move
            ; ground scrolls down, offset decreasing
dmgnddown:  lda #$FF            ; map will be scrolling down, subtracting from offset
            ldy PgOneTop, x     ; check to see if we are at the top
            bne dmgndmove       ; if not at top map line, up is for sure ok
            ldy PgOneOff, x     ; in top map line, at top offset?
            beq dmplayer        ; if at the very top, do not move
dmgndmove:  sta NeedScroll      ; 0=stop, neg=map down/dec off, pos=map up/inc off
            ; move player
dmplayer:   ldy #127
            jsr ticksprite
            ; do not subject the player to the flow vectors or shore collisions
            jsr movesprite
            jsr collsprite
            jsr spriteupd
            ; move logs
            ldy NumLogs         ; this is the sprite number
dmmovelog:  jsr ticksprite
            lda (ZSprTick), y   ; only move on tick - TODO: temporary, implement speed
            bne :+
            jsr flowsprite
            ; TODO consider adding a random wobble to flow
            jsr movesprite
            jsr collshore
            jsr collsprite
            jsr spriteupd
:           dey
            bpl dmmovelog
            
            rts

; tick animation for a single sprite and put sprite number in ZCurrSpr
; enter with Y holding the sprite number, y continues to hold sprite number after
ticksprite: sty ZCurrSpr
            lda (ZSprY), y      ; remember original map row in ZOldY
            sta ZOldY
            lda (ZSprX), y      ; remember original map column in ZOldX
            sta ZOldX
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
; ticksprite must be called first to set ZCurrSpr, ZOldY, ZOldX
; presume that the sprite is always in the water (land has no flow value)
; exits with y holding current sprite number
flowsprite: ldx ZOldY           ; find map line
            ldy ZOldX           ; find map column
            lda MapLineL, x
            sta ZMapPtr
            lda MapLineH, x
            sta ZMapPtr + 1
            lda (ZMapPtr), y    ; load map data
            sta ZMapTmp
            and #%11000000      ; y flow bits
            asl                 ; move to lower two bits (0-3)
            rol
            rol
            eor #$FF            ; invert
            adc #$01            ; carry known to be clear
            sta ZYFlow          ; y flow is between -3 and 0
            lda ZMapTmp         ; get x flow bits
            and #%00111000
            lsr                 ; move to lower three bits (0-6)
            lsr
            lsr
            sec                 ; subtract 3 to get x flow -3 to +3
            sbc #$03
            sta ZXFlow          ; x flow is between -3 and +3
            ; update velocity based on flow vector
            ldy ZCurrSpr
            lda (ZSprYV), y
            sec
            sbc ZYFlow          ; signed comparison of YV and YFlow
            beq logcheckxv      ; y velocity already matches flow speed
            bvc :+
            eor #$80
:           bmi :+              ; branch if YV is more negative than flow speed
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
            sbc ZXFlow          ; signed comparison of XV and XFlow
            beq flowdone        ; x velocity already matches flow speed
            bvc :+
            eor #$80
:           bmi :+              ; branch if XV is more negative than flow speed
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
; enter with y holding the sprite number, exits with y still holding that
; does not disturb x
movesprite: lda (ZSprXV), y     ; handle movement in X coordinate
            bmi msgoleft        ; branch away if moving left
            clc                 ; moving right (or stationary), add to x offset
            adc (ZSprXOff), y
            cmp #$07
            bcc hjustoff        ; branch away if still within the same tile (offset < 7)
            sbc #$07            ; wrap offset (carry known to be set)
            sta ZNewXOff
            lda ZOldX           ; should move to next tile to the right
            cmp #18             ; if we are not already as far right as we can go
            bcc :+              ; branch away if we can move further right (X < 18)
            sta ZNewX           ; stay at the rightmost tile
            lda #$06            ; advance to offset 6 (in case we were not already there)
            sta ZNewXOff
            bne msgovert        ; branch always
:           adc #$01            ; we can move right, inc X (carry known to be clear)
            sta ZNewX
            bne msgovert        ; branch always
hjustoff:   sta ZNewXOff        ; moving changes only X offxet
            lda ZOldX           ; keep X tile in the same place
            sta ZNewX
            jmp msgovert
msgoleft:   clc                 ; log is moving left
            adc (ZSprXOff), y   ; adding a negative number to offset
            bpl hjustoff        ; branch away if still within the same tile (offset >= 0)
            clc
            adc #$07            ; wrap offset
            sta ZNewXOff
            lda ZOldX           ; should move to the next tile to the left
            bne :+              ; branch away if we can move further left
            sta ZNewX           ; stay in the same place
            lda #$00            ; stop at offset 0
            sta ZNewXOff
            beq msgovert        ; branch always
:           sbc #$01            ; we can move left, dec X (carry is known to be set)
            sta ZNewX
msgovert:   lda (ZSprYV), y     ; handle movement in Y coordinate
            bmi msgoup          ; branch away if moving up
            clc                 ; moving down (or staying stationary), add to y offset
            adc (ZSprYOff), y
            cmp #$08
            bcc vjustoff        ; branch away if still within the same tile (offset < 8)
            and #$07            ; wrap offset
            sta ZNewYOff
            lda ZOldY           ; should move to next tile down
            cmp #254            ; if we are not already as far down as we can go
            bcc :+              ; branch away if we can move further down
            sta ZNewY           ; stay in the same place
            lda #$07            ; stop at offset 7 (in case we were not already there)
            sta ZNewYOff
            rts                 ; done
:           adc #$01            ; we can move down, inc Y (carry known to be clear)
            sta ZNewY
            rts                 ; done
vjustoff:   sta ZNewYOff        ; moving changes only Y offset
            lda ZOldY           ; keep Y tile in the same place
            sta ZNewY
            rts                 ; done
msgoup:     clc                 ; sprite is moving up
            adc (ZSprYOff), y   ; adding a negative number to offset
            bpl vjustoff        ; branch if still within the same tile (offset >= 0)
            and #$07            ; wrap offset
            sta ZNewYOff
            lda ZOldY           ; should move to the next tile up
            bne :+              ; branch away if we can move further up
            sta ZNewY           ; stay in the same place
            lda #$00            ; stop at offset 0 (in case we were not already there)
            sta ZNewYOff
            rts                 ; done
:           sec
            sbc #$01            ; decrease Y
            sta ZNewY
            rts                 ; done

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
; ticksprite must be called first (to set ZCurrSpr, ZOldX, ZOldY)
; movesprite must be called first (to set ZNewX, ZNewY, ZNewXOff, ZNewYOff)
collshore:  ldx ZNewY           ; load map byte for proposed new position
            ldy ZNewX
            lda MapLineL, x
            sta ZMapPtr
            lda MapLineH, x
            sta ZMapPtr + 1
            lda (ZMapPtr), y
            and #$04            ; check water bit
            bne shoredone       ; done, movement lands in water, it can proceed
            ; desired movement failed, check if moving just in Y direction would work
            ldy ZOldX           ; check map data for original column
            lda (ZMapPtr), y    ; load map data
            and #$04            ; check water bit
            clc                 ; tell logshyok block to only zero X
            bne logshyok        ; branch away if vertical move lands in water
            ; moving just vertically fails, check if moving just in X direction works
            ldy ZNewX           ; load map byte for new X, old Y
            ldx ZOldY
            lda MapLineL, x
            sta ZMapPtr
            lda MapLineH, x
            sta ZMapPtr + 1
            lda (ZMapPtr), y
            and #$04            ; check water bit
            bne logshxok        ; branch away if horizontal move lands in water
            ; no movement worked, so zero out velocity and stay here.
            sec                 ; tell following block to zero both X and Y
            ; shore impedes horizontal movement only
logshyok:   ldy ZCurrSpr
            lda #$00            ; zero out X velocity
            sta (ZSprXV), y
            lda ZOldX           ; propose that X not move
            sta ZNewX
            lda (ZSprXOff), y
            sta ZNewXOff
            bcc shoredone       ; branch away to rts if we are only zeroing X
            ; shore impedes vertical movement only
logshxok:   ldy ZCurrSpr
            lda #$00            ; zero out Y velocity
            sta (ZSprYV), y
            lda ZOldY           ; propose that Y not move
            sta ZNewY
            lda (ZSprYOff), y
            sta ZNewYOff
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
; ticksprite must be called first (to set ZCurrSpr)
; movesprite must be called first (to set ZNewX, ZNewXOff, ZNewY, ZNewYOff)
; returns with current sprite in Y
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
