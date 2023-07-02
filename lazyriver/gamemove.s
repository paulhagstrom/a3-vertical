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
dmplayer:   ldy #SprPlayer
            jsr ticksprite      ; stores Y in ZCurrSpr, leaves Y unchanged
            ; do not subject the player to the flow vectors or shore collisions
            jsr movesprite      ; enter with sprite in Y, exits with it still there
            jsr spriteupd       ; uses ZCurrSpr, exits with current sprite in Y
            ; move logs
            ldy NumLogs         ; this is the sprite number
dmmovelog:  jsr ticksprite      ; stores Y in ZCurrSpr, leaves Y unchanged
            lda (ZSprMvTick), y ; only move when delay countdown reaches zero
            bne :+
            jsr flowsprite      ; uses ZCurrSpr, exits with sprite in Y
            ; TODO consider adding a random wobble to flow
            jsr movesprite      ; enter with sprite in Y, exits with it still there
            jsr collshore       ; uses ZCurrSpr
            jsr spriteupd       ; uses ZCurrSpr, exits with current sprite in Y
:           dey
            bpl dmmovelog
            ; we want to check collisions post-movement,
            ; but we also want to check collisions all at once so that we
            ; can check each sprite pairing only once
            ; problem is what to do if there is a collision
            ; we can bounce their velocities rather than stop them but there
            ; will be a tick where they actually overlap
            ; if we decrease their velocities they'll stop while overlapped
            ; maybe this is ok for logs against each other
            ; hoping that this won't result in a log bouncing ashore
            ; probably want to treat collisions with player differently
            jsr checkcoll
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
:           lda (ZSprMvTick), y ; decrease movement countdown tick
            sec
            sbc #$01
            sta (ZSprMvTick), y
            bpl :+              ; not time to move yet
            lda (ZSprDelay), y  ; reset movement tick timer
            sta (ZSprMvTick), y
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
            lda (ZSprXV), y     ; decrease X velocity toward flow speed
            sec
            sbc #$01
            sta (ZSprXV), y
            jmp flowdone
:           lda (ZSprXV), y     ; increase X velocity toward flow speed
            clc
            adc #$01
            sta (ZSprXV), y
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
:           sec
            sbc #$01            ; we can move left, dec X
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
            lda (ZMapPtr), y    ; load map data for new Y, original X
            and #$04            ; check water bit
            clc                 ; tell logshyok block to only zero XV
            bne logshyok        ; branch away if vertical move lands in water
            ; moving just vertically fails, check if moving just in X direction works
            ldy ZNewX           ; load map byte for new X, original Y
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

; check the sprite collision matrix
; if we're checking each sprite against each other, we want to do this
; all at once so that we only check each pairing one time
; we will check against the player separately
; logic:
; for each sprite Ref
;   overlap window: top = Ref Y - 7, bottom = Ref Y + 7
;   overlap window: left = Ref X - 2, right = Ref X + 2
;   for each sprite Curr (from 0 to Ref)
;     skip ahead if:
;       Curr Y < overlap top or Curr Y > overlap bottom
;       Curr X < overlap left or Curr X > overlap right
;     if Curr X = Ref X
;       Curr X start = Curr X group 1
;       Curr X end = Curr X group 2
;       Ref X start = Ref X group 1
;       Ref X end = Ref X group 2
;     else if Curr X < Ref X
;       Curr X start and end = Curr X group 2
;       Ref X start and end = Ref X group 1
;     else (Ref X > Curr X)
;       Curr X start and end = Curr X group 1
;       Ref X start and end = Ref X group 2
;     Ref Y line = Ref Y * 8 + Ref Yoffset
;     Curr Y line = Curr Y * 8 + Cur Yoffset
;     Ydiff = Curr Y line - Ref Y line
;     if YDiff > 0 then Ref is higher than Cur
;       Ref Y start = YDiff
;       Curr Y start = Curr Y
;     if YDiff < 0 then Cur is higher than Ref
;       Ref Y start = Ref Y
;       Curr Y start = -YDiff
;     start at Curr Y start and Ref Y start
;       if Curr X start = Ref X start, collision = AND masks for first byte of each
;       if Curr X start = 2, collision = AND masks for Curr 2, Ref 1
;       if Ref X start = 2, collision = AND masks for Curr 1, Ref 2
;       increment current Curr Y, Ref Y
;       if one exceeds 7, we are done
;     handle collision here?
;     record perhaps
;   move to next Curr
; move to next Ref

checkcoll:  ldy NumLogs         ; this is the sprite number
            sty ZRefSpr
            sty ZCurrSpr
ccloop:     jsr collpair
            dec ZRefSpr
            beq :+
            ldy ZRefSpr
            sty ZCurrSpr
            jmp ccloop
:           ldy NumLogs
            sty ZCurrSpr
            ldy #SprPlayer      ; test the player against everything
            sty ZRefSpr
            jsr collpair
            rts

collskip:   jmp ccnext

collpair:   lda (ZSprY), y      ; establish the overlap window
            sta ZOldY
            sta ZOverYTop
            sta ZOverYBot
            dec ZOverYTop
            inc ZOverYBot       ; bottom is the first row outside
            inc ZOverYBot       ; overlap window
            lda (ZSprX), y
            sta ZOldX
            sta ZOverXLeft
            sta ZOverXRight
            dec ZOverXLeft
            inc ZOverXRight     ; right is the first column outside
            inc ZOverXRight     ; overlap window
            lda (ZSprYOff), y
            sta ZOldYOff
            lda (ZSprSprH), y   ; locate the reference mask
            clc
            adc (ZSprXOff), y
            sta ZPtrMaskA + 1
            sta ZPtrMaskB + 1
            lda (ZSprAnim), y
            lsr
            ror
            adc #$40            ; mask offset (carry known to be clear)
            sta ZPtrMaskA
            adc #$20
            sta ZPtrMaskB
cccurloop:  ldy ZCurrSpr
            lda (ZSprY), y
            cmp ZOverYTop
            bcc collskip        ; outside the overlap window
            cmp ZOverYBot
            bcs collskip        ; outside the overlap window
            sta ZNewY
            lda (ZSprX), y
            cmp ZOverXLeft
            bcc collskip        ; outside the overlap window
            cmp ZOverXRight
            bcs collskip        ; outside the overlap window
            sta ZNewX
            cmp ZOldX           ; how do the x columns relate?
            bne :+
            ; x columns are the same, compare both tiles
            lda #$00
            sta ZRefXStart
            sta ZCurrXStart
            lda #$01
            sta ZRefXEnd
            sta ZCurrXEnd
            bne ccy             ; branch always
:           bcc :+
            ; reference x column precedes current x column
            lda #$00
            sta ZCurrXStart
            sta ZCurrXEnd
            lda #$01
            sta ZRefXStart
            sta ZRefXEnd
            bne ccy             ; branch always
            ; reference x column follows current x column
 :          lda #$01
            sta ZCurrXStart
            sta ZCurrXEnd
            lda #$00
            sta ZRefXStart
            sta ZRefXEnd
            ; work out which lines need to be compared
ccy:        lda (ZSprYOff), y
            sec
            sbc ZOldYOff
            sta ZCollODiff      ; offset difference, neg if old > new (old below new)
            lda ZNewY
            sec
            sbc ZOldY
            sta ZCollRDiff      ; row difference, neg if old > new (old below new)
            bne :+              ; branch away if map rows are difference (sign is valid)
            lda ZCollODiff      ; if same row, use offset diff sign instead
:           bpl ccoldnew        ; branch away if new is below old
            ; old is below new
            lda ZCollRDiff
            beq :+
            lda #$08            ; add 8 if they span rows
:           sec
            sbc ZCollODiff
            cmp #$08
            bcc :+
            jmp ccnext          ; no overlap
:           sta ZCurrYStart
            lda #$00
            sta ZRefYStart
            jmp cccomp
            ; new is below old
ccoldnew:   lda ZCollRDiff
            beq :+
            lda #$08            ; add 8 if they span rows
:           clc
            adc ZCollODiff
            cmp #$08
            bcc :+
            jmp ccnext          ; no overlap
:           sta ZRefYStart
            lda #$00
            sta ZCurrYStart
cccomp:     lda (ZSprSprH), y   ; locate the currsprite mask
            clc
            adc (ZSprXOff), y
            sta ZPtrSprA + 1    ; use ZPtrSpr for the currsprite mask
            sta ZPtrSprB + 1
            lda (ZSprAnim), y
            lsr
            ror
            adc #$40            ; mask offset (carry known to be clear)
            sta ZPtrSprA
            adc #$20
            sta ZPtrSprB
            ; check if the masks collide
cctile:     lda ZCurrYStart     ; curr line
            asl
            asl
            asl                 ; x8
            adc ZCurrXStart
            sta ZCollChkB
            lda ZRefYStart      ; ref line
            asl
            asl
            asl                 ; 8
            adc ZRefXStart
            sta ZCollChkA
ccandmasks: ldy ZCollChkA
            lda (ZPtrMaskA), y
            ldy ZCollChkB
            and (ZPtrSprA), y
            bne gotcoll
            ldy ZCollChkA
            lda (ZPtrMaskB), y
            ldy ZCollChkB
            and (ZPtrSprB), y
            bne gotcoll
            ldy ZCurrYStart
            iny
            cpy #$08
            bcs :+              ; branch away if done with curr lines
            sty ZCurrYStart
            ldy ZRefYStart
            iny
            cpy #$08
            bcs :+              ; branch away if done with ref lines
            sty ZRefYStart
            lda ZCollChkA
            clc
            adc #$08
            sta ZCollChkA
            lda ZCollChkB
            clc
            adc #$08
            sta ZCollChkB
            jmp ccandmasks
:           lda ZCurrXStart
            ora ZRefXStart
            bne ccnext          ; branch away if we have done a second tile
            inc ZCurrXStart
            inc ZRefXStart
            jmp cctile
            ; got a collision
gotcoll:    ldy ZCurrSpr        ; for now just bounce
            lda (ZSprXV), y
            eor #$FF
            clc
            adc #$01
            sta (ZSprXV), y
            lda (ZSprYV), y
            eor #$FF
            clc
            adc #$01
            sta (ZSprYV), y
            ldy ZRefSpr
            lda (ZSprXV), y
            eor #$FF
            clc
            adc #$01
            sta (ZSprXV), y
            lda (ZSprYV), y
            eor #$FF
            clc
            adc #$01
            sta (ZSprYV), y
ccnext:     dec ZCurrSpr
            bmi :+
            jmp cccurloop
:           rts

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
