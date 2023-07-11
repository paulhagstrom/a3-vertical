; collisions
; check and handle collisions

SprCollA    = $500
SprCollB    = $600

; check sprites to see if they have hit the shoreline
; if they have, revert their position and stop their movement
; check this after doing sprite-sprite collisions
collshore:  lda NumLogs         ; starting sprite number
            sta ZCurrSpr
:           ldy ZCurrSpr
            lda (ZSprY), y      ; check tile the sprite is now on
            tax
            lda (ZSprX), y
            tay
            lda MapLineL, x
            sta ZMapPtr
            lda MapLineH, x
            sta ZMapPtr + 1
            lda (ZMapPtr), y
            and #$04            ; check water bit
            bne shoreok         ; we're in water, great, move to next
            ; last movement ran aground
            ldy ZCurrSpr        ; put the sprite back where it was
            lda (ZPrevX), y
            sta (ZSprX), y
            lda (ZPrevXOff), y
            sta (ZSprXOff), y
            lda (ZPrevY), y
            sta (ZSprY), y
            lda (ZPrevYOff), y
            sta (ZSprYOff), y
            lda #$00            ; and stop it
            sta (ZSprXV), y
            sta (ZSprYV), y
            lda ZPgIndex        ; propose a shore sound if sprite was onscreen
            beq :+
            lda (ZSprDrXOne), y
            bmi shoreok
            bpl :++
:           lda (ZSprDrXTwo), y
            bmi shoreok
:           lda #SFXShore       ; play "log bumped shore" sound effect
            sta SFXQ
shoreok:    dec ZCurrSpr
            bpl :---
            rts

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
;(.*)$ collshore:  ldx ZNewY           ; load map byte for proposed new position
;(.*)$             ldy ZNewX
;(.*)$             lda MapLineL, x
;(.*)$             sta ZMapPtr
;(.*)$             lda MapLineH, x
;(.*)$             sta ZMapPtr + 1
;(.*)$             lda (ZMapPtr), y
;(.*)$             and #$04            ; check water bit
;(.*)$             bne shoredone       ; done, movement lands in water, it can proceed
;(.*)$             ; desired movement failed, check if moving just in Y direction would work
;(.*)$             ldy ZOldX           ; check map data for original column
;(.*)$             lda (ZMapPtr), y    ; load map data for new Y, original X
;(.*)$             and #$04            ; check water bit
;(.*)$             clc                 ; tell logshyok block to only zero XV
;(.*)$             bne logshyok        ; branch away if vertical move lands in water
;(.*)$             ; moving just vertically fails, check if moving just in X direction works
;(.*)$             ldy ZNewX           ; load map byte for new X, original Y
;(.*)$             ldx ZOldY
;(.*)$             lda MapLineL, x
;(.*)$             sta ZMapPtr
;(.*)$             lda MapLineH, x
;(.*)$             sta ZMapPtr + 1
;(.*)$             lda (ZMapPtr), y
;(.*)$             and #$04            ; check water bit
;(.*)$             bne logshxok        ; branch away if horizontal move lands in water
;(.*)$             ; no movement worked, so zero out velocity and stay here.
;(.*)$             sec                 ; tell following block to zero both X and Y
;(.*)$             ; shore impedes horizontal movement only
;(.*)$ logshyok:   ldy ZCurrSpr
;(.*)$             lda #$00            ; zero out X velocity
;(.*)$             sta (ZSprXV), y
;(.*)$             lda ZOldX           ; propose that X not move
;(.*)$             sta ZNewX
;(.*)$             lda (ZSprXOff), y
;(.*)$             sta ZNewXOff
;(.*)$             bcc shoredone       ; branch away to rts if we are only zeroing XV
;(.*)$             ; shore impedes vertical movement only
;(.*)$ logshxok:   ldy ZCurrSpr
;(.*)$             lda #$00            ; zero out Y velocity
;(.*)$             sta (ZSprYV), y
;(.*)$             lda ZOldY           ; propose that Y not move
;(.*)$             sta ZNewY
;(.*)$             lda (ZSprYOff), y
;(.*)$             sta ZNewYOff
;(.*)$ shoredone:  rts

; check the sprite collision matrix

collmatrix: ldy NumLogs         ; this is the sprite number
            sty ZRefSpr
ccrefloop:  ldy ZRefSpr         ; compare ref sprite against all prior ones 
            dey
            sty ZCurrSpr
            jsr collpairs       ; check the pairs including refspr
            dec ZRefSpr
            bne ccrefloop       ; branch away if there are sprites left
            ldy NumLogs
            sty ZCurrSpr
            ldy #SprPlayer      ; test the player against everything
            sty ZRefSpr
            jsr collpairs
            rts

collskip:   jmp ccnext
            
; assumes that ZRefSpr is set to reference sprite
; will check all sprites ZCurrSpr and lower against ZRefSpr
; and ZCurrSpr is set to first comparison sprite, will check all lower ones as well
collpairs:  ldy ZRefSpr
            lda (ZSprY), y      ; remember ref sprite's y coordinate
            sta ZOldY
            ; establish a quick bounding box that we can check against comparison
            ; sprites.  Nothing outside this bounding box could possibly collide
            ; A comparison sprite that is more than one tile away in any direction
            ; cannot overlap (but one that is one tile away could, depending on offsets)
            ; so we can quickly discard comparison checks as we travel the matrix
            sta ZOverYTop       ; establish the overlap window wrt ref sprite
            sta ZOverYBot
            beq :+              ; skip extending bounds up if we're at the top
            dec ZOverYTop       ; one row prior can collide
:           inc ZOverYBot
            bne :+              ; skip extending bounds down if we're at the bottom
            dec ZOverYBot
:           lda (ZSprX), y      ; remember ref sprite's x coordinate
            sta ZOldX   
            sta ZOverXLeft
            sta ZOverXRight
            beq :+              ; skip extending bounds left if we're at the left
            dec ZOverXLeft      ; one column prior can collide
:           inc ZOverXRight     ; two columns after is the first outside the window
            inc ZOverXRight     ; (doesn't matter that it might be off map right)
            lda (ZSprYOff), y   ; remember pre-movement y offset
            sta ZOldYOff
            ; we will compare the reference sprite to all lower-numbered sprites
            ; find the details of the reference sprite (locate its mask)
            lda (ZSprCollH), y  ; locate the reference sprite's collision mask
            clc
            adc (ZSprXOff), y   ; adjust for shift
            sta ZPtrMaskA + 1
            sta ZPtrMaskB + 1
            lda (ZSprAnim), y
            lsr
            ror                 ; $80 or $00 depending on animation frame
            adc (ZSprCollL), y  ; collision mask low byte (carry known to be clear)
            sta ZPtrMaskA
            adc #$20
            sta ZPtrMaskB
            ; loop through all lower-numbered sprites comparing them to ref sprite
cccurloop:  ldy ZCurrSpr
            ; unless comparison sprite is inside the overlap window, move on to next
            lda (ZSprY), y
            cmp ZOverYTop
            bcc collskip        ; outside the overlap window
            cmp ZOverYBot
            beq :+
            bcs collskip        ; outside the overlap window
:           sta ZNewY           ; remember comparison sprite's y coordinate
            lda (ZSprX), y
            cmp ZOverXLeft
            bcc collskip        ; outside the overlap window
            cmp ZOverXRight     ; this is first column after overlap window
            bcs collskip        ; outside the overlap window
            sta ZNewX           ; remember comparison sprite's x coordinate
            ; compare the map columns of reference and current sprite
            ; if they are the same, we check both columns of the mask
            ; if they are not the same, they will be one apart
            ; so check the right side mask of the leftmost one against
            ; the left side mask of the rightmost one
            cmp ZOldX           ; how do the x columns relate?
            bne :+
            ; x columns are the same, compare both tiles
            lda #$00
            sta ZRefXStart
            sta ZCurrXStart
            beq ccy             ; branch always
:           bcc :+
            ; reference x column precedes current, compare curr L to ref R
            lda #$02
            sta ZRefXStart
            lda #$00
            sta ZCurrXStart
            beq ccy             ; branch always
            ; reference x column follows current, compare curr R to ref L
 :          lda #$02
            sta ZCurrXStart
            lda #$00
            sta ZRefXStart
            ; work out which lines need to be compared
ccy:        lda (ZSprYOff), y   ; comparison sprite's y offset
            sec
            sbc ZOldYOff        ; reference sprite's y offset
            sta ZCollODiff      ; offset difference, neg if old > new (old below new)
            lda ZNewY           ; comparison sprite's row
            sec
            sbc ZOldY           ; reference sprite's row
            sta ZCollRDiff      ; row difference, neg if old > new (old below new)
            bne :+              ; branch away if map rows are different (sign is valid)
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
cccomp:     lda (ZSprCollH), y  ; locate the currsprite collision mask
            clc
            adc (ZSprXOff), y   ; adjust for shift
            sta ZPtrSprA + 1    ; use ZPtrSpr for the currsprite mask
            sta ZPtrSprB + 1
            lda (ZSprAnim), y
            lsr
            ror                 ; $80 or $00 depending on animation frame
            adc (ZSprCollL), y  ; collision mask low byte (carry known to be clear)
            sta ZPtrSprA
            adc #$20
            sta ZPtrSprB
            ; check if the masks collide in one tile
            ; recall: masks are 4 bytes per line (2 tiles),
            ; there is an A and B mask. So left side mask is first
            ; two bytes of line, right side is second two bytes
cctile:     lda ZCurrYStart     ; curr line
            asl
            asl                 ; x4 (4 bytes per half per line)
            adc ZCurrXStart     ; $00 or $02 depending on which side
            sta ZCollChkB       ; curr line mask half
            lda ZRefYStart      ; ref line
            asl
            asl                 ; x4
            adc ZRefXStart      ; $00 or $02 depending on which side
            sta ZCollChkA       ; ref line mask half
            ; TODO - drawing mask is reverse of what I need for collision mask
            ; and this is a LOT of repeated computation on the fly.
ccandmasks: ldy ZCollChkA       ; ref line mask half
            lda (ZPtrMaskA), y
            tax                 ; stash ref A mask in x
            lda (ZPtrMaskB), y
            ldy ZCollChkB       ; curr line mask half
            and (ZPtrSprB), y
            bne gotcoll
            txa
            and (ZPtrSprA), y
            bne gotcoll
            inc ZCollChkA       ; move to next byte of masks (2 bytes per tile pair line)
            inc ZCollChkB
            ldy ZCollChkA       ; ref line mask half
            lda (ZPtrMaskA), y
            tax
            lda (ZPtrMaskB), y
            ldy ZCollChkB       ; curr line mask half
            and (ZPtrSprB), y
            bne gotcoll
            txa
            and (ZPtrSprA), y
            bne gotcoll
            lda ZCollChkA       ; move to next line of masks (4 bytes per tile pair line)
            clc
            adc #$03            ; already added one, adding 3 more yields 4
            sta ZCollChkA
            lda ZCollChkB
            clc
            adc #$03            ; already added one, adding 3 more yields 4
            sta ZCollChkB
            ldy ZCurrYStart
            cpy #$07
            bcs :+              ; branch away if done with curr lines
            iny
            sty ZCurrYStart
            ldy ZRefYStart
            cpy #$07
            bcs :+              ; branch away if done with ref lines
            iny
            sty ZRefYStart
            jmp ccandmasks
:           lda ZCurrXStart     ; check to see if we've done left tile but need to do
            ora ZRefXStart      ; right tile (only zero if we did just left tiles so far)
            bne ccnext          ; branch away if we have already done the right tile of a pair
            lda #$02            ; we were doing the left side of both, do right side now
            sta ZCurrXStart
            sta ZRefXStart
            jmp cctile
            ; two sprites have collided (presently overlap)
            ; we presume they did not previously overlap
            ; so put them back where they were and swap their velocities

            ; got a collision - swap velocities, restore position
gotcoll:    ldy ZCurrSpr        
            lda (ZPrevX), y     ; restore current sprite's pre-movement position.
            sta (ZSprX), y
            lda (ZPrevXOff), y
            sta (ZSprXOff), y
            lda (ZPrevY), y
            sta (ZSprY), y
            lda (ZPrevYOff), y
            sta (ZSprYOff), y
            
            ;(.*)$ lda #$00            ; stop the sprite
            ;(.*)$ sta (ZSprXV), y
            ;(.*)$ sta (ZSprYV), y
            ;(.*)$ ldy ZRefSpr
            ;(.*)$ sta (ZSprXV), y
            ;(.*)$ sta (ZSprYV), y
            ;(.*)$ jmp :++
            
            lda (ZSprXV), y     ; swap velocities
            sta ZCollChkA       ; curr XV -> CollChkA
            lda (ZSprYV), y
            sta ZCollChkB       ; curr YV -> CollChkB
            ldy ZRefSpr
            lda (ZSprXV), y     
            tax                 ; ref XV -> x
            lda (ZSprYV), y
            ldy ZCurrSpr
            sta (ZSprYV), y     ; ref YV -> curr YV
            txa
            sta (ZSprXV), y     ; ref XV (x) -> curr XV
            ldy ZRefSpr
            lda ZCollChkA
            sta (ZSprXV), y     ; curr XV (CollChkA) -> ref XV
            lda ZCollChkB
            sta (ZSprYV), y     ; curr YV (CollChkB) -> ref YV
;(.*)$             jmp :++
;(.*)$             ; if X velocities are the same sign, back up in X dimension
;(.*)$             txa                 ; curr XV
;(.*)$             eor ZCollChkA       ; ref XV
;(.*)$             bmi :+              ; signs differed
;(.*)$             ; restore pre-movement x position - ref sprite
;(.*)$             lda (ZPrevX), y
;(.*)$             sta (ZSprX), y
;(.*)$             lda (ZPrevXOff), y
;(.*)$             sta (ZSprXOff), y
;(.*)$             ; restore pre-movement x position - curr sprite
;(.*)$             ldy ZCurrSpr
;(.*)$             lda (ZPrevX), y
;(.*)$             sta (ZSprX), y
;(.*)$             lda (ZPrevXOff), y
;(.*)$             sta (ZSprXOff), y
;(.*)$             ; if Y velocities are the same sign, back up in the Y dimension
;(.*)$ :           ldy ZCurrSpr
;(.*)$             lda (ZSprYV), y     ; curr YV
;(.*)$             eor ZCollChkB       ; ref YV
;(.*)$             bmi :+              ; signs different
;(.*)$             ; restore pre-movement y position - curr sprite
;(.*)$             lda (ZPrevY), y
;(.*)$             sta (ZSprY), y
;(.*)$             lda (ZPrevYOff), y
;(.*)$             sta (ZSprYOff), y
;(.*)$             ; restore pre-movement y position - ref sprite
;(.*)$             lda (ZPrevY), y
;(.*)$             sta (ZSprY), y
;(.*)$             lda (ZPrevYOff), y
;(.*)$             sta (ZSprYOff), y
;(.*)$ :            
            lda (ZPrevX), y     ; restore reference sprite's pre-movement position.
            sta (ZSprX), y
            lda (ZPrevXOff), y
            sta (ZSprXOff), y
            lda (ZPrevY), y
            sta (ZSprY), y
            lda (ZPrevYOff), y
            sta (ZSprYOff), y
            
;(.*)$ gotcoll:    ldy ZCurrSpr
            ;(.*)$ lda (ZPrevX), y
            ;(.*)$ sta (ZSprX), y
            ;(.*)$ lda (ZPrevXOff), y
            ;(.*)$ sta (ZSprXOff), y
            ;(.*)$ lda (ZPrevY), y
            ;(.*)$ sta (ZSprY), y
            ;(.*)$ lda (ZPrevYOff), y
            ;(.*)$ sta (ZSprYOff), y
            ;(.*)$ lda (ZSprXV), y
            ;(.*)$ eor #$FF
            ;(.*)$ clc
            ;(.*)$ adc #$01
            ;(.*)$ sta ZCollChkA
            ;(.*)$ lda (ZSprYV), y
            ;(.*)$ eor #$FF
            ;(.*)$ clc
            ;(.*)$ adc #$01
            ;(.*)$ sta ZCollChkB
            ;(.*)$ ldy ZRefSpr
            ;(.*)$ sta (ZSprXV), y
            ;(.*)$ eor #$FF
            ;(.*)$ clc
            ;(.*)$ adc #$01
            ;(.*)$ tax
            ;(.*)$ lda (ZSprYV), y
            ;(.*)$ eor #$FF
            ;(.*)$ clc
            ;(.*)$ adc #$01
            ;(.*)$ ldy ZCurrSpr
            ;(.*)$ sta (ZSprYV), y
            ;(.*)$ txa
            ;(.*)$ sta (ZSprXV), y
            ;(.*)$ ldy ZRefSpr
            ;(.*)$ lda ZCollChkA
            ;(.*)$ sta (ZSprXV), y
            ;(.*)$ lda ZCollChkA
            ;(.*)$ sta (ZSprYV), y
            
            ;(.*)$ lda (ZPrevX), y     ; restore reference sprite's pre-movement position.
            ;(.*)$ sta (ZSprX), y
            ;(.*)$ lda (ZPrevXOff), y
            ;(.*)$ sta (ZSprXOff), y
            ;(.*)$ lda (ZPrevY), y
            ;(.*)$ sta (ZSprY), y
            ;(.*)$ lda (ZPrevYOff), y
            ;(.*)$ sta (ZSprYOff), y

            ; got a collision - bounce (reverse velocities)
            ;(.*)$ ldy ZCurrSpr
            ;(.*)$ lda (ZSprXV), y
            ;(.*)$ eor #$FF
            ;(.*)$ clc
            ;(.*)$ adc #$01
            ;(.*)$ sta (ZSprXV), y
            ;(.*)$ lda (ZSprYV), y
            ;(.*)$ eor #$FF
            ;(.*)$ clc
            ;(.*)$ adc #$01
            ;(.*)$ sta (ZSprYV), y
            ;(.*)$ ldy ZRefSpr
            ;(.*)$ lda (ZSprXV), y
            ;(.*)$ eor #$FF
            ;(.*)$ clc
            ;(.*)$ adc #$01
            ;(.*)$ sta (ZSprXV), y
            ;(.*)$ lda (ZSprYV), y
            ;(.*)$ eor #$FF
            ;(.*)$ clc
            ;(.*)$ adc #$01
            ;(.*)$ sta (ZSprYV), y
            lda ZPgIndex        ; propose a bump sound if ref sprite was onscreen
            beq :+
            lda (ZSprDrXOne), y
            bmi ccnext
            bpl :++
:           lda (ZSprDrXTwo), y
            bmi ccnext
:           lda #SFXBump        ; play "log bumped another log" sound effect
            sta SFXQ
            
ccnext:     dec ZCurrSpr
            bmi :+
            jmp cccurloop
:           rts
