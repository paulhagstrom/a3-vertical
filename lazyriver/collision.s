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
:           inc ZOverYBot       ; one row after can collide
            bne :+              ; skip extending bounds down if we're at the bottom
            dec ZOverYBot       ; undo the extension if extending it down wrapped
:           lda (ZSprX), y      ; remember ref sprite's x coordinate
            sta ZOldX   
            sta ZOverXLeft
            sta ZOverXRight
            beq :+              ; skip extending bounds left if we're at the left
            dec ZOverXLeft      ; one column prior can collide
:           inc ZOverXRight     ; two columns after is the first outside the window
            inc ZOverXRight     ; (doesn't matter that it might be off map right)
            lda (ZSprYOff), y   ; remember ref's y offset
            sta ZOldYOff
            ; we will compare the reference sprite to all lower-numbered sprites
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
            ; BELOW IT SHOULD BE BCC AS FAR AS I CAN TELL
            ; BUT BCS SEEMS TO LOOK LIKE IT WORKS BETTER?
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
            ; ZCollODiff is comparison's Y offset - reference's Y offset
            ; and ZCollRDiff is comparison's Y row - reference's Y row
            ; if they are in the same row but at different offsets,
            ;   ZCollODiff being negative means comparison is higher up than reference
            ;       so we need to check the comparison starting at the first row of
            ;       overlap (-ZCollODiff) and the reference starting at zero.
            ;   ZCollODiff being positive means comparison is lower down than reference
            ;       so we need to check the reference starting at the first row of
            ;       overlap (ZCollODiff) and comparison starting at zero
            ; if they are in different rows, they will be one row apart, and row
            ; difference determines which is higher (not offset difference).
            ; Ultimately this means we add 8 to the difference between them.
            ;   If ZCollRDiff is negative, comparison is higher up than reference
            ;       ZCollODiff might be positive or negative
            ;       if comparison were at offset 6 and reference were at offset 2
            ;       then ZCollODiff would be 4, check comparison starting at 8-4, ref at 0
            ;       if ZCollODiff is also negative, they won't overlap.
            ;   If ZCollRDiff is positive, comparison is lower down than reference
            ;       if reference were at offset 6 and comparison were at offset 2
            ;       then ZCollODiff would be -4, check reference starting at 8+(-4), comp at 0
            ;       if ZCollODiff is also positive, they won't overlap.
            ; In this case, if they wind up more than 8 apart, they don't overlap.
            bne :+              ; branch away if map rows are different (sign is valid)
            lda ZCollODiff      ; if same row, use offset diff sign instead
:           bpl ccoldnew        ; branch away if comparison (new) is below ref (old)
            ; old (reference) is below new (comparison)
            lda ZCollRDiff
            beq :+
            lda #$08            ; add 8 if they span rows
:           sec
            sbc ZCollODiff      ; if in the same row, this is negative
            cmp #$08
            bcc :+
            jmp ccnext          ; no overlap
:           sta ZCurrYStart
            lda #$00
            sta ZRefYStart
            jmp cccomp
            ; new (comparison) is below old (reference)
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
            sta ZPtrSprA + 1    ; use ZPtrSpr for the comparison collision mask
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
            ora ZRefXStart      ; right tile (only zero if we did only left tiles so far)
            bne ccnext          ; branch away if we have already done the right tile of a pair
            lda #$02            ; we were doing the left side of both, do right side now
            sta ZCurrXStart
            sta ZRefXStart
            jmp cctile
            ; two sprites have collided (presently overlap)
            ; we presume they did not previously overlap
            ; if there were no flow, sending them apart could be as simple
            ; as reversing their velocities to bounce them apart, or maybe
            ; just swapping their velocities (though I think this has edge cases that fail)
            ; but the problem is that the flow will get added back in again before
            ; the bounce can take effect, and they might wind up continuing to overlap
            ; putting them back in their prior position before swapping the velocities
            ; should avoid this problem, but also might make things look like they
            ; deflect off each other at a distance.
            ; particularly in the situation where one is stopped, we want to keep the
            ; other one from stopping, but then flowing on top again, while the first
            ; one can't move because it's stuck on a shore.
            ; perhaps a collision can simply undo a move?  That kind of works.
            ; TODO - the basic problem I am having right now is that logs fairly
            ; happily wind up moving vertically on top of each other.
            ; perhaps the collision detection isn't quite working?
            ; or this corrective action isn't.
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
