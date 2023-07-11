; lazyriver
; movement processing

SFXQ:       .byte $00           ; proposed sound effect

domove:     lda #$00            ; start with no proposed sound effect
            sta SFXQ
            ldy #SprPlayer      ; move player
            jsr ticksprite      ; stores Y in ZCurrSpr, leaves Y unchanged
            ; do not subject the player to the flow vectors or shore collisions
            jsr movesprite      ; enter with sprite in Y, exits with it still there
            jsr spriteupd       ; uses ZCurrSpr, exits with current sprite in Y
            ; move logs
            ldy NumLogs         ; this is the sprite number
dmmovelog:  jsr ticksprite      ; stores Y in ZCurrSpr, leaves Y unchanged
            bcs :+              ; if not at a movement tick, skip ahead
            jsr flowsprite      ; uses ZCurrSpr, exits with sprite in Y
            jsr movesprite      ; enter with sprite in Y, exits with it still there
            jsr spriteupd       ; uses ZCurrSpr, exits with current sprite in Y
:           dey
            bpl dmmovelog
            jsr collmatrix      ; check sprite-to-sprite collisions
            jsr collshore       ; check whether sprites ran aground
            lda SFXQ            ; has there been a sound effects proposal?
            beq :+
            sta ZFXPtr + 1
            lda #$00
            sta ZFXPtr          ; and play a sound
            lda #$01
            sta FXPlaying       ; start playing the sound
:           rts

; tick animation for a single sprite and put sprite number in ZCurrSpr
; enter with Y holding the sprite number, y continues to hold sprite number after
ticksprite: sty ZCurrSpr
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
            bcs :+              ; not time to move yet
            lda (ZSprDelay), y  ; reset movement tick timer
            sta (ZSprMvTick), y
            lda (ZSprY), y      ; remember original map row in ZOldY
            sta (ZPrevY),y
            sta ZOldY
            lda (ZSprX), y      ; remember original map column in ZOldX
            sta (ZPrevX), y
            sta ZOldX
            lda (ZSprYOff), y   ; remember original y offset row
            sta (ZPrevYOff),y
            lda (ZSprXOff), y   ; remember original x offset column
            sta (ZPrevXOff), y
            clc                 ; tell caller to move this sprite
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
flowdone:   ldx Seed            ; add some random jostle on top of the flow
            inc Seed
            lda Random, x
            and #$03
            sec
            sbc #$01
            cmp #$02
            beq :+
            clc
            adc (ZSprXV), y
            sta (ZSprXV), y
:           inx
            inc Seed
            lda Random, x
            and #$03
            sec
            sbc #$01
            cmp #$02
            beq :+
            clc
            adc (ZSprYV), y
            sta (ZSprYV), y
:           rts

; attempt to move the sprite according to its velocity vector
; enter with y holding the sprite number, exits with y still holding that
; does not disturb x
; will stop a sprite that hits a map edge
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
            lda #$00            ; and stop
            sta (ZSprXV), y
            beq msgovert        ; branch always
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
            lda #$00            ; go to offset 0
            sta ZNewXOff
            sta (ZSprXV), y     ; and stop
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
            cmp #252            ; if we are not already as far down as we can go
            bcc :+              ; branch away if we can move further down
            sta ZNewY           ; stay in the same place
            lda #$00            ; and stop
            sta (ZSprYV), y
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
            lda #$00            ; and stop
            sta (ZSprYV), y
            sta ZNewYOff        ; stop at offset 0 (in case we were not already there)
            rts                 ; done
:           sec
            sbc #$01            ; decrease Y
            sta ZNewY
            rts                 ; done

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
