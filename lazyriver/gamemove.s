; lazyriver
; movement processing

domove:     lda #$82                ; all map-related stuff is in bank 2
            sta ZNewPtr + XByte     ; set up the XByte for all three pointers up here
            sta ZOldPtr + XByte     ; so we do not wind up setting them repeatedly
            ; scroll the background
            ; TODO - check VBL ticks for nudge speed?
            inc MapOff              ; nudge the top
            lda MapOff              ; check to see if we've nudged 8
            cmp #$08
            bne dmmapok
            lda #$00                ; we've nudged 8, so reset to 0
            sta MapOff
            dec MapTop              ; back up the map (we're scrolling down)
            bne dmmapok             ; is there still map left?
            ; we've hit the top of the known map, this would be where a level ends
            ; but for now, we just quit
            ; note: I think this is off by one, we might have one row left.
            inc ExitFlag
            rts
dmmapok:    
            ; move everyone else before moving hero
            lda #$00
            sta ZIsHero         ; not hero - determines sound and score effects of hitting a disk
            ldy NumLogs
            sty ZCurrLog
            ; move each log in turn
            ; empty the bins of dirty segments
            ldx #$08
            lda #$00
:           sta ZDirtStack, x
            dex
            bpl :-
movelog:    ldy ZCurrLog
            lda (ZLogTick), y ; ready to move yet? (up to 8 tick delay for each, governing speed)
            lsr                 ; countdown is just rotating out bits until none are left.
            beq ticksdone
            sta (ZLogTick), y ; not ready to move, decrease ticks and move to check next hoarder
            jmp nextlog
ticksdone:  lda (ZLogSp), y   ; this one can move now, reset ticks for next move after this one
            sta (ZLogTick), y
            lda (ZLogY), y    ; load up the current hoarder's state (segment coords, velocity)
            sta ZOldY
            lda (ZLogX), y
            sta ZOldX
            lda (ZLogXV), y
            sta ZVelX
            lda (ZLogYV), y
            sta ZVelY
            ; logs move based on terrain (flow vector)
            ; TODO: add log movement
            ; idea: log follows flow, but will stop if it hits something (shore, log, protagonist)
nextlog:    dec ZCurrLog
            bmi donelog
            jmp movelog
donelog:    ; logs are finished moving, now move protagonist
            ; protagonist movement follows flow too, but can offset from it
            ; can never go backwards, but against certain flow can stop advancing.
            ; terrain keeps moving forward
            lda PlayerYOff      ; offset from top of tile that the player is
            sec
            sbc VelocityY       ; 
            sta ZIsHero         ; nonzero for hero - sets sound/score effects of hitting a disk
            lda HeroX           ; current position of Hero is OldX/OldY
            sta ZOldX
            lda HeroY
            sta ZOldY
            lda VelocityX       ; current velocity of Hero is VelX/VelY
            sta ZVelX
            lda VelocityY
            sta ZVelY
            jsr trymove         ; attempt to follow trajectory
            bcs posthero        ; the move failed, do not need to do a map update
            lda #$00            ; remove old hero from map
            ldy ZOldX
            sta (ZOldPtr), y
            lda #C_HERO         ; put new hero on map
            ldy ZNewY
            sty HeroY           ; record new Y location
            ldy ZNewX
            sty HeroX           ; record new X location
            sta (ZNewPtr), y    ; put hero in new place on the map
            ; would update on screen here except hero is never on the hires screen
posthero:   lda ZVelX
            sta VelocityX       ; record new X velocity (might have been stopped by a wall)
            lda ZVelY
            sta VelocityY       ; record new Y velocity (might have been stopped by a wall)
            ; find closest of each value of disk for display
            ; (and, honestly, also to help debug my searching algorithm, but may help game play too)
            ldx #$03
heroseek:   txa
            ora #$80
            jsr scandisks       ; look for target disk just of the value currently benig checked
            bcc heroseeky       ; success, record it
            lda #$80
            sta TargDX, x       ; record failure as 80 in the X direction.
            jmp heroseekn
heroseeky:  lda ZTargDX
            sta TargDX, x
            lda ZTargDY
            sta TargDY, x
heroseekn:  dex
            bpl heroseek
            ldx #$01
            stx PlayDirty       ; mark playfield as in need of redraw
            stx MapDirty        ; mark map as in need of updating (hoarders movements)
            lda VelocityY       ; did we need to scroll up based on hero movement?
            sta NeedScroll      ; mark mapfield as in need of a scroll if hero moved in Y direction
            rts
            
; queue potential changes to the list of segments that need to be updated on screen
gmqupdate:  ldy ZOldYY      ; where the head was (segment 2)
            ldx ZOldXX
            jsr hrdirty
            ldy ZOldY       ; where the hands were
            ldx ZOldX
            jsr hrdirty
            ldy ZNewY       ; where the hands are
            ldx ZNewX
            jmp hrdirty     ; rts from there


; check map byte in A for whether it is a collosion or not
; returns with carry set (blocked), or clear (path was clear or contained only a disk)
; it is presumed that if this succeeds, the move WILL happen, because the
; disk gets marked as collected and sound is triggered in this process
checkcoll:  sta ZMapTemp        ; save for later in case we ran into a disk and need to test type
            clc
            and #$3F            ; color bits don't block movement, strip them away
            beq moveclear       ; clear to move, nothing in the way
            cmp #C_DISK         ; disk is the only non-obstacle
            bne mvblocked
            jsr gotdisk         ; do the disk accounting
moveclear:  clc                 ; carry clear = move succeeded (possibly collected a disk)
            rts
mvblocked:  sec                 ; carry set = move blocked
            rts

; called if moving player/antagonist lands on a disk
; assumes x-coordinate is in Y and y-coordinate is in X
gotdisk:    lda ZMapTemp        ; map (disk) was stored here, includes type
            sty DADiskX
            stx DADiskY
            and #$C0            ; determine disk type (color bits)
            pha                 ; stash color bits
            ldx ZIsHero
            beq gotnotsnd       ; if it is not the hero moving, play hoarder sound
            eor #$C0            ; invert value for points (type 0 gets most, 3 least)
            ora #$20            ; add to score if hero got the disk
            lsr
            jsr addscore        ; add type multiplier to the score
            lda #$00
            sta ZFXPtr
            lda #SFXHeroGot     ; play "hero got disk" sound effect
            sta ZFXPtr + 1
            sta FXPlaying       ; start playing the sound
            jmp gotaccount
gotnotsnd:  lda #$00
            sta ZFXPtr
            lda #SFXHrdrGot     ; play "hoarder got disk" sound effect
            sta ZFXPtr + 1
            sta FXPlaying       ; start playing
gotaccount: pla                 ; retrieve disk type (bits 7 and 8)
            asl                 ; shift them over to bits 1 and 2
            rol
            rol
            tax                 ; move type to x
            sed                 ; add in decimal mode to accounting for types
            lda ZIsHero
            beq gotnotact       ; only add to "got" if hero got it
            lda DisksGot, x
            clc
            adc #$01
            sta DisksGot, x     ; got one of this type
            lda #$50            ; start a splash
            sta SplashG, x
gotnotact:  lda DisksLeft, x
            sec
            sbc #$01
            sta DisksLeft, x    ; fewer out there of this type
            lda #$50            ; start a splash
            sta SplashL, x
            cld
            ; remove the disk from the inventory of disks
            ; we have to figure out which disk this was from its coordinates
            ldy NumDisks
gotfind:    lda (ZDiskX), y
DADiskX = *+1
            cmp #INLINEVAR
            bne gotnotthis
            lda (ZDiskY), y
DADiskY = *+1
            cmp #INLINEVAR
            beq gotthis
gotnotthis: dey
            bpl gotfind
            ; should never fall through to here, will corrupt memory (disk=FF) if it does
            ; take the disk off the board, record who has it.
gotthis:    lda ZIsHero
            beq gothoard        ; hoader has it
            lda #$FF            ; hero has it
            jmp gotremove
gothoard:   lda ZCurrHoard      ; keep track of which hoarder has it, for someday
            ora #$80            ; set the high bit
gotremove:  sta (ZDiskX), y     ; if X is negative, will have 80+hoarder or FF (hero) in it
            ; removing the disk from the map is unnecessary
            ; because the hero/hoarder will replace it
            rts

; drop a disk of type in X
dropdisk:   lda DisksGot, x
            bne drophave
            jmp dropfail        ; don't have any to drop
drophave:   stx ZPxScratch      ; stash type
            lda HeroY
            tax                 ; put y-coord in X for dodrop
            jsr setmapptr
            lda MapPtrL
            sta ZPtrA
            lda MapPtrH
            sta ZPtrA + 1
            lda #$82
            sta ZPtrA + XByte
            ldy HeroX
            beq dropright       ; at the left edge, cannot drop left
            dey
            lda (ZPtrA), y      ; is left space empty?
            bne dropright
            jmp dodrop
dropright:  ldy HeroX           ; left wasn't empty, try right instead
            iny
            cpy #$3F
            beq dropup          ; left did not work and right is off the map
            lda (ZPtrA), y      ; is right space empty?
            bne dropup
            jmp dodrop          ; x still holds HeroY from before
dropup:     lda HeroY
            beq dropdown
            sec
            sbc #$01
            tax                 ; put y-coord in X for dodrop
            jsr setmapptr
            lda MapPtrL
            sta ZPtrA
            lda MapPtrH
            sta ZPtrA + 1
            ldy HeroX
            lda (ZPtrA), y
            bne dropdown
            jmp dodrop
dropdown:   lda HeroY           ; left, right, up all failed, try down
            clc
            adc #$01
            beq dropfail        ; down failed too
            tax                 ; put y-coord in X for dodrop
            jsr setmapptr
            lda MapPtrL
            sta ZPtrA
            lda MapPtrH
            sta ZPtrA + 1
            ldy HeroX
            lda (ZPtrA), y
            bne dropfail
dodrop:     sty DroppedX        ; save x-coordinate
            stx DroppedY        ; save y-coordinate
            ldy NumDisks        ; find an open slot, there MUST be one
dropfind:   lda (ZDiskX), y
            bmi dropfound
            dey
            bpl dropfind
            ; should never fall through to here, we had one to drop
DroppedY = *+1
dropfound:  lda #INLINEVAR
            sta (ZDiskY), y
DroppedX = *+1
            lda #INLINEVAR
            sta (ZDiskX), y
            lda ZPxScratch      ; get type back
            sta (ZDiskType), y
            ror
            ror
            ror                 ; move type to high two bits
            pha                 ; push for use with score subtraction
            ora #C_DISK
            ldy DroppedX
            sta (ZPtrA), y      ; and drop it
            pla                 ; get high bit type back
            eor #$C0            ; invert value for points (type 0 gets most, 3 least)
            ora #$20
            lsr
            jsr subscore        ; subtract type multiplier from the score
            ldx ZPxScratch      ; get type back
            sed
            lda DisksGot, x
            sec
            sbc #$01
            sta DisksGot, x
            lda DisksLeft, x
            clc
            adc #$01
            sta DisksLeft, x
            cld
            lda #$00
            sta ZFXPtr
            lda #SFXDrop        ; play the drop sound
            sta ZFXPtr + 1
            sta FXPlaying
            rts
dropfail:   lda #$00
            sta ZFXPtr
            lda #SFXDoh         ; play the drop error sound ("d'oh!")
            sta ZFXPtr + 1
            sta FXPlaying
            rts
