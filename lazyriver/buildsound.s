; lazyriver
; sound effect initialization

; This is called early and fills in static sound data in bank 1.
; It does not switch the banks, and does not present any externally-accessible
; variables.  So it should be ok in bank switched memory.

; Sound effects live in the lower pages, and are copied in by these routines.
; There is room for 29 sound effects, that should be good enough.
; The player does not handle sound effects with more than $FF samples.
; And probably best to steer clear of the lower $300 in the bank.
; One sound effect can be played at a time, and it takes priority over the
; background music.  The way it is set up, a new sound effect triggered will
; immediately supersede any that might currently be playing.
;
; 1F00-1FFF: sound effect 1F
; 1E00-1EFF: sound effect 1E
; ...
; 0300-03FF: sound effect 03
;
; Sounds (both effects and background segments) are finished when they hit a
; negative (high bit set) number.

fxcopy:     ldy #$00
fxcopyloop: lda $4000, y
            sta (ZPtrA), y
            bmi :+
            iny
            bne fxcopyloop
:           rts

SFXHeroGot  = $1F       ; 1F - FXHeroGot - hero got a disk
SFXHrdrGot  = $1E       ; 1E - FXHrdrGot - hoarder got a disk
SFXDoh      = $1D       ; 1D - FXDoh - error sound (drop something you do not have)
SFXDrop     = $1C       ; 1C - FXDrop - drop disk

buildsfx:   lda #$81    ; bank 1
            sta ZPtrA + XByte
            lda #$00
            sta ZPtrA
            ; 1F - FXHeroGot - hero got a disk
            lda #SFXHeroGot
            sta ZPtrA + 1
            lda #<FXHeroGot
            sta fxcopyloop + 1
            lda #>FXHeroGot
            sta fxcopyloop + 2
            jsr fxcopy
            ; 1E - FXHrdrGot - hoarder got a disk
            lda #SFXHrdrGot
            sta ZPtrA + 1
            lda #<FXHrdrGot
            sta fxcopyloop + 1
            lda #>FXHrdrGot
            sta fxcopyloop + 2
            jsr fxcopy
            ; 1D - FXDoh - error sound (drop something you do not have)
            lda #SFXDoh
            sta ZPtrA + 1
            lda #<FXDoh
            sta fxcopyloop + 1
            lda #>FXDoh
            sta fxcopyloop + 2
            jsr fxcopy
            ; 1C - FXDrop - drop disk
            lda #SFXDrop
            sta ZPtrA + 1
            lda #<FXDrop
            sta fxcopyloop + 1
            lda #>FXDrop
            sta fxcopyloop + 2
            jsr fxcopy
            ; all done
            rts

; this is just a constructed sample for the moment
; sample is over when reaches a number with hi bit set
; samples here cannot be very long, a single byte indexes where we are.

FXHeroGot:
            .byte   $0, $3F,  $0, $3F, $0, $3F, $0,  $3F, $0
            .byte   $3F, $0,  $3F, $0, $3F, $0,  $3F, $0, $3F
            .byte   $80

FXHrdrGot:
            .byte   $10, $18, $20, $20, $18, $10, $10, $10 
            .byte   $10, $18, $20, $20, $18, $10, $10, $10 
            .byte   $10, $18, $20, $20, $18, $10, $10, $10 
            .byte   $10, $18, $20, $20, $18, $10, $10, $10 
            .byte   $10, $28, $30, $40, $38, $20, $20, $10 
            .byte   $10, $28, $30, $40, $38, $20, $20, $10 
            .byte   $10, $28, $30, $40, $38, $20, $20, $10 
            .byte   $10, $28, $30, $40, $38, $20, $20, $10 
            .byte   $10, $18, $20, $20, $18, $10, $10, $10 
            .byte   $10, $18, $20, $20, $18, $10, $10, $10 
            .byte   $10, $18, $20, $20, $18, $10, $10, $10 
            .byte   $10, $18, $20, $20, $18, $10, $10, $10
            .byte   $10, $28, $30, $40, $38, $20, $20, $10 
            .byte   $10, $28, $30, $40, $38, $20, $20, $10 
            .byte   $10, $28, $30, $40, $38, $20, $20, $10 
            .byte   $10, $28, $30, $40, $38, $20, $20, $10 
            .byte   $80
            ; $80 ($B6)

FXDrop:
            .byte   $0, $10, $3F, $10, $0, $10, $3F, $10, $0
            .byte   $10, $3F, $10, $0, $10, $3F, $10, $0, $10
            .byte   $0, $10, $3F, $5, $0, $3F, $20, $10, $3F
            .byte   $10, $3F, $20, $0, $30, $3F, $20, $3F, $10
            .byte   $80

FXDoh:
            .byte   $00, $00, $00, $00, $3F, $00, $00, $00, $3F, $00, $00, $00, $2F
            .byte   $00, $00, $00, $00, $3F, $00, $00, $00, $3F, $00, $00, $00, $2F
            .byte   $80
