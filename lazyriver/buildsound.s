; lazyriver
; sound effect initialization

; This is called early and fills in static sound data in bank 2.
; It does not switch the banks, and does not present any externally-accessible
; variables.  So it should be ok in bank switched memory.

; Sound effects live in the lower pages, and are copied in by these routines.
; There is room for 47 sound effects, that should be good enough.
; The player does not handle sound effects with more than $FF samples.
; And probably best to steer clear of the lower $300 in the bank.
; One sound effect can be played at a time, and it takes priority over the
; background music.  The way it is set up, a new sound effect triggered will
; immediately supersede any that might currently be playing.
;
; 7E00-7EFF: sound effect 7E (if I reallly need 47 sound effects)
; ...
; 5000-50FF: sound effect 50
;
; Sounds (both effects and background segments) are finished when they hit a
; negative (high bit set) number.

fxcopy:     ldy #$00
FXCopyAddr = *+1
fxcopyloop: lda INLINEADDR, y
            sta (ZPtrA), y
            bmi :+
            iny
            bne fxcopyloop
:           rts

SFXHeroGot  = $53       ; 1F - FXHeroGot - hero got a disk
SFXHrdrGot  = $52       ; 1E - FXHrdrGot - hoarder got a disk
SFXShore    = $51       ; 1D - SFXShore - error sound (drop something you do not have)
SFXBump     = $50       ; 1C - SFXBump - drop disk

buildsfx:   lda #$82    ; bank 2
            sta ZPtrA + XByte
            lda #$00
            sta ZPtrA
            ; 51 - FXShore - log hits shore
            lda #SFXShore
            sta ZPtrA + 1
            lda #<FXShore
            sta FXCopyAddr
            lda #>FXShore
            sta FXCopyAddr + 1
            jsr fxcopy
            ; 50 - FXBump - logs bump into each other
            lda #SFXBump
            sta ZPtrA + 1
            lda #<FXBump
            sta FXCopyAddr
            lda #>FXBump
            sta FXCopyAddr + 1
            jsr fxcopy
            ; all done
            rts

; this is just a constructed sample for the moment
; sample is over when reaches a number with hi bit set
; samples here cannot be very long, a single byte indexes where we are.

FXBumpx:
            .byte   $0, $3F,  $0, $3F, $0, $3F, $0,  $3F, $0
            .byte   $3F, $0,  $3F, $0, $3F, $0,  $3F, $0, $3F
            .byte   $80

FXShorex:
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

FXBump:
            .byte   $10, $3F, $10, $0, $80
            .byte   $08, $10, $08, $0, $80
            .byte   $80
            .byte   $10, $3F, $10, $0, $10, $3F, $10, $0, $10
            .byte   $0, $10, $3F, $5, $0, $3F, $20, $10, $3F
            .byte   $10, $3F, $20, $0, $30, $3F, $20, $3F, $10
            .byte   $80

FXShore:
            .byte   $20, $00, $00, $00, $20, $00, $00, $00, $80
            .byte   $00, $00, $00, $00, $3F, $00, $00, $00, $3F, $00, $00, $00, $2F
            .byte   $80
