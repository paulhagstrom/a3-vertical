; lazyriver
; Apple III 40-column text region
; top score and progress display
; occupies scan lines 00-07, text line 0.

StatText:   .byte "  Level 00"
            .byte "     lazy "
            .byte "river Scor"
            .byte "e 000000  "

StatCol:    .byte $2D, $2D, $2D, $2D, $2D, $2D, $2D, $2D, $2C, $2C
            .byte $2D, $2D, $2D, $2D, $2D, $3C, $3C, $3C, $3C, $3C
            .byte $3C, $3C, $3C, $3C, $3C, $2D, $2D, $2D, $2D, $2D
            .byte $2D, $2D, $2E, $2E, $2E, $2E, $2E, $2E, $2E, $2E

TextPageA = $0400
TextPageB = $0800

; paint the initial background

paintstat:  lda #$8F
            sta ZPtrA + XByte
            lda #<TextPageA     ; cheating a little because I'm on line 0
            sta ZPtrA
            lda #>TextPageA
            sta ZPtrA + 1
            ldy #$27
:           lda StatText, y
            sta (ZPtrA), y
            dey
            bpl :-
            lda #>TextPageB     ; go to color space
            sta ZPtrA + 1
            ldy #$27
:           lda StatCol, y
            sta (ZPtrA), y
            dey
            bpl :-
            ; fall through to drawlevel

; update level on screen - kept separate because it rarely changes

drawlevel:  lda #<TextPageA
            clc
            adc #$08            ; screen location of level
            sta ZNumPtr
            lda #>TextPageA
            sta ZNumPtr + 1
            lda #$8F
            sta ZNumPtr + XByte
            lda GameLevel
            jmp drawnumber

; draw the score and progress

CharGot:    .byte $00, $0C, $15, $21
CharLeft:   .byte $05, $11, $1A, $26

drawstatus: lda #<TextPageA
            clc
            adc #$24            ; screen location of score (end)
            sta ZNumPtr
            lda #>TextPageA
            sta ZNumPtr + 1
            lda #$8F
            sta ZNumPtr + XByte
            ; DEBUG - BEGIN - REMOVE LATER
            ldy #SprPlayer
            lda (ZSprY), y
            jsr drawnumber
            dec ZNumPtr
            dec ZNumPtr
            ldy #SprPlayer
            lda (ZSprYOff), y
            jsr drawnumber
            dec ZNumPtr
            dec ZNumPtr
            ldy #SprPlayer
            lda (ZSprBgH), y
            jmp drawnumber
            ; DEBUG - END
            ldx #$02
:           lda GameScore, x
            jsr drawnumber
            dec ZNumPtr
            dec ZNumPtr
            dex
            bpl :-
            rts

; put a 2-digit number on screen.
; presumed decimal use of a byte (first nibble 10s, second nibble 1s)
; A holds the number, ZNumPtr holds the screen address of the number.
; Will trigger extended addressing, so set ZNumPtr + XByte to 8F.
; A and Y do not survive.

drawnumber: pha
            lsr
            lsr
            lsr
            lsr
            ora #$30
            ldy #$00
            sta (ZNumPtr), y
            pla
            and #$0F
            ora #$30
            iny
            sta (ZNumPtr), y
            rts

; show the title screen

splash:     bit D_TEXT          ; A3 text
            bit D_PAGEONE       ; be on page 1.
            bit D_NOMIX         ; A3 text
            bit D_LORES         ; A3 text
            lda #$8F
            sta ZPtrA + XByte
            lda #<TextPageA     ; cheating a little because I'm on line 0
            sta ZPtrA
            lda #>TextPageA
            sta ZPtrA + 1
            ldy #$27
:           lda SplashText, y
            sta (ZPtrA), y
            dey
            bpl :-
            lda #>TextPageB     ; go to color space
            sta ZPtrA + 1
            ldy #$27
:           lda SplashCol, y
            sta (ZPtrA), y
            dey
            bpl :-
            rts

; set up for printing loading status
loadset:    lda #<TextPageA
            clc
            adc #$20            ; screen location of status
            sta ZPtrA
            lda #>TextPageA
            sta ZPtrA + 1
            lda #$8F
            sta ZPtrA + XByte
            rts
            
loadstata:  jsr loadset
            ldy #$07
:           lda LoadTextA, y
            sta (ZPtrA), y
            dey
            bpl :-
            rts

loadstatb:  jsr loadset
            ldy #$07
:           lda LoadTextB, y
            sta (ZPtrA), y
            dey
            bpl :-
            rts

loadstatc:  jsr loadset
            ldy #$07
:           lda LoadTextC, y
            sta (ZPtrA), y
            dey
            bpl :-
            rts

SplashText: .byte " Lazy Rive"
            .byte "r  Paul Ha"
            .byte "gstrom 202"
            .byte "3  loading"

SplashCol:  .byte $3C, $3C, $3C, $3C, $3C, $3C, $3C, $3C, $3C, $3C
            .byte $3C, $3C, $2D, $2D, $2D, $2D, $2D, $2D, $2D, $2D
            .byte $2D, $2D, $2D, $2D, $2D, $2D, $2D, $2D, $2D, $2D
            .byte $2D, $2D, $2E, $2E, $2E, $2E, $2E, $2E, $2E, $2E

LoadTextA:  .byte " mapping"
LoadTextB:  .byte "shifting"
LoadTextC:  .byte "painting"