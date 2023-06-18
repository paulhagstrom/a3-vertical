; lazyriver
; sprite routines

; A sprite will be a 7x8 block, which corresponds to 32 bytes.  But given that
; it can be at any x-coordinate, we need to consider it to be a 14x8 block,
; 64 bytes.  We will pre-shift them so that we have these bytes ready to go.
; To put a sprite at an absolute y-coordinate on the screen requires taking
; the smooth scroll parameter into account.  If scroll is set to 2 and we
; want to put it on lines 5-C, then we have to start drawing on line 7.
;
; sprite line 0 - display line 5 - adjusted line 7
; sprite line 1 - display line 6 - adjusted line 0
; sprite line 2 - display line 7 - adjusted line 1
; sprite line 3 - display line 8 - adjusted line A
; sprite line 4 - display line 9 - adjusted line B
; sprite line 5 - display line A - adjusted line C
; sprite line 6 - display line B - adjusted line D
; sprite line 7 - display line C - adjusted line E
;
; That is, to draw on display line y, we draw to
; 8*int(y/8) + (y+offset)%8
;
; To draw the sprite itself, we need to compute the targets and then move
; the data.  The data is 64 bytes, and we want to load the background, stash
; it somewhere, apply a mask, draw, and then store.  That's minimally
; in the ballpark of 1280 cycles per sprite, probably will be somewhat
; more.

; when we draw a sprite, we will have its raster y-coordinate
; and an x-coordinate (0-133, it can't go off the right edge)
; we iterate line from 0 to 7 for the 8 lines of the sprite
; meaning we are aiming to draw on raster y+line
; int((y+line)/8) + (y+line+offset)%8
; horizontally, we use two tile-widths to hold the sprite
; we draw at 0-1 if sprite starts between 0 and 6
; we draw at 1-2 if sprite starts between 7 and 13, etc.
; we have seven versions of those two bytes, with the sprite
; starting at each possible position (e.g., between 0 and 6)
; the last one is 19-20 if sprite starts between 127 and 133.

; TODO - later, try to anchor the sprite to the map, but for
; now just put it on the screen in screen coordinates.

; when we draw a sprite, we need to retrieve the background
; and store it somewhere (64 bytes), AND it with the mask,
; OR it with the graphics and store it on the page.
; and to erase, store the cached background back to the page.
; so we need 64 bytes per visible sprite for background cache.
; we can use $428-7FF and $828-BFF (text pages)
; 440 480 4C0 500 540 580 5C0 600 640 680 6C0 700 740 780 7C0
; so that is 15 active sprites per text page, we can do 30
; relatively easily.  Probably having that many will take too
; long to draw though.
; but, yes, cache background for sprite n in:
; $440 + (n * $40), up to an n of 15.
; or up to an n of 30 if we just ensure there is no n=16.

; draw sprites on nonvisible page

ScrOffset:  .byte 0             ; place to store PgOneOff for drawing page
CurrY:      .byte 0             ; current Y raster
ScrX:       .byte 0             ; byte to start drawing (2x from left)

; set ScrOffset, ZCacheBase, and ZPageBase
pgcompute:  lda ShownPage
            eor #$01            ; switch focus to nonvisible page
            and #$01            ; 0 if page 1 is nonvisible, 1 if page 2 is nonvisible
            tax
            ror                 ; carry
            ror                 ; $80
            ror                 ; $40
            sta ZPageBase       ; $40 if nonvisible is page 2, $00 if page 1
            txa
            asl
            asl
            adc #$04
            sta ZCacheBase      ; $08 if nonvisible is page 2, $04 if page 1
            lda PgOneOff, x     ; keep track of the scroll value of the screen
            sta ScrOffset
            rts

setsprites: 
            jsr pgcompute       ; set ScrOffset and PgIndex
            
            ; set up pointer banks
            ; TODO - optimize so that these don't need to be set repeatedly
            lda #$80            ; bank zero (display)
            sta ZPtrE + XByte   ; graphics A
            sta ZPtrF + XByte   ; graphics B
            lda #$8F            ; access to text pages
            sta ZPtrG + XByte   ; background cache A
            sta ZPtrH + XByte   ; background cache B
            lda #$81            ; bank 1 (map and graphics assets)
            sta ZPtrA + XByte   ; data A
            sta ZPtrB + XByte   ; data B
            sta ZPtrC + XByte   ; mask A
            sta ZPtrD + XByte   ; mask B

            ; locate sprite data
            ; starts at $1500 + (n/2)*$700 + (n%2)*$80 + (x-offset)*$100
            ; cheat for now since we have only one sprite, it'll just be $1500+off*$100
            
            lda PlayerXOff      ; select which shift, each shift is $100
            clc
            adc #$15            ; sprite base page
            sta ZPtrA + 1
            sta ZPtrB + 1
            sta ZPtrC + 1
            sta ZPtrD + 1
            lda #$00            ; data A
            sta ZPtrA
            adc #$20            ; data B
            sta ZPtrB
            adc #$20            ; mask A
            sta ZPtrC
            adc #$20            ; mask B
            sta ZPtrD
            
            lda PlayerY         ; where we would like to start
            jsr scrcompute      ; compute the adjusted lines
            lda PlayerX
            asl                 ; x2
            sta ScrX            ; byte at which to start drawing sprite
            
            lda #$40            ; save background into $440, $460
            sta ZPtrG
            lda #$60
            sta ZPtrH
            lda ZCacheBase
            sta ZPtrG + 1
            sta ZPtrH + 1
            
            ldx #$00
            stx ZSprLine

            ; do a line
            ; set ZPtrE and ZPtrF to point to pages A and B of nondisplayed page
spblit:
            ldy ZTileCache, x
            lda YHiresH, y
            clc
            adc ZPageBase
            sta ZPtrF + 1       ; in bank 0, page B
            sec
            sbc #$20
            sta ZPtrE + 1       ; in bank 0, page A
            lda YHiresL, y
            clc
            adc ScrX
            sta ZPtrE
            sta ZPtrF
            
            ldy #$03
spblitline: lda (ZPtrE), y      ; screen byte A
            sta (ZPtrG), y      ; save background A
            and (ZPtrC), y      ; mask A
            ora (ZPtrA), y      ; draw
            sta (ZPtrE), y      ; replace screen byte A
            lda (ZPtrF), y      ; screen byte B
            sta (ZPtrH), y      ; save background B
            and (ZPtrD), y      ; mask B
            ora (ZPtrB), y      ; draw
            sta (ZPtrF), y      ; replace screen byte B
            dey
            bpl spblitline

            inc ZSprLine
            ldx ZSprLine
            cpx #$08
            beq spdone            
            
            ; push all data pointers ahead 4 bytes
            clc                 ; nothing below should set carry
            lda ZPtrA
            adc #$04
            sta ZPtrA
            lda ZPtrB
            adc #$04
            sta ZPtrB
            lda ZPtrC
            adc #$04
            sta ZPtrC
            lda ZPtrD
            adc #$04
            sta ZPtrD
            lda ZPtrG
            adc #$04
            sta ZPtrG
            lda ZPtrH
            adc #$04
            sta ZPtrH
            jmp spblit
spdone:     
            lda PlayerX         ; remember where we drew this for erasing
            sta PlayXPrev
            lda PlayerY
            sta PlayYPrev
            rts

; compute the adjusted lines
; enter with A holding the screen line we're targeting
; (the sprite would go from there to there plus 7)

scrcompute: sta ZPxScratch
            clc
            adc ScrOffset
            and #$07            ; (y+offset)%8
            tay                 ; stash in y
            ldx #$00
:           lda ZPxScratch      ; retrieve PlayerY
            and #%11111000      ; 8*(int(y/8))
            sta ZTileCache, x
            tya                 ; (y+offset)%8
            clc
            adc ZTileCache, x
            sta ZTileCache, x
            iny                 ; add one to y+offset
            tya
            and #$07            ; and mod 8
            tay
            inc ZPxScratch      ; increase display line
            inx
            cpx #$08
            bne :-
            rts
            
; erase sprites on nonvisible page
; TODO - this is computing too much,
; should keep track of where the bytes are and just blast them back
clrsprites:
            jsr pgcompute       ; set ScrOffset and PgIndex
            
            ; set up pointer banks
            ; TODO - optimize so that these don't need to be set repeatedly
            lda #$80            ; bank zero (display)
            sta ZPtrE + XByte   ; graphics A
            sta ZPtrF + XByte   ; graphics B
            lda #$8F            ; access to text pages
            sta ZPtrG + XByte   ; background cache A
            sta ZPtrH + XByte   ; background cache B
            
            lda PlayYPrev       ; where we would like to start
            jsr scrcompute      ; compute the adjusted lines
            lda PlayXPrev
            asl                 ; x2
            sta ScrX            ; byte at which to start drawing sprite
            
            lda #$40            ; background saved into $440, $460
            sta ZPtrG
            lda #$60
            sta ZPtrH
            lda ZCacheBase
            sta ZPtrG + 1
            sta ZPtrH + 1
            
            ldx #$00
            stx ZSprLine

            ; do a line
            ; set ZPtrE and ZPtrF to point to pages A and B of nondisplayed page
csblit:
            ldy ZTileCache, x
            lda YHiresH, y
            clc
            adc ZPageBase
            sta ZPtrF + 1       ; in bank 0, page B
            sec
            sbc #$20
            sta ZPtrE + 1       ; in bank 0, page A
            lda YHiresL, y
            clc
            adc ScrX
            sta ZPtrE
            sta ZPtrF
            
            ldy #$03
csblitline: lda (ZPtrG), y      ; saved background A
            sta (ZPtrE), y      ; screen byte A
            lda (ZPtrH), y      ; saved background B
            sta (ZPtrF), y      ; screen byte B
            dey
            bpl csblitline

            inc ZSprLine
            ldx ZSprLine
            cpx #$08
            beq csdone            
            
            ; push all data pointers ahead 4 bytes
            clc                 ; nothing below should set carry
            lda ZPtrG
            adc #$04
            sta ZPtrG
            lda ZPtrH
            adc #$04
            sta ZPtrH
            jmp csblit
csdone:            
            rts
            