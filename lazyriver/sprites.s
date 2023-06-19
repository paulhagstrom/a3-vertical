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
; actually $480 might be the first safe one in $400 because
; I am using $428-477 for a line buffer in mapscroll.
; draw sprites on nonvisible page

PgIndex:    .byte 0             ; 0 if page 1 nondisplayed, 1 page 2
ScrOffset:  .byte 0             ; place to store PgOneOff for drawing page
ScrX:       .byte 0             ; byte to start drawing (2x from left)
SprDrawn:   .byte 0, 0          ; nonzero if sprite was drawn on this page
SprX:       .byte 0, 0          ; X at which the sprite was drawn
SprY:       .byte 0, 0          ; Y at which the sprite was drawn

; TODO - SprDrawn should exist for each sprite
; point is not to erase a sprite we never drew, useful here for
; the first time through with the player sprite

; set ScrOffset, ZCacheBase, and ZPageBase, PgIndex
; return with x holding PgIndex
pgcompute:  lda ShownPage
            eor #$01            ; switch focus to nonvisible page
            and #$01            ; 0 if page 1 is nonvisible, 1 if page 2 is nonvisible
            tax
            lsr                 ; carry
            ror                 ; $80
            ror                 ; $40
            sta ZPageBase       ; $40 if nonvisible is page 2, $00 if page 1
            txa                 ; pgindex
            asl                 ; 2
            asl                 ; 4
            adc #$04            ; carry known to be clear
            sta ZCacheBase      ; $04 if nonvisible is page 1, $08 if page 2
            lda PgOneOff, x     ; keep track of the scroll value of the screen
            sta ScrOffset
            stx PgIndex
            rts

; compute the adjusted lines
; enter with A holding the screen line we're targeting
; (the sprite would go from there to there plus 7)
; assumes ScrOffset is already set for this screen

scrcompute: sta ZPxScratch
            clc
            adc ScrOffset
            and #$07            ; (y+offset)%8
            tay                 ; stash in y
            ldx #$00
:           lda ZPxScratch      ; retrieve PlayerY
            and #%11111000      ; 8*(int(y/8))
            sta ZTileCache, x
            tya                 ; retrieve (y+offset)%8
            clc
            adc ZTileCache, x   ; add to 8*(int(y/8))
            sta ZTileCache, x   ; and store it in the cache
            iny                 ; add one to y+offset
            tya
            and #$07            ; and mod 8
            tay                 ; put it back in y for next time around
            inc ZPxScratch      ; increase display line
            inx
            cpx #$08
            bne :-
            rts

; set the pointers into the graphics page (ZPtrE for A, ZPtrF for B)
; assuming that they will be accessed indirectly, so based at $0000
; instead of $2000. (1A: $0000, 1B: $2000, 2A: $4000, 2B: $6000)
; enter with:
; - x being the line of the sprite we are drawing (0-8)
; assumes:
; - ZPageBase has been set to $00 or $40 (pgcompute) for target page
; - ScrX is the byte to start at (2x x-coordinate)
; - scrcompute has computed the line adjustments for scroll offset

setgrptrs:  ldy ZTileCache, x   ; adjusted raster line for sprite line x
            lda YHiresH, y      ; look up $20-based line start high byte
            clc
            adc ZPageBase       ; adjust for page ($20 or $60)
            sta ZPtrF + 1       ; addressing within bank 0, B half
            sec
            sbc #$20            ; back to A half ($00 or $40)
            sta ZPtrE + 1       ; in bank 0, A half
            lda YHiresL, y
            clc
            adc ScrX            ; byte to draw on
            sta ZPtrE
            sta ZPtrF
            rts
            
setsprites: jsr pgcompute       ; set ScrOffset, PgIndex, ZCacheBase, ZPageBase
            
            ; set up pointer banks
            ; TODO - optimize so that these don't need to be set repeatedly
            lda #$80            ; bank zero (display at $0000-7FFF)
            sta ZPtrE + XByte   ; graphics A (e.g., 0000-1FFF)
            sta ZPtrF + XByte   ; graphics B (e.g., 2000-3FFF)
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
            lda #$00            ; data A (e.g., $1500)
            sta ZPtrA
            adc #$20            ; data B (e.g., $1520)
            sta ZPtrB
            adc #$20            ; mask A (e.g., $1540)
            sta ZPtrC
            adc #$20            ; mask B (e.g., $1560)
            sta ZPtrD
            
            lda PlayerY         ; where we would like to start
            jsr scrcompute      ; compute the adjusted lines
            lda PlayerX         ; map tile (0-19) we are on
            asl                 ; x2
            sta ScrX            ; byte at which to start drawing (evens 0-38)
            
            lda #$80            ; save background into $480, $4A0
            sta ZPtrG
            lda #$A0
            sta ZPtrH
            lda ZCacheBase      ; 4 or 8 depending on which page we're drawing on
            sta ZPtrG + 1
            sta ZPtrH + 1
            
            ldx #$00
spblit:     stx ZSprLine
            jsr setgrptrs       ; set ZPtrE/F for pages A/B of nondisplayed page
            
            ldy #$03
spblitline: lda (ZPtrE), y      ; screen byte A
            sta (ZPtrG), y      ; save background A
            and (ZPtrC), y      ; mask A
            ora (ZPtrA), y      ; data A
            ;lda DebugBrMagA, y
            sta (ZPtrE), y      ; replace screen byte A
            lda (ZPtrF), y      ; screen byte B
            sta (ZPtrH), y      ; save background B
            and (ZPtrD), y      ; mask B
            ora (ZPtrB), y      ; data B
            ;lda DebugBrMagB, y
            sta (ZPtrF), y      ; replace screen byte B
            dey
            bpl spblitline

            ldx ZSprLine
            inx
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
            ldx PgIndex
            lda #$01
            sta SprDrawn, x     ; mark this sprite as having been drawn
            lda PlayerY
            sta SprY, x         ; remember where we drew it
            lda ScrX
            sta SprX, x
            rts

; erase sprites on nonvisible page
; TODO - this is computing too much,
; should keep track of where the bytes are and just blast them back
clrsprites: jsr pgcompute       ; set ScrOffset, PgIndex, ZCacheBase, ZPageBase
            lda SprDrawn, x     ; check to be sure this sprite was actually drawn
            beq csdone          ; branch away to return if it was not drawn
            lda #$00
            sta SprDrawn, x     ; mark (in advance) this sprite as erased
            
            ; set up pointer banks
            ; TODO - optimize so that these don't need to be set repeatedly
            lda #$80            ; bank zero (display)
            sta ZPtrE + XByte   ; graphics A
            sta ZPtrF + XByte   ; graphics B
            lda #$8F            ; access to text pages
            sta ZPtrG + XByte   ; background cache A
            sta ZPtrH + XByte   ; background cache B
            
            lda SprX, x         ; recall the byte we drew the sprite at
            sta ScrX            ; should be (prior) PlayerX (in tiles) x2
            lda SprY, x         ; recall the Y we drew the sprite at
            jsr scrcompute      ; compute the adjusted lines
            
            lda #$80            ; background saved into $480, $4A0
            sta ZPtrG
            lda #$A0
            sta ZPtrH
            lda ZCacheBase      ; $4 if drawing on page 1, $8 if drawing on page 2
            sta ZPtrG + 1
            sta ZPtrH + 1
            
            ldx #$00
csblit:     stx ZSprLine
            jsr setgrptrs       ; set ZPtrE/F for pages A/B of nondisplayed page
            
            ldy #$03
csblitline: lda (ZPtrG), y      ; saved background A
            sta (ZPtrE), y      ; screen byte A
            lda (ZPtrH), y      ; saved background B
            sta (ZPtrF), y      ; screen byte B
            dey
            bpl csblitline

            ldx ZSprLine
            inx
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

; brown = 1000
DebugBrownA: .byte %00001000
DebugBrownB: .byte %01000100
DebugBrownC: .byte %00100010
DebugBrownD: .byte %00010001
; magenta = 0001
DebugMagenA: .byte %01000100
DebugMagenB: .byte %00100010
DebugMagenC: .byte %00010001
DebugMagenD: .byte %00001000
; brown/magenta A/B pairs
DebugBrMagA: .byte %00001000, %00100010, %01000100, %00010001
DebugBrMagB: .byte %01000100, %00010001, %00100010, %00001000
