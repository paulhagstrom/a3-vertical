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

; when we draw a sprite, we need to retrieve the background
; and store it somewhere (64 bytes), AND it with the mask,
; OR it with the graphics and store it on the page.
; and to erase, store the cached background back to the page.
; so we need 64 bytes per visible sprite for background cache.

; detect which page we are drawing to and set up some variables relating to that
; sets:
;   ZScrOffset (scroll offset of the target screen)
;   ZScrTop (map line corresponding to top line of screen)
;   ZCacheBase (offset into the half of the background cache that is relevant)
;   ZPageBase (offset into the half of the graphics memory that is relevant)
;   ZPgIndex (0 if we are targeting page 1, 1 if we are targeting page 2)
; returns with x holding ZPgIndex
pgcompute:  lda ShownPage
            eor #$01            ; switch focus to nonvisible page
            and #$01            ; 0 if page 1 is nonvisible, 1 if page 2 is nonvisible
            tax
            lsr                 ; carry
            ror                 ; $80
            ror                 ; $40
            sta ZPageBase       ; $40 if nonvisible is page 2, $00 if page 1
            ror                 ; $20
            sta ZCacheBase      ; $20 if nonvisible is page 2, $00 if page 1
            lda PgOneOff, x     ; keep track of the scroll value of the screen
            sta ZScrOffset
            lda PgOneTop, x     ; keep track of the map line at the top of screen
            sta ZScrTop
            stx ZPgIndex
            rts

; compute the mapping between the absolute lines the sprite will occupy and
; the adjusted lines taking into account the page scroll.
; And find the background cache
; reuses ZTileCache to hold the 8 adjusted lines
; pgcompute must be called first (to set ZScrOffset and ZCacheBase)
; ZCurrSpr must hold sprite number
; enter with A holding the screen line we're targeting for the first sprite line
; exits with Y holding sprite number
adjcompute: sta ZPxScratch      ; stash sprite absolute raster
            clc
            adc ZScrOffset
            and #$07            ; (y+offset)%8
            tay                 ; stash in y
            ldx #$00
:           lda ZPxScratch      ; retrieve sprite absolute raster
            and #%11111000      ; 8*(int(y/8))
            sta ZTileCache, x
            tya                 ; retrieve (y+offset)%8
            clc
            adc ZTileCache, x   ; add to 8*(int(y/8))
            sta ZTileCache, x   ; and record the adjusted line
            iny                 ; add one to y+offset
            tya
            and #$07            ; and mod 8
            tay                 ; put it back in y for next time around
            inc ZPxScratch      ; increase display line
            inx
            cpx #$08
            bne :-
            ; set up the cache base
            ldy ZCurrSpr
            lda (ZSprBgL), y    ; set ZPtrCacheA/B to background cache address
            sta ZPtrCacheA
            clc
            adc #$20
            sta ZPtrCacheB
            lda (ZSprBgH), y
            clc
            adc ZCacheBase      ; $00 for page 1, $20 for page 2
            sta ZPtrCacheA + 1  ; $10+ for page 1, $30+ for page 2
            sta ZPtrCacheB + 1            
            rts

; set the pointers into the graphics page (ZPtrScrA for A, ZPtrScrB for B)
; they will be accessed indirectly, so based at $0000 not $2000
; (1A: $0000, 1B: $2000, 2A: $4000, 2B: $6000)
; pgcompute must be called first (to set ZPageBase)
; adjcompute must be called first (to set ZTileCache)
; enter with:
; - x being the line of the sprite we are drawing (0-8)
; - ZScrX is the byte to start at drawing at (2 times tile x-coordinate)
; x survives, a and y do not

setgrptrs:  ldy ZTileCache, x   ; adjusted raster line for sprite line x
            lda YHiresH, y      ; look up $20-based line start high byte
            clc
            adc ZPageBase       ; adjust for page ($20 or $60)
            sta ZPtrScrB + 1    ; addressing within bank 0, this is B half
            sec
            sbc #$20            ; back to A half ($00 or $40)
            sta ZPtrScrA + 1    ; in bank 0, A half
            lda YHiresL, y
            clc
            adc ZScrX           ; byte to draw on
            sta ZPtrScrA
            sta ZPtrScrB
            rts

; draw the sprites on the nonvisible page
; draw the sprites in reverse order, ending with the player
; in case of overlap, lower number sprites will be on top,
; player atop all.

setsprites: jsr pgcompute       ; ZScrOffset, etc.
            ; draw the log sprites
            lda NumLogs         ; NumLogs is 0-based
            sta LogsLeft
setlog:     ldy LogsLeft        ; this is the sprite number
            jsr putsprite       ; draw the log
            dec LogsLeft
            bpl setlog
            ; draw player
            ldy #$7F            ; player sprite is number 127
            jsr putsprite       ; draw the player
sproffscr:  rts                 ; cheat and use this rts for putsprite below

; if top of screen is 232, offset 0
; and sprite is on the map at 234, offset 4
; then we start drawing the sprite at line
; 8 + 8 * (234 - 232) + 4 = 28
; if screen offset were 2, then only 6 lines of 232 are displayed.
; so sprite starts off the top of the screen if it were on 232 with offset < 2 
; so actual target raster is:
; 8 + (8 * (ytile - ytop)) + yoffset - scroffset
; if that is less than 8, sprite is at least partially offscreen
; similarly if ytile - ytop is >=22 and offsets not 0, sprite is partially offscreen
; draw one sprite
; entry:
;   y = sprite number to draw
putsprite:  sty ZCurrSpr
            lda (ZSprY), y      ; check to see if Y coordinate is onscreen
            sec
            sbc ZScrTop
            bcc sproffscr       ; sprite is above top screen line, skip
            cmp #22             ; conservative for now, would be ok if offsets are 0
            bcs sproffscr       ; sprite is below the bottom screen line, skip
            asl                 ; compute target y raster
            asl
            asl                 ; distance in map lines from top, times 8
            clc
            adc #$08            ; plus 8 (after score text line)
            sec
            sbc ZScrOffset      ; decreased by screen offset
            clc
            adc (ZSprYOff), y   ; increased by sprite Y coordinate offset
            ldx ZPgIndex
            beq :+
            sta (ZSprDrYTwo), y ; remember where we drew this (on page 2)
            .byte $2C           ; opcode for BIT, eats next instruction
:           sta (ZSprDrYOne), y ; remember where we drew this (on page 1)
            jsr adjcompute      ; compute the adjusted lines, locate bg cache
            lda (ZSprX), y      ; load x map position (0-18)
            asl                 ; x2 to get byte position
            sta ZScrX           ; byte to start drawing at (evens 0-38)
            ldx ZPgIndex
            beq :+              ; branch away if we're drawing on page 1
            sta (ZSprDrXTwo), y ; remember where we drew this (on page 2)
            .byte $2C           ; opcode for BIT, eats next instruction
:           sta (ZSprDrXOne), y ; remember where we drew this (on page 1)
            lda (ZSprSprH), y   ; page where the sprite data starts
            clc
            adc (ZSprXOff), y   ; x shift
            sta ZPtrSprA + 1
            sta ZPtrSprB + 1
            sta ZPtrMaskA + 1
            sta ZPtrMaskB + 1
            lda (ZSprAnim), y   ; current frame
            lsr                 ; into carry
            ror                 ; $80 (frame 2) or $00 (frame 1)
            sta ZPtrSprA        ; data A (e.g., $1500 or $1580)
            adc #$20            ; data B (e.g., $1520 or $15A0)
            sta ZPtrSprB
            adc #$20            ; mask A (e.g., $1540 or $15C0)
            sta ZPtrMaskA
            adc #$20            ; mask B (e.g., $1560 or $15E0)
            sta ZPtrMaskB
            
            ldx #$00                ; start at line 0
spblit:     jsr setgrptrs           ; set ZPtrScrA/B for pages A/B based on line
            ldy #$03
spblitline: lda (ZPtrScrA), y       ; screen byte A
            sta (ZPtrCacheA), y     ; save background A
            and (ZPtrMaskA), y      ; mask A
            ora (ZPtrSprA), y       ; data A
            sta (ZPtrScrA), y       ; replace screen byte A
            lda (ZPtrScrB), y       ; screen byte B
            sta (ZPtrCacheB), y     ; save background B
            and (ZPtrMaskB), y      ; mask B
            ora (ZPtrSprB), y       ; data B
            sta (ZPtrScrB), y       ; replace screen byte B
            dey
            bpl spblitline
            inx
            cpx #$08
            beq spdone              ; branch away if we have done all the lines
            jsr pushcache           ; push cache pointers ahead 4 bytes
            jsr pushdata            ; push data pointers ahead 4 bytes
            jmp spblit
spdone:     rts

; push the cache pointers ahead 4 bytes
pushcache:  clc                     ; nothing below should set carry
            lda ZPtrCacheA
            adc #$04
            sta ZPtrCacheA
            lda ZPtrCacheB
            adc #$04
            sta ZPtrCacheB
            rts

; push the data pointers ahead 4 bytes
; assumes carry is already clear
pushdata:   lda ZPtrSprA
            adc #$04
            sta ZPtrSprA
            lda ZPtrSprB
            adc #$04
            sta ZPtrSprB
            lda ZPtrMaskA
            adc #$04
            sta ZPtrMaskA
            lda ZPtrMaskB
            adc #$04
            sta ZPtrMaskB
            rts

; erase sprites on nonvisible page
clrsprites: jsr pgcompute       ; set ZScrOffset, etc
            ; clear the player
            ldy #$7F
            jsr clrsprite
            ; clear the log sprites, in reverse (in case of overlap)
            ldy #$00
clrlog:     sty LogsLeft
            jsr clrsprite
            ldy LogsLeft
            cpy NumLogs
            beq :+              ; branch away if we did the last one
            iny
            bne clrlog          ; branch always
:           rts

; clear a single sprite
; entry: Y holds the sprite number
clrsprite:  sty ZCurrSpr
            lda ZPgIndex        ; check to see if it was actually drawn
            beq :+              ; branch away if we are dealing with page 1
            lda (ZSprDrXTwo), y ; recall where we drew this (on page 2)
            bmi csdone          ; skip away if it was not drawn
            sta ZScrX
            lda #$FF            ; mark it (in advance) as erased
            sta (ZSprDrXTwo), y
            lda (ZSprDrYTwo), y ; recall where we drew this (on page 2)
            jmp :++
:           lda (ZSprDrXOne), y ; recall where we drew this (on page 1)
            bmi csdone          ; skip if the sprite was not drawn
            sta ZScrX           ; should be (prior) x (in tiles) x2
            lda #$FF            ; mark it (in advance) as erased
            sta (ZSprDrXOne), y
            lda (ZSprDrYOne), y ; recall where we drew this (on page 1)
:           jsr adjcompute      ; compute the adjusted lines, locate bg cache
            ldx #$00
csblit:     jsr setgrptrs       ; set ZPtrScrA/B for pages A/B of nondisplayed page
            ldy #$03
csblitline: lda (ZPtrCacheA), y     ; saved background A
            sta (ZPtrScrA), y       ; screen byte A
            lda (ZPtrCacheB), y     ; saved background B
            sta (ZPtrScrB), y       ; screen byte B
            dey
            bpl csblitline
            inx
            cpx #$08
            beq csdone    
            jsr pushcache           ; push cache pointers ahead 4 bytes
            jmp csblit
csdone:     rts

; brown = 1000
;DebugBrownA: .byte %00001000
;DebugBrownB: .byte %01000100
;DebugBrownC: .byte %00100010
;DebugBrownD: .byte %00010001
; magenta = 0001
;DebugMagenA: .byte %01000100
;DebugMagenB: .byte %00100010
;DebugMagenC: .byte %00010001
;DebugMagenD: .byte %00001000
; brown/magenta A/B pairs
;DebugBrMagA: .byte %00001000, %00100010, %01000100, %00010001
;DebugBrMagB: .byte %01000100, %00010001, %00100010, %00001000
