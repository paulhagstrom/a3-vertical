; lazyriver
; sprite routines

; A sprite will be a 7x8 block, which corresponds to 32 bytes.  But given that
; it can be at any x-coordinate, we need to consider it to be a 14x8 block,
; 64 bytes.  We will pre-shift them so that we have these bytes ready to go.
; The y-coordinates are made additionally complicated due to the smooth
; scroll.
;
; Sprites live on the map, which means they have an x and y coordinate in map
; tiles, and then an x and y offset from the top left of the map tile.
; Their position on the screen is determined by these coordinates in conjunction
; with the map row responsible for the first line of the screen and the smooth
; scroll offset of the screen.
;
; Concretely: suppose the top row is map line 232, the screen has scrolled 4
; and the sprite (Y) is at map line 234, offset 2.  The first hires line is
; on line 8, since the top 8 lines are in text mode showing the score.
; The first group of 8 lines display 4 lines of row 232 and 4 lines of row 233.
; The next group of 8 lines display 4 lines of row 233 and 4 lines of row 234.
; This second group will contain the sprite's start line, 2 from the top of 234.
; Lines  8-11 contain row 233 lines 0-3  1
; Lines 12-15 contain row 232 lines 4-7  0
; Lines 16-19 contain row 234 lines 0-3  2
; Lines 21-23 contain row 233 lines 4-7  1
; Lines 24-27 contain row 235 lines 0-3  3
; Lines 28-31 contain row 234 lines 4-7  2
; So: the sprite's start line (234+2) is line 18.
; and it extends 8 lines: 18 19 28 29 30 31 24 25
; crazy right?
; operationally, it is:
;                       yoffset 2               3   4   5   6   7   8   9
;                   yoffset % 8 2               3   4   5   6   7   0   1
; (y - top)                     234-232 = 2     2   2   2   2   2   2   2
; - 1 if yoffset%8 < scroffset  2 < 4? Y, = 1   1   2   2   2   2   1   1
; * 8                           1 * 8 = 8       8   16  16  16  16  8   8
; + yoffset                     + 2 = 10        11  20  21  22  23  16  17
; + 8 (score line)              + 8 = 18        19  28  29  30  31  24  25
; 
; we don't really need to do that computation for every line, we can do it
; for the first one and then get the other lines incrementally, like:
;
; prior                 18  19  28  29  30  31  24
; - 8 (score line)      10  11  20  21  22  23  16
; + 1                   11  12  21  22  23  24  17
; (%8                   3   4   5   6   7   0   1)
; +8 if %8 = scroffset  11  20  21  22  23  24  17
; -8 if %8 = 0          11  20  21  22  23  16  17
; +8 (score line)       19  28  29  30  31  24  25
; 
; which also means that we can compute the start line when we draw
; the sprite, then save it for use when we erase the sprite, and
; not also save the map coordinates of the sprite being erased
; (since the map coordinates might change between draw and erase).
;
; horizontally, we use two tile-widths to hold the sprite
; we draw at 0-1 if sprite starts between 0 and 6
; we draw at 1-2 if sprite starts between 7 and 13, etc.
; we have seven versions of those two bytes, with the sprite
; starting at each possible position (e.g., between 0 and 6)
; the last one is 19-20 if sprite starts between 127 and 133.

; when we draw a sprite, we need to retrieve the background first
; and store it somewhere (64 bytes), AND it with the mask,
; OR it with the graphics and store it on the page.
; and to erase, store the cached background back to the page.
; so we need 64 bytes per visible sprite for background cache.

; detect which page we are drawing to and set up some variables relating to that
; called before starting to draw any sprites, no entry requirements
; sets:
;   ZScrOffset (scroll offset of the target screen)
;   ZScrTop (map line corresponding to top line of screen)
;   ZCacheBase (offset into the half of the background cache that is relevant)
;   ZPageBase (offset into the half of the graphics memory that is relevant)
;   ZPgIndex (0 if we are targeting page 1, 1 if we are targeting page 2)
; returns with x holding ZPgIndex, y unaffected
pgparams:   lda ShownPage
            eor #$01            ; switch focus to nonvisible page
            and #$01            ; 0 if page 1 is nonvisible, 1 if page 2 is nonvisible
            sta ZPgIndex
            tax
            lsr
            ror
            ror
            sta ZPageBase       ; $40 if nonvisible is page 2, $00 if page 1
            ror
            sta ZCacheBase      ; $20 if nonvisible is page 2, $00 if page 1
            lda PgOneOff, x     ; scroll value of the screen
            sta ZScrOffset
            lda PgOneTop, x     ; map line at the top of screen
            sta ZScrTop
            rts

; calculate the raster line where the sprite starts
; enter with y holding the sprite number
; exits with starting raster line in A (and X invalidated), y persists
; also exits with carry clear if A was valid, or set if sprite was offscreen
calcraster: lda (ZSprY), y      ; check to see if Y coordinate is onscreen
            sec
            sbc ZScrTop
            bcc sproffscr       ; sprite is above top screen line, skip
            cmp #22             ; conservative for now, would be ok if offsets are 0
            bcs sproffscr       ; sprite is below the bottom screen line, skip
            tax                 ; stash difference
            lda (ZSprYOff), y   ; sprite Y coordinate offset (will be mod 8 already)
            cmp ZScrOffset      ; is Y offset < smooth scroll parameter?
            bcs :+              ; branch away if not
            dex                 ; subtract 1 from difference is so
:           txa
            asl
            asl
            asl                 ; adjusted difference * 8
            adc (ZSprYOff), y   ; + Y offset (carry known to be clear)
            adc #$08            ; skip past the top text line (carry still clear)
            bcc :+              ; branch always
sproffscr:  sec
:           rts

; lookup/compute the sprite paramters (background cache location and screen lines)
; when computing the screen lines, must take screen's smooth scroll into account
; pgparams must be called earlier (to set ZScrOffset and ZCacheBase)
; raster computation and cache location are combined because both drawing and erasing
; sprites need to do both.  Not logically related, but saves an extra jsr/rts trip.
; enter with A holding the (absolute) raster line we're targeting for the first sprite line
; that should be computed with calcraster or by retrieving it from the cache
; assumes ZCurrSpr has already been set with sprite number
; will put the adjusted lines into ZRastCache (8 bytes), and set ZPtrCacheA/B
; exits with Y holding sprite number, A and X are invalidated
sprparams:  ldx #$00
            beq sprrastb        ; branch always, first one already passed in
sprrasta:   sec
            sbc #$07            ; -8 to adjust for score line, +1 to move to next
            pha
            and #$07            ; mod 8
            tay                 ; stash mod 8
            pla
            cpy ZScrOffset      ; mod 8 = smooth scroll paramter?
            bne :+              ; branch away if not
            clc
            adc #$08            ; +8 if mod 8 = smooth scroll parameter
:           cpy #$00            ; mod 8 = 0?
            bne :+              ; branch away if not zero
            sec
            sbc #$08            ; -8 if mod 8 was zero
:           clc
            adc #$08            ; +8 to adjust for score line
sprrastb:   sta ZRastCache, x
            inx
            cpx #$08
            bne sprrasta
            ; set up the pointers to the background cache for this sprite
            ldy ZCurrSpr
            lda (ZSprBgL), y    ; set ZPtrCacheA/B to background cache address
            sta ZPtrCacheA
            clc
            adc #$20
            sta ZPtrCacheB
            lda (ZSprBgH), y    ; has a $1000 base
            clc
            adc ZCacheBase      ; $00 for page 1, $20 for page 2
            sta ZPtrCacheA + 1
            sta ZPtrCacheB + 1            
            rts

; set the pointers into the graphics page (ZPtrScrA for A, ZPtrScrB for B)
; they will be accessed indirectly, so based at $0000 not $2000
; (1A: $0000, 1B: $2000, 2A: $4000, 2B: $6000)
; pgparams must be called first (to set ZPageBase)
; sprparams must be called first (to set ZRastCache)
; enter with:
; - x being the line of the sprite we are drawing (0-7)
; - ZScrX is the byte to start at drawing at (2 times tile x-coordinate)
; x survives, a and y do not
setgrptrs:  ldy ZRastCache, x   ; adjusted raster line for sprite line x
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
setsprites: jsr pgparams        ; get the page parameters (ZScrOffset, etc.)
            ; draw the log sprites
            lda NumLogs         ; NumLogs is 0-based
            sta LogsLeft
setlog:     ldy LogsLeft        ; this is the sprite number
            jsr putsprite       ; draw the log
            dec LogsLeft
            bpl setlog
            ; draw player
            ldy #SprPlayer      ; player sprite
            jsr putsprite       ; draw the player
            rts

; draw one sprite
; entry:
;   y = sprite number to draw            
putsprite:  sty ZCurrSpr        ; stash the sprite number where we can find it
            jsr calcraster      ; compute raster for top line of sprite
            bcs spdone          ; branch away if it is not onscreen
            pha                 ; stash it
            lda (ZSprX), y      ; load x map position (0-18)
            asl                 ; x2 to get byte position
            sta ZScrX           ; byte to start drawing at (evens 0-38)            
            ldx ZPgIndex
            beq :+              ; branch away if we are drawing to page 1
            sta (ZSprDrXTwo), y ; remember X where we drew this (on page 2)
            pla
            sta (ZSprDrYTwo), y ; remember Y where we drew this (on page 2)
            jmp :++
:           sta (ZSprDrXOne), y ; remember X where we drew this (on page 1)
            pla
            sta (ZSprDrYOne), y ; remember Y where we drew this (on page 1)
:           jsr sprparams       ; compute the adjusted lines, locate bg cache
            lda (ZSprSprH), y   ; page where the sprite data starts
            clc
            adc (ZSprXOff), y   ; add shift for x offset
            sta ZPtrSprA + 1
            sta ZPtrSprB + 1
            sta ZPtrMaskA + 1
            sta ZPtrMaskB + 1
            lda (ZSprAnim), y   ; current frame
            lsr
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
clrsprites: jsr pgparams        ; get the page parameters (ZScrOffset, etc.)
            ; clear the player
            ldy #SprPlayer
            jsr clrsprite
            ; clear the log sprites, in reverse (in case of overlap)
            ldy #$00
clrlog:     sty LogsLeft
            jsr clrsprite
            ldy LogsLeft
            cpy NumLogs
            beq cssdone         ; branch away if we did the last one
            iny
            bne clrlog          ; branch always
cssdone:    rts

; clear a single sprite
; entry: Y holds the sprite number
clrsprite:  sty ZCurrSpr
            lda ZPgIndex
            beq :+              ; branch away if we are dealing with page 1
            lda (ZSprDrXTwo), y ; recall where we drew this (on page 2)
            bmi csdone          ; skip away if it was not drawn
            sta ZScrX           ; should be (prior) x (in tiles) x2
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
:           jsr sprparams       ; compute the adjusted lines, locate bg cache
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
