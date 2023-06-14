; lazyriver
; A3 hires display map routines

; Smooth scrolling can dramatically reduce the amount of data that needs to
; be moved to scroll the entire screen, since you only need to move 24 lines
; instead of 192.  However, even moving 24 lines is a lot of data, and it is
; impossible to move all those bytes within the space of a single VBL, meaning
; that there will be tearing.  Fully unrolling loops and using stack relocation
; tricks can get close, but to do anything but scroll (like, to have sprites)
; would exhaust the time we have between screen refreshes.  Plus, a fully
; unrolled loop is essentially a very space-inefficient framebuffer for an
; eighth of the screen, and it will complicate sprite handling (which needs
; to use the other lines too).  So, rather than trying to move data during
; VBL, we'll use the maximally efficient framebuffer we already have: page 2.
; A3 Hires allows for page flipping, with page 1 in $2000-5FFF and page 2 in
; $6000-9FFF.  That's a lot of memory for graphics, but the upside is: no
; tearing and very little cycle counting.  We just draw on the page we're not
; looking at until we're done, then wait for VBL and flip which page we're
; looking at.
;
; To use smooth scrolling together with page flipping means that we lose half
; of the advantage smooth scrolling gave us, but it's still 4x fewer bytes to
; move than without smooth scrolling (vs. 8x fewer without page flipping).
; That is, we have to move the invisible page twice potentially.
; Note: if we are scrolling less of the screen (as in diskhero), it is more
; achievable.  There, there are only 8 lines to copy.  What's different here
; is that we are going for essentially a full-screen (184 line) scroll.
;
; One strategy I considered was to use page 1 for even offsets and page 2
; for odd offsets, which would work relatively well if the background always
; continually scrolls at full speed (one line per game clock).  But if the
; background ever stops scrolling, then we're not flipping and so we need to
; erase and redraw all the sprites in the confines of a single VBL.  This
; would only allow for maybe 5 sprites before we run out of time.
;
; A more flexible strategy that will allow for more sprites without tearing
; is to use the pages as just framebuffers, kept basically in sync and always
; alternating whether the background scrolls or not.  This still will often
; require shifting 2 lines on the invisible page.  Ultimately this will wind
; up be simpler to handle.
;
; To scroll. since we are moving one of every 8 lines, it's actually
; pretty natural.  We can move each block of $80 ($77 really, with the
; screen holes), either up $80 or down $80, to advance or retreat 8 lines.
; Takes some care around the edges, but can be about as efficient as a
; memory move generally, without needing to do a lot of computation about
; what lines start where.
;
; Lines that correspond to addresses between $2000-23F7 (matches Apple II
; layout).  Second half of these lines are in $4000-43F7, page 2 is
; from $6000-9FF7.
;
; 2000: 00 40 80 08 48 88
; 2100: 10 50 90 18 58 98
; 2200: 20 60 A0 28 68 A8
; 2300: 30 70 B0 38 78 B8
;
; For sprites: We need them to be erased by the time we scroll the
; background.  The page-flipping strategy will thus look like this:
;
; at VBL downbeat, flip to page A
; erase sprites on page B
; scroll page B to match page A (if they don't already match)
; do movement computations (scroll? sprites? collisions?)
; scroll page B if needed
; draw sprites on page B
; (swap B and A meanings and repeat)
;
; My guess on the speed here is that it will take 2 refreshes to do both
; scrolls, maybe 2 more to erase and draw the sprites, maybe 1 more to do
; movement computations.  So the guess is that we'll wind up with a frame
; swap every 5 refreshes, or 6fps.  That should look pretty good on an
; 8-bit machine.  VBL is about 9100 cycles, screen painting 12480, with
; 520 cycles spent every 8 lines during screen painting.  So given that
; we have the top 8 lines in text mode, we have about 9620 cycles after
; VBL starts before the beam hits the graphics again, minus how many get
; eaten by interrupt handling (for, e.g., sound).  However, by using page-
; flipping, we don't really need to care much about the beam, we just have
; about 21580 cycles per refresh from which to figure how our fps is.
;
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
; int(y/8) + (y+offset)%7
;
; To draw the sprite itself, we need to compute the targets and then move
; the data.  The data is 64 bytes, and we want to load the background, stash
; it somewhere, apply a mask, draw, and then store.  That's minimally
; in the ballpark of 1280 cycles per sprite, probably will be somewhat
; more.

; movement processing happens on the VBL downbeat, so wait until the next one to 
; update graphics.
; This checks to see if scrolling is needed (domove communicates via NeedScroll).
; called always but if no scrolling is needed, it exits quickly with carry clear.
; if it does scroll, it will exit with carry set to indicate that VBL is probably used up.
; value for NeedScroll is: 0=stop, neg=map down/dec off, pos=map up/inc off

fixscroll:  clc                 ; carry clear = both "scroll up" and "was quick"
NeedScroll = *+1
            lda #INLINEVAR      ; 0 - no scroll needed, >7F map down, else map up
            beq noscroll
            bmi scrolldn
            bcc scrollup        ; branch always
scrolldn:   sec
scrollup:   jsr scrollmap       ; scroll the screen (using smooth scroll)
            lda #$00
            sta NeedScroll      ; we no longer need a scroll
            sec                 ; tell eventloop we took some time
noscroll:   rts

; synchronize the scroll on the page we are not looking at with the scroll
; of the page we are looking at.
; PgOneOff and PgTwpOff will always be either equal or one apart.
; if they are equal, quit quickly, if they're one apart, bring them together.
; if offset 1 > offset 2 and we are looking at page 1, clc (increase p2)
; if offset 1 > offset 2 and we are looking at page 2, sec (decrease p1)
; if offset 2 > offset 1 and we are looking at page 1, sec (decrease p2)
; if offset 2 > offset 1 and we are looking at page 2, clc (increase p1)
; scrollmap always operates on the page we are not looking at

syncscroll: lda PgOneOff
            cmp PgTwoOff
            bne syncneeded
            clc                 ; equal also means carry set, so need to clear
            rts                 ; return, indicating that we spent no time
            ; sync needed
syncneeded: bcc ssptwohigh      ; branch away if page 1 is less than page 2
            ; page 1 is greater than page 2
            lda ShownPage
            eor #$01
            lsr
            ; carry set if we're looking at p1, clear if we're looking at p2
            ; which tells scrollmap what direction to scroll the non-visible page
            bpl ssdoscroll      ; branch always
            ; page 2 is greater than page 1
ssptwohigh: lda ShownPage
            lsr
            ; carry clear if we're looking at p1, set if we're looking at p2
            ; which tells scrollmap what direction to scroll the non-visible page
ssdoscroll: jsr scrollmap
            sec                 ; tell the event loop we took substantial time
            rts

; read a line of tiles from the map into the zero page cache ($00-$13)
; cache will hold index into tile graphics assets (shape x 32, each shape is 32 bytes of data)
; a line is 20 ($14) bytes long in the map representation
; enter with: X being the row of the map we are caching
; assumes:
; - ZMapPtr + XByte has already been set to #$82 (bank 2)
; - ZP is at $1A00
; does not disturb X

tilecache:  lda MapLineL, x
            sta ZMapPtr
            lda MapLineH, x
            sta ZMapPtr + 1
            ; read the map line, compute indices into tile graphics, and cache
            ; buffer the pointers into tile graphics for the line
            ldy #19
pmcache:    lda (ZMapPtr), y    ; load map byte (shape to draw)
            lsr
            ror
            ror
            ror                 ; multiply by 32 for index into tiles
            sta ZTileCache, y
            dey
            bpl pmcache
            rts

; set up pointers and banks for graphics interaction
; bank 0 for graphics, ZPtrA for map tiles
; X, Y, carry survive
gfxinit:    lda #$82            ; map is bank 2
            sta ZMapPtr + XByte
            sta ZPtrA + XByte
            lda #$34            ; tile graphics start at $3400
            sta ZPtrA + 1
            lda #$00
            sta R_BANK          ; move to bank zero for the graphics
            sta ZPtrA           ; tile graphics low byte
            rts

; paint a single line from cached tiles on the nonvisible page
; call with:
; - X holding the physical graphics line (0-BF)
; - Y holding the offset into tile we should draw onto the physical graphics line
; assumes:
; - tile cache holds the tile graphics addresses for tiles including this line
; - ZP is $1A00
; - ZPtrA already points to tile asset data in bank 2
; each tile takes two bytes on the screen, so index of start of pixel data
; on the screen is 2x the index into cache of tile data
; (That is: tile 3 is filling bytes 6 and 7 on the screen)
; does not preserve A, X, or Y.

paintline:  lda YHiresLA, x     ; look up the bases for all four tile bytes
            sta PMLineA
            sta PMLineB
            lda YHiresLB, x
            sta PMLineC
            sta PMLineD
            lda ShownPage
            eor #$01            ; focus on unshown page
            and #$01
            lsr                 ; carry set for page 2 clear for page 1
            ror                 ; $80 if page 2 and carry clear
            ror                 ; $40 if page 2 and carry clear
            pha                 ; save for setting page B
            adc YHiresHA, x
            sta PMLineA + 1
            sta PMLineC + 1
            pla                 ; $40 if page 2, 0 if page 1, carry still clear
            adc YHiresHB, x
            sta PMLineB + 1
            sta PMLineD + 1
            tya                 ; line of tile to draw
            asl
            asl                 ; x4 (each line in tile is 4 bytes)
            sta ZTileOff        ; remember this for reuse later
            clc                 ; clear for addition, will remain clear
            lda #19             ; start at the last tile column
            sta ZMapX
pmraster:   ldx ZMapX           ; tile column
            lda ZTileCache, x   ; load cached tile art start offset
            adc ZTileOff        ; add line offset
            tay
            txa
            asl                 ; tile column x2 for graphics memory offset
            tax
            lda (ZPtrA), y      ; first byte of tile graphics
PMLineA = *+1
            sta INLINEADDR, x   ; put in graphics page $20
            iny
            lda (ZPtrA), y
PMLineB = *+1
            sta INLINEADDR, x   ; put in graphics page $40
            iny
            lda (ZPtrA), y
PMLineC = *+1
            sta INLINEADDR, x   ; put in graphics page $20, next byte
            iny
            lda (ZPtrA), y
PMLineD = *+1
            sta INLINEADDR, x   ; put in graphics page $40, next byte
            dec ZMapX
            bpl pmraster
            rts

; paint the whole page (called at the outset) on the nonvisible page
; (updates afterwards are incremental, drawing single lines and using smooth scroll)
; assumes smooth scroll offset is 0
; will be followed by a call to copypage that will copy our work over to page 1

paintpage:  lda R_BANK          ; save bank (but assume we are already in 1A00 ZP) 
            sta PMBankSave      ; save it inline within later restore code.
            jsr gfxinit         ; set up pointers and switch banks (paintline needs it)
            lda #$BF            ; we are drawing from the bottom up
            sta ZCurrScrL       ; current screen line
            lda PgOneTop        ; map line of the top row on the screen
            clc
            adc #22             ; +22 to get to map line of bottom row
            sta ZCurrMapL       ; becomes the current line
            lda #23
            sta ZLinesLeft
pmgetmap:   ldx ZCurrMapL
            jsr tilecache
            ; go through the 8 raster lines for this map tile
            lda #$07
            sta ZCurrOff
pmtile:     ldx ZCurrScrL
            ldy ZCurrOff
            jsr paintline
            ; move to next raster line in tile
            dec ZCurrScrL
            dec ZCurrOff
            bpl pmtile
            ; move to next map line (moving up the screen)
            dec ZCurrMapL
            dec ZLinesLeft
            bne pmgetmap
            ; painting map is finished, restore bank and exit
PMBankSave = *+1
            lda #INLINEVAR
            sta R_BANK
            rts

; copy the whole of page 2 (presumed just to have been painted)
; to page 1

copypage:   lda R_BANK          
            sta CPBankSave      ; save bank inline within later restore code.
            lda #$00            ; switch to bank 0 for graphics
            sta R_BANK
            lda #$60            ; beginning of page 1
            sta CMSrcPg
            lda #$20            ; beginning of page 2
            sta CMTrgPg
            ldy #$40            ; we are moving $40 pages
            ldx #$00
CMSrcPg = *+2
cmloop:     lda $6000, x        ; INLINEADDR with 00 low byte
CMTrgPg = *+2
            sta $2000, x        ; INLINEADDR with 00 low byte
            inx
            bne cmloop
            inc CMSrcPg
            inc CMTrgPg
            dey
            bne cmloop
            ; copying page is finished, restore bank and exit
CPBankSave = *+1
            lda #INLINEVAR
            sta R_BANK
            rts

; imagine you are at TopRow 230, TopOff 0
; and you increase the nudge, which means you are now drawing from 1-7 and then 0.
; this has the effect of scrolling the map up the screen
; you need to copy line 0 of 231 to line 0 of 230.
; and so on down the screen until you get to the last row.
; there, you need to copy line 0 of 253 (230+23) into line 0 of 252 (230+22)
; nudge went from 0 to 1 and we are doing everything wrt the lowest of these, 0.

; now imagine you are back at TopRow 230, TopOff 0
; and you decrease the nudge, which means you are now drawing 7 and then 0-6.
; this should have the effect of scrolling the map down the screen
; you need to copy line 7 of 229 to line 7 of 230, and so on down the screen.
; except for this to work you have to go up the screen.
; copy line 7 of 251 into line 7 of 252, then of 250 into 251, etc.
; nudge went from 0 to 7 and we are doing everything wrt the lowest of these, 7
; 

; in general, increasing nudge from oldnudge to oldnudge + 1
; this makes the screen scroll up; abstractly:
; copy line 1 of tile 2 to line 1 of tile 1, etc.
; then draw new line 1 of tile 24 drawn from line 1 of "tile 25"
; specifically:
; copy graphics line 10 + oldnudge to 08 + oldnudge
; copy graphics line 18 + oldnudge to 10 + oldnudge
; ...there are C0 (192) total lines ...
; copy graphics line A8 + oldnudge to B0 + oldnudge
; copy graphics line B0 + oldnudge to B8 + oldnudge
; draw map line for graphics line C0 + oldnudge on graphics line B8 + oldnudge
; and then advance nudge to become oldnudge + 1
;
; to move back up (decreasing nudge from nudge + 1 to nudge)
; this makes the screen scroll down
; abstractly: copy line 8 of tile 23 to line 8 of tile 24, etc.
; then draw new line 8 of tile 1 drawn from line 8 of "tile 0"
; specifically:
; copy graphics line B0 + nudge to B8 + nudge
; copy graphics line A8 + nudge to B0 + nudge
; ...there are C0 (192) total lines ...
; copy graphics line 10 + nudge to 18 + nudge
; copy graphics line 08 + nudge to 10 + nudge
; draw map line for graphics line 00 + nudge on graphics line 08 + nudge

; as usual, it is hard to wrap my head around this.
; Suppose TopRow is 232 and TopOff is 0, where we start.
; and the river scrolls down.
; so the old line 08 is now displayed on line 09
; TopOff becomes 7, need to draw new top line on line 0F (will draw at top)
; so I load in the map line 231 (post-move TopRow), and buffer line 7 (post-move TopOff) into 0
; copy all the line 7s down from end of screen to top 
; if it were going the other way,
; TopOff becomes 1, TopRow stays 232.
; now I need to copy from top to bottom, and then buffer line TopRow+24's line old offset (0) into 0
; this is pretty doable.

; scrollmap will effect a vertical movement of the map regions of the nonvisible screen.
; locates new line and puts it in raster 0, then calls copylines to do the scroll.
; enter with:
; - carry clear: move the map up (hero downward), increasing nudge
; - carry set: move the map down (hero upward), decreasing nudge

scrollmap:  lda R_BANK          ; save bank
            sta SMBankSave      ; (but assume we are already in 1A00 ZP)
            ror                 ; put carry in hi bit (the other 7 bits don't matter for anything)
            sta SMIncDec
            asl                 ; restore carry
            jsr gfxinit         ; set up pointers and switch banks (paintline needs it)
            lda ShownPage       
            eor #$01            ; switch focus to nonvisible page
            and #$01            ; 0 if page 1 is nonvisible, 1 if page 2 is nonvisible
            tax
            lda PgOneOff, x     ; scroll offset for nonvisible page 
            bcs smdecn          ; branch away if we are decreasing nudge (carry still set/clear)
            ; we are increasing nudge (map scrolls upward)
            sta SMNVal          ; use current (smaller) offset for copylines
            adc #$01            ; carry is known to be clear, increase nudge
            and #$07
            bne smincnow        ; nudge did not wrap
            ; nudge wrapped, so we're now looking at the next map tile line
            inc PgOneTop, x
smincnow:   sta PgOneOff, x     ; store new scroll offset value for nonvisible page
            lda PgOneTop, x
            adc #21             ; carry is still known to be clear, new line is in bottom row
            tax
            bcc smdraw          ; branch always
            ; we are decreasing nudge (map scrolls downward)
smdecn:     bne smdecnow        ; nudge will not wrap, top map line stays the same, proceed
            ; nudge will wrap, so we're now looking at the previous map tile line
            dec PgOneTop, x     ; move top of map up one line
            lda #$07
            sta PgOneOff, x     ; set offset to 7 in the new map line on nonvisible page
            sta SMNVal          ; use new (smaller) offset for copylines
            bne smdrawtop       ; branch always
            ; nudge will not wrap, so map line will not change
smdecnow:   dec PgOneOff, x
            lda PgOneOff, x
            sta SMNVal
smdrawtop:  lda PgOneTop, x
            tax
smdraw:     jsr tilecache       ; cache the tiles for map line
            ldy SMNVal          ; line of tile to draw
            ldx #$00            ; draw to raster 0
            jsr paintline
            ; now shift all the lines (and copy in the buffered line 0)
SMNVal = *+1
            ldy #INLINEVAR      ; lower of current nudge and new nudge
SMIncDec = *+1
            lda #INLINEVAR      ; hi bit reports status on entry of carry
            asl                 ; send it back to the carry bit
            jsr copylines
SMBankSave = *+1
            lda #INLINEVAR      ; restore the bank
            sta R_BANK
            rts

; copylines rolls the whole screen (all lines below raster 7)
; raster line 0 is the new line that will be fed in at the appropriate edge
; call this after filling raster line 0 with the new line
; there are 23 lines to copy (one being from the line 0 buffer)
; enter with:
;    - Y holding lower of current nudge and new nudge
;    - carry clear for nudge increase (scroll up, hero down, Y is current),
; or - carry set for nudge decrease (scroll down, hero up, Y is new)
; assumes we are already in bank 0 (video data)
; on exit: ZP set to $1A00, no registers survive

ScrollDir:  .byte 0             ; preserve entry carry in high bit
MemOffset:  .byte 0             ; $0 for first 8K of page, $20 for second 8K

LineBuffer  =   $428            ; we are not using any text after line 1

copylines:  lda #$00
            ror                 ; move carry to hi bit
            sta ScrollDir       ; used for branching on direction later
            lda #$20            ; start with second half of graphics page (e.g., $4000-5FFF)
            sta MemOffset
clhalfpage: lda ShownPage
            eor #$01            ; switch focus to nonvisible page
            and #$01            ; 0 if page 1 is nonvisible, 1 if page 2 is nonvisible
            ror                 ; into carry
            ror                 ; $80 for p2
            ror                 ; $40 for p2, carry clear
            adc YHiresHA, y     ; high byte of first line on nonvisible page A
            adc MemOffset       ; be in either first $2000 or second $2000 of the page
            bit ScrollDir       ; will be negative if we are decreasing scroll
            bpl clinc           ; branch if increasing scroll, first target also in first $100
            ; decreasing scroll
            ;           save 2380-23CF (lines 38 and 78, discard line B8)
            ;           move 2300->2380 (30->38, 70->78, B0->B8)
            ; loop 1:   move 2280->2300 (28->30, 68->70, A8->B0)
            ;           move 2200->2280 (20->28, 60->68, A0->A8)
            ; loop 2:   move 2180->2200 (18->20, 58->60, 98->A0)
            ;           move 2100->2180 (10->18, 50->58, 90->98)
            ; loop 3:   move 2080->2100 (08->10, 48->50, 88->90)
            ;           move 2000->2180 (00->08, 40->48, 80->88) (puts new line in place)
            ;           move saved line 38->40, saved line 78->80
            sta SrcNewLn        ; line 0 address (first $100 of graphics)
            adc #$03            ; if decreasing, first target is in last $100, e.g. $2300
            sta R_ZP
            sta TargDecH
            ldx #$4F
cldeclast:  lda $80, x          ; save lines $38/$78 for later (do not need $B8)
            sta LineBuffer, x
            lda $00, x          ; move 2300->2380
            sta $80, x
            dex
            bpl cldeclast
            ldx #$27            ; move line $B0 to $B8 (finish prior triplet)
cldeclastb: lda $50, x
            sta $D0, x
            dex
            bpl cldeclastb
            dec R_ZP            ; first pass, TargDecH now $23, R_ZP is $22
            ldy #$03
cldecg:     ldx #$77
cldecl:     lda $80, x          ; e.g., 2280->2300
TargDecH = *+2
            sta $2300, x        ; INLINEADDR
            lda $00, x          ; e.g., 2200->2280
            sta $80, x
            dex
            bpl cldecl
            dey
            beq cldecdone       ; we've done all three, leave with ZP still at $20
            dec TargDecH
            dec R_ZP
            bne cldecg          ; branch always
cldecdone:  ldx #$4F
cldecrest:  lda LineBuffer, x   ; move lines $38/$78 to $40/$80
            sta $28, x
            dex
            bpl cldecrest
            bmi clpgdone          ; branch always
            ; increasing scroll 
            ;           save 2028-234F (lines 40 and 80)
            ;           move 20A8->2028 (48->40, 88->80, discard 08)
            ; loop 1:   move 2100->2080 (10->08, 50->48, 90->88)
            ;           move 2180->2100 (18->10, 58->50, 98->90)
            ; loop 2:   move 2200->2180 (20->18, 60->58, A0->98)
            ;           move 2280->2200 (28->20, 68->60, A8->A0)
            ; loop 3:   move 2300->2280 (30->28, 70->68, B0->A8)
            ;           move 2380->2300 (38->30, 78->70, B8->B0)
            ;           move saved line 40->38, saved line 80->78
            ;           move new line 00->B8
clinc:      sta R_ZP
            sta TargIncH
            ldx #$4F            
clinczero:  lda $28, x
            sta LineBuffer, x   ; save lines $40/$80 for later
            lda $A8, x          ; move lines $48->$40, $88->$80
            sta $28, x
            dex
            bpl clinczero
            inc R_ZP            ; first pass through, TargIncH is now $20, R_ZP is $21            
            ldy #$03            ; move this many blocks of $77+$77 (sextuplets of lines)
clincg:     ldx #$77
clincl:     lda $00, x          ; e.g. 2100->2080
TargIncH = *+2
            sta $2080, x        ; INLINEADDR 
            lda $80, x          ; e.g. 2180->2100
            sta $00, x
            dex
            bpl clincl
            dey
            beq clincdone       ; we've done all three, leave with ZP still at $23
            inc TargIncH
            inc R_ZP
            bne clincg          ; branch always
            ; move lines $40 and $80, saved earlier, to $38 and $78.
clincdone:  ldx #$4F
clincrest:  lda LineBuffer, x
            sta $80, x
            dex
            bpl clincrest
            ; copy line 0 to B8
            ldx #$27
SrcNewLn = *+2
clincnew:   lda $2000, x
            sta $D0, x
            dex
            bpl clincnew
            ; above covers one of the two graphics areas per page, now need
            ; to go get the other one, $2000 above the first one.
clpgdone:   lda MemOffset
            beq cldone          ; branch if we just finished both halves of the page
            lda #$00            ; second half finished, go back to do the first half
            sta MemOffset
            jmp clhalfpage
cldone:     lda #$1A            ; restore ZP to $1A00
            sta R_ZP
            rts
