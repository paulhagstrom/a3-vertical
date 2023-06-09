; lazyriver
; A3 hires display map routines

; if we are scrolling the whole screen minus the top line, here is what we need:
; we need to copy 23 lines (one of which comes from a buffer)
; each line has 80 bytes (4 per 7 pixels)
; LDA absolute,y and STA Zero,x is 4+4=8 cycles.  16 to cover both pages.
; 368 cycles to do 23 lines if unrolled (both pages)
; dex dey is 4 cycles, branch back taken is 3.
; So, can do 375 per column
; 40 columns, 15000.  And that is too many for a VBL even unrolled that way.
; we can buy some time if we start at the top and go down (racing the beam)?
; PHA is 3 cycles. Can I halt interrupts? It would hurt the sound.
; how much can I unroll?
; STA ZP is 3 LDA absolute is 4, 7 cycles, 5 bytes.
; A full line would be 80 of those. 560 cycles, 400 bytes.
; 23 of those would be 12880 cycles, 9200 bytes, about $2400.
; so we could fill page 2 with unrolled instructions, and it would still take two VBLs.
; we have around 9000 cycles during VBL, probably less due to sound interrupts.
; the only way to move that many bytes without tearing is going to be page flipping.
; due to how the smooth scrolling works, it's going to look bad if it gets half drawn
; before the offset is changed.
;
; so. Page flipping.  That's going to look super smooth anyway, probably.
; put odd offsets on page 1, even ones on page 2.
; if we start at offset 0, we'll be looking at page 2.
; paint both pages, page 1 at offset 1, page 2 at offset 0.
; to scroll map down (so not using what we already have on page 1)
; offset decreases by 2 from what was drawn already.
; we draw lines 7-8 of new lower map row in lines 0-1
; we copy lines 7-8 of row 23 to lines 7-8 of row 24, 22 to 23, 1 to 2
; and line 0-1 to lines 7-8 of row 1
; then at VBL flip to page 1, set offset to 7
; this probably will take, even at high speed, 4 VBLs to do.  So we're down to 7.5 fps.
; fast for an Apple, but still kind of slower than I'd hoped.
; plus we need to do sprite background preservation, and this loses a lot of memory we'd otherwise
; have had. 2, 4, 6, 8, we're already at $A000.
; 
; LDA immediate 2 PHA 3, that's not too bad. 5 per. Would be same for sta ZP.
; PHA saves a byte but isn't interrupt-friendly
; LDA immediate 2 STA R_ZP 4 LDX immediate 2 TSX 2 = 10 (or 6 without the stack part)
; per half-line: 200 cycles 160 bytes for first page, 6 and 5 to switch,
; so per line 406 cycles, 325 bytes.
; for 23 lines, 9338, just makes it, maybe. 7475 bytes, under $2000.
; so suppose I buffer all the lines into this executing code first, could
; still use stack pushing or ZP writing to do it.  But I can take 2-3 VBLs to do it.
; PHA technique would allow more bytes to stay constant though, just need to push the
; data in, change the stack and pointer for different lines
; LDX #$-- STA R_ZP LDX -- TSX LDA #$-- PHA LDA #$-- PHA...
; fill in two (page, stack pointer for destination, then just read and buffer
; feels complicated.  Would page flipping be less complicated?
; I think it would, plus it would allow for more flexibility with sprites.
; if it takes 10 VBLs to prepare to switch, that's still 3fps.
; have to figure out how to save the background so we don't need to recompute it.
; have to preserve 400-427 and 800-27, but otherwise 300-FFF is probably available.
; game is from A000 up and graphics are from 2000-9FFF.

; unrelated thinking aloud, if I write up a 2-line copylines, maybe this parallax
; thing can work again? Front pane shifts 2 lines at a time, but only one, 4 possible offsets.
; back plane is fully redrawn every time, has to be.  But it's the back plane so it
; is narrower.  Maybe draw middle 20 bytes, 96 lines, then front 80 bytes, 24 lines.
; still is going to need page flipping to work, but the speedup from 80x96 (or 80x192)
; to 80x24 should be significant.

; another unrelated thinking aloud, I could maybe use a lower-resource graphics mode.
; but can't page flip the 16-color hires one.  Might be able to get it to work within.
; VBL. Can only set colors in groups of 7.  This would provide a higher resolution (280)
; but wouldn't really change the blitting resource requirements, still requires filling
; all of $2000-27 and $400-27 per line.  But can scroll.  Same for super hires, even
; more detail but monochrome, same blitting requirements.  The only lower resource
; version would be A2 graphics, which would only require blitting 40/line instead of 80.
; medres would work ok for river and banks, but the sprites would be pretty constrained
; colorwise (blue background, per-line color foreground).  Could try that, but most of
; the same tech would be involved.

; there is actually a ton of drawing time while the screen is being painted.
; 192 lines, 65 cycles each, 12480 cycles total.
; 70 lines of VBL, 4550 cycles at 1MHz, 9100 cycles at 2MHz
; so if I can keep ahead of the beam... problem is the copying goes against the beam,
; though, if the screen is scrolling down.  So, maybe shift the orientation and force
; up scrolling (as an experiment at least), with the logs traveling downward?
; 2 ldx
; 2 ldy
; 4 lda abs,y
; 4 sta zp,x
; 4 lda abs,y
; 4 sta zp,x
; 2 dex
; 2 dey
; 3 branch taken
; so for one line, 4 + (40 * (16 + 7)) - 1 = 923 cycles.
; or 3 + 40*(16*1 + 7)
; but for a second line, 4+(40*(32+7))-1 = 1563 cycles (not 1846, 283 fewer)
; eight lines: 5403, five lines: 
; twelve lines 3 + 40*(16*12 + 7): 7963.
; 13: 8603
; so we just might be able to do the top half of the screen during VBL (first 12 lines)
; and then the bottom half before the beam catches up.  We would have 65*(8*13) = 6760
; cycles I think before it gets to the top of the undrawn region.
; and then we have 11 more lines to draw, should take 7323 cycles. Close but not quite.
; if I unroll harder
; 2+4 set ZP
; 2 lda #
; 3 sta ZP
; x40 = 200 cycles, 160 bytes
; repeat = 400 cycles per line, 320 bytes
; can just about get everything in though in that case. 9200 cycles if interrupts
; are stopped.  Sound interrupts might eat around 700 cycles.
; 
; sei (+2, 1)
; point ZP/stack, set stack pointer (+10, 8)
; lda # pha (+5, 3) 40 = (+200, 120)
; point ZP/stack, set stack pointer (+10, 8)
; lda # pha (+5, 3) 40 = (+200, 120)
; point ZP/stack, set stack pointer (+10, 8)
; cli (+2, 1)
; +2, 1 in, +12, 9 out, +420, 256 per line
; sound would be about 65 cycles
; 4 lines, +1680, 1024
; 3 lines, +1260, 768
; so:
; in +2, 1
; draw 4 lines +1680, 1024
; do sound +65
; x5 (gets to 20 lines drawn)
; out +12, 9
; 
; so: +8749, 5130ish (not counting sound calls) gets 20 drawn and 5 sounds
; all still within VBL, and should be able to get the last three out before beam arrives.
; last three +1260, 768 -- only the first might scrape by before clock downshifts.
; problem with sound is that it will be "about" 65 cycles, conditions vary its length
; using a timer will burn many cycles though, probably around 550-650.
; stuffing the blitter will of course take well longer than a cycle.
; full screen pass (draw+VBL) is about 21580 cycles. Maybe it can almost fit.
; then need to draw sprites (erase, shift lines, mask, save background, draw)
; then finally VBL->set graphics modes and blit
;

; movement processing happens on the VBL downbeat, so wait until the next one to 
; update graphics.
; This checks to see if scrolling is needed (domove communicates via NeedScroll).
; called always but if no scrolling is needed, it exits quickly with carry clear.
; if it does scroll, it will exit with carry set to indicate that VBL is probably used up.
; value for NeedScroll is: 0=stop, neg=map down/dec off, pos=map up/inc off

fixscroll:  clc                 ; clear carry signifies both "scroll up" and "was quick"
NeedScroll = *+1
            lda #INLINEVAR      ; 0 - no scroll needed, >7F map down, else map up
            beq noscroll
            bmi scrolldn
            bcc scrollup        ; branch always
scrolldn:   sec
scrollup:   jsr scrollmap       ; scroll the screen (using smooth scroll)
            sec                 ; tell eventloop we took some time
            lda #$00
            sta NeedScroll      ; we no longer need a scroll
noscroll:   rts

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

; paint a single line from cached tiles
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
            lda YHiresHA, x
            sta PMLineA + 1
            sta PMLineC + 1
            lda YHiresHB, x
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

; paint the whole map (called at the outset)
; (updates afterwards are incremental, drawing single lines and using smooth scroll)
; assumes smooth scroll offset is 0

paintmap:   lda R_BANK          ; save bank (but assume we are already in 1A00 ZP) 
            sta PMBankSave      ; save it inline within later restore code.
            jsr gfxinit         ; set up pointers and switch banks
            lda #$BF            ; we are drawing from the bottom up
            sta ZCurrScrL       ; current screen line
            lda TopRow          ; map line of the top row on the screen
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

; scrollmap will effect a vertical movement of the map regions of the screen.
; locates new line and puts it in raster 0, then calls copylines to do the scroll, then moves hardware scroll
; enter with:
; - carry clear: move the map up (hero downward), increasing nudge
; - carry set: move the map down (hero upward), decreasing nudge
; it will finish by setting NudgeVal correctly so that the interrupt handler will display it right.

scrollmap:  lda R_BANK          ; save bank
            sta SMBankSave      ; (but assume we are already in 1A00 ZP)
            ror                 ; put carry in hi bit (the other 7 bits don't matter for anything)
            sta SMIncDec
            asl                 ; restore carry
            jsr gfxinit         ; go to bank 0, where (hires) graphics memory lives
            lda TopOff          ; current nudge value (offset into map's top line that we're at)
            bcs smdecn          ; branch away if we are decreasing nudge
            ; we are increasing nudge (map scrolls upward)
            sta SMNVal          ; use current (smaller) offset for copylines
            adc #$01            ; carry is known to be clear, increase nudge
            and #$07
            bne smincnow        ; nudge did not wrap
            ; nudge wrapped, so we're now looking at the next map tile line
            inc TopRow
smincnow:   sta TopOff          ; store new nudge value in map offset variable
            lda TopRow
            adc #21             ; carry is still known to be clear, new line is in bottom row
            tax
            bcc smdraw          ; branch always
            ; we are decreasing nudge (map scrolls downward)
smdecn:     bne smdecnow        ; nudge will not wrap, top map line stays the same, proceed
            ; nudge will wrap, so we're now looking at the previous map tile line
            dec TopRow          ; move top of map up one line
            lda #$07
            sta TopOff          ; set offset to 7 in the new map line
            sta SMNVal          ; use new (smaller) offset for copylines
            bne smdrawtop       ; branch always
            ; nudge will not wrap, so map line will not change
smdecnow:   dec TopOff
            lda TopOff
            sta SMNVal
smdrawtop:  ldx TopRow
smdraw:     jsr tilecache       ; cache the tiles for map line
            ldy SMNVal          ; line of tile to draw
            ldx #$00            ; draw to raster 0
            jsr paintline
            ; now copy everything to visible screen regions
            ; stall for VBL
            lda VBLTick
smstall:    cmp VBLTick
            beq smstall
SMNVal = *+1
            ldy #INLINEVAR      ; lower of current nudge and new nudge
SMIncDec = *+1
            lda #INLINEVAR      ; hi bit reports status on entry of carry
            asl                 ; send it back to the carry bit
            jsr copylines
SMBankSave = *+1
upddone:    lda #INLINEVAR      ; restore the bank
            sta R_BANK
            ; adjust the smooth scroll offset
            ldy TopOff
            lda TwelveBran, y
            sta NudgeVal
NudgeVal = *+1                  ; smooth scroll parameter x $0C
            bpl nudge0          ; INLINEVAR - branch always
nudge0:     bit SS_XXN
            bit SS_XNX
            bit SS_NXX
            jmp postnudge
nudge1:     bit SS_XXY
            bit SS_XNX
            bit SS_NXX
            jmp postnudge
nudge2:     bit SS_XXN
            bit SS_XYX
            bit SS_NXX
            jmp postnudge
nudge3:     bit SS_XXY
            bit SS_XYX
            bit SS_NXX
            jmp postnudge
nudge4:     bit SS_XXN
            bit SS_XNX
            bit SS_YXX
            jmp postnudge
nudge5:     bit SS_XXY
            bit SS_XNX
            bit SS_YXX
            jmp postnudge
nudge6:     bit SS_XXN
            bit SS_XYX
            bit SS_YXX
            jmp postnudge       ; at this point (each block but last: 39, last (nudge 7): 36) 
nudge7:     bit SS_XXY
            bit SS_XYX
            bit SS_YXX
postnudge:  rts

; TwelveBran is a table of multiples of $0C, used as a branch table when setting NudgeVal
TwelveBran: .byte   $00, $0C, $18, $24, $30, $3C, $48, $54

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

; notes: interrupts too tight to use stack for speed increase here
; zero page is used for blitting speed, so variables below can't be in ZP

; this still seems to be too slow to finish in the VBL
; we should have about 9000 cycles in VBL, but it is flickering
; will need to see what can be done to speed this up.
; maybe generate some unrolled loops. We have to draw 23 lines, move 80 bytes per line
; so it is super tight, not sure how sprites will figure into this
; might try page flipping, but that would require drawing twice as many lines
; still 4 times fewer than manual scrolling would require, but not 8.

ScrollDir:  .byte 0             ; preserve entry carry in high bit
LinesLeft:  .byte 0             ; lines remaining to draw

copylines:  bcc clincnud        ; branch away if we are increasing nudge
            ; decreasing smooth scroll parameter ("nudge")
            tya                 ; this should be the new nudge value (current nudge-1)
            adc #$B7            ; first target line is at the bottom (carry is set, we are adding $B8)
            tay
            sty ScrollDir       ; use for branching to subtract (negative number)
            bmi clsettrg        ; branch always
            ; increasing smooth scroll parameter ("nudge")
clincnud:   tya                 ; for this we want the current nudge value (later will become this + 1)
            adc #$08            ; first target line is at the top, carry known to be clear
            tay
            sty ScrollDir       ; use for branching to add (positive number)
clsettrg:   lda #22             ; move 23 lines (last copying from buffer), countdown in LinesLeft
            sta LinesLeft
            lda YHiresS, y      ; set the first target line based on start line we were passed
            sta TargS           ; target low, end of the line
            lda YHiresHA, y
            sta TargHA          ; target A high
            clc
            adc #$20            ; compute HB
            sta TargHB          ; target B high
clnext:     lda ScrollDir       ; restore carry from entry
            asl
            tya
            bcs clsub           ; we are subtracting, go do that
            adc #$08            ; source is 8 lines after target
            bcc clprep          ; branch always
clsub:      sbc #$08            ; source is 8 lines before target
clprep:     tay
            pha                 ; remember source line we computed for the next iteration's target
clsetsrc:   lda YHiresS, y      ; end of line pointer not used in source but passed on to be target after
            sta SourceS
            lda YHiresLA, y     ; source start-of-line low byte
            sta SourceA         ; modify code in upcoming loop (page 1 source)
            sta SourceB         ; modify code in upcoming loop (page 2 source)
            lda YHiresHA, y
            sta SourceA + 1     ; source A high
            clc
            adc #$20            ; compute HB
            sta SourceB + 1     ; source B high
TargHA = *+1
            lda #INLINEVAR      ; point ZP at target A page
            sta R_ZP
TargS = *+1
            ldx #INLINEVAR      ; start x at the end of the target line
            ldy #$27            ; start y at the end of the source line
SourceA = *+1
clsrca:     lda INLINEADDR, y   ; this address is modified to be exactly at the start of the line
            sta Zero, x         ; this address is at a page boundary, so x may be more than $27
            dex
            dey
            bpl clsrca
TargHB = *+1
            lda #INLINEVAR      ; point ZP at target B page
            sta R_ZP
            ldx TargS           ; start x at the end of the target line
            ldy #$27            ; start y at the end of the source line
SourceB = *+1
clsrcb:     lda INLINEADDR, y   ; this address is modified to be exactly at the start of the line
            sta Zero, x         ; this address is at a page boundary, so x may be more than $27
            dex
            dey
            bpl clsrcb
            ; the source we just used will now become the target for the next one
SourceS = *+1
            lda #INLINEVAR
            sta TargS
            lda SourceA + 1
            sta TargHA
            lda SourceB + 1
            sta TargHB
            dec LinesLeft
            beq cllast          ; last line source is an offscreen buffer
            bmi cldone          ; if we have done everything, exit
            pla                 ; lines remain, compute next source
            tay
            bne clnext          ; branch always
cldone:     lda #$1A            ; restore ZP to $1A00
            sta R_ZP
            rts
cllast:     ldy #$00            ; offscreen buffer line is line 0
            pla                 ; toss out saved line
            jmp clsetsrc
