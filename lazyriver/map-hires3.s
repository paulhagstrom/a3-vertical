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
; wpi;d only allow for maybe 5 sprites before we run out of time.
;
; A more flexible strategy that will allow for more sprites without tearing
; is to use the pages as just framebuffers, kept basically in sync and always
; alternating whether the background scrolls or not.  This still will often
; require shifting 2 lines on the invisible page.  Ultimately this will wind
; up be simpler to handle.
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
;
;
;
; even with the assistance of smooth scrolling, we have a LOT of data to move
; during VBL and before the beam catches up.  We are scrolling the whole screen
; which means we need to move 80 bytes of data per line for 23 lines.
; That's 1840 bytes to move, and given that we have 9100 cycles during VBL
; and loads and stores take 3-4 cycles each, it's clear that extreme measures
; are required to move all that data in time.  Since it is impossible to move
; that much data during VBL, we're going to bank on getting far enough down the
; screen during VBL that the beam won't catch up before we finish.  We have
; 12480 cycles at 1MHz while the screen is being painted, it gets 8 lines every
; 520 cycles.

; page flipping would be an option, but it reduces the amount of assistance we
; get from smooth scrolling (and it uses up a lot of the available memory).
; current best option is to have an unrolled dedicated blitter.
; using a relocated stack to push with, and immediate mode data loading,
; we can move one byte using 3 instruction and 5 cycles.  That gives us
; a ballpark of 5520 bytes for the blitter, 9200 cycles.


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
; one thing that occurs to me.  If we're doing all the lines, do we need to stop at #40?
; what if we did 6 lines at a time, from 0-240?  That might wind up being legit.
; first 8 lines are low 00, next 8 are low 80, repeats for first 64 lines.
; then 28 A8, then D0 50.  In blocks of 64 lines, it goes 20, 20, 21, 21, 22, 22, 23, 23.
; so if I'm copying line 10 to 08, 18 to 10, etc., it's n to n-8.
; 2000 holds line 0 (0-27), 40 (28-4F), 80 (50-77), 08 (80-A7), 48 (A8-CF), 88 (D0-F7)
; 2000: 00 40 80 08 48 88
; 2100: 10 50 90 18 58 98
; 2200: 20 60 A0 28 68 A8
; 2300: 30 70 B0 38 78 B8
; 
; 2100->2080, 2180->2100, 2200->2180, 2280->2200, 2300->2280, 2380->2300
; 2028->2380, 20A8->2028, 2128->20A8, 21A8->2128, 2228->21A8, 22A8->2228, 2328->22A8, 23A8->2328
; 2050->23A8, 20D0->2050, 2150->20D0, 21D0->2150, 2250->21D0, 22D0->2250, 2350->22D0, 23D0->2350
; new line goes into 23D0-23F7.
; generally speaking, I'm copying something to $80 before it.
; so there's an antecedent problem if I try to do this will full pages, but maybe I can do it with $80s.
; If I take X from 77 to 0 and copy
; 2000+x -> 2380+x ([2000->2380], 2028->23A8, 2050->23D0) ([0->B8], 40->38, 80->78)
; 2080+x -> 2000+x ([2080->2000], 20A8->2028, 20D0->2050) ([8->0], 48->40, 88->80)
; 2100+x -> 2080+x (2100->2080, 2128->20A8, 2150->20D0) (10->8, 50->48, 90->88)
; 2180+x -> 2100+x (2180->2100, 21A8->2128, 21D0->2150) (18->10, 58->50, 98->90)
; 2200+x -> 2180+x (2200->2180, 2228->21A8, 2250->21D0) (20->18, 60->58, A0->98)
; 2280+x -> 2200+x (2280->2200, 22A8->2228, 22D0->2250) (28->20, 68->60, A8->A0)
; 2300+x -> 2280+x (2300->2280, 2328->22A8, 2350->22D0) (30->28, 70->68, B0->A8)
; 2380+x -> 2300+x (2380->2300, 23A8->2328, 23D0->2350) (38->30, 78->70, B8->B0)
; should save a few cycles.
; but does it work? Do I ever overwrite something I need later?
; if I start at 2080, I'm ok to 2380, but I overwrite 40 and 80 and need those for 2000.
; I draw the new line at B8.
; So, it's basically the first line that is at risk, 2000+x
; I need to do that before 2080+x but it interacts with 2380+x
; I don't need 0->B8 (though I can use it to draw the new line actually)
; I don't need 8->0
; not sure I really gain a lot here.
; might as well just go from 27 to 0 on 23 different unrolled lines.
; but if I unroll fully then I can't finish in the VBL. As detailed earlier.
; 
; for simplicity I would rather avoid stuffing a blitter if I can, seems redundant
; to move the data twice.
;
; and looking at atomic defense it really seems like AH didn't try all that hard.
; why does HIS code work?
; 
; ok, let me look again, why can't I just do 2000<2080.23FFM, essentially?
; that's what I was trying to determine above, right?  How could this not work?
; the only issues are writing line 40 to 38 and 80 to 78 since those get overwritten.
; can I cache those before VBL?
; so, stash 2028-20F7 somewhere, VBL.
; point ZP at 20
; ldy #$04      ;2
; page:
; ldx #$80      ;2
; bne skip      ;3
; up:
; ldx #$00      ;2
; skip:
; lda $80, x    ;4
; sta $00, x    ;4
; inx           ;2
; bne up        ;3/2 [13 per loop, 256 loops, 3328 cycles]
; bit ZP        ;4 test to see if we are in $40 or $20
; bvc inhi      ;3/2 branch if in $20
; inc ZP        ;6 if we're in $40, we want to shift to $21
; inhi:
; lda ZP        ;4
; eor #$60      ;2 switch between $20 and $40
; sta ZP        ;4
; dey           ;2
; bne page      ;3/2 [9+2 overhead]
; 
; ok, that seems about as tight as I can make something with this logic.
; inner loop: 13 per loop, 256 passes, moves 256 bytes. 3328 cycles.
; that sets a floor. We need to move 21-20 22-21 23-22 41-40 42-41 43-41
; of easy movement, which is 6 pages, 19968 minimum.
; how can I move 256 bytes faster?
; I can use PHA if I can count back.
; point stack at $2000
; ldx #$7f      ;2
; txs           ;2
; skip:
; lda $2080, x  ;4
; pha           ;3
; dex           ;2
; bpl skip      ;2/3 [12 per loop, 256 loops, 3072 cycles]
; so floor is now 18432, a little better.
; if I unroll this without having to stuff the blitter
; point stack to $2000, pointer to $7F
; lda $21FF     ;4
; pha           ;3
; lda $21FE     ;4
; pha           ;3
; ... 
; lda $2100     ;4
; pha           ;3
; total 7 cycles 4 bytes per byte moved, 1792, 1024 to move 256 bytes.
; that's looking good.  If I move 8 of these, that's 14336, 8192.
; I can get 5 done during VBL at basically the theoretical maximum speed.
; but to get a different offset, I need to write one hi byte per byte moved
; too into the blitter.  I have to move 1024 bytes.  So just writing is
; 4096 cycles.
; that's not quite the theoretical maximum, because I could
; stuff the blitter with values too, then it would be
; 5 cycles, 3 bytes per byte moved.
; so 1280, 768 bytes to move 256 bytes.
; or 10240, 6144 for 8.
;
; so 1664 + (11+3228)*3 = 1664 + 9717 = 11381
; won't make it within VBL, but might it beat the beam?
; 3239*2 = 6478 + 1664 = 8142
; so line 30 is the earliest risk.  I think there are 520 cycles per 8 block.
; so line 30 will arrive within 6 of those. 3120 cycles.
; so I have to finish by 12220.  Very close.  It works except for lines 38 and 78.
; I have to get line 38 done by 12740. Gives me around 1359 cycles to do it.
; Oh, wait. Crap. No. This is only half the job, I need to move the $40 page too.
; I might be able to use the stack, but it only provides a benefit in one direction.
; pla is 3 but pha is 4 same as sta zero,x.
; ZP 20 stack 21, ZP 21 stack 20, ZP 22 stack 23, ZP 23 stack 22
; 
; you know, LDA absolute, X is still 4 if it doesn't cross a page boundary.
; ZP isn't buying me as much as I thought it was.

; so, stash 2028-20F7, 4028-40F7 somewhere, VBL.
; ldx #$00      ;2
; up:
; lda $2080, x  ;4
; sta $2000, x  ;4
; lda $2100, x  ;4
; sta $2080, x  ;4
; ...
; lda $2380, x  ;4
; sta $2300, x  ;4
; lda $4080, x  ;4
; sta $4000, x  ;4
; ...
; lda $4380, x  ;4
; sta $4300, x  ;4
; inx           ;2 [above: 16 per 80 for both pages, 6 80s, 96, so 98*128=12544]
; bpl up        ;3/2 [13 per loop, 256 loops, 3328 cycles]
; 
; seems like I'm trying to break physical laws here getting this to go faster.
; pla only can work for one scroll direction, and it takes 6 cycles to shift the
; ZP/stack to somewhere else. Still, 6x23x2 = 276 cycles of overhead to do this
; line by line, may not be significant if I can save in the big loops.
; 
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

