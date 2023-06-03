; lazyriver
; A3 hires display routines

MapDirty:   .byte   0           ; nonzero if map needs to be redrawn due to movement
NeedScroll: .byte   0           ; pos/neg if map needs to be scrolled up(clc)/down(sec) due to Hero Y movement

; read a line of tiles from the map into the zero page cache ($00-$13)
; cache will hold index into tile graphics assets (shape x 32, each shape is 32 bytes of data)
; a line is 20 ($14) bytes long in the map representation
; call with X being the row of the map we are caching
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

; paint a single line
; call with:
; - X holding the physical graphics line (0-BF)
; - Y holding the offset into tile we should draw
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

; set up pointers and banks for graphics interaction
; bank 0 for graphics, ZPtrA for map tiles
gfxinit:    lda #$00
            sta R_BANK          ; move to bank zero for the graphics
            sta ZPtrA           ; tile graphics low byte
            lda #$82            ; map is bank 2
            sta ZMapPtr + XByte
            sta ZPtrA + XByte
            lda #$34            ; tile graphics start at $3400
            sta ZPtrA + 1
            rts

; paint the whole map (called at the outset)
; (updates afterwards are incremental, drawing single lines and using smooth scroll)
; assumes smooth scroll offset is 0

paintmap:   lda R_BANK          ; save bank (but assume we are already in 1A00 ZP) 
            sta PMBankSave      ; save it inline within later restore code.
            jsr gfxinit         ; set up pointers and switch banks
            lda #$#00
            sta ZCurrScrL       ; current screen line
            lda MapTop          ; map line of the top row on the screen
            clc
            adc #23             ; +23 to get to map line of bottom row
            sta ZCurrMapL       ; becomes the current line
            lda #23
            sta ZLinesLeft
pmgetmap:   ldx CurrMapL
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
            bpl pmgetmap
            ; painting map is finished, restore bank and exit
PMBankSave = *+1
            lda #INLINEVAR
            sta R_BANK
            rts

; loose code for scrolling - TODO remove
            ; do smooth scroll first while we wait for beam to travel horizontally
NudgeVal = *+1                  ; smooth scroll parameter x $0C - directly modifies the interrupt handler code
            bne nudge0          ;3 [24] then 15 cycles, 12 bytes per block -- INLINEVAR
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
            ; polling below doesn't work on real hardware, seems to work fine in MAME.
            ; though also does not seem to matter whether I wait for bit set or bit clear.
postnudge:  ;bit R_TONEHBL       ; 4 burn cycles until HBL arrives (expecting 15, about 2-3 loops)



; if the screen needs a scroll due to the Hero moving in the Y direction, do it.
; NeedScroll will be 0 if nothing needed, else pos or neg based on hero's Y velocity
fixscroll:  clc
            lda NeedScroll
            beq noscroll
            bmi scrolldn
            jsr scrollmap       ; scroll the screen (using smooth scroll) (up/clc)
            sec                 ; tell eventloop we took some time
            jmp fixedscr
scrolldn:   sec
            jsr scrollmap       ; scroll the screen (using smooth scroll) (down/sec)
            sec                 ; tell eventloop we took some time
fixedscr:   lda #$00
            sta NeedScroll      ; we no longer need a scroll
noscroll:   rts

; scrollmap will effect a vertical movement of the map regions of the screen.
; we are skipping the first 8 scan lines (they are A3 text, for messages/score)
; otherwise we are scrolling the whole screen (in general covering 24 map lines, 23 if offset is 0)
; if you call it with carry clear, it will move the map up (hero downward), increasing nudge
; if you call it with carry set, it will move the map down (hero upward), decreasing nudge
; it will finish by setting NudgeVal correctly so that the interrupt handler will display it right.

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
; draw map line for graphics line 00 + nudge on graphics line 00 + nudge

; as usual, it is hard to wrap my head around this.
; Suppose MapTop is 232 and MapOff is 0, where we start.
; and the river scrolls down.
; so the old line 08 is now displayed on line 09
; MapOff becomes 7, need to draw new top line on line 0F (will draw at top)
; so I load in the map line 231 (post-move MapTop), and buffer line 7 (post-move MappOff) into 0
; copy all the line 7s down from end of screen to top 
; if it were going the other way,
; MapOff becomes 1, MapTop stays 232.
; now I need to copy from top to bottom, and then buffer line MapTop+24's line old offset (0) into 0
; this is pretty doable.

 
scrollmap:  bcs umdec           ; if we are decrementing nudge, skip past the incrementing parm block
            lda #$B8            ; first copy target raster line (then up, copying toward zero)
            sta PTopRastA
            lda #$01            ; remember that we are incrementing (will later be added to PNudge for NudgePos)
            sta ZPInc
            lda
            jmp umbegin         ; skip past the decrementing parm block
umdec:      lda #$08            ; first copy target raster line in lower field (then down, copying away from zero)
            sta PTopRastA
            lda #$23            ; map offset back from HeroY for newly drawn line in top field.
            sta ZTopMapOff
            lda #$00            ; remember that we are decrementing (will later be added to PNudge for NudgePos)
            sta ZPInc
umbegin:    lda R_BANK          ; save bank
            sta UMBankSave      ; (but assume we are already in 1A00 ZP)
            lda #$00            ; go to bank 0, where (hires) graphics memory lives
            sta R_BANK
            ldx #$00            ; buffer new line in raster 00
            lda MapTop
            lda HeroY           ; find the new data line for the top field
            sec
            sbc ZTopMapOff      ; counting back from HeroY to either top or bottom of top field
            sta ZMapOffset      ; store the map offset we will draw top field line from
            jsr drawline
            ; now copy everything to visible screen regions
            ; TOOD - stall until we get to VBL?
            
umcopy:     ldy ScrRegion       ; stall for screen mode to leave hires region
            cpy #$00            ; wait for bottom field hires region to pass
            bne umcopy
PBotRastA = *+1
            lda #INLINEVAR      ; first raster line (inc=top, dec=bottom) in copy operation in bottom field
            clc
            adc ZNudge          ; newnudge/oldnudge
            ldy ZPInc           ; carry should still be clear
            bne :+              ; set carry if we are decrementing
            sec
:           ldx #$41            ; new bottom field line buffer
            jsr copylines       ; copy lines that can be copied
PTopRastA = *+1
            lda #INLINEVAR      ; first raster line (inc=top, dec=bottom) in copy operation in top field
            clc
            adc ZNudge          ; newnudge/oldnudge
            ldy ZPInc           ; carry should still be clear
            bne :+              ; set carry if we we decrementing
            sec
:           ldx #$40            ; new top field line buffer
            jsr copylines       ; copy lines that can be copied
UMBankSave = *+1
upddone:    lda #INLINEVAR      ; restore the bank
            sta R_BANK
            lda ZPInc           ; advance PNudge to NudgePos (adds one if we were incrementing)
            clc
            adc ZNudge          ; note: critical this is mod 8 or interrupt will spin off to the void
            and #$07
            tay                 ; translate to NudgeVal (multiply by $0C)
            lda TwelveBran, y
            sta NudgeVal
            rts

; copylines rolls the whole screen below raster 8
; raster line 0 is the new line that will be fed in at the edge
; call this after filling raster line 0 with the new line
; there are 23 lines to copy (one being from the line 0 buffer)
; uses self-modifying code and ZP repointing to quickly copy the graphics lines
; enter with start line (first target line) in A,
; carry clear for increase nudge, carry set for decrease nudge,
; e.g. with A=B8, carry set, if scrolling down (hero moving up)
; assumes we are already in bank 0 (video data), and sets ZP to $1A00 on exit
; interrupts with sfx would be too tight to use the stack to push
; nothing (A, X, Y) is preserved

copylines:  tay                 ; move first target line into Y
            pha                 ; and remember it 
            lda #$00            ; set LinesDec to 1 if carry was set or 0 if carry was clear
            rol
            sta LinesDec
            lda #23             ; move 23 lines (last copying from buffer), countdown in LinesLeft
            sta ZLinesLeft
            lda YHiresS, y      ; set the first target line based on start line we were passed
            sta TargS           ; target low, end of the line
            lda YHiresHA, y
            sta TargHA          ; target A high
            clc
            adc #$20            ; compute HB
            sta TargHB          ; target B high
LinesDec = *+1
lmnext:     lda #INLINEVAR      ; restore carry from entry
            ror
            pla                 ; recall the target line and compute the source line from it
            bcs :+              ; if carry is set we are subtracting
            adc #$08            ; if carry is clear we are adding
            jmp lmprep
:           sbc #$08
lmprep:     tay
            pha                 ; remember source line we computed for the next iteration's target
lmsetsrc:   lda YHiresS, y      ; end of line pointer not used in source but passed on to be target after
            sta SourceS
            lda YHiresL, y      ; source start-of-line low byte
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
lmsrca:     lda INLINEADDR, y   ; this address is modified to be exactly at the start of the line
            sta Zero, x         ; this address is at a page boundary, so x may be more than $27
            dex
            dey
            bpl lmsrca
TargHB = *+1
            lda #INLINEVAR      ; point ZP at target B page
            sta R_ZP
            ldx TargS           ; start x at the end of the target line
            ldy #$27            ; start y at the end of the source line
SourceB = *+1
lmsrcb:     lda INLINEADDR, y   ; this address is modified to be exactly at the start of the line
            sta Zero, x         ; this address is at a page boundary, so x may be more than $27
            dex
            dey
            bpl lmsrcb
            ; the source we just used will now become the target for the next one
SourceS = *+1
            lda #INLINEVAR
            sta TargS
            lda SourceA + 1
            sta TargHA
            lda SourceB + 1
            sta TargHB
            dec LinesLeft
            beq lmlast          ; last line source is an offscreen buffer
            bpl lmnext          ; if we're not even to the last line yet, compute next source
            lda #$1A            ; restore ZP to $1A00
            sta R_ZP
            rts
lmlast:     ldy #$00            ; offscreen buffer line is line 0
            pla                 ; toss out saved line
            jmp lmsetsrc


; do the hires page lookup and ZP setup
; enter with X holding the target line on the graphics page, assumes we are in 1A00 ZP
; returns with X holding the low byte of the starting/leftmost byte on the line
; also updates ZLineStart in 1A00 ZP and sets up ZOtherZP in all ZPs.
; exits in HGR1 ZP since all callers want to go there immediately.
prepdraw:   lda YHiresHA, x     ; get 2000-based address of current line on screen
            ; engineer it so that ZOtherZP in hgr pages always points to the other ZP to flip quickly.
            sta ZOtherZP        ; store HGR1 page in 1A00 ZP.
            sta R_ZP            ; switch to HGR page 1 ZP
            tay                 ; stash it for putting in other ZP's ZOtherZP.
            clc
            adc #$20            ; second page is $2000 higher than first
            sta ZOtherZP        ; store HGR2 2 ZP in HGR1's ZP
            sta R_ZP            ; go to HGR2 ZP
            tya
            sta ZOtherZP        ; recall and store HGR1's ZP in HGR2's ZP
            lda #$1A            ; and go back to 1A00 ZP.
            sta R_ZP
            ; lo byte is same on either page, store it in 1A00 page.
            lda YHiresL, x
            sta ZLineStart
            tax
            rts

; enter with X holding the target line on the graphics page (raster)
; and A holding the map line we will be drawing there
; drawlineb is a second entry point if the map pointer is already set
; this assumes that 1A00 is the normal ZP we start in, and bank 0 (hgr) is switched in
drawline:   jsr setmapptr       ; load mapptr for map line to draw
drawlineb:  jsr prepdraw
            ; draw border bits
            lda ZOtherZP        ; HGR1
            sta R_ZP
            lda #BorderBitC
            tay
            lda #BorderBitA
            pha                 ; store first byte on HGR1 line byte 0
            sta Zero, x
            inx
            tya                 ; store third byte on HGR1 line byte 1
            sta Zero, x
            txa                 ; skip X ahead to the other side of the line
            clc
            adc #$26
            tax
            tya
            sta Zero, x         ; store third byte on HGR1 line byte $27
            dex
            pla
            sta Zero, x         ; store first byte on HGR1 line byte $26
            lda ZOtherZP        ; HGR2
            sta R_ZP
            lda #BorderBitD
            tay
            lda #BorderBitB
            pha
            sta Zero, x         ; store second byte on HGR2 line byte $26
            inx
            tya         
            sta Zero, x         ; store fourth byte on HGR2 line byte $27
            txa                 ; skip X back to the left of the line
            sec
            sbc #$26
            tax
            tya
            sta Zero, x         ; store fourth byte on HGR2 line byte 1
            dex
            pla
            sta Zero, x         ; store second byte on HGT2 line byte 0
            lda #$1A            ; go back to 1A00 ZP.
            sta R_ZP
            ; push left edge to the right 7 pixels to center the map fields
            inc ZLineStart
            inc ZLineStart
            ; point ZMapPtr at the left side of present line in the map data.
            lda MapPtrL
            sta ZMapPtr
            lda MapPtrH
            sta ZMapPtr + 1
            lda #$82            ; map stuff is in bank 2
            sta ZMapPtr + XByte
            ; we have 64 map data bytes, will draw them over 128 pixels.
            ; which really means drawing 63 bytes over 126 pixels.
            ; using 4 bytes to represent 14 pixels and 7 map data bytes.
            ; mapbytes: 0  7  14  21  28  35  42  49  56  (63) (ZCurrMapX)
            ; pixbytes: 0  4   8  12  16  20  24  28  32  (36) (ZCurrDrawX)
            lda #62
            sta ZCurrMapX       ; right edge of last group of map bytes
            lda ZLineStart      ; add 32 to left edge of graphics memory for line
            clc
            adc #32             ; to get left edge of last group of graphics memory for line
            sta ZCurrDrawX
toplineseg: jsr drawseg         ; draw a single 14-pixel segment
            ; drawseg will move ZCurrMapX back 7 units while buffering map
            ; if that didn't run off the edge of the map, then back up the pixel pointer too
            lda ZCurrMapX
            bmi :+              ; we had run off the left edge of the line, so now we are done
            lda ZCurrDrawX
            sec
            sbc #$04
            sta ZCurrDrawX
            jmp toplineseg
:           rts

; Compute where in screen memory a map line would be.  Enter with map line in A.
; The logic of the game outside is such that it will not be called if line is fully off screen,
; but might be called if the line is under the playfield (and it would work regardless).
; This took a lot of scribbling on paper, but here is an algorithm that seems to work.
; if Map > HeroY, bottom field, on screen if 3 < Map-HeroY < 24
; if HeroY > Map, top field, on screen if 3 < HeroY-Map < 24
; TL = HeroY - 23 (top field) or TL = HeroY + 4 (bottom field) (top line)
; T8 = TL & 07 (top field) or T8 = (TL + 1) & 07 (bottom field) (screen scroll start value of top line)
; TLZ = TL - T8 (what the top map line was when nudge was 0, lines between TLZ and TL now "off the top")
; MD = Map - TLZ, M8 = MD & 07 (map difference, distance to top of field, and the mod 8 value)
; base = 8 * ( M8 < T8 ) (whether the line has already been nudged into the prior group of 8)
; raster = MD - base + 20 (top field) or + 90 (bottom field)
findraster: ldx #$90        ; raster base - default assumption is video base of lower field
            sta MapTemp     ; entered with the map line we are locating in A
            sec             ; is it actually on screen?
            sbc HeroY       ; compute map line minus HeroY (to see if it in bounds)
            bcs belowhero   ; branch if result is positive, map line is below hero (lower field)
            bpl froff       ; this wrapped but remained positive (< -7F), very far away
            eor #$FF        ; map line is above HeroY (upper field), so this was negative
            adc #$01        ; make it positive to see if it is within screen bounds
            ldx #$20        ; we are in upper field so change raster base to upper field
            pha             ; stash while we compute TL (top line)
            lda HeroY
            sec
            sbc #$23
            sta TopLine
            sta TopLine4N   ; preparing to work out T8, mod 8 step still remains
            pla             ; restore distance map line is above hero
            jmp chbound
belowhero:  bmi froff       ; this did not wrap but is nevertheless negative (> 7F), very far away
            pha             ; stash while we compute TL (top line)
            lda HeroY
            clc
            adc #$04
            sta TopLine
            sta TopLine4N   ; preparing to work out T8
            inc TopLine4N   ; T8 is based on TL+1 when in the lower field (only 7 lines in playfield)
            pla             ; restore distance map line is below hero
chbound:    cmp #$04        ; if the distance to hero is less than 4
            bcc froff       ; it is off screen (in the playfield)
            cmp #$23        ; or more than 23
            bcs froff       ; it is off screen (beyond borders)
TopLine4N = *+1
            lda #INLINEVAR  ; T8 = TL mod 8 (or TL+1 mod 8 in lower field), on-screen nudge progress
            and #$07
            sta TopNudge
TopLine = *+1
            lda #INLINEVAR  ; TLZ = TL - T8, the top line when nudge was zero
            sec
TopNudge = *+1
            sbc #INLINEVAR
            sta TopLineZ
MapTemp = *+1
            lda #INLINEVAR  ; MD = Map - TLZ, distance between map line and top of its field, when nudge was zero
            sec
TopLineZ = *+1
            sbc #INLINEVAR
            sta MapDiff
            and #$07        ; M8 = nudge value at which point this line moves to prior 8-line group
            cmp TopNudge
            txa
            bcs :+          ; M8 >= T8, keep base where it is, nudge has not reached point where this line moves
            sec             ; M8 < T8, so back up the base by 8 lines, it is drawn in prior group
            sbc #$08
:           clc             ; raster = base line (from X) + MD, which factors out smooth scroll nudge
MapDiff = *+1
            adc #INLINEVAR
            ; A should now hold the line in video memory corresponding to the map
            rts
froff:      lda #$00        ; return zero (which is clearly not a valid line in the context of this game)
            rts             ; if the line is offscreen.

; mark an x,y as dirty and in need of redrawing
; call with x-coord in X, y-coordinate in Y

hrdirty:    lda DivSeven, x     ; find the bin by dividing by seven
            tax
            lda HRBinLow, x
            sta ZOldPtr         ; co-opt ZOldPtr to point to bin (already directed to bank 2)
            lda HRBinHigh, x
            sta ZOldPtr + 1
            tya                 ; save y-coordinate for stashing
            ldy ZDirtStack, x   ; position of next one to be added to the stack
            inc ZDirtStack, x   ; increase stack pointer for this bin
            sta (ZOldPtr), y
            rts

; go through the dirty segments and redraw them
; could maybe be segmented into computation and then screen update
; but seems to be quick enough now just writing updates directly

hrcleanup:  lda MapDirty
            bne :+
            clc                 ; tell event loop that we did not spend appreciable time
            rts
:           lda R_BANK
            sta GMBank
            lda #$00            ; switch to bank 0 so we can address graphics memory
            sta R_BANK
            lda #$82
            sta ZMapPtr + XByte
            lda HeroY           ; determine what is even potentially on screen
            sec                 ; for quick filtering as we loop through updates
            sbc #$23
            bcs :+
            lda #$00            ; went into the void, so 0 is the minimum
:           sta GMMinMap
            lda HeroY
            clc
            adc #$24            ; $24 because we are doing >=
            bcc :+
            lda #$FF            ; went into the void, so FF is the maximum
:           sta GMMaxMap
            ; now process them all (even if some are redundant, probably few will be)
            ldx #$08            ; doing 9 bins
            stx GMcurrbin
hrcheckbin: ldy ZDirtStack, x
            beq hremptybin      ; branch away if nothing in the current bin
            lda HRBinLow, x     ; consolidate the setup for this bin so we only do it once
            sta ZOldPtr
            lda HRBinHigh, x
            sta ZOldPtr + 1
            lda MapEnds, x
            sta GMMapEnd
            lda MapPixG, x
            sta GMMapPixG
hrbinloop:  ldy ZDirtStack, x
            dey                 ; stack pointer points one above last valid value
            lda (ZOldPtr), y    ; get y-coordinate from end of bin stack
GMMinMap = *+1
            cmp #INLINEVAR      ; don't bother if it is offscreen
            bcc hrboffscr
GMMaxMap = *+1
            cmp #INLINEVAR
            bcs hrboffscr
            sta GMYcoord
            jsr findraster
            beq hrbunderpf      ; off screen (probably under playfield)
            tax
GMYcoord = *+1
            lda #INLINEVAR
            jsr setmapptr
            jsr prepdraw
GMMapPixG = *+1
            lda #INLINEVAR
            clc
            adc ZLineStart      ; groups start from the left edge
            sta ZCurrDrawX
GMMapEnd = *+1
            lda #INLINEVAR
            sta ZCurrMapX
            lda MapPtrL         ; point ZMapPtr at the left side of present line in the map data.
            sta ZMapPtr
            lda MapPtrH
            sta ZMapPtr + 1
            jsr drawseg         ; finally, draw the segment
GMcurrbin = *+1
hrbunderpf: ldx #INLINEVAR
hrboffscr:  dec ZDirtStack, x
            bne hrbinloop
hremptybin: dex
            stx GMcurrbin
            bpl hrcheckbin
            ; done
GMBank = *+1
            lda #INLINEVAR
            sta R_BANK
            lda #$00
            sta MapDirty        ; updates are now done
            sec                 ; signal to event loop that we spent some time here
            rts

; draw a segment on the screen.
; should have already called prepdraw with x holding the raster line to set up ZPs and ZLineStart
; enter with ZCurrDrawX = offset of first byte of 4-byte group of graphics memory (i.e. 4 for second group)
; calls calcseg, which also requires:
; ZCurrMapX being the right edge of group of map bytes (i.e. 6 for first group)
; ZMapPtr should point to the map line (as derived from setmapptr)
; and will return with ZCurrMapX backed up to the right edge of the preceding group of map bytes
; points ZP into graphics, pulls bytes to store out of $1A00 ZP from outside

drawseg:    jsr calcseg
            ldx ZCurrDrawX
            lda ZOtherZP        ; HGR 1
            sta R_ZP
            lda Zero1A + ZPixByteA
            sta Zero, x
            lda Zero1A + ZPixByteC
            sta $01, x
            lda Zero1A + ZPixByteE
            sta $02, x
            lda Zero1A + ZPixByteG
            sta $03, x
            lda ZOtherZP        ; HGR 2
            sta R_ZP
            lda Zero1A + ZPixByteB
            sta Zero, x
            lda Zero1A + ZPixByteD
            sta $01, x
            lda Zero1A + ZPixByteF
            sta $02, x
            lda Zero1A + ZPixByteH
            sta $03, x
            lda #$1A
            sta R_ZP
            rts

; calculate the bytes behind a single 14-pixel segment
; enter with:
; ZCurrMapX being the right edge of group of map bytes (i.e. 6 for first group)
; ZMapPtr should point to the map line (as derived from setmapptr)
; will translate map bytes into pixels, stage in ZPixByteB-H, and return graphics bytes in ZPixByteA-H.
; Note: will also back ZCurrMapX up to the beginning of the preceding map byte group, while it buffers
calcseg:    lda #$06            ; we will buffer seven map elements
            sta ZBufCount
bufmap:     ldy ZCurrMapX
            lda (ZMapPtr), y
            ; translate the map byte into the two pixels it will be displaying.
            ; this information comes from FontDots, which we cached into ZFontDots (1A ZP)
            tay                 ; stash the map byte
            and #$3F            ; strip any color bits
            tax
            lda ZFontDots, x    ; get the pixels
            sta ZPxScratch      ; stash the pixels
            txa
            and #%00110000      ; test to see if this is 0-F (those have color info in two high bits)
            beq :+              ; branch if this is an element with an indexed color (char < $10)
            lda ZPxScratch      ; color was as retrieved, these are the final pixels
            jmp bufmappix
:           tya                 ; recall the map byte to grab the color bits
            asl
            rol
            rol                 ; move color bits into lower two bits to serve as color index
            and #$03
            tax
            tya                 ; recall the map byte one last time
            and #$3F            ; filter out color bits
            cmp #C_DISK         ; if it is a disk, use the disk colors
            bne usemapcol       ; otherwise, use the general colors (walls)
            lda DiskColors, x   ; load the indexed color for disks
            jmp applycolor
usemapcol:  lda MapColors, x    ; load the indexed color
applycolor: and ZPxScratch      ; apply to the pixels (should be 1111 or 0000 else color would be affected)
bufmappix:  ldx ZBufCount
            sta ZPixByteB, x    ; stage pixels in last 7 bytes of scratch space
            dec ZCurrMapX       ; move the map pointer back
            dec ZBufCount       ; keep going until we have buffered 7 map elements
            bpl bufmap
            ; fall through to translate pixel data into screen bytes

; This part below could be called independently, it's quite general
; It assumes there are 14 pixels of data stored in the seven bytes ZPixByteB-ZPixByteH
; And it smears those bits across eight graphics bytes, with result in ZPixByteA-ZPixByteH
; Those bytes are then ready to be written to the video memory (A, C, E, G to $2000-based page)
; Uses A and Y but not X or stack

            ; the pixels have now been translated, we can send them to the screen
            ; the 7 pixels on the stack each use 8 bits, but we need to smear them across the
            ; 8 bytes of graphics memory using 7 bits at a time.  I know, right?
            ;
            ; As per the Apple /// Level 2 Service Reference Manual:
            ; There are two distinct screen pages in this mode but the mapping of the
            ; individual pages is, at first encounter, a bit difficult to master. Good
            ; luck!
            ;  o The display dot represents a sequence of 4 data bits in the RAM
            ;    display area.
            ;  o Two rams are used starting at 2000 and 4000 respectively and alternate
            ;    bytes are fetched from each ram area.
            ;  o In any video mode only 7 of the 8 bits of each byte are displayed
            ; With this information in mind...and remembering that each pixel in this mode
            ; is made from 4 bits...you can see that you need 4 bytes of information to get
            ; 7 pixels.  The way in which these bytes may into picture elements is shown
            ; below.
            ;
            ; [conspiracy-corkboard.jpg]
            ;
            ; |   2000      |   4000      |   2001      |   4001      |
            ; | | | | | | | | | | | | | | | | | | | | | | | | | | | | |
            ; |LSB       MSB|             |             |             | 
            ; |-- P1 -|-- P2 -|-- P3 -|-- P4 -|-- P5 -|-- P6 -|-- P7 -|
            ;
            ; end quote
            ; 
            ; Colors:
            ; 0000 0 black      0100 4 darkgreen    1000 8 brown    1100 C green
            ; 0001 1 magenta    0101 5 grey1        1001 9 orange   1101 D yellow
            ; 0010 2 darkblue   0110 6 medblue      1010 A grey2    1110 E aqua
            ; 0011 3 purple     0111 7 lightblue    1011 B pink     1111 F white
            ;
            ; The bits increase steadily in significance from pixel 1 to pixel 7.
            ; LSB->MSB
            ;  1000100  0100010  0010001  0001000
            ; MSB->LSB
            ;  0010001  0100010  1000100  0001000
            ; 00010001 00100010 01000100 00001000
            ; byte 0 (byte 0 page 1): -1110000 [0+0] 421/8421
            lda ZPixByteB       ; pixels 0-1
            tay                 ; remember for later
            and #$7F
            sta ZPixByteA       ; output byte
            ; byte 1 (byte 0 page 2): -3322221 [0+1+1] 21/8421/8
            tya                 ; recall color of pixel 1
            asl                 ; move hi bit of pixel 1 color
            rol                 ; into lo bit of byte 1
            and #$01
            sta ZPxScratch      ; stash bit of pixel 1
            lda ZPixByteC       ; pixels 2-3
            tay                 ; remember for later
            asl                 ; move pixel 2's and 3's bits up
            and #%011111110     ; and chop off the two hi bits of pixel 3
            ora ZPxScratch      ; and then add pixel 1's last bit in
            sta ZPixByteB       ; output byte
            ; byte 2 (byte 1 page 1): -5444433 [1+2+2] 1/8421/84
            tya                 ; recall color of pixel 3
            asl
            rol
            rol                 ; put pixel 3's hi 2 bits in low bits
            and #$03            ; isolate the pixel 3 color's higher two bits
            sta ZPxScratch      ; and stash them
            lda ZPixByteD       ; pixels 4-5
            tay                 ; remember for later
            asl                 ; shift them up
            asl
            ora ZPxScratch      ; add in pixel 3's hi 2 bits
            and #$7F            ; chop off the msb
            sta ZPixByteC       ; output byte
            ; byte 3 (byte 1 page 2): -6666555 [2+3] 8421/842
            tya                 ; recall color of pixel 5
            asl                 ; move higher 3 bits of pixel 5 into low 3 bits
            rol
            rol
            rol
            and #$07
            sta ZPxScratch
            lda ZPixByteE       ; pixels 6-7
            tay                 ; remember for later
            asl
            asl
            asl                 ; move pixel 6 left three
            ora ZPxScratch      ; and add in pixel 5's bits
            and #$7F            ; chop off the msb
            sta ZPixByteD       ; output byte
            ; byte 4 (byte 2 page 1): -8887777 [3+4] 421/8421
            tya                 ; recall color of pixels 6-7
            lsr                 ; demote pixel 7 to low nibble
            lsr
            lsr
            lsr
            sta ZPxScratch
            lda ZPixByteF       ; pixels 8-9
            tay                 ; remember for later
            asl                 ; promote pixel 8 to high nibble
            asl
            asl
            asl
            ora ZPxScratch
            and #$7F            ; chop off the msb
            sta ZPixByteE       ; output byte
            ; byte 5 (byte 2 page 2): -AA99998 [4+4+5]  21/8421/8
            tya                 ; recall color of pixels 8 and 9
            lsr                 ; get highest pixel 8 bit into lsb
            lsr                 ; putting pixel 9 in the right place too
            lsr
            sta ZPxScratch
            lda ZPixByteG       ; pixels A-B
            tay                 ; remember for later
            lsr                 ; rotate A's low two pixels into bits 7 and 6
            ror
            ror
            ror
            and #%01100000      ; and isolate just those bits 7 and 6
            ora ZPxScratch      ; add in pixels 9 and 8
            sta ZPixByteF       ; output byte
            ; byte 6 (byte 3 page 1): -CBBBBAA [5+5+6] 1/8421/84
            tya                 ; recall color of pixels A-B
            lsr                 ; move pixel A's high two bits to lowest two
            lsr                 ; also puts pixel B in the right place
            and #%00111111      ; clear out last space for pixel C
            sta ZPxScratch
            lda ZPixByteH       ; pixels C-D
            tay                 ; remember for later
            lsr                 ; rotate low bit of pixel C into bit 6
            ror
            ror
            and #%01000000      ; isolate that low bit of pixel C
            ora ZPxScratch      ; add it in to pixels A and B
            sta ZPixByteG       ; output byte
            ; byte 7 (byte 3 page 2): -DDDDCCC [6+6] 8421/842
            tya                 ; recall color of pixels C and D
            lsr                 ; shift away the lsb and strip msb
            sta ZPixByteH       ; output byte
            ; the 14 pixels are now computed and stored in ZPixByteA-H
            rts

