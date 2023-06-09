; DISKHERO
; Apple III 40-column text region
; middle primary play field display
; occupies scan lines 40-8F, text lines 08-11.
; display map lines 20-26 (where the top of the upper playfield is 0)
;           (or, in other words, up 3 and down 3 from the center)

BorderChar  = $00       ; C_SPACE
BorderColA  = $5A       ; grey2 background
BorderColB  = $A5       ; grey1 background

; Characters that fill the lines just above and just below the playfield
FrameText:  .byte C_WALL_RUD,   C_WALL_H,   C_WALL_H,   C_WALL_H,   C_WALL_H
            .byte C_WALL_H,     C_WALL_H,   C_WALL_H,   C_WALL_H,   C_WALL_H
            .byte C_WALL_H,     C_WALL_H,   C_WALL_H,   C_WALL_H,   C_WALL_H
            .byte C_WALL_H,     C_WALL_H,   C_WALL_H,   C_WALL_H,   C_WALL_H
            .byte C_WALL_H,     C_WALL_H,   C_WALL_H,   C_WALL_H,   C_WALL_H
            .byte C_WALL_H,     C_WALL_H,   C_WALL_H,   C_WALL_H,   C_WALL_H
            .byte C_WALL_H,     C_WALL_H,   C_WALL_H,   C_WALL_H,   C_WALL_H
            .byte C_WALL_H,     C_WALL_H,   C_WALL_H,   C_WALL_H,   C_WALL_LUD
            
; The only static stuff needing initialization are top/bottom border characters.
; Even though it does not require blazing speed, I use the stack to push -- just
; to exemplify the technique.  In the main game code, interrupts come too fast to
; use the stack (not interrupt-safe, can't disable interrupts for so long).
; (This code is executed at the beginning, while interrupts have been suspended.)
; Exits with ZP set to $1A00.

IPcount:    .byte   0
IPeol:      .byte   0           ; end of line pointer

initplay:   tsx                 ; preserve stack and environment register
            stx IPstack
            lda R_ENVIRON
            sta IPenv
            and #%11111011      ; set stack bit to zero to get alternate stack
            sta R_ENVIRON
            lda #$01
            sta IPcount
            ldy #$08            ; start by drawing text line 8
iploop:     lda YLoresHA, y     ; look up (high byte of) where this line starts
            eor #$01            ; Compute where where ZP has to be
            sta R_ZP            ; in order to put stack to be where we want it
            ldx YLoresS, y      ; end of line pointer (start + $27)
            stx IPeol           ; save the end of line pointer for later
            txs                 ; point stack pointer at end of line
            ldy #$27
:           lda FrameText, y
            pha                 ; push onto the screen
            dey
            bpl :-
            lda R_ZP            ; switch to color page
            adc #$04
            sta R_ZP
            ldx IPeol           ; recall end of line address
            txs                 ; point stack pointer at end of line
            lda #BorderColB     ; outer border color
            ldy #$27
:           pha
            dey
            bpl :-
            dec IPcount         ; check if we have done the bottom line yet
            bmi ipshadow        ; if we've already drawn line 10, exit
            ldy #$10            ; otherwise, draw line 10
            jmp iploop
ipshadow:   ldy #$11            ; draw line 11 (space, with shadow color)
            lda YLoresHA, y
            eor #$01            ; this is where ZP has to be
            sta R_ZP            ; in order for stack to be where we want it
            ldx YLoresS, y
            stx IPeol           ; save the end of line pointer for later
            txs                 ; point stack pointer at end of line
            lda #C_SPACE        ; C_SPACE is the character (blank) for that line
            ldy #$27
:           pha                 ; push onto the screen
            dey
            bpl :-
            lda R_ZP
            clc
            adc #$04            ; go to color space
            sta R_ZP
            ldx IPeol           ; recall end of line address
            txs                 ; point stack pointer at end of line
            ldy #$27
            lda #BorderColB     ; shadow color
:           pha
            dey
            bpl :-
            lda #$1A            ; back to $1A00 for ZP
            sta R_ZP
IPenv = *+1
            lda #INLINEVAR      ; back to true stack (or whatever the entry setting was)
            sta R_ENVIRON
IPstack = *+1
            ldx #INLINEVAR  ; restore stack pointer
            txs
            lda #$00        ; reset buffer ready flag
            sta PlayCTS
            lda #$01        ; set buffer-needs-updating flag
            sta PlayDirty
            rts
            
; Map2Thumb is a table showing which lores column corresponds to
; which map/hires column.  Indexed by map column, this gives lores column.
Map2Thumb:  .byte   $00, $00, $01, $01, $02, $02, $03
            .byte   $04, $04, $05, $05, $06, $06, $07
            .byte   $08, $08, $09, $09, $0A, $0A, $0B
            .byte   $0C, $0C, $0D, $0D, $0E, $0E, $0F
            .byte   $10, $10, $11, $11, $12, $12, $13
            .byte   $14, $14, $15, $15, $16, $16, $17
            .byte   $18, $18, $19, $19, $1A, $1A, $1B
            .byte   $1C, $1C, $1D, $1D, $1E, $1E, $1F
            .byte   $20, $20, $21, $21, $22, $22, $23

; Blast the queued up lines into video memory
blitplay:   lda PlayCTS         ; go only when the buffer is ready
            bne :+
            clc                 ; signal to event loop that no appreciable time was wasted
            rts
:           lda #$00            ; reset clear to send to "nope"
            sta PlayCTS
            ldy #$07
            sty CurrBline
:           lda ScrRegion       ; stall for screen mode to leave playfield region
            cmp #$04            ; wait for playfield region to pass
            bne :-
CurrBline = *+1
blitpfloop: ldy #INLINEVAR
            bne blitpfnorm
            ; in the special case of the last line, it is drawing
            ; from the color space to the target lines $08 and $10
            ldx PlayBufSrc          ; get source line
            lda YLoresL, x
            sta ScrChrSrc
            sta ScrColSrc
            lda YLoresHA, x
            clc
            adc #$04                ; fetch from color space
            sta ScrChrSrc + 1
            sta ScrColSrc + 1
            ldx #$08                ; get target line (08)
            lda YLoresS, x
            sta ScrChrTrgL
            lda YLoresHA, x
            clc
            adc #$04                ; send to color space
            sta R_ZP
            ldx #$10                ; get other target line (10)
            lda YLoresS, x
            sta ScrColTrgL
            lda YLoresHA, x
            clc
            adc #$04                ; send to color space
            sta ScrColTrgH
            jmp blitpfdo
            ; this is just the normal case, write chr and col for the line
blitpfnorm: ldx PlayBufSrc, y       ; get source line
            lda YLoresL, x
            sta ScrChrSrc
            sta ScrColSrc
            lda YLoresHA, x
            sta ScrChrSrc + 1
            clc
            adc #$04
            sta ScrColSrc + 1
            ldx PlayBufTrg, y       ; get target line
            lda YLoresS, x
            sta ScrColTrgL
            sta ScrChrTrgL
            lda YLoresHA, x
            sta R_ZP
            clc
            adc #$04
            sta ScrColTrgH
ScrChrTrgL = *+1
blitpfdo:   ldx #INLINEVAR
            ldy #$27
ScrChrSrc = *+1
:           lda $400, y         ; INLINEVAR - source char address
            sta Zero, x
            dex
            dey
            bpl :-
ScrColTrgL = *+1
            ldx #INLINEVAR
ScrColTrgH = *+1
            ldy #INLINEVAR
            sty R_ZP
            ldy #$27
ScrColSrc = *+1
:           lda $800, y         ; INLINEVAR - source color address
            sta Zero, x
            dex
            dey
            bne :-
            dec CurrBline
            bmi blitpfdone
            jmp blitpfloop
blitpfdone: lda #$1A            ; restore ZP to $1A00
            sta R_ZP
            sec                 ; signal to event loop that we burnt some time
            rts

; Lines we draw to (staging buffers for lines 8-F)
; These live in the text area that is not displayed because the hires graphics regions are overtop them.
PlayBufSrc: .byte   $15, $04, $05, $06, $07, $12, $13, $14  ; char and color spaces for lines in playfield area
PlayBufTrg: .byte   $08, $09, $0A, $0B, $0C, $0D, $0E, $0F  ; target lines, to blit onto

PlayCTS:    .byte   0           ; nonzero if playfield has been buffered and is ready to blit
PlayDirty:  .byte   0           ; nonzero if playfield needs to be redrawn due to movement

PlayLeft:   .byte   0
PlayRight:  .byte   0
PlayTop:    .byte   0
BorderV:    .byte   0           ; playfield border counter (total vertical border columns)
BorderR:    .byte   0           ; playfield border counter (total border columns on the right)
BorderRYet: .byte   0           ; playfield border counter (right border columns yet to draw)
BorderL:    .byte   0           ; memory index of rightmost playfield column offset from YLoresL after border done
BorderS:    .byte   0           ; memory index of leftmost playfield column from YLoresS after border done
BorDataA:   .byte   0           ; character or color inside border on left and right
BorDataB:   .byte   0           ; character or color on border on left and right

; in order to keep hero in the middle, five columns are used by a frame
; based on hero position, 5 total, high-nibble-of-HeroX of those are on the right side

; first step, compute boundaries of playfield, void extents
drawplay:   clc                 ; signal to event loop that no appreciable time was wasted
            lda PlayCTS         ; check to see if buffer is already prepared
            beq :+
            rts                 ; if so, do nothing
:           lda PlayDirty       ; check to see if anything has moved since last draw
            bne :+
            rts                 ; if not, do nothing
:           lda #$01            ; set buffer prepared flag (in anticipation of finishing buffer in here) 
            sta PlayCTS
            lda #$00            ; reset buffer-needs-updating flag (in anticipation of updating in here)
            sta PlayDirty
            lda HeroX           ; take high nibble of HeroX - that is BorderR
            lsr                 ; that is, if HeroX is at 10, there are 2 border cols on the right, 3 on left
            lsr                 ; and if HeroX is at 3F, there are 4 borders on the right, 1 on left
            lsr
            lsr
            sta BorderR         ; BorderR is how many columns (0-based) of border are on the right
            ldy #$00            ; use Y to store zeros where we need them
            lda HeroX           ; HeroX + $11 is the last drawn column inside the border
            clc
            adc #$11
            sta PlayRight       ; map column of the right edge of visible playfield (could be off the map)
            sec
            sbc #$22            ; back 22 from the last drawn column is the first drawn column
            bcs pmnovoidl       ; if it is still on the map, there is no left void, skip ahead
            eor #$FF            ; inverse of the number is the left void
            adc #$01
            sta VoidL
            sty PlayLeft        ; leftmost playfield map column is zero
            jmp pmcheckr
pmnovoidl:  sta PlayLeft        ; store leftmost playfield map column
            sty VoidL           ; and record the fact that there is no left void
pmcheckr:   lda PlayRight       ; if PlayRight - #$3E (last map column) is positive, that is VoidR
            sec
            sbc #$3E
            bmi pmnovoidr       ; branch away if there is no VoidR
            sta VoidR           ; store the right void
            lda #$3E            ; and adjust rightmost map column in playfield to be 3E.
            sta PlayRight
            jmp pmcheckt
pmnovoidr:  sty VoidR           ; record that there is no right void
pmcheckt:   lda HeroY           ; check for top void
            sec
            sbc #$03
            bcs pmnovoidu       ; branch away if there is no top void
            eor #$ff
            adc #$01
            sta VoidU           ; top void is the negative of HeroY - 3.
            sty PlayTop         ; top map line is 0
            jmp dppostvoid
pmnovoidu:  sta PlayTop         ; record top map line in playfield (HeroY-3)
            sty VoidU           ; and that there is no top void
; at this point:
; VoidL, VoidR are number of void columns on left and right respectively.
; VoidU is number of void rows above HeroY.
; PlayLeft, PlayRight are the first, last map columns being drawn onscreen, respectively.
; BorderR is the number of border columns on the right.  (There will be 5-BorderR on the left.)
; draw the top and bottom borders (containing thumb). Just colors, chars will already be there
dppostvoid: ldx PlayRight       ; last visible column on the playfield
            lda Map2Thumb, x    ; corresponds to right edge of thumb
            pha
            ldx PlayLeft        ; first visible column on the playfield
            lda Map2Thumb, x    ; coresponds to left edge of thumb
            pha
            ldy PlayBufSrc      ; we are at the top of the playfield box (dest: 08 and 10)
borderh:    lda YLoresHA, y     ; $800 base (color space) is computed from char space
            clc
            adc #$04
            sta R_ZP            ; point ZP at correct line on color page
            ldx YLoresL, y      ; get the left edge index
            inx                 ; skip past the first two columns
            inx                 
            stx dothumb + 1     ; store as the zero base in the upcoming loop
            pla
            sta ThumbL
            pla
            sta ThumbR
            ldx #$23            ; paint $24 characters
doborder:   lda #$A5            ; border (non-thumb) color by default
ThumbR = *+1
            cpx #INLINEVAR
            beq midthumb        ; we are on the right edge of the thumb, do thumb color
            bcs dothumb         ; we have not passed the right edge of the thumb already
ThumbL = *+1
            cpx #INLINEVAR      ; we have passed the right edge, have we escaped the left edge?
            bcc dothumb         ; we have not escaped off the left edge of the thumb
midthumb:   lda #$02            ; thumb color
dothumb:    sta Zero, x         ; plant the color (address modified to be start of line + 2)
            dex
            bpl doborder
; now draw the inner playfield
            lda #$01            ; zero was the borders we just did.
            sta CurScrLine
innerplay:  dec VoidU           ; burn through upper void lines first if there are any
            bmi pfstart         ; branch away if done with upper void
            jsr playvoid        ; draw the void at CurScrLine
            inc CurScrLine
            jmp innerplay
pfstart:    lda PlayTop         ; get address of first drawn line (now that top void is passed)
            jsr setmapptr
pfline:     lda MapPtrH         ; see if we are in the four lines below the map
            cmp #$60
            beq :+              ; if so, branch away to draw a void line, else:
            jsr loresline       ; draw the current MapPtr map line at CurScrLine
            jmp pfnext          ; and advance
:           jsr playvoid        ; draw a void line if we're in the void
pfnext:     lda MapPtrL         ; advance map pointer (even if we are in the void)
            clc
            adc #$40
            sta MapPtrL
            bcc :+
            inc MapPtrH
:           inc CurScrLine      ; move to the next screen line
            lda CurScrLine
            cmp #$08            ; have we already done the last one ($07)?
            bne pfline          ; if not (more to draw), go up and do them
            lda #$1A            ; return ZP to its proper place
            sta R_ZP
            lda #$00            ; reset the "dirty" flag, playfield has been redrawn since move
            sta PlayDirty
            sec                 ; signal to event loop that we burnt some time
            rts

; draw the playfield left/right border and compute edges of line/void to draw
; after: BorderL, BorderS hold the low bytes of the screen memory addresses for line (left, right)
; assumes that ZP register can be manipulated with abandon, will be returned to $1A00 somewhere else, later
playbord:   lda #BorderChar
            sta BorDataA        ; outer border character
            sta BorDataB        ; inner border character
            ldx CurScrLine
            ldy PlayBufSrc, x
            lda YLoresHA, y     ; $400 base (char space)
pbdraw:     sta R_ZP            ; point ZP at appropriate space
            lda #$04            ; draw five border columns total
            sta BorderV
            lda BorderR         ; save a local copy of this that we can decrement
            sta BorderRYet
            ldx YLoresS, y      ; address of the right edge of the line
            lda BorDataB        ; draw right border outer value first
            sta Zero, x         ; do this outside the loop
            lda BorDataA        ; switch to inside value
            dex
            dec BorderV         ; we have done one of the five
            dec BorderRYet      ; and we have done one of the right side ones
            bmi pbrdone         ; skip loop if that was the only right side one
:           sta Zero, x
            dex
            dec BorderV         ; we have done one of the five
            dec BorderRYet      ; and we have done one of the right side ones
            bpl :-              ; if more right side ones remain, do them
pbrdone:    stx BorderS         ; this is where line or void will start (right edge)
            ldx YLoresL, y      ; address of the left edge of the line
            lda BorDataB        ; draw left border outer value first
            sta Zero, x         ; do this outside the loop
            lda BorDataA        ; switch to inside value
            inx
            dec BorderV         ; we have done one of the five
            bmi pbldone         ; skip loop if that was the only left side one
:           sta Zero, x
            inx
            dec BorderV         ; we have done one of the five
            bpl :-              ; if more remain (all on the left now), do them
pbldone:    stx BorderL         ; this is where line of void will start (left edge)
            ; if we didn't just do border colors, do border colors
            lda BorDataA        ; first time through used #BorderChar
            cmp #BorderColA     ; if we already switched it to colors
            beq pbdone          ; we are finished and can leave
            lda #BorderColB     ; otherwise, change the data to colors
            sta BorDataB
            lda #BorderColA
            sta BorDataA
            lda R_ZP            ; we stored the char space high byte in here
            clc                 ; and color space is $04 away
            adc #$04            ; so find $800 base color space and do it again
            jmp pbdraw          ; and do it again but with colors to the color space
pbdone:     rts
            
; draw a void line in the playfield
; assumes that ZP register can be manipulated with abandon, will be returned to $1A00 somewhere else, later
playvoid:   jsr playbord        ; draw the border and compute the edges
            ldx CurScrLine
            ldy PlayBufSrc, x
            lda YLoresHA, y     ; $800 base (char space)
            sta R_ZP            ; go to char space
            lda BorderL         ; computed first left column
            sta pvchar + 1      ; store it in the upcoming instruction as the 0-base
            lda #C_SPACE        ; void character is C_SPACE
            ldx #$22            ; drawing 35 columns between borders (0 to $22)
pvchar:     sta Zero, x         ; address modified to be based at start of current line
            dex
            bpl pvchar
            lda R_ZP            ; find $800 base (col space)
            clc
            adc #$04
            sta R_ZP            ; go to col space
            lda BorderL         ; computed first left column
            sta pvcol + 1       ; store it in the upcoming instruction as the 0-base
            lda #$10            ; magenta background, black foreground
            ldx #$22            ; drawing 35 columns between borders
pvcol:      sta Zero, x         ; address modified to be based at start of current line
            dex
            bpl pvcol
            rts

; draw a lores line in the playfield, assumes MapPtr is set.
; leaves with the ZP register dangling out in graphics space, assumes it will be fixed by someone else
loresline:  jsr playbord        ; draw the border and compute the edges
            lda #$1A            ; we want to be in $1A00 ZP for this part
            sta R_ZP
            lda MapPtrL         ; store MapPtr in 1A00 ZP ZMapPtr
            sta ZMapPtr
            lda MapPtrH
            sta ZMapPtr + 1
            lda #$82
            sta ZMapPtr + XByte
            ; store the text data in $1A00 ZP (color) and $100 stack (char) as we build it
            ldx #$22            ; we are drawing 35 characters across
            ldy VoidR           ; if there is void to the right, draw it first
            beq pfpostvoid
:           lda #$10            ; magenta background, black foreground
            sta Zero, x
            lda #C_SPACE
            pha
            dex
            dey
            bpl :-
pfpostvoid: stx ZPxScratch      ; column after processing the right void
            ldy PlayRight       ; first map column in this row
pfmapdraw:  lda (ZMapPtr), y    ; get the map data
            sta ZCharTemp       ; save a copy while we tinker with it
            and #%00110000      ; test to see if this is 0-F (separate color info)
            beq pfindexed       ; branch if this is an element with an indexed color
            ldx ZCharTemp       ; this is an element with an intrinsic color (in ZFontCol)
            lda ZFontCol, x     ; so look it up
            jmp gotcolor
pfindexed:  lda ZCharTemp
            asl
            rol
            rol                 ; move color bits into lower two bits to serve as color index
            and #$03
            tax
            lda ZCharTemp       ; now that we got the color
            and #$3F            ; strip the color bits
            sta ZCharTemp
            cmp #C_DISK         ; if it is a disk, use the disk colors
            bne useplaycol
            lda DiskColors, x
            jmp gotcolor
useplaycol: lda MapColors, x    ; load the indexed color
gotcolor:   and #$0F            ; keep only the foreground color (background = black/0)
gotcolorb:  ldx ZPxScratch      ; current X column
            sta Zero, x         ; store color in ZP (1A00)
            lda ZCharTemp
            pha                 ; push character to stack
            dey                 ; decrement map index
            bmi leftvoid        ; branch away if we have reached the left void
            dec ZPxScratch      ; decrement saved copy of map column
            bpl pfmapdraw       ; if there are still columns left, keep going
            jmp pfdone
leftvoid:   dex                 ; we touched the void, any left to draw?
            bmi pfdone          ; nope, all finished
            lda #C_SPACE        ; left edge void, push a magenta blank
            pha
            lda #$10            ; magenta
            sta Zero, x
            jmp leftvoid
pfdone:     ldx CurScrLine      ; now, send what we collected to the screen staging buffer
            ldy PlayBufSrc, x   ; TODO: could maybe optimize by sending to staging buffer first and skipping this
            lda YLoresHA, y     ; $400 base (character space)
            sta R_ZP            ; go to character memory
            lda BorderL         ; left edge inside border
            sta pfwchar + 1     ; store it in the upcoming instruction as 0-base
            ldx #$00            ; start drawing from the left (we pushed into stack l-to-r)
:           pla                 ; pull the character
pfwchar:    sta Zero, x         ; store it in character space (base modified to point to line's left edge)
            inx
            cpx #$23            ; 0-22 are all there are
            bne :-
            lda R_ZP            ; move to $800 base (color space)
            clc
            adc #$04
            sta R_ZP
            lda BorderL         ; left edge inside border
            sta pfwcol + 1      ; store it in the upcoming instruction as 0-base
            ldx #$22            ; this time we don't use the stack so draw in reverse
:           lda Zero1A, x       ; sneakily extract from the ZP we stored it in
pfwcol:     sta Zero, x         ; and put it in the ZP we point at (modified to point to line's left edge)
            dex
            bpl :-
            lda #$01            ; done, ready to send
            sta PlayCTS
            rts
