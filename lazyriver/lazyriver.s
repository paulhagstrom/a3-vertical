; lazyriver
; Paul Hagstrom, 2023

            .segment "CODE"
            .setcpu "6502"

            .include "zpdef.s"
            .org     $A000 - 14
            
; SOS interpreter header
            .byte    "SOS NTRP"
            .word    0000
            .word    CodeStart
            .word    (CodeEnd-CodeStart)

CodeStart:  jmp gameinit

            .include "buildmap.s"
;            .include "buildsound.s"
            .include "artdefine.s"
            .include "interrupts.s"
            .include "lookups.s"

Seed:       .byte   0                       ; current place in the "random" number table

PlayerX:    .byte   0                       ; X-coordinate of player on the map.
PlayerY:    .byte   0                       ; Y-coordinate of player on the map.
PlayerYOff: .byte   0                       ; Y offset of player from top of map tile.
VelocityX:  .byte   0                       ; X-velocity of player (neg, 0, pos)
VelocityY:  .byte   0                       ; Y-velocity of player (neg, 0, pos)
MapTop:     .byte   0                       ; map row at top of the screen
MapOff:     .byte   0                       ; offset into tile the map row at top of screen in

MapPtrL:    .byte   0                       ; Holds address of left edge of a map line (low)
MapPtrH:    .byte   0                       ; Holds address of left edge of a map line (high)

NumLogs:    .byte   0                       ; number of logs on map

; The following setting governs how often the game clock goes off, which is when movement
; is processed.  Values under 3 risk leaving not enough time to do everything else when
; movement isn't being processed, but over 3 start feeling pretty pokey.  Best to try to
; keep it at 3 or below and make everything more efficient.
MoveDelay   = 3                             ; VBLs per game tick (3 seems about minimum possible)

CurScrLine: .byte   0
CurMapLine: .byte   0

; main game event loop

ExitFlag = *+1                              ; keyboard handler makes this nonzero to trigger exit
eventloop:  lda #INLINEVAR                  ; if ExitFlag becomes nonzero (within keyboard processing)
            bne alldone                     ; then exit
KeyCaught = *+1                             ; keyboard interrupt pushes a caught key in here
            lda #INLINEVAR                  ; check if we have recently caught a key (keyboard interrupt)
            beq :+
            jsr handlekey                   ; if there was a key, handle it
VBLTick = *+1                               ; ticked down for each VBL, governs game speed
:           lda #INLINEVAR                  ; wait for game clock to tick
            bpl offtick                     ; based on number of VBLs set in MoveDelay
            ; on the game clock, do movement and update animation
            jsr domove                      ; game clock has ticked, move everyone around
            lda #MoveDelay                  ; reset the game clock
            sta VBLTick
            jmp eventloop                   ; go back up to schedule in all the other stuff
            ; when not on the game clock, do everything else until we hit the game clock again
offtick:
            jsr fixscroll                   ; scroll the playfield if needed
            bcs eventloop                   ; go back around if we spent some time
            jsr hrcleanup                   ; patch visible match regions based on movement
            bcs eventloop                   ; go back around if we spent some time
            jsr drawstatus                  ; redraw score (TODO: do only when there is an update)
            jmp eventloop
            
alldone:    lda #$7F                        ;disable all interrupts
            sta RD_INTENAB
            sta RD_INTFLAG
            lda #$7F
            sta RE_INTENAB
            sta RE_INTFLAG
IRQSaveA = *+1
            lda #INLINEVAR                  ; restore the IRQ vector
            sta IRQVECT
IRQSaveB = *+1
            lda #INLINEVAR
            sta IRQVECT + 1
IRQSaveC = *+1
            lda #INLINEVAR
            sta IRQVECT + 2
            brk                             ; SOS TERMINATE
            .byte   TERMINATE
            .word   *-2

; process keypress

handlekey:  
            cmp #$C9            ; I (up, scroll map down)
            bne :+
            lda #$00
            sta VelocityX
            lda #$80
            sta VelocityY
            jmp keydone
:           cmp #$CA            ; J (left)
            bne :+
            lda #$80
            sta VelocityX
            lda #$00
            sta VelocityY
            jmp keydone
:           cmp #$CB            ; K (stop)
            bne :+
            lda #$00
            sta VelocityX
            sta VelocityY
            jmp keydone
:           cmp #$CC            ; L (right)
            bne :+
            lda #$01
            sta VelocityX
            lda #$00
            sta VelocityY
            jmp keydone
:           cmp #$AC            ; , (down, scroll map up)
            bne :+
            lda #$00
            sta VelocityX
            lda #$01
            sta VelocityY
            jmp keydone
:           cmp #$C5            ; E (exit)
            bne keydone            
            inc ExitFlag        ; tell event loop we are exiting
keydone:    lda #$00
            sta KeyCaught
            rts

; compute map pointer, based on A upon entry.  Map data is in bank 2, $2000-5FFF.
; each line is $40 bytes in storage (only 3F bytes used), so take A and multiply by
; $40.  This is equivalent to rotating A right twice into a zero and swapping the bytes.
; If current map pointer is something like 00000101 (5), shift bits to translate to:
; MapPtrL: 01000000 (40) MapPtrH: 00100001 (11) ($2140 and $40 bytes there)

setmapptr:  pha
            lda #$00
            sta MapPtrL
            pla
            lsr                 ; shift lower two bits of map line
            ror MapPtrL         ; into higher bits of MPL
            lsr                 ; (multiplying by $40, the length of a map line)
            ror MapPtrL
            clc
            adc #$20            ; map data starts at $2000, add this to H.
            sta MapPtrH
            rts

; grab a random number seed from the fastest part of the realtime clock.
; I don't think this actually works, but something like this would be a good idea.
seedRandom: lda #$00
            sta R_ZP            ; request smallest RTC byte
            lda IO_CLOCK        ; close enough to random for now
            sta Seed            ; Seed is just used as the index into the "random" number table
            lda #$1A            ; return the ZP to $1A00
            sta R_ZP
            rts

; initialize initial environment and game variables

gameinit:   sei                 ; no interrupts while we are setting up
            ;     0------- 2MHz clock           (1=1MHz)
            ;     -1------ C000.CFFF I/O        (0=RAM)
            ;     --1----- video enabled        (0=disabled)
            ;     ---1---- Reset key enabled    (0=disabled)
            ;     ----0--- C000.CFFF read/write (1=read only)
            ;     -----1-- True stack ($100)    (0=alt stack)
            ;     ------1- ROM#1                (0=ROM#2)
            ;     -------1 F000.FFFF RAM        (1=ROM)
            lda #%01110111      ; 2MHz, video, I/O, reset, r/w, ram, ROM#1, true stack
            sta R_ENVIRON            
            jsr seedRandom      ; seed the "random" number list
            jsr buildmap        ; set up map data (in bank 2)
            jsr buildgfx        ; define graphics assets
            ;jsr buildsfx        ; define sound effects
            jsr setupenv        ; arm interrupts
            lda #232            ; top map row when we start (makes bottom row 255)
            sta MapTop
            lda #$09            ; start player kind of in the middle
            sta PlayerX         ; this is the X coordinate of the player on the map (0-13)
            lda #$FC            ; Start down near the bottom
            sta PlayerY         ; this is the Y coordinate of the player on the map (0-FF)
            lda #$00            
            sta PlayerYOff      ; this is the Y offset of the player from the top of the tile
            sta ExitFlag        ; reset quit signal (detected in event loop)
            sta KeyCaught
            sta VelocityX       ; player X velocity, can be negative, zero, or positive
            sta VelocityY       ; player Y velocity, can be negative, zero, or positive
            lda #MoveDelay      ; game clock - setting number of VBLs per movement advance
            sta VBLTick
            lda #$01            ; number of logs, this ought to be level-dependent
            sta NumLogs
            lda TwelveBran      ; start at smooth scroll offset 0 (first one in TwelveBran)
            sta NudgeVal        ; the smooth scroll offset is affectionately called the "nudge"
            bit IO_KEYCLEAR     ; clear keyboard so we notice any new keypress
            bit D_PAGEONE       ; be on page 1.  Nothing presently uses page 2 for anything.
            bit D_TEXT          ; A3 text
            bit D_NOMIX         ; A3 text
            bit D_LORES         ; A3 text
            bit SS_XXN          ; start at smooth scroll offset zero
            bit SS_XNX
            bit SS_NXX
            bit D_SCROLLON      ; turn on smooth scroll (can stay on throughout)
            jsr paintmap        ; paint visible map
            cli                 ; all set up now, commence interrupting
            jmp eventloop       ; wait around until it is time to quit

CodeEnd     = *