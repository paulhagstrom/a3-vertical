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

            .include "artdefine.s"
            .include "buildmap.s"
;            .include "buildsound.s"
            .include "gamemove.s"
            .include "interrupts.s"
            .include "lookups.s"
            .include "map-hires3.s"
            .include "status-text40.s"

Seed:       .byte   0                       ; current place in the "random" number table

PlayerX:    .byte   0                       ; X-coordinate of player on the map.
PlayerY:    .byte   0                       ; Y-coordinate of player on the map.
PlayerYOff: .byte   0                       ; Y offset of player from top of map tile.
VelocityX:  .byte   0                       ; X-velocity of player (neg, 0, pos)
VelocityY:  .byte   0                       ; Y-velocity of player (neg, 0, pos)
TopRow:     .byte   0                       ; map row at top of the screen
TopOff:     .byte   0                       ; offset into tile the map row at top of screen in
PgOneOff:   .byte   0                       ; smooth scroll offset on page 1
PgTwoOff:   .byte   0                       ; smooth scroll offset on page 2

GameLevel:  .byte   0
GameScore:  .byte   0, 0, 0

NumLogs:    .byte   0                       ; number of logs on map

; The following setting governs how often the game clock goes off, which is when movement
; is processed.  Values under 3 risk leaving not enough time to do everything else when
; movement isn't being processed, but over 3 start feeling pretty pokey.  Best to try to
; keep it at 3 or below and make everything more efficient.
MoveDelay   = 3                             ; VBLs per game tick (3 seems about minimum possible)

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
            jsr domove                      ; game clock has ticked, move everyone (takes time)
            lda #MoveDelay                  ; reset the game clock
            sta VBLTick
            jmp eventloop                   ; go back up to schedule in all the other stuff
            ; when not on the game clock, do everything else until we hit the game clock again
offtick:
            ;jsr clrsprite                   ; erase sprites on nonvisible page
            ;bcs eventloop                   ; go back around if we spent some time
            jsr syncscroll                  ; scroll nonvisible page to match visible one
            bcs eventloop                   ; go back around if we spent some time
            jsr fixscroll                   ; scroll nonvisible page if movement is needed
            bcs eventloop                   ; go back around if we spent some time
            ;jsr setsprite                   ; draw sprites on nonvisible page
            ;bcs eventloop                   ; go back around if we spent some time
            jsr drawstatus                  ; redraw score
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
            lda #$FF
            sta VelocityY
            jmp keydone
:           cmp #$CA            ; J (left)
            bne :+
            lda #$FF
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
            lda #232            ; top map row when we start (makes bottom row 255)
            sta TopRow
            lda #$09            ; start player kind of in the middle
            sta PlayerX         ; this is the X coordinate of the player on the map (0-13)
            lda #$FC            ; Start down near the bottom
            sta PlayerY         ; this is the Y coordinate of the player on the map (0-FF)
            lda #$00            
            sta TopOff          ; 
            sta NeedScroll      ;
            sta PlayerYOff      ; this is the Y offset of the player from the top of the tile
            sta ExitFlag        ; reset quit signal (detected in event loop)
            sta KeyCaught
            sta VelocityX       ; player X velocity, can be negative, zero, or positive
            sta VelocityY       ; player Y velocity, can be negative, zero, or positive
            lda #MoveDelay      ; game clock - setting number of VBLs per movement advance
            sta VBLTick
            lda #$01            ; number of logs, this ought to be level-dependent
            sta NumLogs
            lda #<D_PAGEONE     ; inline in the interrupt handler
            sta ShownPage       ; visible page, HBL uses this to know where to switch to
            bit IO_KEYCLEAR     ; clear keyboard so we notice any new keypress
            bit D_TEXT          ; A3 text
            ; the following are set immediately by VBL/HBL handlers so they don't matter
            ;bit D_PAGEONE       ; be on page 1.
            ;bit D_NOMIX         ; A3 text
            ;bit D_LORES         ; A3 text
            bit SS_XXN          ; start at smooth scroll offset zero
            bit SS_XNX
            bit SS_NXX
            ; due to a MAME bug this is set at HBL, so doesn't matter here 
            ;bit D_SCROLLON      ; turn on smooth scroll (can stay on throughout)
            jsr paintpage       ; paint whole map (onto page 2, nonvisible page)
            jsr copypage        ; copy page 2 to page 1
            jsr paintstat       ; paint status area
            jsr setupenv        ; arm interrupts
            cli                 ; all set up now, commence interrupting
            jmp eventloop       ; wait around until it is time to quit

CodeEnd     = *