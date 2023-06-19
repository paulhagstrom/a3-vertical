; lazyriver
; Paul Hagstrom, 2023

            .segment "CODE"
            .setcpu "6502"

            .include "zpdef.s"
            .org     $A000 - 14
            ; note: if file gets bigger than 6144 then have to be lower than $A000
            ; start leaves  start   leaves  start   leaves
            ; 9F00  6400    9E00    6656    9D00    6912
            ; 9C00  7167    9B00    7423    9A00    7679
            ; 9900  7935    9800    8191    9700    8447
            ; 9600  8703    9500    8959    9400    9215
            ; 9300  9471

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
            .include "mapscroll.s"
            .include "sprites.s"
            .include "status-text40.s"

Seed:       .byte   0                       ; current place in the "random" number table

PlayerX:    .byte   0                       ; X-coordinate (tile, 0-19) of player
PlayerXOff: .byte   0                       ; X offset of player from left of tile (0-7)
PlayerY:    .byte   0                       ; Y-coordinate of player on the map.
;PlayerYOff: .byte   0                       ; Y offset of player from top of map tile.
VelocityX:  .byte   0                       ; X-velocity of player (neg, 0, pos)
VelocityY:  .byte   0                       ; Y-velocity of player (neg, 0, pos)
GroundVel:  .byte   0                       ; Y-velocity of the ground (neg, 0, pos)
PgOneTop:   .byte   0                       ; map row at top of the screen on page 1
PgTwoTop:   .byte   0                       ; map row at top of the screen on page 2 (must follow PgOneTop)
PgOneOff:   .byte   0                       ; scroll offset on page 1
PgTwoOff:   .byte   0                       ; scroll offset on page 2 (must follow PgOneOff) 

GameLevel:  .byte   0
GameScore:  .byte   0, 0, 0

NumLogs:    .byte   0                       ; number of logs on map

; The following settings govern when how often the movement clock goes off, and when
; the page swaps, in VBLs.  This is kind of set by guesswork and trial and error.
; Need to leave enough time in MoveDelay to allow drawing/erasing sprites,
; scrolling both pages.  Need to set FlipTick late enough that movement, draw sprites, 
; and one scroll can happen first, before reaching FlipTick (so it waits for the VBL).
; Beyond that, no problem setting them higher to slow down game speed for difficulty management.
; VBLTick   00: do movement computations (scroll? sprites? collisions?) (1 tick?)
;           06- scroll page B if needed (1 tick?)
;           05- draw sprites on page B (1 tick?)
;           03- flip to page A (swap B and A meanings)
;           02- erase sprites on page B (1 tick?)
;           01- scroll page B to match page A (if they don't already match) (1 tick?)

MoveDelay   = 5                             ; VBLs per game tick (3 seems about minimum possible)
FlipTick    = 3                             ; Tick just after which the page flip happens.

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
            bne dotask                      ; based on number of VBLs set in MoveDelay
            ; on the game clock, do movement and update animation
            lda #MoveDelay                  ; reset the game clock
            sta VBLTick
            lda #$01                        ; restart task list from the start
            sta TaskStep
TaskStep = *+1
dotask:     lda #INLINEVAR
            cmp #$01                        ; do movement processing
            bne :+
            jsr domove
            inc TaskStep
            bne eventloop
:           cmp #$02                        ; scroll ground on nonvisible page if needed
            bne :+
            jsr fixscroll
            inc TaskStep
            bne eventloop
:           cmp #$03                        ; draw sprites on nonvisible page
            bne :+
            jsr setsprites
            inc TaskStep
            bne eventloop
:           cmp #$04                        ; wait for FlipTick and flip pages
            bne :+
            lda #FlipTick
            cmp VBLTick
            bcs eventloop
            lda ShownPage
            eor #$01
            sta ShownPage
            jsr fixnudge
            inc TaskStep
            bne eventloop
:           cmp #$05                        ; erase sprites on (newly) nonvisible page
            bne :+
            jsr clrsprites
            inc TaskStep
            bne eventloop
:           cmp #$06                        ; sync ground scroll on nonvis page with vis page
            bne :+
            jsr syncscroll
            inc TaskStep
            bne eventloop
:           cmp #$07                        ; draw score
            bne :+
            jsr drawstatus
            inc TaskStep
:           bne eventloop
            
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
:           cmp #$AC            ; , (down)
            bne :+
            lda #$00
            sta VelocityX
            lda #$01
            sta VelocityY
            jmp keydone
:           cmp #$D1            ; Q (ground stop)
            bne :+
            lda #$00
            sta GroundVel
            jmp keydone
:           cmp #$DA            ; Z (down, scroll ground up)
            bne :+
            lda #$01
            sta GroundVel
            jmp keydone
:           cmp #$C1            ; A (up, scroll ground down)
            bne :+
            lda #$FF
            sta GroundVel
            jmp keydone
:           cmp #$C5            ; E (exit)
            bne keydone            
            inc ExitFlag        ; tell event loop we are exiting
keydone:    lda #$00
            sta KeyCaught
            rts

; set the smooth scroll parameter on the visible page
fixnudge:   lda ShownPage
            and #$01            ; 0 if page 1 is visible, 1 if page 2 is visible
            tay
            lda PgOneOff, y     ; offset value of visible page
            tay
            lda TwelveBran, y   ; jump table lookup for this offset value
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

; TwelveBran is a table of multiples of $0C, used as a branch table when setting smooth scroll
TwelveBran: .byte   $00, $0C, $18, $24, $30, $3C, $48, $54

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
            jsr buildmap        ; set up map data (in bank 1)
            jsr buildgfx        ; define graphics assets
            ;jsr buildsfx        ; define sound effects
            lda #232            ; top map row when we start (makes bottom row 255)
            sta PgOneTop        ; top map row - page 1
            sta PgTwoTop        ; top map row - page 2
            lda #$09            ; start player kind of in the middle
            sta PlayerX         ; this is the X coordinate of the player on the map (0-19)
            lda #128            ; Start down near the bottom of the screen
            sta PlayerY         ; this is the Y coordinate of the player on the map (0-FF)
            sta TaskStep        ; unallocated task step to stall until game clock ticks
            lda #$00            
            sta PgOneOff        ; scroll offset 0 - page 1
            sta PgTwoOff        ; scroll offset 0 - page 2
            sta NeedScroll      ;
            ;sta PlayerYOff      ; this is the Y offset (0-7) of the player from the top of the tile
            sta PlayerXOff      ; this is the X offset (0-7) of the player from the left of the tile
            sta ExitFlag        ; reset quit signal (detected in event loop)
            sta KeyCaught
            sta VelocityX       ; player X velocity, can be negative, zero, or positive
            sta VelocityY       ; player Y velocity, can be negative, zero, or positive
            sta GroundVel       ; ground velocity, can be negative, zero, or positive
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