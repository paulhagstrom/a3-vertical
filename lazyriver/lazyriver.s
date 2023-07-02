; lazyriver
; Paul Hagstrom, 2023

            .segment "CODE"
            .setcpu "6502"

            .include "zpdef.s"
            .org     $9F00 - 14
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

            .include "artdefine.s"          ; pixels for tiles and sprites
            .include "artinit.s"            ; transform art definitions to usable form
            .include "buildmap.s"           ; generate the map
            .include "spriteinit.s"         ; initialize the sprites
;            .include "buildsound.s"
            .include "gamemove.s"           ; handle game movement
            .include "interrupts.s"         ; interrupt routines
            .include "lookups.s"            ; precomputed lookup tables
            .include "mapscroll.s"          ; smooth scrolling routines
            .include "sprites.s"            ; sprite drawing routines
            .include "status-text40.s"      ; text status bar routines

Seed:       .byte   0                       ; current place in the "random" number table

GroundVel:  .byte   0                       ; Y-velocity of the ground (neg, 0, pos)
PgOneTop:   .byte   0                       ; map row at top of the screen on page 1
PgTwoTop:   .byte   0                       ; map row at top of the screen on page 2 (must follow PgOneTop)
PgOneOff:   .byte   0                       ; scroll offset on page 1
PgTwoOff:   .byte   0                       ; scroll offset on page 2 (must follow PgOneOff) 

GameLevel:  .byte   0
GameScore:  .byte   0, 0, 0

NumLogs:    .byte   0                       ; number of logs on map (zero based)

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
MoveDelay = *+1
            cmp #INLINEVAR
            bcc dotask                      ; based on number of VBLs set in MoveDelay
            ; on the game clock, do movement and update animation
            lda #$00                        ; restart task list from the start
            sta VBLTick
            sta TasksDone
TasksDone = *+1
dotask:     lda #INLINEVAR
            bne eventloop                   ; branch back if just waiting to flip
            lda ShownPage                   ; flip pages and fix the smooth scroll value
            eor #$01
            sta ShownPage
            jsr fixnudge
            ;lda #$00
            ;sta ZDebugN                     ; restart log
            jsr clrsprites                  ; erase sprites on (newly) nonvisible page
            jsr syncscroll                  ; sync ground scroll on nonvis page with vis page
            jsr fixscroll                   ; scroll ground on nonvisible page if needed
            jsr domove                      ; do movement processing
            jsr drawstatus                  ; draw score
            jsr setsprites                  ; draw sprites on nonvisible page
            inc TasksDone
            lda VBLTick                     ; check if VBLTick > MoveDelay
            cmp MoveDelay
            bcc eventloop                   
            sta MoveDelay                   ; ensure consistent timing by increasing MoveDelay
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

handlekey:  tax
KeyFlag = *+1
            lda #INLINEVAR
            and #$02            ; shift key down
            sta KeyFlag         ; just make it nonzero for shift, for BIT+BNE
            txa
            ldy #SprPlayer      ; player velocity is attached to player sprite
            cmp #$C9            ; I (up)
            bne :+++
            lda (ZSprYV), y
            sec
            sbc #$01
            tax
            sec                 ; make sure we aren't moving faster than -3
            sbc #<-3            ; (borderline magic signed number comparison)
            bvc :+
            eor #$80
:           bmi :+              ; new velocity is < -3, so don't store it
            txa
            sta (ZSprYV), y
:           jmp keydone
:           cmp #$AC            ; , (down)
            bne :+++
            lda (ZSprYV), y
            clc
            adc #$01
            tax
            sec
            sbc #$04
            bvc :+
            eor #$80
:           bpl :+
            txa
            sta (ZSprYV), y
:           jmp keydone
:           cmp #$CA            ; J (left)
            bne :+++
            lda (ZSprXV), y
            sec
            sbc #$01
            tax
            sec
            sbc #<-3
            bvc :+
            eor #$80
:           bmi :+
            txa
            sta (ZSprXV), y
:           jmp keydone
:           cmp #$CC            ; L (right)
            bne :+++
            lda (ZSprXV), y
            clc
            adc #$01
            tax
            sec
            sbc #$04
            bvc :+
            eor #$80
:           bpl :+
            txa
            sta (ZSprXV), y
:           jmp keydone
:           cmp #$CB            ; K (stop)
            bne :+
            lda #$00
            sta (ZSprXV), y
            sta (ZSprYV), y
            jmp keydone
:           cmp #$C1            ; A (ground stop)
            bne :+
            lda #$00
            sta GroundVel
            jmp keydone
:           cmp #$DA            ; Z (down, scroll ground up)
            bne :+
            lda #$01
            sta GroundVel
            jmp keydone
:           cmp #$D1            ; Q (up, scroll ground down)
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

; set up the pointers to go to the correct banks so we can just presume they are set.

setmemory:  lda #$81                    ; bank 1
            sta ZMapPtr + XByte         ; map is in bank 1 (buildmap, mapscroll)
            sta ZPtrSprA + XByte        ; sprite data are in bank 1 (artdefine, sprite)
            sta ZPtrSprB + XByte        ; sprite data are in bank 1 (artdefine, sprite)
            sta ZPtrMaskA + XByte       ; sprite masks are in bank 1 (artdefine, sprite)
            sta ZPtrMaskB + XByte       ; sprite masks are in bank 1 (artdefine, sprite)
            lda #$82                    ; bank 2
            sta ZPtrCacheA + XByte      ; background cache A (sprite)
            sta ZPtrCacheB + XByte      ; background cache B (sprite)
            lda #$8F                    ; bank 0 in $2000-A000
            sta ZPtrScrA + XByte        ; graphics A (e.g., 0000-1FFF)
            sta ZPtrScrB + XByte        ; graphics B (e.g., 2000-3FFF)
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
            lda #$07            ; number of logs (0-based), this ought to be level-dependent
            sta NumLogs
            jsr setmemory       ; set up pointer pages
            jsr splash          ; show title screen
            jsr seedRandom      ; seed the "random" number list
            jsr loadstata
            jsr buildmap        ; set up map data (in bank 1)
            jsr loadstatb
            jsr buildgfx        ; define graphics assets
            jsr spriteinit      ; initialize sprites
            ;jsr buildsfx        ; define sound effects
            lda #232            ; top map row when we start (makes bottom row 255)
            sta PgOneTop        ; top map row - page 1
            sta PgTwoTop        ; top map row - page 2
            lda #$00            
            sta PgOneOff        ; scroll offset 0 - page 1
            sta PgTwoOff        ; scroll offset 0 - page 2
            sta NeedScroll      ;
            sta ExitFlag        ; reset quit signal (detected in event loop)
            sta KeyCaught
            sta GroundVel       ; ground velocity, can be negative, zero, or positive
            lda #<D_PAGEONE     ; inline in the interrupt handler
            sta ShownPage       ; visible page, HBL uses this to know where to switch to
            bit IO_KEYCLEAR     ; clear keyboard so we notice any new keypress
            bit SS_XXN          ; start at smooth scroll offset zero
            bit SS_XNX
            bit SS_NXX
            jsr loadstatc
            jsr paintpage       ; paint whole map (onto page 2, nonvisible page)
            jsr copypage        ; copy page 2 to page 1
            jsr paintstat       ; paint status area
            jsr setupenv        ; arm interrupts
            lda #$00
            sta MoveDelay       ; game clock - setting number of VBLs per movement advance
            sta VBLTick         ; last act is to reset VBLTick
            cli                 ; all set up now, commence interrupting
            jmp eventloop       ; wait around until it is time to quit

CodeEnd     = *