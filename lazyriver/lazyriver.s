; lazyriver
; Paul Hagstrom, 2023

            .segment "CODE"
            .setcpu "6502"

            .include "zpdef.s"
            .org     $9C00 - 14
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
            .include "buildsound.s"
            .include "gamemove.s"           ; handle game movement
            .include "collision.s"          ; handle sprite collisions
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
            ; now we have flipped, start building the nonvisible page
GroundJmp = *+1
            lda #INLINEVAR                  ; neg (map dec) if ground jumps down, pos (map inc) if up
            beq :+                          ; branch away if ground is not jumping
            ; we are jumping
            tax                             ; stash jump direction in X
            jsr jumpscroll                  ; jump and repaint page (uses direction in X)
            lda #$00
            sta GroundJmp                   ; reset GroundJmp
            jmp evjumped
            ; we are not jumping but might still be syncing to a jump
:           jsr syncjump                    ; check if we need to sync to a jump
            bcs :+                          ; branch away if we didn't need to sync to a jump
evjumped:   jsr byesprites                  ; forget we drew sprites on the nonvisible page
            lda #$00
            sta VBLTick                     ; reset VBLTick so we don't slow down due to jumping
            beq evmove                      ; branch always
            ; not jumping and not syncing to a jump, so erase sprites, sync background, scroll if needed
:           jsr clrsprites                  ; erase sprites on (newly) nonvisible page
            jsr syncscroll                  ; sync ground scroll on nonvis page with vis page
            jsr fixscroll                   ; scroll ground on nonvisible page if needed
evmove:     jsr domove                      ; do movement processing
            jsr drawstatus                  ; draw score
            jsr setsprites                  ; draw sprites on nonvisible page
            jsr gndtrack                    ; adjust groundspeed based on player position
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
:           cmp #$D3            ; S (sound)
            bne :+
            lda PlaySound
            eor #$01
            sta PlaySound
            jmp keydone
:           cmp #$D8            ; X (exit)
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
            sta ZPtrSprA + XByte        ; sprite data are in bank 1 (artdefine, sprite)
            sta ZPtrSprB + XByte        ; sprite data are in bank 1 (artdefine, sprite)
            sta ZPtrMaskA + XByte       ; sprite masks are in bank 1 (artdefine, sprite)
            sta ZPtrMaskB + XByte       ; sprite masks are in bank 1 (artdefine, sprite)
            lda #$80                    ; map is in bank 1 but must be accessed from bank 0
            sta ZMapPtr + XByte         ; map is in bank 1 (buildmap, mapscroll)
            lda #$82                    ; bank 2
            sta ZPtrCacheA + XByte      ; background cache A (sprite)
            sta ZPtrCacheB + XByte      ; background cache B (sprite)
            sta ZFXPtr + XByte          ; sound effect playing pointer
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
            jsr buildsfx        ; define sound effects
            lda #$01
            sta PlaySound       ; default to sound on
            lda #232            ; top map row when we start (makes bottom row 255)
            sta PgOneTop        ; top map row - page 1
            sta PgTwoTop        ; top map row - page 2
            lda #$00            
            sta PgOneOff        ; scroll offset 0 - page 1
            sta PgTwoOff        ; scroll offset 0 - page 2
            sta ExitFlag        ; reset quit signal (detected in event loop)
            sta KeyCaught
            sta GroundVel       ; ground velocity, can be negative, zero, or positive
            sta GroundJmp       ; ground velocity, can be negative, zero, or positive
            lda #<D_PAGEONE     ; inline in the interrupt handler
            sta ShownPage       ; visible page, HBL uses this to know where to switch to
            bit IO_KEYCLEAR     ; clear keyboard so we notice any new keypress
            bit SS_XXN          ; start at smooth scroll offset zero
            bit SS_XNX
            bit SS_NXX
            jsr loadstatc
            ldy #$01            ; point to nonvisible page (2) for initial painting
            jsr paintpage       ; paint whole map
            jsr copypage        ; copy page 2 to page 1
            jsr paintstat       ; paint status area
            jsr setupenv        ; arm interrupts
            lda #$05            ; real hardware settles to MoveDelay of 5, so start there
            sta MoveDelay       ; game clock - setting number of VBLs per movement advance
            lda #$00
            sta FXPlaying       ; no sound effect currently playing
            sta VBLTick         ; last act is to reset VBLTick
            cli                 ; all set up now, commence interrupting
            jmp eventloop       ; wait around until it is time to quit

CodeEnd     = *