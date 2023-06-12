; lazyriver
; Interrupt-related routines
;
; modes we are using and their index values above.  xC = branch table value
; 0 xF = 00 - 40 char Apple III color               text    nomix   lores
; 1 xF = 0F - Fg/bg hires (280x192, 16 colors)      text    nomix   hires
; 2 xF = 1E - super hires (560x192, b/w)            gr      mix     hires
; 3 xF = 2D - 140x192 A3 Hires (140x192, color)     text    mix     hires   scroll

; a 6502 interrupt takes minimum 7 cycles from IRQ to the first instruction
; of the handler executing.  The current instruction finishes first, and if
; it is a long one (like a 6 cycle RTS or something), if might be 13 cycles
; after the interrupt before we start executing here.
; The blanking intervals are not going to wait around for us, though.
; Lines are drawn every 65 cycles at 1MHz, 40 drawing, 25 blanking.
; The HBL interrupt operates by first setting the smooth scroll.
; This is likely to happen while a line is being painted, but mostly will
; not matter, since it is either off or hasn't changed.  Small risk of tearing.
; Then it resets the timer, should be right as drawing is finishing.
; Then is sets the mode, should be during the next HBL.
; Seems to look ok both on real hardware and in MAME.
; After that it emits the audio sample that was queued up
; (to try to get it come out at very regular intervals), and then queues
; up the next one (a process which has some wide timing variance).
; I attempted to try polling right at the end of the draw to see if I could
; wait until an HBL signal actually came in, to help sync MAME and real hardware
; and be more certain that the mode switch was happening in the blanking interval.
; But the polling (copied from Atomic Defense) did not work on real hardware,
; though it seemed to work in MAME.  Mysterious.

; This code embeds some of the global game variables within it, so the interrupt code
; never looks up the next mode, or nudge value, because the game just stores it inline.
; to speed up both mode and nudge processing, the values are multiples of $0C or $0F, used
; as a branch offset.  The TwelveBran table above can be used to look up those multiples.
; If you are storing into NudgeVal (held directly in the interrupt code), use the smooth
; scroll parameter to look up the multiple of $0C in TwelveBran.

; VBL interrupt handler
; screen timing: 65 1MHz cycles per scan line, 192 lines, should be painting for 12480 cycles.
; then 70 lines' worth of VBL.  Would be 4550 cycles at 1MHz, but we are running at 2MHz, so ~9000
; During drawing, we have a HBL interrupt every 520 cycles.  During VBL the clock jumps up, so
; we want the interrupt to be about every 1040 ($410) cycles.  We should be able to fit 7 in.

; processor goes at 1Mhz. 17030 cycles per refresh. Basically 60 refreshes per sec.
; 8-line HBL group triggers about 32 times per refresh cycle (including those during VBL)
; so we're looking at max audio sampling around 2KHz.

; VBL handler
; 29 cycles to get here, 50 cycles in here [79 total]
intvbl:     lda #$06            ;2 reset the HBL counter for switch to a3 hires mode
            sta RE_T2CL         ;4 (starts counting after VBL is over, so no rush)
            lda #$00            ;2
            sta RE_T2CH         ;4 
            sta D_NOMIX         ;4 set screen to Apple III color text mode for after VBL
            sta D_LORES         ;4
            sta D_PAGEONE       ;4 only page 1 makes sense for A3 color text
            sta D_SCROLLOFF     ;4 MAME bug requires us to turn this off for text
            ;sta D_TEXT         ; don't need to hit this switch because it does not change
            lda #$10            ;2 clear the VBL (CB1) interrupt
            sta RE_INTFLAG      ;4
            dec VBLTick         ;6 bump VBL countdown - in event loop code
            pla                 ;4
            rti                 ;6

; keyboard interrupt handler - just pass it on to the event loop
; 21 cycles to get here, 28 cycles in here [49 total]
intkey:     lda IO_KEY          ;4 load keyboard register
            sta KeyCaught       ;4 tell event loop to process this - in event loop code
            bit IO_KEYCLEAR     ;4 clear keyboard register
            lda #$01            ;2 clear the keyboard (CA2) interrupt
            sta RE_INTFLAG      ;4
            pla                 ;4
            rti                 ;6

; check for interrupts other than HBL after we have established that it is not HBL
; 12 to get here, 38 if we recognize nothing [50 total]
; 
nothbl:     lda RE_INTFLAG      ;4 check for other interrupts
            lsr                 ;2 check for keyboard ($01)
            bcs intkey          ;2/3 if yes, go handle it [after 21]
            lsr                 ;2 check for clock ($02)
            bcs intclock        ;2/3 if yes, go handle it [after 25]
            and #$04            ;2 was it VBL? ($10 but we rotated right twice already)
            bne intvbl          ;2/3 if yes, go handle it [after 29]
            pla                 ;4
            rti                 ;6 [38 if we did not recognize an interrupt]

; main interrupt handler entry point
; after VBL completes, will draw 8 lines in text mode at the top, then fire HBL.
; main task at HBL is to switch the graphics mode to a3 hires.
; keyboard sensing is also handled by interrupt, just depositing the result where
; the move routines can see it.
; sound later will be handled by a timed interrupt, running through a buffered
; if there is one.
; changing the smmoth scroll offset will be handled in the drawing code, which will
; pause for VBL so that it can do all the drawing during the blank.
; 
; - first thing is to check for HBL and handle it
; non-HBL branches to up above.
; here: HBL is going to be the split between top text mode (8 lines) and rest of a3 hires mode
; so should only be one HBL per screen refresh.

inthandle:  pha                 ;3 stash A because we need it
            lda RE_INTFLAG      ;4 identify the interrupt we got
            and #$20            ;2 [9] is it HBL?
            beq nothbl          ;2/3 branch+jump off to the rest of the interrupt handlers if not
            ; this is the HBL interrupt, and we've burned 11 cycles already, took 7 minimum to start.
            sta D_MIX           ;4 switch to a3 hires mode
            sta D_HIRES         ;4 [19]
ShownPage = *+1                 ; set this to either $54 (page 1) or $55 (page 2)
            sta D_PAGEONE       ;4 [23]            
            ;sta D_TEXT         ; no need to hit this switch because it does not change
            sta D_SCROLLON      ;4 [27] MAME bug requires this to be turned on and off
            sta RE_INTFLAG      ;4 [31] clear the HBL interrupt (A is still $20)
clockout:   pla                 ;4 [35]
            rti                 ;6 [done with HBL after 41]

; timer1 interrupt handler, to handle audio
; skipped for simplicity, maybe add in later.
; 25 cycles to get here
; for now though it won't be used because the interrupt isn't turned on.
; first thing we do is send the sound to the DAC so that it can be done very regularly
; then we compute the next sound for next time the timer fires
; sound data is in bank 1.
; to fire a sound, have ZFXPtr set to point to it, make FXPlaying nonzero,
; sound will stop when it hits a sample with the high bit set.

intclock:   lda #$02            ;2 clear the timer1 interrupt
            sta RE_INTFLAG      ;4
PlaySound = *+1                 ; play sound effects switch (user controlled)
            lda #INLINEVAR      ;2 check to see if we are supposed to be playing sound
            beq clockout        ;2/3 if we are not playing sound, head to exit [after 11, 21 until out]
FXPlaying = *+1                 ; nonzero if a sound effect is playing
            lda #INLINEVAR      ;2 [12] see if a sound effect is playing
            beq clockout        ;2/3 if we are not playing sound, head to exit [after 15, 25 until out]
SampleOut = *+1
            lda #INLINEVAR      ;2 [16] load sample we prepared earlier
            sta R_TONEHBL       ;4 send it to the DAC
            lda R_ZP            ;4 stash ZP because we will use it
            pha                 ;3
            lda #$1A            ;2 move ZP to $1A00 (so extended addressing works and for access to pointers)
            sta R_ZP            ;4
            sty IntYStash       ;3 [36]
            ldy #$00            ;2 [38]
            lda (ZFXPtr), y     ;5* [43*] load next sfx sample
            bmi haltsound       ;2/3 halt sound if we are finished [after 46*, 73* until out]
            sta SampleOut       ;4
            inc ZFXPtr          ;5 [54*, 74* until out] move to the next sample (sfx cannot exceed $100 samples)
IntYStash = *+1
timerout:   ldy #INLINEVAR      ;2 [20 cycles to get out from here]
            pla                 ;4
            sta R_ZP            ;4
            pla                 ;4
            rti                 ;6
haltsound:  sty FXPlaying       ;4 halt sfx playback
            jmp timerout        ;3

; arm interrupts and setup interrupt handler environment

setupenv:   ; save IRQ vector and then install ours
            lda IRQVECT
            sta IRQSaveA
            lda IRQVECT + 1
            sta IRQSaveB
            lda IRQVECT + 2
            sta IRQSaveC
            lda #$4C             ; jmp
            sta IRQVECT
            lda #<inthandle
            sta IRQVECT + 1
            lda #>inthandle
            sta IRQVECT + 2
            lda #$00
            sta FXPlaying       ; no sound effect currently playing
            sta PlaySound       ; no sound effects play at all
            sta ZFXPtr
            lda #$81            ; sound information in bank 1
            sta ZSoundPtr + XByte
            sta ZFXPtr + XByte
            ; TODO: set up sound and screen splitting variables if used

            ; bank register - $FFEF - E-VIA input register A
            ; ZP register - $FFD0 - D-VIA input/output register B
            
            ; set environment - $FFDF - D-VIA input register A
            ; because we will use ZP to draw, we need stack to be true most of the time
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
            
            ; D-VIA
            ; register A: environmental register (out)
            ; register B: zero page register - and RTC (out)
            ; CA1 - global slot IRQ
            ; CA2 - sw1, which I think is the open apple key
            ; CB1 - serial out, CB2 - serial in.  Usually printer.  Maybe joystick.
            
            ; E-VIA
            ; register A:
            ; [x-------] Any IRQ (in)
            ; [-x------] Closed apple key (in)
            ; [--x-----] Slot 2 IRQ (in)
            ; [---x----] Slot 1 IRQ (in)
            ; [-----xxx] Selected bank (out)
            ; register B:
            ; [--xxxxxx] Sound generator (out)
            ; [-x------] I/O count
            ; [x-------] nmi in a slot
            ; CA1 - RTC interrupt (neg edge active in)
            ; CA2 - keyboard interrupt (ind neg edge active) - sets bit 0 of IFR
            ; CB1, CB2, shift - VBL
            
            ; disable & clear all D-VIA interrupts (MSB=clear, other bits=interrupts)
            ; D-VIA Int - flag $FFDD, enable $FFDE
            ;     0------- disable
            ;     -1------ timer 1
            ;     --1----- timer 2
            ;     ---1---- CB1
            ;     ----1--- CB2
            ;     -----1-- shift register
            ;     ------1- CA1
            ;     -------1 CA2
            lda #%01111111
            sta RD_INTENAB
            sta RD_INTFLAG
            
            ; D-VIA Aux control - $FFDB
            ;     0------- T1 timer, PB7 disabled
            ;     1x------ T1 timer, PB7 one-shot output (10) or square wave output (11)
            ;     -0------ T1 timer, timed interrupt each time T1 is loaded (one-shot)
            ;     -1------ T1 timer, continueous interrupts
            ;     --0----- T2 timer, timed interrupt (1=count down with pulses on PB6)            
            ;     ---000-- Shift Reg: disabled
            ;     ---100-- Shift Reg: shift out free running at T2 rate
            ;     ---1---- Shift Reg: shift out
            ;     ---0---- Shift Reg: shift in
            ;     ----01-- Shift Reg: under control of T2
            ;     ----10-- Shift Reg: under control of O2
            ;     ----11-- Shift Reg: under control of ext clock
            ;     ------1- PB: Enable latching (0=disable)
            ;     -------1 PA: Enable latching (0=disable)
            ; D-VIA, no timers enabled            
            lda #%00000000
            sta RD_AUXCTL
            
            ; D-VIA - CA1 is any slot IRQ, CA2 is some switch?; CB1, CB2 are SCO/SER, probably joystick?
            ; CB2 - [hi nibble: 011-] independent interrupt input pos edge
            ; CB1 - [hi nibble: ---1] pos active edge
            ; CA2 - [lo nibble: 011-] independent interrupt input pos edge
            ; CA1 - [lo nibble: ---0] neg active edge
            ; high nibble here is largely irrelevant, ineterrupts CB1, CB2 not used
            ; maybe CB1, CB2 relate to joystick.
            lda #%01110110
            sta RD_PERCTL
            
            ; disable & clear certain E-VIA interrupts (MSB=clear, other bits=interrupts)
            ; not sure why only some are disabled and cleared, this is based on Atomic Defense
            ; E-VIA Int -  flag $FFED, enable $FFEE
            ;     0------- disable
            ;     -0------ timer 1
            ;     --0----- timer 2
            ;     ---0---- CB1 (VBL)
            ;     ----1--- CB2 (VBL)
            ;     -----1-- shift register (VBL)
            ;     ------1- CA1 (RTC)
            ;     -------0 CA2 (keyboard)
            lda #%00001110        ; disable & clear CB2, shift register, CA1
            sta RE_INTENAB
            sta RE_INTFLAG
            
            ; E-VIA Aux control - $FFEB
            ; [0-------] T1 timer, PB7 disabled
            ; [1x------] T1 timer, PB7 one-shot output (10) or square wave output (11)
            ; [-0------] T1 timer, timed interrupt each time T1 is loaded (one-shot)
            ; [-1------] T1 timer, continueous interrupts
            ; [--0-----] T2 timer, timed interrupt (1=count down with pulses on PB6)
            ; [---000--] Shift Reg: disabled
            ; [---100--] Shift Reg: shift out free running at T2 rate
            ; [---1----] Shift Reg: shift out
            ; [---0----] Shift Reg: shift in
            ; [----01--] Shift Reg: under control of T2
            ; [----10--] Shift Reg: under control of O2
            ; [----11--] Shift Reg: under control of ext clock
            ; [------1-] PB: Enable latching (0=disable)
            ; [-------1] PA: Enable latching (0=disable)
            ; 
            ; T1 has two latches and a 16 bit counter.  Down to zero -> interrupt
            ; one shot keeps counting, free-run resets and counts again
            ; T2 can count PB6 negatives, or run in one shot mode.
            ; Count: load number to count into T2, dec on pulses, interrupt at zero, counting continues
            ; PB6 appears to be (at least?) HBL.
            ; E-VIA: enable timer 2, one-shot.  HBL.
            ; was this
            ;lda #%00100000
            ;     00100000 - timer2 count down pulses on PB6 (HBL)
            ;     01100000 - timer1 continuous (restart when hits zero)
            lda #%11100000 ; enable timer1 and timer2
            sta RE_AUXCTL
            
            ; E-VIA - CA2 is keyboard, CA1 is clock; CB1, CB2 are VBL
            ; CB2 011- hi nibble - independent interrupt input pos edge (VBL)
            ; CB1 ---0 hi nibble - neg active edge (VBL)
            ; CA2     001- lo nibble independent interrupt input neg edge (keyboard)
            ; CA1     ---0 lo nibble neg active edge (clock)
            lda #%01100010
            ;lda #%00100010 ; neg active edge?  CB2 may be VBLx8.
            sta RE_PERCTL
            
            ; E-VIA Int Enable
            ;     1------- enable
            ;     -0------ timer 1
            ;     --1----- timer 2 (I/O count, HBL)
            ;     ---1---- CB1 (VBL)
            ;     ----0--- CB2 (VBL)
            ;     -----0-- shift register (VBL)
            ;     ------0- CA1 (clock)
            ;     -------1 CA2 (keyboard)
            ; E-VIA - enable timer2 (HBL), CB1 (VBL), CA2 (keyboard)
            lda #%10110001
            sta RE_INTENAB
            
            ; E-VIA Int Flag
            ;     0------- no function I believe?
            ;     -0------ timer 1 
            ;     --1----- timer 2 (I/O count, HBL)
            ;     ---1---- CB1 (VBL)
            ;     ----0--- CB2 (VBL)
            ;     -----0-- shift register (VBL)
            ;     ------0- CA1 (clock)
            ;     -------1 CA2 (keyboard)
            ; clear timer2, CB1, CA2
            lda #%00110001
            sta RE_INTFLAG
            ; set up the clock timer for audio -- later
            ;lda #$10            ; clock interval is $410, this is the $10 part.
            ;sta RE_T1CL         
            ;lda #%10000010      ; enable CA1 (RTC)
            ;sta RE_INTENAB
            ;lda #$04
            ;sta RE_T1CH         ; start the clock for $410 cycles
            rts
