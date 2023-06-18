; lazyriver definitions

; ZP use.  Convention: prefix addresses in ZP with Z so I can identify them
; in the code for cycle counting and addressing mode options.
; collecting the definitions in here in order to avoid inadvertent collisions

XByte       = $1601     ; Interp extended address offset (for ZP 1A)
Zero1A      = $1A00     ; for those times you want to write to ZP without using ZP
INLINEVAR   = $33       ; mnemonic to remind me where variable is inline in code
INLINEADDR  = $2020     ; mnemonic to remind me where variable is inline in code
Zero        = $00

; Used in interpreter (1A00) ZP.

ZTileCache  = $00   ; $14 bytes of tile cache, used for buffering display
ZTileOff    = $14   ; offset into tile graphics, used while drawing a line
ZMapX       = $15   ; current column in the map being drawn (when drawing a line)
ZCurrScrL   = $16   ; current screen line (used in paintmap)
ZCurrOff    = $17   ; current offset into tile (used in paintmap)
ZCurrMapL   = $18   ; current map line (used in paintmap)
ZLinesLeft  = $19   ; lines remaining to draw (used in paintmap)

; scratch pointers
ZPtrA       = $20
ZPtrB       = $22
ZPtrC       = $24
ZPtrD       = $26
ZPtrE       = $28
ZPtrF       = $2A
ZPtrG       = $2C
ZPtrH       = $2E

; game state variables
; so far not really used
ZLogX       = $30   ; pointer to log X coordinates data
ZLogY       = $32   ; pointer to log Y coordinates data
ZLogXV      = $34   ; pointer to log X velocity data
ZLogYV      = $36   ; pointer to log Y velocity data
ZLogType    = $38   ; pointer to log type data
ZLogTick    = $3A   ; pointer to log tick data
ZLogAnim    = $3C   ; pointer to log current frame data
ZLogPeriod  = $3E   ; pointer to log tick period data
ZLogSp      = $40   ; pointer to log speed, maybe redundant. Ticks per move.

ZSpriteOff  = $4C   ; used in sprites, difference between sprite and page offset

ZMapTemp    = $4D   ; temporary storage for a map byte being tested for collision
ZXXTemp     = $4E   ; temporary storage for second segment x-coordinate
ZFrame      = $4F   ; temporary storage for current animation frame

; pointers used during movement processing
ZOldPtr     = $50   ; original position of element moving
ZNewPtr     = $52   ; new position of element moving
ZTailPtr    = $54   ; original position of second segment of element moving (hoarder)
; variables used during movement processing
ZCurrLog    = $56   ; current log being processed

ZOldX       = $59   ; original premovement X
ZOldY       = $5A   ; original premovement Y
ZNewX       = $5B   ; postmomvement X
ZNewY       = $5C   ; postmomvement X
ZVelX       = $5D   ; velocity X
ZVelY       = $5E   ; velocity Y
ZTargX      = $5F   ; target X
ZTargY      = $60   ; target Y
ZTargD      = $61   ; target distance
ZTargV      = $62   ; target value
ZTargDX     = $63   ; target vector X
ZTargDY     = $64   ; target vector Y
ZTargDTemp  = $65   ; target distance temporary variable
ZTargDXTemp = $66   ; target vector X temporary variable
ZTargDYTemp = $67   ; target vector Y temporary variable
ZIsHero     = $68   ; nonzero if the hero is being processed, 0 otherwise

ZTopMapOff  = $6F   ; hires map parm: map offset back from HeroY for new line

ZMapPtr     = $76

; interrupt handler
ZSoundPtr   = $D2   ; current sample in background music
ZFXPtr      = $D4   ; current sample in sound effect

; staging for hires3 segment bytes
ZPixByteA   = $D6
ZPixByteB   = $D7
ZPixByteC   = $D8
ZPixByteD   = $D9
ZPixByteE   = $DA
ZPixByteF   = $DB
ZPixByteG   = $DC
ZPixByteH   = $DD

ZPixByteI   = $DE
ZPixByteJ   = $DF
ZPixByteK   = $E0
ZPixByteL   = $E1
ZPixByteM   = $E2
ZPixByteN   = $E3
ZPixByteO   = $E4
ZPixByteP   = $E5

ZSprLeft    = $E8
ZShiftsLeft = $E9
ZSprLine    = $EA
ZPageBase   = $EB
ZCacheBase  = $EC
ZSprLnsLeft = $ED

; Addresses are used when ZP is pointed at a display buffer
; these are not in conflict with addresses in the 1A00 ZP,
; generally limited to screen holes: 78-7F, F8-FF

ZNumPtr     = $7D   ; pointer for screen target for drawnumber
ZPxScratch  = $FF

; I/O softswitches

IO_KEY      = $C000     ; keyboard
IO_KEYFLAG  = $C008     ; keyboard flag
IO_KEYCLEAR = $C010     ; keyboard strobe
IO_CLOCK    = $C070     ; real time clock

; graphics soft switches

D_GRAPHICS  = $C050     ; "clear text mode"
D_TEXT      = $C051     ; set text mode
D_NOMIX     = $C052     ; "clear mix mode"
D_MIX       = $C053     ; "set mix mode"
D_PAGEONE   = $C054     ; "clear PG2 mode"
D_PAGETWO   = $C055     ; PG2 mode
D_LORES     = $C056     ; "clear hires mode"
D_HIRES     = $C057     ; set hires mode
SS_XXN      = $C0E0     ; smooth scroll bit 0 off
SS_XXY      = $C0E1     ; smooth scroll bit 0 on
SS_XNX      = $C0E2     ; smooth scroll bit 1 off
SS_XYX      = $C0E3     ; smooth scroll bit 1 on
SS_NXX      = $C0E4     ; smooth scroll bit 2 off
SS_YXX      = $C0E5     ; smooth scroll bit 2 on

D_SCROLLOFF = $C0D8     ; disable smooth scroll
D_SCROLLON  = $C0D9     ; enable smooth scroll (vertical shift)

F_XFERON    = $C0DB     ; turn on font transfer
F_XFEROFF   = $C0DA     ; turn off font transfer

; VIA registers

R_TONEHBL   = $FFE0     ; tone (bits 0-5), I/O count (6), slot nmi (7)
RE_T1CL     = $FFE6     ; ffex timer1 count low byte
RE_T1CH     = $FFE7     ; ffex timer1 count high byte
RE_T2CL     = $FFE8     ; ffex timer2 count low byte
RE_T2CH     = $FFE9     ; ffex timer2 count high byte
RD_AUXCTL   = $FFDB     ; ffdx auxiliary control register
RE_AUXCTL   = $FFEB     ; ffex auxiliary control register
RD_PERCTL   = $FFDC     ; ffdx peripheral control register
RE_PERCTL   = $FFEC     ; ffex peripheral control register
RD_INTFLAG  = $FFDD     ; ffdx interrupt flag register
RE_INTFLAG  = $FFED     ; ffex interrupt flag register
    ; bit 7=IRQ, 6=timer 1, 5=timer 2, 4=CB1 (vblx1), 3=CB2 (vblx8), 2=shift, 1=CA1, 0=kbd
RD_INTENAB  = $FFDE     ; ffdx interrupt enable register
RE_INTENAB  = $FFEE     ; ffex interrupt enable register

R_ENVIRON   = $FFDF     ; Environment register
R_BANK      = $FFEF     ; Bank register
R_ZP        = $FFD0     ; zero page register
IRQVECT     = $FFCD     ; Monitor IRQ points here

; SOS Calls

TERMINATE   = $65
;REQUEST_SEG = $40
;RELEASE_SEG = $45
;OPEN        = $C8
;READ        = $CA
;CLOSE       = $CC
