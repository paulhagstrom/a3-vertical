; lazyriver definitions

; ZP use.  Convention: prefix addresses in ZP with Z so I can identify them
; in the code for cycle counting and addressing mode options.
; collecting the definitions in here in order to avoid inadvertent collisions

; overall memory map:
; $400-427      text of scoreline at the top
; $2000-9FFF    graphics buffers, page 1A, 1B, 2A, 2B (bank 0)
; $A000->       game code
; bank 1:
; $0000-13FF    map data (tiles, flow) (256 rows, 20 columns)
; $1400-14FF    tile graphic data (8 tiles, 8 lines)
; $1500-7EFF    sprite graphic data (2 frames, 7 shifts, 8 lines, 15 sprites, $700 per)
; bank 2:
; $300-AFF      sprite tracking variables, groups of $80
; $1000-4FFF    sprite background caches ($1000 page 1, $3000 page 2)
; $5000-7EFF    sound effect data ($100 per effect) (planned)

XByte       = $1601     ; Interp extended address offset (for ZP 1A)
;Zero1A      = $1A00     ; for those times you want to write to ZP without using ZP
INLINEVAR   = $33       ; mnemonic to remind me where variable is inline in code
INLINEADDR  = $2020     ; mnemonic to remind me where variable is inline in code
;Zero        = $00

; Used in interpreter (1A00) ZP.

ZTileCache  = $00   ; $14 bytes of tile cache, used for buffering display
ZTileOff    = $14   ; offset into tile graphics, used while drawing a line
ZMapX       = $15   ; current column in the map being drawn (when drawing a line)
ZCurrScrL   = $16   ; current screen line (used in paintmap)
ZCurrOff    = $17   ; current offset into tile (used in paintmap)
ZCurrMapL   = $18   ; current map line (used in paintmap)
ZLinesLeft  = $19   ; lines remaining to draw (used in paintmap)

ZPgIndex    = $1A   ; nondisplayed page (0 if page 1 nondisplayed, or 1, used in sprites)
ZScrOffset  = $1B   ; PgOneOff for drawing page (used in sprites)
ZScrX       = $1C   ; byte to draw sprite on, twice x-coordinate (sprites)
ZScrTop     = $1D   ; map row of the top line of the display (sprites)
;ZSpriteOff  = $1A   ; used in sprites, difference between sprite and page offset
;ZMapTemp    = $1B   ; temporary storage for a map byte being tested for collision
;ZXXTemp     = $1C   ; temporary storage for second segment x-coordinate
;ZFrame      = $1D   ; temporary storage for current animation frame

; scratch pointers
ZPtrA       = $1E
ZPtrSprA    = $20
ZPtrSprB    = $22
ZPtrMaskA   = $24
ZPtrMaskB   = $26
ZPtrScrA    = $28
ZPtrScrB    = $2A
ZPtrCacheA  = $2C
ZPtrCacheB  = $2E

; sprite variables
ZSprX       = $30   ; pointer to sprite X coordinates data
ZSprY       = $32   ; pointer to sprite Y coordinates data
ZSprXOff    = $34   ; pointer to sprite X coordinates offset data
ZSprYOff    = $36   ; pointer to sprite Y coordinates offset data
ZSprXV      = $38   ; pointer to sprite X velocity data
ZSprYV      = $3A   ; pointer to sprite Y velocity data
ZSprType    = $3C   ; pointer to sprite type data
ZSprTick    = $3E   ; pointer to sprite tick data
ZSprAnim    = $40   ; pointer to sprite current frame data
ZSprPeriod  = $42   ; pointer to sprite tick period data
ZSprDrXOne  = $44   ; pointer to X byte sprite drawn in (or minus if not drawn)
ZSprDrYOne  = $46   ; pointer to Y raster sprite drawn in
ZSprDrXTwo  = $48   ; pointer to X byte sprite drawn in (or minus if not drawn)
ZSprDrYTwo  = $4A   ; pointer to Y raster sprite drawn in
ZSprBgL     = $4C   ; pointer to low byte of pointer to sprite cached background
ZSprBgH     = $4E   ; pointer to high byte of pointer to sprite cached background
ZSprSprH    = $50   ; pointer to high byte of sprite data (correlates with ZSprType)

; pointers used during movement processing
;ZOldPtr     = $50   ; original position of element moving
;ZNewPtr     = $52   ; new position of element moving
; variables used during movement processing
ZMapTmp     = $55   ; map byte
ZCurrSpr    = $56   ; current sprite being processed
ZYFlow      = $57   ; map y flow
ZXFlow      = $58   ; map x flow

ZOldX       = $59   ; original premovement X
ZOldY       = $5A   ; original premovement Y
ZNewX       = $5B   ; postmovement X
ZNewXOff    = $5C   ; postmovement X offset
ZNewY       = $5D   ; postmovement Y
ZNewYOff    = $5E   ; postmovement Y offset
;ZIsHero     = $68   ; nonzero if the hero is being processed, 0 otherwise

;ZTopMapOff  = $6F   ; hires map parm: map offset back from HeroY for new line
ZWidth      = $60   ; river width (used in buildmap)
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
