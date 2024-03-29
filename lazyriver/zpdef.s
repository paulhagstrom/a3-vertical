; lazyriver definitions

; ZP use.  Convention: prefix addresses in ZP with Z so I can identify them
; in the code for cycle counting and addressing mode options.
; collecting the definitions in here in order to avoid inadvertent collisions

; overall memory map:
; $400-427      text for scoreline at the top
; $500-5FF      collision list, higher numbered sprite
; $600-6FF      collision list, lower numbered sprite
; $800-827      colors for scoreline at the top
; $2000-9FFF    graphics buffers, page 1A, 1B, 2A, 2B (bank 0)
; $A000->       game code
; bank 1:
; $0000-13FF    map data (tiles, flow) (256 rows, 20 columns)
; $1400-14FF    tile graphic data (8 tiles, 8 lines)
; $1500-58FF    sprite graphic data (2 frames, 7 shifts, 8 lines, 10 sprites, $700 per)
; $5900-76FF    sprite collision maps ($380 per)
; bank 2:
; $300-F7F      sprite tracking variables, groups of $80
; $1000-4FFF    sprite background caches ($1000 page 1, $3000 page 2)
; $5000-7EFF    sound effect data ($100 per effect) (planned)

XByte       = $1601     ; Interp extended address offset (for ZP 1A)
INLINEVAR   = $33       ; mnemonic to remind me where variable is inline in code
INLINEADDR  = $2020     ; mnemonic to remind me where variable is inline in code

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
ZSprTick    = $3E   ; pointer to sprite animation countdown tick data
ZSprAnim    = $40   ; pointer to sprite current frame data
ZSprPeriod  = $42   ; pointer to sprite animation period data
ZSprDrXOne  = $44   ; pointer to X byte sprite drawn in on page 1 (or minus if not drawn)
ZSprDrYOne  = $46   ; pointer to Y raster sprite drawn in on page 1
ZSprDrXTwo  = $48   ; pointer to X byte sprite drawn in on page 2 (or minus if not drawn)
ZSprDrYTwo  = $4A   ; pointer to Y raster sprite drawn in on page 2
ZSprBgL     = $4C   ; pointer to low byte of pointer to sprite cached background
ZSprBgH     = $4E   ; pointer to high byte of pointer to sprite cached background
ZSprSprH    = $50   ; pointer to high byte of sprite data (correlates with ZSprType)
ZSprDelay   = $52   ; pointer to sprite frame delay between movements (fractional velocity)
ZSprMvTick  = $54   ; pointer to countdown to next movement (fractional velocity)
ZPrevX      = $56   ; pointer to sprite X pre-movement (if we need to revert it)
ZPrevY      = $58   ; pointer to sprite Y pre-movement (if we need to revert it)
ZPrevXOff   = $5A   ; pointer to sprite X offset pre-movement (if we need to revert it)
ZPrevYOff   = $5C   ; pointer to sprite Y offset pre-movement (if we need to revert it)
ZSprCollH   = $5E   ; pointer to high byte of sprite collision mask
ZSprCollL   = $60   ; pointer to low byte of sprite collision mask
ZSprColRev  = $62   ; pointer to collision checked / sprite reverted flags

; variables used during movement processing
ZCollODiff  = $64   ; collision check offset difference
ZCollRDiff  = $65   ; collision check row difference
ZCollChkA   = $66   ; collision check scratch A
ZCollChkB   = $67   ; collision check scratch B
ZCurrYStart = $68   ; current sprite y comparison start
ZRefYStart  = $69   ; reference sprite y comparison start
ZOldYLine   = $6A   ; reference sprite y line
ZOldYOff    = $6B   ; reference sprite offset
ZCurrXStart = $6C   ; current sprite - x column comparison start
ZRefXStart  = $6D   ; reference sprite - x column comparison start
ZCollStash  = $6E   ; collision detect stash byte
ZOverXLeft  = $6F   ; overlap window left
ZOverXRight = $70   ; overlap window right
ZOverYTop   = $71   ; overlap window top
ZOverYBot   = $72   ; overlap window bottom
ZRefSpr     = $74   ; reference sprite in a pair, testing collisions
ZMapTmp     = $75   ; map byte
ZCurrSpr    = $76   ; current sprite being processed
ZYFlow      = $77   ; map y flow
ZXFlow      = $78   ; map x flow
ZOldX       = $79   ; original premovement X
ZOldY       = $7A   ; original premovement Y
ZNewX       = $7B   ; postmovement X
ZNewXOff    = $7C   ; postmovement X offset
ZNewY       = $7D   ; postmovement Y
ZNewYOff    = $7E   ; postmovement Y offset
ZBytesLeft  = $7F   ; countdown of bytes left in collision detection

ZRastCache  = $80   ; 8 bytes of adjusted y coordinates for drawing sprites

ZWidth      = $88   ; river width (used in buildmap)
ZNumColl    = $89   ; collision count (length of collision list)

ZMapPtr     = $8A
ZNumPtr     = $8C   ; pointer for screen target for drawnumber

; interrupt handler
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

ZPxScratch  = $Fd
ZPxScratchX = $FF
ZPxScratchY = $FF

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
