; lazyriver
; sprite routines

; A sprite will be a 7x8 block, which corresponds to 32 bytes.  But given that
; it can be at any x-coordinate, we need to consider it to be a 14x8 block,
; 64 bytes.  We will pre-shift them so that we have these bytes ready to go.
; To put a sprite at an absolute y-coordinate on the screen requires taking
; the smooth scroll parameter into account.  If scroll is set to 2 and we
; want to put it on lines 5-C, then we have to start drawing on line 7.
;
; sprite line 0 - display line 5 - adjusted line 7
; sprite line 1 - display line 6 - adjusted line 0
; sprite line 2 - display line 7 - adjusted line 1
; sprite line 3 - display line 8 - adjusted line A
; sprite line 4 - display line 9 - adjusted line B
; sprite line 5 - display line A - adjusted line C
; sprite line 6 - display line B - adjusted line D
; sprite line 7 - display line C - adjusted line E
;
; That is, to draw on display line y, we draw to
; int(y/8) + (y+offset)%7
;
; To draw the sprite itself, we need to compute the targets and then move
; the data.  The data is 64 bytes, and we want to load the background, stash
; it somewhere, apply a mask, draw, and then store.  That's minimally
; in the ballpark of 1280 cycles per sprite, probably will be somewhat
; more.

; PgOneTop - map tile line visible in the top line of the screen
; sprite Y - map tile line contining the first line of the sprite
; PgOneOff - scroll value of the page
; sprite Y off - lines into the tile we start drawing the sprite
; So the math is a little bit complicated, even if PgOneTop and sprite Y
; match, the top of the sprite might not be on the screen, if PgOneOff is
; more than sprite Y off.
; raster line of the top of the sprite is:
; 8 + (sprite Y - PgOneTop)*8 + (sprite Y off - PgOneOff)
; sprite extends to that + 7
; 
; YOU ARE HERE - 
; Was trying to set this up so the logs could float around in some
; stable place on the map.  But for getting sprites to work, it might
; be simpler to just have a sprite at an absolute screen coordinate,
; and just work out the raster line and allow it to move within the
; screen bounds, with the background just a kind of decoration.
; once that is work, I could try to check collistions with the map
; and anchor things to map coordinates?

; find raster line of sprite
; come in with A = map Y coordinate, Y = offset

sprraster:  sec                 ; A has map tile coordinate on entry
            sbc PgOneTop, x
            bcs sprrb
            ; map tile coordinate is above the screen top
            cmp #<-1
            bcc sproff
            ; map tile coordinate is just above the top, maybe partly on screen
            ; TODO - do something rational here
sprrb:
            asl
            asl
            asl                 ; x8
            sta ZSpriteOff      ; save for later adding
            tya                 ; offset comes in in Y
            sec
            sbc PgOneOff, x
            ; 
            adc ZSpriteOff
            adc #$08
            rts
sproff:     ; sprite is off screen
            rts

; draw sprites on nonvisible page

PgIndex:    .byte 0             ; 0 if drawing on page 1, 1 if drawing on page 2

setsprites:
            lda ShownPage       
            eor #$01            ; switch focus to nonvisible page
            and #$01            ; 0 if page 1 is nonvisible, 1 if page 2 is nonvisible
            tax
            stx PgIndex
            
            ; draw player sprite
            ; player is on the map at PlayerX, PlayerY,
            ; offset by PlayerXOff, PlayerYOff
            
            lda PlayerY
            ldy PlayerYOff
            ;jsr sprraster
            
            rts

; erase sprites on nonvisible page

clrsprites:
            rts
            