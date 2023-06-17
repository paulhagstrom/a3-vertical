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
; int(y/8) + (y+offset)%8
;
; To draw the sprite itself, we need to compute the targets and then move
; the data.  The data is 64 bytes, and we want to load the background, stash
; it somewhere, apply a mask, draw, and then store.  That's minimally
; in the ballpark of 1280 cycles per sprite, probably will be somewhat
; more.

; when we draw a sprite, we will have its raster y-coordinate
; and an x-coordinate (0-133, it can't go off the right edge)
; we iterate line from 0 to 7 for the 8 lines of the sprite
; meaning we are aiming to draw on raster y+line
; int((y+line)/8) + (y+line+offset)%8
; horizontally, we use two tile-widths to hold the sprite
; we draw at 0-1 if sprite starts between 0 and 6
; we draw at 1-2 if sprite starts between 7 and 13, etc.
; we have seven versions of those two bytes, with the sprite
; starting at each possible position (e.g., between 0 and 6)
; the last one is 19-20 if sprite starts between 127 and 133.

; TODO - later, try to anchor the sprite to the map, but for
; now just put it on the screen in screen coordinates.

; when we draw a sprite, we need to retrieve the background
; and store it somewhere (64 bytes), AND it with the mask,
; OR it with the graphics and store it on the page.
; and to erase, store the cached background back to the page.
; so we need 64 bytes per visible sprite for background cache.
; we can use $428-7FF and $828-BFF (text pages)
; 440 480 4C0 500 540 580 5C0 600 640 680 6C0 700 740 780 7C0
; so that is 15 active sprites per text page, we can do 30
; relatively easily.  Probably having that many will take too
; long to draw though.
; but, yes, cache background for sprite n in:
; $440 + (n * $40), up to an n of 15.
; or up to an n of 30 if we just ensure there is no n=16.

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
            