.target "6502"
.org $1000
.setting "OutputTxtAddressFormatFirst" = "{0:x04}: "
.setting "OutputTxtAddressFormatNext" = "{0:x04}: "

//**************************************************************************************************************************************************
//
//                        HI-RES PROCEDURE LABELS
//
//**************************************************************************************************************************************************
    
    hgr     .equ    $F3E2
    hclr    .equ    $F3F2       ; Clears current screen to black1
    bkgnd   .equ    $F3F6       ; Clears current screen to last plotted HCOLOR
    hcolor  .equ    $F6F0       ; Set HCOLOR to contents of x register
    hposn   .equ    $F411       ; Position hi-res cursor without plotting. Enter
                                ; with x, y (low, high) = horizontal position.
                                ; accumulator = vertical position.
    hplot   .equ    $F457       ; Identical to HPOSN, but plots current HCOLOR at coordinates given.
    hfind   .equ    $F5CB       ; Returns current cursor position. Useful after a DRAW to find were
                                ; you've been left. Coordinates returned in: $E0, $E1 = horizontal (low, high), $E2 = veritcal
    hlin    .equ    $F53A       ; Draws a line from last plot to point given.
                                ; accumulator, x (low, high) = horizontal. y = vertical position.
    shnum   .equ    $F730       ; Puts address of shape number indicated by x-register
                                ; into $1A, $1B: returns with x,y (low, high) also set to
                                ; address of that shape-table entry.
    draw    .equ    $F601       ; Draw shape pointed to by x, y (low, high) in current
                                ; HCOLOR. Note: x, y point to specific entry, not the
                                ; beginning of the table. Call shnum first.
    xdraw   .equ    $F65D       ; Erases shape just drawn (if there) by doing an exclusive
                                ; OR with the screen data. Load x, y (low, high) with
                                ; address of shape to xdraw or call shnum first with x-register = shape number.

//**************************************************************************************************************************************************
//
//                        GENERAL PROCEDURE LABELS
//
//**************************************************************************************************************************************************
    
    home        .equ    $FC58   ; clear screen
    keyboard    .equ    $C000   ; look for keystroke
    keystrobe   .equ    $C010   ; clear keystroke
    saveReg     .equ    $FF4A   ; dump registers to zero page
    dTable      .equ    $0390   ; starting location for general data table

//**************************************************************************************************************************************************
//
//                        ZERO-PAGE POINTERS
//
//**************************************************************************************************************************************************
    
    xv          .equ    $06     ; (1 BYTE) x-velocity (bit 8 determines direction pos or neg. bits 1-7 determine speed)
                                ; default is 1 (0000 0001) indicating positive direction at 1 dot per move
    yv          .equ    $07     ; (1 BYTE) y-velocity (bit 8 determines direction pos or neg. bits 1-7 determine speed)
                                ; default is 1 (0000 0001) indicating positive direction at 1 dot per move
    cx          .equ    $08     ; (2 BYTES) current x-position (low byte)
                                ; $09 will contain (high byte)
    cy          .equ    $19     ; (1 BYTE) current y-position
    ox          .equ    $FB     ; (2 BYTES) old x-position (used for erasing old screen location)
    oy          .equ    $FD     ; (1 BYTE) old y-position (used for erasing old screen location)
    vtemp       .equ    $FA     ; (1 BYTE) for holding negative first 7 bits of negative velocity
    delaySeed   .equ    $E3     ; (1 BYTE) controls the speed of the main loop
    addrPtr1    .equ    $EB     ; (2 BYTE) shared temp pointer for indirect indexing of addresses
    addrPtr2    .equ    $ED     ; (2 BYTE) shared temp pointer for indirect indexing of addresses
    OneBytePtr1 .equ    $EF     ; (1 BYTE) shared temp pointer for fast storage
    OneBytePtr2 .equ    $FE     ; (1 BYTE) shared temp pointer for fast storage
    OneBytePtr3 .equ    $FF     ; (1 BYTE) shared temp pointer for fast storage
    DebugByte   .equ    $1E     ; (1 BYTE) $10 or $01. If $01, go into step-by-step debug mode. Toggled with Cntrl-D

//**************************************************************************************************************************************************
//
//                        dTABLE INDEXES
//
//**************************************************************************************************************************************************
    
    SDtblLoAddrIndex    .equ    00   ; dTable offset to absolute address of scrata string start (LO BYTE)-> DEFAULT = $0300
    SDtblHiAddrIndex    .var    01   ; dTable offset to absolute address of scrata string start (HI BYTE)-> DEFAULT = $0300    

//**************************************************************************************************************************************************
//
//                        SCREEN AND VARIABLE SETUP
//
//**************************************************************************************************************************************************
    
    SETUP       jsr     INITMAIN        ; set up variables and screen
                jsr     SCRATAPRINT


//**************************************************************************************************************************************************
//
//                        MAIN PROGRAM LOOP
//
//**************************************************************************************************************************************************
    
    MAINLOOP    jsr     PLOT            ; plot the dot
                jsr     KEYLISTEN       ; listen for keypress
                lda     delaySeed       ; delay seed
                jsr     $FCA8           ; wait
                jsr     MOVEX           ; check and set x posn accordingly
                jsr     XCOLLISIONCHK   ; check for collision with left/right border
                jsr     MOVEY           ; check and set y posn accordingly
                jsr     ERASE           ; unplot the dot
                jmp     MAINLOOP        ; rinse and repeat


//**************************************************************************************************************************************************
//                                  8-BIT BREAK UP - MAIN CODE
//**************************************************************************************************************************************************

INITMAIN            jsr     home            ; clear the text screen
                    lda     #$50            ; initial speed for main loop delay
                    sta     delaySeed       ;
                    lda     #$10
                    sta     DebugByte       ; default debug state is off
                    /*
                    INITVELOCITY: set x and y velocities
                    */
    initVelocity    lda     #$01            ; using same velocities for both vectors
                    sta     xv              ; store 1 for x velocity
                    sta     yv              ; store 1 for y velocity
                    /*
                    INITCURSOR: store cx and cy starting coordinates
                    */
                    ldx     #$8C            ; (low byte of x position) this is decimal 140, or the center of x
                    stx     cx              ; store low byte of x posn
                    ldy     #$00            ; (high byte of x position)
                    sty     cx+1            ; store high byte of x posn
                    lda     #$50            ; this is decimal 80, or the center of y
                    sta     cy              ; store y posn
                    jsr     hgr             ; initialize the hi-res screen
    endInitMain     rts

                    //**************************************************************
                    //                  DEBUG DASHBOARD SETUP                      *
                    //**************************************************************

SCRATAPRINT         ldx     #SDtblLoAddrIndex   ; index to dTable location.
                    lda     dTable,X            ; retrieve lo-byte for scrata table address.
                    sta     addrPtr1            ; store scrata table address lo-byte in ZP temp pointer.
                    ldx     #SDtblHiAddrIndex   ; index to dTable location.
                    lda     dTable,X            ; retrieve hi-byte for scrata table address.
                    sta     addrPtr1+1          ; store scrata table address hi-byte in ZP temp pointer.
                    ldy     #$00                ; Init Y to start reading from Scrata Table.
                    lda     (addrPtr1),Y        ; get 1st byte from scrata table (termination character)
                    sta     OneBytePtr1         ; store termination character to ZP.
                    jsr     home                ; clear screen.

ReadChar            jsr     GetScrnLoc          ; routine will get screen location from scrata.
                    lda     #$00                ; initial scrnOffset is $00.
                    sta     OneBytePtr3         ; store scrnOffset to ZP.
    getChar         ldy     OneBytePtr2         ; get dataOffset.
                    lda     (addrPtr1),Y        ; get next char from scrata table.
    checkZero       beq     ReadChar            ; found $00 terminator for string sequence?
    checkTerm       cmp     OneBytePtr1         ; found termination byte for scrata table?
                    bne     PrintChar           ; print ascii byte on screen.
    endScrata       rts                         ; end SCRATAPRINT and return to callee.

GetScrnLoc          iny                         ; increment y index to point to next byte in scrata table.
    getLowByte      lda     (addrPtr1),Y        ; get screen location low byte.
                    sta     addrPtr2            ; store scrnLoc lo-byte to zero-page.
                    iny                         ; inc offset to next mem location in data.
    getHighByte     lda     (addrPtr1),Y        ; get screen location high byte.
                    sta     addrPtr2+1          ; store scrnLoc hi-byte to zero page.
                    iny                         ; inc offset to next mem location in data.
                    sty     OneBytePtr2         ; store new offset in ZP for callee.
    endGetScrnLoc   rts                         ; new screen location saved. Return to callee.

PrintChar           ldy     OneBytePtr3         ; load Y Register with scrnOffset
                    sta     (addrPtr2),Y        ; print char at scrnLoc + Y (on screen location)
                    inc     OneBytePtr3         ; advance screen offset
                    inc     OneBytePtr2         ; advance data offset
    resume          jmp     getChar             ; done. get next char

//**************************************************************************************************************************************************

PLOT                ldx     #$03            ; WHITE = 3
                    jsr     hcolor          ; set the color to white (contents of x-register)
                    ldx     cx              ; grab low byte of x
                    stx     ox
                    ldy     cx+1            ; grab high byte of x  
                    sty     ox+1   
                    lda     cy              ; grab y
                    sta     oy
                    jsr     hplot           ; plot the dot
    endPlot         rts

//**************************************************************************************************************************************************

KEYLISTEN           lda     keyboard        ; key press ?
                    cmp     #$80            ; is it a real key?
                    bcc     endKeyListen    ; nope! keep running.
                    sta     keystrobe       ; clear the keyboard buffer.
    minusKey        cmp     #$AD            ; '-' key pressed ?
                    bne     plusKey         ; nope. did we press '+'
                    inc     delaySeed       ; speed things up a bit!
    plusKey         cmp     #$AB            ; '+' key pressed ?
                    bne     debug           ; nope. did we press Cntl-D ?
                    dec     delaySeed       ; slow things down a bit!
    debug           cmp     #$84            ; 'Cntl-D' pressed ?
                    bne     endKeyListen    ; nope. go to end of routine.
                    lda     DebugByte       ; get debug byte from ZP.
                    eor     #$11            ; if $10, make it $01 and vice-versa.
    endKeyListen    rts                     ; done. return to main loop.


//**************************************************************************************************************************************************
/*
This subroutine checks velocity and current x posn and then increments
or decrements the x posn accordingly
*/
MOVEX               lda     xv              ; this is our bit mask to compare to xv
                    bpl     incX            ; traveling in positive direction.incX
                        
    decX            sec                     ; beq failed. going in negative direction. 
                                            ; set carry flag in prep. for subtraction
                    lda     xv              ; retrieve velocity.
                    and     #$7f            ; mask off the 8th bit (only use 1-7 for velocity)
                    sta     vtemp           ; store masked xv value to zero-page.
                    lda     cx              ; retrieve low byte of x posn
                    sbc     vtemp           ; Subtract x velocity from x posn
                    sta     cx              ; store low byte calculation to cx
                    bcs     endXmove        ; carry set. Exit subroutine
                    lda     cx+1            ; retrieve high byte of x posn
                    sec
                    sbc     #$01            ; subtract one from high byte
                    sta     cx+1            ; store new value for high byte
                    jmp     endXmove        ; done. exit subroutine

    incX            lda     cx              ; Get cx low byte
                    clc                     ; Make sure carry flag is not set
                    adc     xv              ; Add velocity to A-register
                    sta     cx              ; Store new value for low byte
                    bcc     endXmove        ; No carry set. Exit subroutine
                    lda     cx+1            ; retrieve high byte of x posn
                    adc     #$00            ; add carry to high bit. 
                    sta     cx+1            ; store new value for high byte
                        
    endXmove        jsr     REGOUT
                    rts                     ; Done. Next check is for collision with xmax.

//**************************************************************************************************************************************************

XCOLLISIONCHK:      lda     xv              ; this is our bit mask to compare to xv
                    bpl     chkXRight       ; traveling in positive direction.
    chkXLeft:       lda     cx+1            ; retrieve high byte of x posn
                    cmp     #$ff            ; if Z-Flag, need to check low byte
                    bne     endXcolChk      ; no collision
                    lda     cx              ; retrieve low byte of x posn
                    cmp     #$80            ; less than 80 is negative. passing 00 will flip positive.
                    bmi     endXcolChk      ; no collision
    XcollideL:      lda     xv              ; retrieve velocity
                    sec
                    sbc     #$80            ; change xv to positive direction
                    sta     xv
                    lda     #$00
                    sta     cx+1            ; ensure high byte is set to min
                    lda     #$01
                    sta     cx              ; set low byte to one pixel greater than min
                    jmp     endXcolChk      ; Done.
    chkXRight:      lda     cx+1            ; retrieve high byte of x posn
                    cmp     #$01            ; if Z-flag, need to check low byte
                    bne     endXcolChk      ; no collision
                    lda     cx              ; retrieve low byte of x posn
                    cmp     #$16            ; have we hit or exceeded right edge?
                    bcc     endXcolChk      ; no collision. Carry set means we passed the right edge.
    XcollideR:      lda     xv              ; retrieve velocity
                    clc
                    adc     #$80            ; change xv to negative direction
                    sta     xv
                    lda     #$01
                    sta     cx+1            ; ensure high byte is set to max
                    lda     #$15
                    sta     cx              ; set low byte to one pixel less than max
    endXcolChk:     rts

//**************************************************************************************************************************************************

MOVEY               ldx     cy              ; grab y posn
                    ldy     yv              ; grab y velocity
                    bpl     incY            ; traveling in positive direction.
    decY            tya                     ; retrieve velocity.
                    and     #$7f            ; mask off the 8th bit (only use 1-7 for velocity)
                    sta     vtemp           ; store masked yv value to zero-page.
                    txa                     ; retrieve y posn
                    sec
                    sbc     vtemp           ; Subtract y-velocity from y posn
                    tax                     ; store new y posn in x register
                    cmp     #$80            ; easier to detect collision when not negative
                    bpl     endYmove        ; 
                    sec                     ;
                    sbc     vtemp
                    bpl     endYmove        ; 
    YcollideT:      lda     #$01            ; reset cy to 1
                    tax                     ; send new y posn to x register
                    tya                     ; get y velocity from y register
                    sec
                    sbc     #$80            ; time to go in a positive directionb
                    tay                     ; send new y velocity to y-register
                    jmp     endYmove        ; done. exit subroutine

    incY            txa                     ; Get y posn from x register
                    clc
                    adc     yv              ; add velocity to y position
                    tax
                    cmp     #$80            ; easier to detect collision when bpl
                    bmi     endYmove        ; don't care right now if bmi.
    YcollideB:      cmp     #$9F            ; check to see if we've collided
                    bmi     endYmove        ; bpl means we exceeded #$9F
                    lda     #$9e
                    tax                     ; Store new y posn
                    tya                     ; get y velocity from y register
                    clc
                    adc     #$80            ; time to move in a negative direction
                    tay                     ; send new y velocity to y register                       
    endYmove        stx     cy              ; save new y posn
                    sty     yv              ; save y velocity
                    rts                     ; Done.

//**************************************************************************************************************************************************

ERASE               ldx     #$00            ; BLACK = 0
                    jsr     hcolor          ; set the color to black (contents of x-register)
                    ldx     ox              ; grab low byte of x
                    ldy     ox+1            ; grab high byte of x     
                    lda     oy              ; grab y
                    jsr     hplot           ; unplot the dot
    endErase        rts

//**************************************************************************************************************************************************

REGOUT
                    jsr     saveReg         ; dump register contents to zero-page.
                    ldx     #$45            ; lo-byte for ZP address.
                    stx     addrPtr1        ; store lo-byte for ZP register address.
                    ldx     #$00            ; hi-byte for ZP address.
                    stx     addrPtr1+1      ; store hi-byte for ZP register address.
                    ldx     #$02            ; dTable offset for screen locations.
                    ldy     #$00            ; ZP address index
    getRegAddress   lda     dTable,X        ; get lo-byte of screen address.
                    sta     addrPtr2        ; store lo-byte of screen address in ZP.
                    inx                     ; increment offset.
                    lda     dTable,X        ; get hi-byte of screen address.
                    sta     addrPtr2+1      ; store hi-byte of screen address in ZP.
    loadRegister    lda     ($45),Y         ; loop through ZP locations for A,X and Y registers.  
                    lsr                     ; shift hi-byte into lo-byte.
                    lsr                     ;
                    lsr                     ;
                    lsr                     ;
                    jsr     NibbleCheck     ; check hi order nibble for 0-9 or A-Z.
                    lda     ($45),Y         ; get a fresh copy of register and read lo-byte.
                    and     #$0F            ; mask out the hi-byte.
                    jsr     NibbleCheck     ; check lo order nibble for 0-9 or A-Z.
                    cpy     #$02            ; if we've read ZP location $47, we're done here.
                    beq     endRegOut       ; done. return to main program loop.
    nextRegSetup    inx                     ; increment ZP offset to read next register.
                    iny                     ; increment offset to read next dTable screen location.
                    jmp     getRegAddress   ; do it all again for next register.
    NibbleCheck     cmp     #$0A
                    bmi     number
                    cmp     #$10
                    bmi     letter 
    number          clc
                    adc     #$B0
                    jmp     charOut
    letter          sec
                    sbc     #$09
                    clc
                    adc     #$C0
    charOut         sty     OneBytePtr1     ; tuck Y Registers ZP offset into temp ptr.
                    ldy     #$00            ; use Y for Indirect Index of ZP.
                    sta     (addrPtr2),Y    ; place char on screen.
                    ldy     OneBytePtr1     ; restore Y to offset count for ZP indexing.
                    inc     addrPtr2        ; increment lo-byte of screen address.
    endRegOut       rts

//**************************************************************************************************************************************************
//                  DASHBOARD TEXT CONTENT. STORED IN MEMORY LOC $0300
//**************************************************************************************************************************************************

dashtext    .org  $0300     

_termChar   .byte   $FF                                                         ; scrata termination character

scrnLoc1    .byte   $50,$06
r1c1        .byte   $D8,$AD,$C3,$CF,$CF,$D2,$C4,$BA,$A0,$A0,$A0,$A0,$A0         ; "X-COORD:     " (inverse :) will be followed by low and high bytes for x
r1c2        .byte   $A0,$FC,$A0,$C1,$AD,$D2,$C5,$C7,$BA,$A0,$A0,$A0,$A0         ; " | A-REG:    " (inverse :) will be followed by A register content
r1c3        .byte   $A0,$FC,$A0,$8E,$D6,$AD,$C2,$C4,$C9,$DA,$C3,$00             ; " | NV-BDIZC"

scrnLoc2    .byte   $D0,$06
r2c1        .byte   $D9,$AD,$C3,$CF,$CF,$D2,$C4,$BA,$A0,$A0,$A0,$A0,$A0         ; "Y-COORD:     " (inverse :)
r2c2        .byte   $A0,$FC,$A0,$D8,$AD,$D2,$C5,$C7,$BA,$A0,$A0,$A0,$A0         ; " | X-REG:    " (inverse :) will be followed with X register content
r2c3        .byte   $A0,$FC,$A0,$DF,$DF,$DF,$DF,$DF,$DF,$DF,$DF,$00             ; " | ________"

scrnLoc3    .byte   $50,$07
r3c1        .byte   $D8,$AD,$D6,$C5,$CC,$CF,$C3,$BA,$A0,$A0,$A0,$A0,$A0         ; "X-VELOC:     " (inverse :) will be followed by x velocity
r3c2        .byte   $A0,$FC,$A0,$D9,$AD,$D2,$C5,$C7,$BA,$A0,$A0,$A0,$A0         ; " | Y-REG:    " (inverse :) will be followed with Y register content
r3c3        .byte   $A0,$FC,$A0,$00                                             ; " | " followed by contents of flags

scrnLoc4    .byte   $D0,$07
r4c1        .byte   $D9,$AD,$D6,$C5,$CC,$CF,$C3,$BA,$A0,$A0,$A0,$A0,$A0         ; "Y-VELOC: " (inverse :) will be followed by y velocity
r4c2        .byte   $A0,$FC,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0         ; " |           "
r4c3        .byte   $A0,$FC                                                     ; " |"
terminate   .byte   $FF


//**************************************************************************************************************************************************
//                  DATA TABLE FOR GENERAL STORAGE
//**************************************************************************************************************************************************

datatable   .org  $0390

_SDtblLoAddrIndex   .byte   $00         ; (OFFSET = 00) lo byte for SCRATA table
_SDtblHiAddrIndex   .byte   $03         ; (OFFSET = 01) hi byte for SCRATA table
_ARegLoNibbleScrnLo .byte   $67         ; (OFFSET = 02) lo byte for screen position to print high order Nibble of A-Register
_ARegHiNibbleScrnHi .byte   $06         ; (OFFSET = 03) hi byte for screen position to print high order Nibble of A-Register
_XRegLoNibbleScrnLo .byte   $E7         ; (OFFSET = 04) lo byte for screen position to print high order Nibble of X-Register
_XRegHiNibbleScrnHi .byte   $06         ; (OFFSET = 05) hi byte for screen position to print high order Nibble of X-Register
_YRegLoNibbleScrnLo .byte   $67         ; (OFFSET = 06) lo byte for screen position to print high order Nibble of Y-Register
_YRegHiNibbleScrnHi .byte   $07         ; (OFFSET = 07) hi byte for screen position to print high order Nibble of Y-Register