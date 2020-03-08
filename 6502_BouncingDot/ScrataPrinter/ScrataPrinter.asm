.target "6502"
.org $1000
.setting "OutputTxtAddressFormatFirst" = "{0:x04}: "
.setting "OutputTxtAddressFormatNext" = "{0:x04}: "

/**************************************************************************************************************************************
SCRATA (SCREEN AND DATA) ROUTINE:

    AUTHOR: Scott Braley
    DATE:   2/1/2020

    - Will read in data from pre-determined data location and print to screen.
    - Each sequence of character bytes should be preceded with a low byte, high byte (screen location), respectively.
    - Does not place a space between strings. For that reason, include $A0, where needed, for required spaces.
    - terminating a string sequence with $00 will signal the routine to read the next two bytes of data as the next screen location.
    - terminate the scrata data with a custom HEX value (default is $FF)

**************************************************************************************************************************************/
/**************************************************************************************************************************************

    USAGE:
        
        scrn lo-byte, scrn hi-byte, ascii char, ...[ascii chars], $00
        ...... more data table lines.......
        scrn lo-byte, scrn hi-byte, ascii char, ...[ascii chars], ascii termination char

        *modify settings as required.
        *provide data table at memory location specified
    
    EXAMPLE:

        myScreenLocation   .data   $50,$06                                ; screen location, in lo-byte, hi-byte order
        myOutputString     .data   $C1,$D0,$D0,$CC,$C5,$A0,$C1,$C9,$C5    ; "APPLE PIE"
        myNextScrnLoc      .data   $D0,$06
        myLastString       .data   $D9,$D5,$CD                            ; "YUM"
        terminationStr     .data   $FF
        
*************************************************************************************************************************************/
// Zero-Page pointer locations
scrnOffset  .equ    $06         ; pointer to screen location offset ---------------------------> DEFAULT = $00
dataOffset  .equ    $07         ; pointer to current char in scrata data ----------------------> DEFAULT = $FF
scrnLoc     .equ    $08         ; pointer to absolute address of starting screen address ------> FIRST TWO BYTES (LO,HI) OF DATA STRINGS
dataLoc     .equ    $EB         ; pointer to absolute address of scrata string start ----------> DEFAULT = $0300
termChar    .equ    $ED         ; character to signal end of scrata ---------------------------> DEFAULT = $FF

SCRATAPRINT         lda     #$00            ;
                    sta     dataLoc         ; save scrata table low byte.
                    lda     #$03            ; 
                    sta     dataLoc+1       ; save scrata table high byte.
                    ldy     #$00            ; set y index counter to $00
                    lda     (dataLoc),Y     ; get 1st byte from scrata table (termination character)
                    sta     termChar        ; store termination character to zero-page.
                    jsr     $FC58           ; clear screen.

ReadChar            jsr     GetScrnLoc      ; routine will get screen location from scrata.
                    lda     #$00            ; initial scrnOffset is $00.
                    sta     scrnOffset      ; 
    getChar         ldy     dataOffset      ; get dataOffset.
                    lda     (dataLoc),Y     ; get next char from scrata table.
    checkZero       beq     ReadChar        ; found $00 terminator for string sequence?
    checkTerm       cmp     termChar        ; found termination byte for scrata table?
                    bne     PrintChar       ; print ascii byte on screen.
    endScrata       rts                     ; end SCRATAPRINT and return to callee.

GetScrnLoc          iny                     ; increment y index to point to next byte in scrata table.
    getLowByte      lda     (dataLoc),Y     ; get screen location low byte.
                    sta     scrnLoc         ; store scrnLoc lo-byte to zero-page.
                    iny                     ; inc offset to next mem location in data.
    getHighByte     lda     (dataLoc),Y     ; get screen location high byte.
                    sta     scrnLoc+1       ; store scrnLoc hi-byte to zero page.
                    iny                     ; inc offset to next mem location in data.
                    sty     dataOffset      ; store new offset in charPtr for callee.
    endGetScrnLoc   rts                     ; new screen location saved. Return to callee.

PrintChar           ldy     scrnOffset
                    sta     (scrnLoc),Y     ; print char at scrnLoc + Y (on screen location)
                    inc     scrnOffset      ; advance screen offset
                    inc     dataOffset      ; advance data offset
    resume          jmp     getChar         ; done. get next char

/*
DASHBOARD TEXT CONTENT. STORED IN MEMORY LOC $0300
*/
dashtext    .org  $0300     

scrataInit  .byte   $FF                                                         ; termination char

scrnLoc1    .byte   $50,$06
r1c1        .byte   $D8,$AD,$C3,$CF,$CF,$D2,$C4,$BA,$A0,$A0,$A0,$A0,$A0         ; "X-COORD: " (inverse :) will be followed by low and high bytes for x
r1c2        .byte   $A0,$FC,$A0,$C1,$AD,$D2,$C5,$C7,$BA,$A0,$A0,$A0,$A0         ; " | A-REG: " (inverse :) will be followed by A register content
r1c3        .byte   $A0,$FC,$A0,$8E,$D6,$AD,$C2,$C4,$C9,$DA,$C3,$00             ; " | NV-BDIZC"

scrnLoc2    .byte   $D0,$06
r2c1        .byte   $D9,$AD,$C3,$CF,$CF,$D2,$C4,$BA,$A0,$A0,$A0,$A0,$A0         ; "Y-COORD: " (inverse :)
r2c2        .byte   $A0,$FC,$A0,$D8,$AD,$D2,$C5,$C7,$BA,$A0,$A0,$A0,$A0         ; " | X-REG: " (inverse :) will be followed with X register content
r2c3        .byte   $A0,$FC,$A0,$DF,$DF,$DF,$DF,$DF,$DF,$DF,$DF,$00             ; " | ________"

scrnLoc3    .byte   $50,$07
r3c1        .byte   $D8,$AD,$D6,$C5,$CC,$CF,$C3,$BA,$A0,$A0,$A0,$A0,$A0         ; "X-VELOC: " (inverse :) will be followed by x velocity
r3c2        .byte   $A0,$FC,$A0,$D9,$AD,$D2,$C5,$C7,$BA,$A0,$A0,$A0,$A0         ; " | Y-REG: " (inverse :) will be followed with Y register content
r3c3        .byte   $A0,$FC,$A0,$00                                             ; " | " followed by contents of flags

scrnLoc4    .byte   $D0,$07
r4c1        .byte   $D9,$AD,$D6,$C5,$CC,$CF,$C3,$BA,$A0                         ; "Y-VELOC: " (inverse :) will be followed by y velocity

terminate   .byte   $FF