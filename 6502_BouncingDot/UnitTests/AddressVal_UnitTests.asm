.target "6502"
.org $1000
.setting "OutputTxtAddressFormatFirst" = "{0:x04}: "
.setting "OutputTxtAddressFormatNext" = "{0:x04}: "

ICL                 .equ    $06
lastInst            .equ    $08
error               .equ    $EC

lda     #$00
sta     error

/*
  Test will produce a value of $01 at location $EC
*/

// TEST 1 should evaluate without error
// ran 2/26/2020 - PASSED
/* TEST1   lda     #$00
        sta     ICL
        lda     #$03
        sta     ICL+1
        lda     #$00
        sta     lastInst
        lda     #$04
        sta     lastInst+1

        jsr     AddressVal
        rts */

// TEST 2 should evaluate without error
// ran 2/26/2020 - PASSED
/* TEST2   lda     #$50
        sta     ICL
        lda     #$03
        sta     ICL+1
        lda     #$25
        sta     lastInst
        lda     #$04
        sta     lastInst+1

        jsr     AddressVal
        rts */

// TEST 3 should evaluate WITH error
// ran 2/26/2020 - PASSED
/* TEST3   lda     #$50
        sta     ICL
        lda     #$03
        sta     ICL+1
        lda     #$25
        sta     lastInst
        lda     #$03
        sta     lastInst+1

        jsr     AddressVal
        rts */

// TEST 4 should evaluate WITH error
// ran 2/26/2020 - PASSED
/* TEST4   lda     #$50
        sta     ICL
        lda     #$03
        sta     ICL+1
        lda     #$25
        sta     lastInst
        lda     #$02
        sta     lastInst+1

        jsr     AddressVal
        rts */

// TEST 5 should evaluate without error
// ran 2/26/2020 - PASSED
/* TEST5   lda     #$50
        sta     ICL
        lda     #$03
        sta     ICL+1
        lda     #$AD
        sta     lastInst
        lda     #$90
        sta     lastInst+1

        jsr     AddressVal
        rts */

// TEST 6 should evaluate without error
// ran 2/26/2020 - PASSED
/* TEST6   lda     #$50
        sta     ICL
        lda     #$AC
        sta     ICL+1
        lda     #$AD
        sta     lastInst
        lda     #$BF
        sta     lastInst+1

        jsr     AddressVal
        rts */

// TEST 7 should evaluate WITH error
// ran 2/26/2020 - PASSED
TEST7   lda     #$50
        sta     ICL
        lda     #$BF
        sta     ICL+1
        lda     #$AD
        sta     lastInst
        lda     #$BE
        sta     lastInst+1

        jsr     AddressVal
        rts

//|================================|
//|    ADDRESS VALIDATION          |
//|================================|
//
//  PARAMETERS:
//  - ICL: set by caller to contain start address for program being debugged
//  - lastInst: set by caller to contain end address for program being debugged
//
//* Address Validation: Need to make sure the caller has supplied valid start and stop addresses
//* Works just like a Select Case statement
//* lastAddHi - curAddHi -> beq? passed AND lastAddLo - curAddLo -> bcs? All Good!!
//* lastAddHi - curAddHi -> bcs? All Good!!
//* any other condition would be a fail
//______________________________________________________________________________________________________________________________________________________________________
    AddressVal      lda     lastInst+1              ; get hi-byte of last address
                    cmp     ICL+1                   ; compare to hi-byte of current address
                    beq     cmpLoByte               ; hi-bytes match. make sure lo bytes are correct
                    bcs     endVal                  ; all looks good. return to main
                    bcc     logError
    cmpLoByte       lda     lastInst                ; get lo-byte of last address
                    cmp     ICL                     ; compare to lo-byte of current address
                    bcs     endVal                  ; all looks good. return to main
                    ; fall through to logError
    logError        lda     #$01                    ; error code for invalid address
                    jsr     ErrorLogger             ; end of the line
                    jmp     Quit                    ; stop debugger
endVal              rts                             ; END OF ADDRESS VALIDATION

//|================================|
//|     ERROR LOGGER               |
//|================================|
// PARAMETERS:
// - A Register contains error number
//______________________________________________________________________________________________________________________________________________________________________

    ErrorLogger     sta     error           ; store error in error memory location

//______________________________________________________________________________________________________________________________________________________________________

quit rts