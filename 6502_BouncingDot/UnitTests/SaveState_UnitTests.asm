.target "6502"
.org $1000
.setting "OutputTxtAddressFormatFirst" = "{0:x04}: "
.setting "OutputTxtAddressFormatNext" = "{0:x04}: "

callerRegisters     .equ    $8FF6   ; F6 = A-reg, F7 = X-reg, F8 = Y-reg, F9 = P-reg, FA = S-reg

// TEST INSTANCE:
// value of registers during test are:
// A=FF, X=98, Y=D8, P=00 S=B7
//
// value of callerRegisters after test are:
// A=FF, X=98, Y=D8, P=00 S=B7



//|================================|
//|     SAVE STATE                 |
//|================================|
//______________________________________________________________________________________________________________________________________________________________________
    SaveState       ldx     #$00
    SaveLoop        lda     $45,X
                    sta     callerRegisters,X
                    inx  
                    cpx     #$05
                    bcc     SaveLoop
                    rts                             ; END OF STATE SAVE