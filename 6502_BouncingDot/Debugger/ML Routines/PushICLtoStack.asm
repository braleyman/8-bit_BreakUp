//|================================|
//|     PUSH ICL TO STACK          |
//|================================|
// PARAMETERS: NONE
// Description: saves memory location to return to after jsr
//______________________________________________________________________________________________________________________________________________________________________
    PushInst        ldx     sandboxStPtr    ; get pointer for sandbox stack
    pushHiByte      lda     ICL+1           ; get hi-byte of IC
                    sta     sandboxStack,X  ; push hi-byte to sandboxStack
                    jsr     decStackPtr     ; move stack pointer 
    pushLoByte      lda     ICL             ; get lo-byte of ICL
                    sta     sandboxStack,X  ; push lo-byte to stack
                    jsr     decStackPtr     ; move stack pointer
                    stx     sandboxStPtr    ; save current pointer position
endPushInst         rts                     ; END OF PUSHINST
//______________________________________________________________________________________________________________________________________________________________________
