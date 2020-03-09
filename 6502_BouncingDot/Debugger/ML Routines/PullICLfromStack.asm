//|================================|
//|     PULL ICL FROM STACK        |
//|================================|
// PARAMETERS: sandboxStPtr should be pointing at next location in stack
//
// Description: Restore ICL low and high bytes to the next instruction following a jsr
//______________________________________________________________________________________________________________________________________________________________________
    PullInst        ldx     sandboxStPtr    ; get pointer for sandbox stack
                    jsr     incStackPtr     ; move stack pointer up one position to read lo-byte
    pullLoByte      lda     sandboxStack,X  ; get lo-byte of ICL
                    sta     ICL             ; store lo-byte to ICL
                    jsr     incStackPtr     ; move stack pointer one more position to read hi-byte
    pullHiByte      lda     sandboxStack,X  ; get hi-byte of ICL
                    sta     ICL+1           ; store hi-byte to ICL+1
                    stx     sandboxStPtr    ; store new value of pointer
endPullInst         rts                     ; END OF PUSHINST
//______________________________________________________________________________________________________________________________________________________________________
