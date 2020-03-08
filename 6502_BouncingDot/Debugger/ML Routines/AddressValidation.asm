//|================================|
//|    ADDRESS VALIDATION          |
//|================================|
//
// PARAMETERS:
//              - ICL: set by caller to contain start address for program being debugged
//              - lastInst: set by caller to contain end address for program being debugged
//              - jsrCount should be in a valid state
//
// Description: Should be called as needed to determine if last instruction of program being debugged has been reached.
//
// Dependencies: jsrCount
//              - WILL EXIT IF jsrCount is greater than zero.
//              - this is to prevent accidentally comparing the ICL to the lastInst value when the ICL is currently
//                stepping through a routine outside of the main program being debugged.
//
//  Post State: following registers are changed
//              - A
//
//______________________________________________________________________________________________________________________________________________________________________
    
    AddressVal      lda     jsrCount                ; get current jsrCount
                    cmp     #$00                    ; only need to worry about checking if in the main loop
                    bne     endVal                  ; don't evaluate unless debugger is in main program
                    ; falls through if in the main program being debugged
                    lda     lastInst+1              ; get hi-byte of last address
                    cmp     ICL+1                   ; compare to hi-byte of current address
                    beq     cmpLoByte               ; hi-bytes match. make sure lo bytes are correct
                    bcs     endVal                  ; all looks good. return to main
                    bcc     logError
    cmpLoByte       lda     lastInst                ; get lo-byte of last address
                    cmp     ICL                     ; compare to lo-byte of current address
                    bcc     logError                ; lo-byte of last instruction is less than starting instruction
                    bcs     endVal                  ; all looks good. return to main
                    ; fall through if last byte of instruction to be debugged
    setEndFlag      lda     #$01                    ; 
                    sta     EndFlag                 ; set EndFlag for main loop
                    bne     endVal                  ; (ALWAYS)                    
    logError        lda     #$01                    ; error code for invalid address
                    jsr     ErrorLogger             ; end of the line
                    jmp     Quit                    ; stop debugger
endVal              rts                             ; END OF ADDRESS VALIDATION
//______________________________________________________________________________________________________________________________________________________________________
