//|================================|
//|     SAVE STATE                 |
//|================================|
//
// Description: Save the current register states prior to entering debugger.
//              Will be used to reset registers to pre-debugging state prior to exiting back to caller.
//
// 2/26/2020 - Unit test passed (PASSED)
// 3/8/2020 - Was thinking about this and, though unit tests passed, I'm not sure the state of registers are important at this point?
//            Will have to try it and see. I'm thinking the caller's program should really be the place to save/restore registers.
//______________________________________________________________________________________________________________________________________________________________________
    SaveState       ldx     #$00
    SaveLoop        lda     $45,X
                    sta     callerRegisters,X
                    inx  
                    cpx     #$05
                    bcc     SaveLoop
                    rts                             ; END OF SaveState
//______________________________________________________________________________________________________________________________________________________________________