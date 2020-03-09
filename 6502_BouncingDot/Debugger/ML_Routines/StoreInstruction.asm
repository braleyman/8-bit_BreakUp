//|============================================|
//|    GET AND STORE OPCODE AND INSTRUCTION    |
//|============================================|
// PARAMETERS: The following memory locations should contain valid data
//              - instByteLengthParam
//
// NOTES: Needs to be re-tested
//
// UPDATES:
//      - No longer uses the X register for loop. Everything is done with the Y register
//      - Calls IncInstr during each loop
// 
//____________________________________________________________________________________________________________________________________________________________________
    StoreInst       ldy     #$00                    ; Y is loop counter
    instLoop        lda     (ICL),Y                 ; get next byte of instruction
                    sta     instSandbox,Y           ; store in next instSandbox location
                    jsr     IncInst                 ; increment ICL as we read in bytes
                    iny                             ;
                    cpy     instByteLengthParam     ; have we reached end of instruction
                    bcc     instLoop                ; read in next instruction byte
                    ; fall through here if done reading instruction bytes
                    sty     ICidx                   ; store current ICidx
endReadInst         rts                             ; END OF READ INSTRUCTION ROUTINE
//______________________________________________________________________________________________________________________________________________________________________
