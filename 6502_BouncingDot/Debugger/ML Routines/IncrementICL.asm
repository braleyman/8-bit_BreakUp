//|================================|
//|     INCREMENT ICL              |
//|================================|
//
// Description: Use instByteLengthParam to increment ICL low and high bytes.
//              Should be called anytime ICL needs to be incremented to read next memory location.
// Dependencies: Calls AddressVal after incrementing ICL to determine if last instruction of program
//               being debugged has been reached.
//______________________________________________________________________________________________________________________________________________________________________

    IncInst         inc     ICL                     ; increment ICL index
                    bne     endIncInst              ; lo-byte is still in same page of memory
    pageFlip        inc     ICL+1                   ; if #$00, we flipped mem page. Need to increment hi-byte
    endIncInst      jsr     AddressVal              ; see if we've reached the end address for debugging
                    rts                             ; END OF INCREMENT ICL
//______________________________________________________________________________________________________________________________________________________________________
