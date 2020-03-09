//|===============================================|
//|    GET BYTE LENGTH                            |
//|===============================================|
// - THIS IS WHERE THE MAGIC HAPPENS                  
// - Description: determines number of bytes in current instruction, including the op-code
// -              saves the byte length to instByteLengthParam @ $8FF2
// -              WILL ALSO:
// -                  - Check for a BRK instruction and halt further debugging
//
// - Dependencies: Uses OddOrEvenHB and ErrorLogger routines                  
//
// - Post State: The following memory locations will be conditioned
//                    - oddEvenFlag
//                    - impliedInstFlag
//                    - instByteLengthParam
//               The following registers are affected
//                    - A will contain error code || byte length
//                    - Y will contain opcode
//
// - fixed several bugs on 2/25/2020. tested with several op-codes. All worked fine. (PASSED)
// - 3/8/2020. I believe changes were made. need to start a git repo for this bad boy.
//______________________________________________________________________________________________________________________________________________________________________
                    ; interpret op-code for instruction byte length. (20 is a special case, JSR, having a 2 byte instruction)
    GetByteLength   jsr     OddOrEvenHB             ; set the oddEvenFlag for Hi-Byte
                    ldy     ICidx                   ; 
                    lda     (ICL),Y                 ; read opcode for instruction
    chkBrk          cmp     #$00                    ; is it a break statement?
                    bne     chkRTS                  ;
                    ; fall through if there's a break statement
                    lda     #$02                    ; error code for BRK
                    jsr     ErrorLogger             ; go directly to jail. do not pass go.
                    jmp     quit                    ; stop debugger
                    ;---------------------------------------------------------------------------------------------------------------------------------------------------
    chkRTS          cmp     #$60                    ;
                    bne     chkRTI                  ;
                    beq     isImplied               ; (ALWAYS)
                    ;---------------------------------------------------------------------------------------------------------------------------------------------------
    chkRTI          cmp     #$40                    ;
                    bne     chkJSR                  ; no, check for jsr
                    beq     isImplied               ; (ALWAYS)
                    ;---------------------------------------------------------------------------------------------------------------------------------------------------
    chkJSR          cmp     #$20                    ; check for JSR
                    bne     check9                  ; not JSR. check for lo-byte of 9                
                    beq     odd                     ; (ALWAYS) branch to odd to load X register for 2 instruction bytes
                    ;---------------------------------------------------------------------------------------------------------------------------------------------------
    check9          and     #$0F                    ; mask out the OpCode's hi byte
                    cmp     #$09                    ; (SPECIAL CASE) handle lo-byte of nine different. lo-byte of 9 could be 2 or 3 bytes
                    beq     specialCase             ; op-code ends with #$9
                    bne     byteCheck               ; op-code does not end with #$0 or 9
                    ; instruction is immediate or absolute,Y. even or odd check will determine instruction byte length
    specialCase     lda     oddEvenFlag             ; check oddEvenFlag to determine byte length
                    bne     odd                     ; flag = 1 for odd
    even            lda     #$02                    ; even, so 2-byte instruction (Op-code plus 1 more byte)
                    bne     storeByteLength         ; (ALWAYS) bne is faster than jump
    odd             lda     #$03                    ; odd, so 3-byte instruction (Op-code plus 2 more bytes)
                    bne     storeByteLength         ; (ALWAYS) bne is faster than jump
                    ;---------------------------------------------------------------------------------------------------------------------------------------------------
                    // - we can use a simple compare scheme to find instruction byte length for each op-code
    byteCheck       cmp     #$07                    ; lo-byte value of 1-6 is a 2 byte instruction (Op-code plus 1 byte)
                    bmi     twoByte                 ;
    chk1byte        cmp     #$0B                    ; lo-byte value of 8 or A is an implied instruction, so 1 bytes
                    bpl     threebyte               ; must be 3 byte instruction at this point
                    ; fall through if implied instruction. Might as well set flag for use in the ADDRESSING SETUP routine
    isImplied       lda     #$01                    ; 
                    sta     impliedInstFlag         ; set the impliedInstFlag
                    bne     storeByteLength         ;
    threebyte       lda     #$03                    ; everything else must be a 3 byte instruction
                    bne     storeByteLength         ; (ALWAYS) bne is faster than jump
    twoByte         lda     #$02                    ;
storeByteLength     sta     instByteLengthParam     ; store byte count
endGetByteLength    rts                             ; END GET BYTE LENGTH
//______________________________________________________________________________________________________________________________________________________________________
