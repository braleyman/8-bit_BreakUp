.target "6502"
.org $1000
.setting "OutputTxtAddressFormatFirst" = "{0:x04}: "
.setting "OutputTxtAddressFormatNext" = "{0:x04}: "

ICL                 .equ    $06
ICidx               .equ    $19
instByteLengthParam .equ    $08
OddEvenFlag         .equ    $09
ImpliedInstFlag     .equ    $EB
error               .equ    $EC

/*
TEST SETUPS:

1) opcode AD is loaded at 0300 and is a 3 byte opcode
   OddOrEvenHB should set OddEvenFlag to 1 = odd
   instByteLengthParam should be 03
   ImpliedInstFlag should be 00

2) opcode E9 is loaded at 0300 and is a 2 byte opcode
   OddEvenFlag should be 0 = even
   instByteLengthParam should be 02
   ImpliedInstFlag should be 00

3) opcode 20 is loaded at 0300 and is a 3 byte opcode
   OddEvenFlag should be 0 = even
   instByteLengthParam should be 03
   ImpliedInstFlag should be 00

4) opcode 4C is loaded at 0300 and is a 3 byte opcode
   OddEvenFlag should be 0 = even
   instByteLengthParam should be 03
   ImpliedInstFlag should be 00

5) opcode EA is loaded at 0300 and is an implied instruction, 1 byte
   OddEvenFlag should be 0 = even
   instByteLengthParam should be 01
   ImpliedInstFlag should be 01 
*/

/* 
INITIAL VALUES:
- Test instructions stored at 0300 intially cleared to 00
- ICIdx set to 00
- instSandbox is $9000-$9002 and cleared to 00
- instByteLengthParam will be set by the GetByteLength routine inital value is 00
- OddEvenFlag is set by the routine initial value is 00
- ImpliedInstFlag is set by the routine initial value is 00
*/

lda     #$00
sta     ICidx
sta     instByteLengthParam
sta     OddEvenFlag
sta     ImpliedInstFlag
sta     error
sta     $0300

lda     #$00
sta     ICL
lda     #$03
sta     ICL+1


TEST1   lda     #$18
        sta     $0300
        jsr     GetByteLength
        rts

//|===============================================|
//|    DETERMINE OPCODE TYPE AND BYTE LENGTH      |
//|===============================================|
// - THIS IS WHERE THE MAGIC HAPPENS                  
// - read in operand and 1 or 2 byte instruction

// - do some simple checks for break and rts
// - store operand to sandox ($9000)
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

//|================================|
//|     ODD OR EVEN Hi-Byte        |
//|================================|
//______________________________________________________________________________________________________________________________________________________________________
    OddOrEvenHB     lsr                     ; Shifts hi-byte 5 times to check LSB for even or odd
                    lsr
                    lsr
                    lsr
                    lsr                     
                    bcc     setEvenHB       ; hi byte LSB is even
                    ; falls through if odd
                    lda     #$01            ;
                    sta     OddEvenFlag     ; set OddEvenFlag to odd
                    bne     endOddOrEvenHB  ; (ALWAYS)
    setEvenHB       lda     #$00            ;
                    sta     OddEvenFlag     ; set OddEvenFlag to even
endOddOrEvenHB      rts                     ; end ODD OR EVEN Hi-Byte

//|================================|
//|     ERROR LOGGER               |
//|================================|
// PARAMETERS:
// - A Register contains error number
//______________________________________________________________________________________________________________________________________________________________________

    ErrorLogger     sta     error           ; store error in error memory location

//______________________________________________________________________________________________________________________________________________________________________

quit rts