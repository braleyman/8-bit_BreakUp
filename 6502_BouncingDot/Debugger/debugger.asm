.target "6502"
.org $1000
.setting "OutputTxtAddressFormatFirst" = "{0:x04}: "
.setting "OutputTxtAddressFormatNext" = "{0:x04}: "

ICL                 .equ    $06     ; $06 and $07 contain base address of next instruction to debug
ICidx               .equ    $19     ; Instruction Counter Index
lastInst            .equ    $08     ; $08 and $09 contain last instruction of program
hookInst            .equ    $1E     ; $1E and $1F are where the caller would place a hook to their own routine
addressingType      .equ    $8FF3   ; used in the supporting routine, ADDRESSING SETUP, and in the OPCODE ANALYSIS routine
                                    ; 00 = relative 
                                    ; 01 = absolute: abs
                                    ; 02 = indirect: (abs)
                                    ; 03 = indexed: abs,x || abs,y
                                    ; 04 = indexed by X || Y: zp,x || zp,y
                                    ; 05 = indexed indirect (pre-indexed) (zp,x)
                                    ; 06 = indirect indexed (post-indexed) (zp),y

error               .equ    $8FF4   ; one byte error code goes here
callerRegisters     .equ    $8FF5   ; F5 = A-reg, F6 = X-reg, F7 = Y-reg, F8 = P-reg, F9 = S-reg
jsrCount            .equ    $8FFA   ;
EndFlag             .equ    $8FFB   ;
OddEvenFlag         .equ    $8FFC   ; Hi-byte flag, 00 = even, 01 = odd
impliedInstFlag     .equ    $8FFD   ; 0 is default, 1 = implied instruction
executeFlag         .equ    $8FFE   ; 00 = do not execute instruction, 01 = execute instruction
sandboxIndex        .equ    $8FFF   ; index pointer to current location in instSandbox
instSandbox         .equ    $9000   ; starting location for running next instruction in isolation

ICLParam_LB         .equ    $8FEF   ; lo-byte parameter for the UpdateICL routine
ICLParam_HB         .equ    $8FF0   ; hi-byte parameter for the UpdateICL routine
instByteLengthParam .equ    $8FF1   ; byte length parameter for ReadBytes routine
pushParam           .equ    $8FF2   ; 1 = push, for ReadBytes routine

sandboxStack        .equ    $9000   ; debugger stack (goes from 9004 - 90FF)
sandboxStPtr        .equ    $8FEE   ; pointer for sandboxStack
stack               .equ    $0100   ; location for stack
rdkey               .equ    $FD0C   ; monitor routine for getting keyboard input
saveReg             .equ    $FF4A   ; monitor routine for saving registers to ZP

//  ********************************************************************************************************************************************************************
//  ********************************************************************************************************************************************************************
//  *********************                                                                                                                        ***********************
//  *********************                                               DEBUG MAIN LOOOP                                                         ***********************
//  *********************                                                                                                                        ***********************
//  ********************************************************************************************************************************************************************
//  ********************************************************************************************************************************************************************

                    jsr     SaveState               ; 1st step - save register state for caller's program
                    jsr     AddressVal              ; 2nd step - make sure caller has provided valid start and stop address for their program
                    jsr     InitVariables           ; 3rd step - Initialize debugger variables
                    jsr     GetByteLength           ; 4th step - Examine opcode and determine byte length of instruction
                    jsr     StoreInst               ; 5th step - Read in and store instruction to sandbox memory
                    jsr     OpCodeAnalysis          ; 6th step - Analyze opcode to determine next step


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

//|================================|
//|     RESTORE STATE              |
//|================================|
//
// Description: Restore registers to pre-debugger states
//
// 2/26/2020 - Unit test passed (PASSED)
// 3/8/2020 - See notes for SaveState routine.
//______________________________________________________________________________________________________________________________________________________________________
    RestoreState    jsr     $FF3F                   ; monitor routine to reload registers from zp
                    rts                             ; END OF RestoreState
//______________________________________________________________________________________________________________________________________________________________________

//|================================|
//|    INITIALIZE VARIABLES        |
//|================================|
// PARAMETERS: NONE
//
// Description: Set default state for memory locations used by debugger
//______________________________________________________________________________________________________________________________________________________________________                
    InitVariables   lda     #$00
                    sta     sandboxIndex            ; index starting position needs to be 0
                    sta     jsrCount                ; default = 0
                    sta     pushParam               ; always starts at 0 = no push
                    sta     executeFlag             ; default = 0
                    sta     instByteLengthParam     ; always starts at 0
                    sta     ICidx                   ; always starts at 0
                    lda     ICL                     ;
                    sta     ICLParam_LB             ; set initial value of param to lo-byte mem location
                    lda     ICL+1                   ;
                    sta     ICLParam_HB             ; set initial value of param to hi-byte mem location
endInit             rts                             ; END OF DEBUGGER INITIALIZATION
//______________________________________________________________________________________________________________________________________________________________________

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

//|============================================|
//|    INSTRUCTION ANALYSIS                    |
//|============================================|
//
// PARAMETERS:
//              - jsrCount in a valid state
//              - impliedInstFlag in a valid state
//              - sandbox should already contain opcode plus instruction bytes 
// 
//____________________________________________________________________________________________________________________________________________________________________
// TODO: This is the start of the opcode analysis
    opCodeAnalysis  ldy     #$00
                    lda     instSandbox,Y           ; read opcode for instruction
                    tax                             ; store copy of opcode
                    ;------------------------------------------------------------------------------------------------------------------------------------------------
    checkRtiRts     cmp     #$60                    ; if opcode is rts, see if we reached end of the program
                    bne     checkRti                ; 
                    ; need to check if rts is a return from jsr or end of program
    checkRtiRts2    lda     jsrCount                ; get current jsrCount
                    bne     rtsReturn               ; we're exiting a branch
                    ; fall through if end of program
                    lda     #$FF                    ; value for end program
                    jmp     setupAddressing         ; (ALWAYS) end of program.
                    ;------------------------------------------------------------------------------------------------------------------------------------------------
    checkRti        cmp     #$40                    ; rti means the caller must be debugging an ISR
                    bne     checkImplied            ;
                    lda     #$FF                    ; value for end program.
                    jmp     setupAddressing         ; (ALWAYS) end of program. return to debugger main loop.
                    ;------------------------------------------------------------------------------------------------------------------------------------------------
    checkImplied    lda     impliedInstFlag         ; 01 = ImpliedInstruction
                    beq     checkJSR                ; not implied, so continue on to check for JSR
                    ; fall through here if implied flag set
                    lda     #$99                    ; addressSetup value for implied instruction
                    jmp     setupAddressing         ; (ALWAYS)
                    ;------------------------------------------------------------------------------------------------------------------------------------------------
    rtsReturn       jsr     noExecute
                    lda     #$60                    ; 60 = RTS
                    jmp     setupAddressing         ; done
                    ;------------------------------------------------------------------------------------------------------------------------------------------------
    checkJSR        txa                             ; get fresh copy of opcode
                    cmp     #$20                    ; jsr?
                    bne     check4C                 ; nope. see if it's a jump
                    ; will fall through if opcode is a jsr
                    jsr     noExecute
                    lda     #$20                    ; 20 = JSR
                    jmp     setupAddressing         ;
    noExecute       lda     #$00                    ; 
                    sta     executeFlag             ; update executeFlag to 0 (don't execute a jsr)
                    rts                             ;
                    ;------------------------------------------------------------------------------------------------------------------------------------------------
                    ; compare one-off operands that don't follow the group as a whole, like 4C (jmp), 6C (jmp), and 00,A0,C0,E0 (brk, ldy, cpy, cpx)
    check4C         cmp     #$4C                    ; jmp absolute?
                    bne     check6C                 ; not a jmp absolute, check if jmp indirect
                    ; falls through if 4C
                    lda     #$02                    ; absolute addressing
                    bne     setupAddressing         ; (ALWAYS)
                    ;---------------------------------------------------------------------------------------------------------------------------------------------------
    check6C         cmp     #$6C                    ; jmp indirect?
                    bne     branchCheck             ; not a jmp indirect, continue to check for a branching opcode
                    ; falls through if 6C
                    lda     #$03                    ; indirect addressing
                    bne     setupAddressing         ; (ALWAYS)
                    ;---------------------------------------------------------------------------------------------------------------------------------------------------
                    // 10,30,50,70,90,B0,D0,F0 are branching instructions
                    // A0 (LDY), C0 (CPY), E0 (CPX) are not branching instructions
                    // 00,20,40, and 60 have already been handled up to this point
    branchCheck     txa                             ; get copy of operand
                    ; mask operand with #$0F to retain lo-byte only.
                    and     #$0F                    ; mask out the OpCode's hi byte
                    bne     oddEvenFork             ; Not a branching instruction. branches have 0 in lo-byte
                    ; falls through if we have a POSSIBLE branching instruction (lo-byte of 0). Need to rule out one-offs LDY, CPY and CPX (A0,C0,E0)
                    txa                             ; get fresh copy of operand from X register
                    and     #$10                    ; want to focus on bit 1 of high byte to determine if even or odd
                    cmp     #$00                    ; 
                    beq     immediate               ; LDY, CPY, and CPX will be zero (even hi-bytes). Hence, not branching operands
                    ; falls through if branching instruction
                    lda     #$00                    ; addressing value for relative branch
                    beq     setupAddressing         ; (ALWAYS)
    immediate       lda     #$01                    ; addressing value for immediate
                    bne     setupAddressing         ; (ALWAYS)
                    ;---------------------------------------------------------------------------------------------------------------------------------------------------
                    ; time to check OddEvenFlag and decide to fork to even or odd hi-byte addressing checks
    oddEvenFork     lda     OddEvenFlag             ;
                    bne     oddAddressing           ; hi-byte is odd, so check for ZP,X ; ABS,X ; ABS,Y ; (Indirect),Y
                    ; falls through if evenAddressing
evenAddressing      ; even = immediate || zero-page || absolute || ZP indexed indirect (pre-indexed) (zp,x)
                    ; the immediate addressing operands LDY, CPY, and CPX were accounted for under the branchCheck label
                    ; the remaining immediate addressing operands with lo-bytes 2 and 9 need to complete check for immediate addressing
    checkImmediate  txa                             ; get copy of operand
                    and     #$0F                    ; keep lo-byte
                    cmp     #$02                    ; check if LDX immediate
                    beq     immediate               ; 
                    cmp     #$09                    ; check if ORA, AND, EOR, ADC, LDA, CMP, SBC Immediate
                    beq     immediate               ;
                    cmp     #$01                    ; check if (indirect,x)
                    beq     indirectX               ;
                    ; falls through if not immediate || (indirect,x) instruction and now check to see if ZP instruction
    checkZP         txa                             ; get copy of operand
                    ; just need to change next 2 lines to asl 5 times and check carry
                    jsr     OddOrEvenLB             ;
                    lda     OddEvenFlag             ; if MSB of lo-byte = 0 then it's zp, 1 is ABS
                    bne     absolute                ; carry set = ABS
                    ; falls through if zp addressing
                    lda     #$05                    ; addressing value for ZP
                    bne     setupAddressing         ; (ALWAYS)
    absolute        lda     #$02                    ; addressing value for ABS
                    bne     setupAddressing         ; (ALWAYS)
                    ;---------------------------------------------------------------------------------------------------------------------------------------------------
    indirectX       lda     #$07                    ; ZP indexed indirect (pre-indexed) (zp,x)
                    bne     setupAddressing         ; (ALWAYS)
oddAddressing       ; odd = (zp,x || zp,y) || abs,x || abs,y || ZP indirect indexed (post-indexed) (zp),y
                    txa                             ; get copy of operand
                    and     #$0F                    ; keep lo-byte
    checkIndY       cmp     #$01                    ; check if ZP indirect indexed (post-indexed) (zp),y
                    bne     checkAbsY               ; not (zp),y
                    ; falls through if (indirect),Y
                    lda     #$08                    ; addressing value for (zp),y
                    bne     setupAddressing         ; (ALWAYS)
    checkAbsY       cmp     #$09                    ; check if ABS,Y
                    bne     checkZpAbsX             ; not ABS,y
                    ; falls through if ABS,Y
                    lda     #$04                    ; addressing value for ABS,Y
                    bne     setupAddressing         ; (ALWAYS)
                    ; use OddOrEvenLB to set OddEvenFlag
    checkZpAbsX     txa                             ; get copy of operand
                    jsr     OddOrEvenLB             ; call routine to set flag
                    lda     OddEvenFlag             ; if MSB of lo-byte = 0 then it's zp,x || zp,y
                    bcs     absXY                   ; 
                    ; falls through if zp,x || zp,y
                    lda     #$06                    ; addressing value for zp,x || zp,y
                    bne     setupAddressing         ; (ALWAYS)
    absXY           lda     #$04                    ; addressing value for abs,x || abs,y
    setupAddressing sta     addressingType          ; store value of addressing type
                    jsr     addressingSetup         ; call support routine to setup debugger based on addressing type
endOpCodeAnalysis   rts                             ; END OF OPCODE ANALYSIS
//______________________________________________________________________________________________________________________________________________________________________


                 



                    
    
   /*                  dex                             ; decrement x to prepare for next byte
                    beq     chkBrFlag               ; see if we need to intercept a branch or jump
                    bcs     ReadInst                ; 
    chkBrFlag       lda     jsrCount                ; check flag to see if we have to repeat process for another line
                    beq     PrepForExec             ; time to execute copied line of code
                    lda     instSandbox, Y          ; read back instruction for hi-byte of branch
                    sta     ICL+1                   ; change hi-byte of current instruction to hi-byte of branch
                    dey                             ; decrement Y index to get lo-byte of branch
                    lda     instSandbox, Y          ; read back byte for lo-byte of branch
                    sta     ICL                     ; change lo-byte of current instruction to lo-byte of branch
                    dec     jsrCount                ; decrement branch count by one
                    jmp     getOpCode               ; go back and do it again for the branch

    PrepForExec     inc     sandboxIndex            ;
                    ldy     sandboxIndex
                    lda     #$60                    ; need to append the sandbox with an RTS to return when finished
                    sta     instSandbox,Y           ; 
                    jsr     instSandbox             ; execute instruction stored in sandbox
                    jsr     saveReg
                    //**********************************************************************************************
                    //*                         USER HOOK ADDRESS HERE: REMEMBER TO UPDATE WHEN COMPLETE 
                    //*
                    //*      USER CAN TAP IN TO READ STATE OF REGISTERS AND USE MONITOR TO DISASSEMBLE INSTRUCTION AT $9000
                    //**********************************************************************************************
                    jsr     hookInst                ; default is rdkey address. 
                    jsr     rdkey                   ; wait for user to press key to continue to next instruction
                    cmp     #$92                    ; did the user press ctrl-R ?
                    beq     Quit                    ; quit debugger
                    ; test for end of code */
//|================================|
//|     QUIT DEBUGGER              |
//|================================|
// TODO: MAKE THIS A FULL FLEDGED QUIT TO RESTORE REGISTERS WHEN FINISHED.
//______________________________________________________________________________________________________________________________________________________________________
    Quit            rts                             ; temporary
//______________________________________________________________________________________________________________________________________________________________________

//  ********************************************************************************************************************************************************************
//  ********************************************************************************************************************************************************************
//  *********************                                                                                                                        ***********************
//  *********************                                               SUPPORT ROUTINES                                                         ***********************
//  *********************                                                                                                                        ***********************
//  ********************************************************************************************************************************************************************
//  ********************************************************************************************************************************************************************

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
                    sta     oddEvenFlag     ; set oddEvenFlag to odd
                    bne     endOddOrEvenHB  ; (ALWAYS)
    setEvenHB       lda     #$00            ;
                    sta     oddEvenFlag     ; set oddEvenFlag to even
endOddOrEvenHB      rts                     ; end ODD OR EVEN Hi-Byte
//______________________________________________________________________________________________________________________________________________________________________

//|================================|
//|     ODD OR EVEN Lo-Byte        |
//|================================|
//______________________________________________________________________________________________________________________________________________________________________
    OddOrEvenLB     asl                     ; Shifts lo-byte 5 times to check MSB for even or odd
                    asl
                    asl
                    asl
                    asl                     
                    bcc     setEvenLB       ; lo-byte MSB is even
                    ; falls through if odd
                    lda     #$01            ;
                    sta     oddEvenFlag     ; set oddEvenFlag to odd
                    bne     endOddOrEvenLB  ; (ALWAYS)
    setEvenLB       lda     #$00            ;
                    sta     oddEvenFlag     ; set oddEvenFlag to even
endOddOrEvenLB      rts                     ; end ODD OR EVEN Lo-Byte
//______________________________________________________________________________________________________________________________________________________________________

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

//|================================|
//|     ADDRESSING SETUP           |
//|================================|
//         
// PARAMETERS: Following memory locations should already be conditioned:
//             - sandbox ($9000-$9002) should contain opcode and instruction byte(s)
//             - instByteLengthParam should contain proper value for number of bytes in instruction, to include the opcode.
//               example: AD 65 07 should condition the instByteLengthParam to a value of $03
//             - addressingType should be set to proper value, as per below:
                                        ; 00 = relative branch
                                        ; 01 = immediate
                                        ; 02 = absolute: abs
                                        ; 03 = indirect: (abs)
                                        ; 04 = indexed: abs,x || abs,y
                                        ; 05 = ZP
                                        ; 06 = ZP indexed by X || Y: zp,x || zp,y
                                        ; 07 = ZP indexed indirect (pre-indexed) (zp,x)
                                        ; 08 = ZP indirect indexed (post-indexed) (zp),y
                                        ; 20 = JSR
                                        ; 60 = RTS
                                        ; 99 = Implied
                                        ; FF = end of program
//              
// Description: Use addressing value from opcode analysis routine to condition sandbox stack and other memory locations prior to executing
//              routine will:
//                           - handle an RTS and RTI separate from other implied instructions
//                           - push/pull addresses from sandboxStack as required
//                           - place an rts in sandbox, immediately following the last byte of the instruction
//                           - increment the low and high bytes for ICL
//                           - increment/decrement jsrCount as required
//                           - clear impliedInstFlag as required           

// CUT FROM OPCODE ANALYSIS
//                    
//                    
//                    
//                    
//                    

//______________________________________________________________________________________________________________________________________________________________________
    AddressingSetup     ldy     sandboxIndex        ; 
                        lda     addressingType      ; get the addressing type
    endOfProgram        cmp     #$FF                ; End of program (RTI or an RTS if it's the end of the program)
                        bne     impAddressing       ;
                        jmp     Quit                ; go to quit for now.
                        // TODO: WHAT DO WE WANT TO DO FOR ENDING THE PROGRAM??
                        //       - restore register contents
                        //       - ??
                        ;-----------------------------------------------------------------------------------------------------------------------------------------
    impAddressing       cmp     #$99                ; Implied Addressing (Exluding RTS and RTI)
                        bne     rtsAddressing       ; 
                        // TODO: SHOULD JUST NEED TO EXECUTE AN IMPLIED INSTRUCTION
                        jmp     endAddressingSetup  ; nothing to do.
                        ;-----------------------------------------------------------------------------------------------------------------------------------------
    rtsAddressing       cmp     #$60                ; Returning from JSR
                        bne     jsrAddressing       ;
                        // TODO: SHOULD PULL RETURN LOCATION FROM SANDBOX STACK, AND DECREMENT jsrCount

                        ;-----------------------------------------------------------------------------------------------------------------------------------------
    jsrAddressing       cmp     #$20                ; JSR
                        bne     relAddressing       ;
                        // TODO: SHOULD PUSH RETURN LOCATION TO SANDBOX STACK, AND INCREMENT jsrCount
                        inc     jsrCount            ; increment jsrCount
                        jsr     PushInst            ; push return address for jsr onto the stack
                        jsr     UpdateICL           ; need to load address of jsr routine into ICL and ICL+1
                        ;-----------------------------------------------------------------------------------------------------------------------------------------
    relAddressing       cmp     #$00                ; relative branching?
                        bne     absAddressing       ;
                        // TODO: NEED TO CALCULATE RELATIVE OFFSET AND SAVE TO ICL
                        ;-----------------------------------------------------------------------------------------------------------------------------------------
    immAddressing       cmp     #$01                ; immediate?
                        bne     absAddressing       ;
    absAddressing       cmp     #$02                ; Absolute: abs?
                        bne     indAddressing       ; 
                        ; nothing special here. Just need to read in next 2 memory locations and set ICL
                        lda     (ICL),y             ; get lo-byte of absolute address
                        sta     ICLParam_LB         ; save lo-byte parameter
                        iny                         ;
                        lda     (ICL),y             ; get hi-byte of absolute address
                        sta     ICLParam_HB         ; save hi-byte parameter
                        jsr     UpdateICL           ; call UpdateICL
    indAddressing       cmp     #$03                ; Indirect: (abs)?
                        bne     absXYaddressing        ;
                        ; should check to make sure location of jmp address is not stored on a page boundary
                        ; if it is, an error should be thrown to warn the caller that the 6502 has a bug that won't
                        ; interpret the address correctly

    absXYaddressing     cmp     #$04                ; Indexed: abs,x or abs,y?
                        bne     zpAddressing        ; 
    zpAddressing        cmp     #$05                ; zp?
                        bne     zpXYaddressing      ; 
    zpXYaddressing      cmp     #$06                ; ZP indexed by X or Y: zp,x or zp,y?   
                        bne     preIdxAddressing    ; 
    preIdxAddressing    cmp     #$07                ; ZP indexed indirect (pre-indexed): (zp,x)?
                        bne     pstIdxAddressing    ; 
    pstIdxAddressing    cmp     #$08                ; ZP indirect indexed (post-indexed): (zp),y?
    showInstr
endAddressingSetup      rts                         ; end ADDRESSING SETUP
//______________________________________________________________________________________________________________________________________________________________________

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

//|================================|
//|    DECREMENT STACK POINTER     |
//|================================|
// PARAMETERS: X-Register should contain the sandboxStPtr value
// Description: decrements stack pointer and checks for overflow of sandbox stack
//______________________________________________________________________________________________________________________________________________________________________
    decStackPtr     dex                     ; decrement pointer
                    cmp     #$03            ; make sure the debugger stack hasn't overflowed
                    bne     endDecStackPtr  ; haven't overflowed so continue
                    ; falls through if the stack has overflowed
    overflow        lda     #$04            ; error value for stack overflow
                    jsr     ErrorLogger     ; call error logger
                    jmp     Quit            ; end program
endDecStackPtr      rts                     ;   
//______________________________________________________________________________________________________________________________________________________________________

//|================================|
//|    INCREMENT STACK POINTER     |
//|================================|
// PARAMETERS: X-Register should contain the sandboxStPtr value
// Description: increments stack pointer
//______________________________________________________________________________________________________________________________________________________________________
    incStackPtr     inx                     ; decrement pointer
                    cmp     #$FF            ; check if stack pointer is already at top
                    bne     endIncStackPtr  ; haven't underflowed so continue
                    ; falls through if the stack has underflowed
    underflow       lda     #$05            ; error value for stack underflow
                    jsr     ErrorLogger     ; call error logger
                    jmp     Quit            ; end program
endIncStackPtr      rts                     ;
//______________________________________________________________________________________________________________________________________________________________________

//|================================|
//|     PULL ICL FROM STACK        |
//|================================|
// PARAMETERS: ??
//______________________________________________________________________________________________________________________________________________________________________
    PullInst        ldx     sandboxStPtr    ; get pointer for sandbox stack
                    jsr     incStackPtr     ; move stack pointer up one position to read lo-byte
    pullHiByte      lda     sandboxStack,X  ; get lo-byte of ICL
                    sta     ICL             ; store lo-byte to ICL
                    jsr     incStackPtr     ; move stack pointer 
    pullLoByte      lda     sandboxStack,X  ; get hi-byte of ICL
                    sta     ICL+1           ; store hi-byte to ICL+1
                    stx     sandboxStPtr    ; store new value of pointer
endPullInst         rts                     ; END OF PUSHINST
//______________________________________________________________________________________________________________________________________________________________________

//|================================|
//|     UPDATE ICL             |
//|================================|
// PARAMETERS: None
//
//______________________________________________________________________________________________________________________________________________________________________
    UpdateICL       ldx     instByteLengthParam
    loop            lda     instSandbox,x           ; get byte from sandbox
                    lda     ICLParam_HB
                    sta     ICL+1
endUpdateICL    rts    
//______________________________________________________________________________________________________________________________________________________________________

//|================================|
//|     ERROR LOGGER               |
//|================================|
// PARAMETERS:
// - A Register contains error number
//______________________________________________________________________________________________________________________________________________________________________

    ErrorLogger     sta     error           ; store error in error memory location

//______________________________________________________________________________________________________________________________________________________________________