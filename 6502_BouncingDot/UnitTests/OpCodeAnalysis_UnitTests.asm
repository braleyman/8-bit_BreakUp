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

// SET SANDBOX STACK
lda #$FF
sta sandboxStPtr

// SET FLAGS AND SUCH
lda #$00
sta impliedInstFlag

lda #$00
sta OddEvenFlag

lda #$00
sta jsrCount

lda #$00
sta addressingType

/* lda #$01
sta impliedInstFlag */
lda #$01
sta executeFlag

// CLEAR DUMMY PROGRAM AND SANDBOX LOCATIONS
lda #$00
sta $8FF3
sta $0300
sta $0301
sta $0302
sta $9000
sta $9001
sta $9002

// SET DUMMY PROGRAM
/* lda #$02
sta ICL
lda #$03
sta ICL+1 */
lda #$65
sta $9000
/* lda #$50
sta $9001
lda #$04
sta $9002 */

// ABSOLUTE ADDRESSING CHECKS

// TEST SETUP:
//  - Implied Flag to 0
//  - Odd Even Flag to 0
//  - addressingType to 0
//  - 9000: xx 50 04
// TEST RESULTS:
//
// Absolute Addressing
// AD - PASSED
// 6D - PASSED
// 2D - PASSED
// 0E - PASSED
// 2C - PASSED
// CD - PASSED
// EC - PASSED
// CC - PASSED
// CE - PASSED
// 4D - PASSED
// EE - PASSED
// 4C - PASSED
// 20 - PASSED
//
// Implied Addressing (Only need to test 1 because the routine just looks for the implied instruction flag to be set.)
// 18 - PASSED
//
// Zero Page Addressing
// 65 - PASSED
// 25 - PASSED
// 06 - PASSED
// 24 - PASSED
// C5 - PASSED
// 45 - PASSED
// E6 - PASSED
//
// Zero Page,X
// 75 - PASSED
// 35 - PASSED
// D5 - PASSED
// F6 - PASSED
//
// Absolute, X
// 7D - PASSED
// DD - PASSED
// 3E - PASSED
// 3C - PASSED(65c02)
// BC - PASSED
//
// Absolute, Y
// B9 - PASSED
// 19 - PASSED
// 59 - PASSED
// F9 - PASSED
//
// (Indirect, X)
// 61 - PASSED
// 21 - PASSED
// C1 - PASSED
// 41 - PASSED
// A1 - PASSED
// 81 - PASSED
//
// (Indirect),Y
// B1 - PASSED
// 71 - PASSED
// 31 - PASSED
// D1 - PASSED
// 11 - PASSED
//
// Immediate
// A9 - PASSED
// E9 - PASSED
// A0 - PASSED
// 49 - PASSED
// 29 - PASSED
//
// Relative
// 50 - PASSED
// 90 - PASSED
// B0 - PASSED
// 30 - PASSED
// 10 - PASSED
// 70 - PASSED

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


// DUMMY END POINTS
addressingSetup     rts
Quit                rts

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

//|================================|
//|     CHECK LAST INSTRUCTION     |
//|================================|
//
// Description: Should be called as needed to determine if last instruction of program being debugged has been reached.
// Dependencies: jsrCount
//              - WILL EXIT IF jsrCount is greater than zero.
//              - this is to prevent accidentally comparing the ICL to the lastInst value when the ICL is currently
//                stepping through a routine outside of the main program being debugged.
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
//|     UPDATE ICL                 |
//|================================|
// PARAMETERS:
//
// - ICLParam_LB
// - ICLParam_HB
//______________________________________________________________________________________________________________________________________________________________________
    UpdateICL       lda     ICLParam_LB
                    sta     ICL
                    lda     ICLParam_HB
                    sta     ICL+1
endUpdateICL        rts


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
                    sta     OddEvenFlag     ; set OddEvenFlag to odd
                    bne     endOddOrEvenLB  ; (ALWAYS)
    setEvenLB       lda     #$00            ;
                    sta     OddEvenFlag     ; set OddEvenFlag to even
endOddOrEvenLB      rts                     ; end ODD OR EVEN Lo-Byte

//|================================|
//|     ERROR LOGGER               |
//|================================|
// PARAMETERS:
// - A Register contains error number
//______________________________________________________________________________________________________________________________________________________________________

    ErrorLogger     sta     error           ; store error in error memory location
                    rts