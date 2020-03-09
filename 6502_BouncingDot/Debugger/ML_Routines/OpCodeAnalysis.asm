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
