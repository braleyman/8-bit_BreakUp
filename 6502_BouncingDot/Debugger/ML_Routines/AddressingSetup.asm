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
