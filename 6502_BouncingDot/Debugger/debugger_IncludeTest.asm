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

.include "Debugger/ML_Routines/SaveState.asm"