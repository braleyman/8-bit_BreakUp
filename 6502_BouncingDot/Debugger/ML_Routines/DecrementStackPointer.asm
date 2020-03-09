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
