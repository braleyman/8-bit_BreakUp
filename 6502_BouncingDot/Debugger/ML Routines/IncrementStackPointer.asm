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
