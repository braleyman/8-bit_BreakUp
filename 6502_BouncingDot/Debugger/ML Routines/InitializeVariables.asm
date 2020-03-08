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
