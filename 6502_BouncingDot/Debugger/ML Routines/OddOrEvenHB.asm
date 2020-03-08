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
