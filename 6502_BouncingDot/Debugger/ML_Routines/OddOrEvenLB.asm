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
