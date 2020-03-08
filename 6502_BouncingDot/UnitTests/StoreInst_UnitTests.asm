.target "6502"
.org $1000
.setting "OutputTxtAddressFormatFirst" = "{0:x04}: "
.setting "OutputTxtAddressFormatNext" = "{0:x04}: "

ICL                 .equ    $06
ICidx               .equ    $19
instSandbox         .equ    $9000
instByteLengthParam .equ    $08

; TEST: GET AND STORE OPCODE AND INSTRUCTIONS

; ICidx = 0
; instSandbox cleared
; ICL and ICL+1 set to address $0300
; save test instruction at 0300: AD 03 03
; bytes 0300-0302 should appear at 9000-9002
; ICidx should = 03 when complete

; RESULTS: 02/24/2020
; 9000: AD 03 03 (PASSED)
; ICidx = $03 (PASSED)

; 3 byte test
/* lda #$00
sta ICidx
sta instSandbox
sta instSandbox+1
sta instSandbox+2
sta ICL
lda #$03
sta instByteLengthParam
lda #$03
sta ICL+1
lda #$AD
sta $0300
lda #$03
sta $0301
lda #$03
sta $0302
jsr StoreInst
rts */

; ICidx = 0
; instSandbox cleared
; ICL and ICL+1 set to address $0300
; save test instruction at 0300: A9 54
; bytes 0300-0301 should appear at 9000-9001
; ICidx should = 02 when complete

; RESULTS: 02/25/2020
; 9000: A9 54 (PASSED)
; ICidx = $02 (PASSED)

;2 byte test
/* lda #$00
sta ICidx
sta instSandbox
sta instSandbox+1
sta instSandbox+2
sta ICL
lda #$02
sta instByteLengthParam
lda #$03
sta ICL+1
lda #$A9
sta $0300
lda #$54
sta $0301
jsr StoreInst
rts */

; ICidx = 0
; instSandbox cleared
; ICL and ICL+1 set to address $0300
; save test instruction at 0300: EA
; byte 0300 should appear at 9000
; ICidx should = 01 when complete

; RESULTS: 02/25/2020
; 9000: EA (PASSED)
; ICidx = $01 (PASSED)

;1 byte test
lda #$00
sta ICidx
sta instSandbox
sta instSandbox+1
sta instSandbox+2
sta ICL
lda #$01
sta instByteLengthParam
lda #$03
sta ICL+1
lda #$EA
sta $0300
jsr StoreInst
rts

StoreInst           ldy     ICidx                   ; 
                    ldx     #$00                    ; x is loop counter
    instLoop        lda     (ICL),Y                 ; get next byte of instruction
                    sta     instSandbox,X           ; store in next instSandbox location
                    iny                             ;
                    inx                             ;
                    cpx     instByteLengthParam     ; have we reached end of instruction
                    bcc     instLoop                ; read in next instruction byte
                    ; fall through here if done reading instruction bytes
                    sty     ICidx                   ; store current ICidx
endReadInst         rts                             ; END OF READ INSTRUCTION ROUTINE
            