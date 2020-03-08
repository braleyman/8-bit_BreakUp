.target "6502"
.org $1000
.setting "OutputTxtAddressFormatFirst" = "{0:x04}: "
.setting "OutputTxtAddressFormatNext" = "{0:x04}: "

31   LMNEM    EQU   $2C
32   RTNL     EQU   $2C
35   RTNH     EQU   $2D
38   FORMAT   EQU   $2E
40   LENGTH   EQU   $2F
46   YSAV     EQU   $34
47   YSAV1    EQU   $35
48   CSWL     EQU   $36
49   CSWH     EQU   $37
52   PCL      EQU   $3A
53   PCH      EQU   $3B
54   XQT      EQU   $3C
59   A3L      EQU   $40
60   A3H      EQU   $41
65   ACC      EQU   $45
68   STATUS   EQU   $48
66   XREG     EQU   $46
67   YREG     EQU   $47
82   IRQLOC   EQU   $03FE
97   PADDL0   EQU   $C064
98   PTRIG    EQU   $C070


F879: 90 04     173  SCRN2    BCC   RTMSKZ     ;IF EVEN, USE LO H
F87B: 4A        174           LSR
F87C: 4A        175           LSR
F87D: 4A        176           LSR              ;SHIFT HIGH HALF BYTE DOWN
F87E: 4A        177           LSR
F87F: 29 0F     178  RTMSKZ   AND   #$0F       ;MASK 4-BITS
F881: 60        179           RTS
F882: A6 3A     180  INSDS1   LDX   PCL        ;PRINT PCL,H
F884: A4 3B     181           LDY   PCH
F886: 20 96 FD  182           JSR   PRYX2
F889: 20 48 F9  183           JSR   PRBLNK     ;FOLLOWED BY A BLANK
F88C: A1 3A     184           LDA   (PCL,X)    ;GET OP CODE
F88E: A8        185  INSDS2   TAY
F88F: 4A        186           LSR              ;EVEN/ODD TEST
F890: 90 09     187           BCC   IEVEN
F892: 6A        188           ROR              ;BIT 1 TEST
F893: B0 10     189           BCS   ERR        ;XXXXXX11 INVALID OP
F895: C9 A2     190           CMP   #$A2
F897: F0 0C     191           BEQ   ERR        ;OPCODE $89 INVALID
F899: 29 87     192           AND   #$87       ;MASK BITS
F89B: 4A        193  IEVEN    LSR              ;LSB INTO CARRY FOR L/R TEST
F89C: AA        194           TAX
F89D: BD 62 F9  195           LDA   FMT1,X     ;GET FORMAT INDEX BYTE
F8A0: 20 79 F8  196           JSR   SCRN2      ;R/L H-BYTE ON CARRY
F8A3: D0 04     197           BNE   GETFMT
F8A5: A0 80     198  ERR      LDY   #$80       ;SUBSTITUTE $80 FOR INVALID OPS
F8A7: A9 00     199           LDA   #$00       ;SET PRINT FORMAT INDEX TO 0
F8A9: AA        200  GETFMT   TAX
F8AA: BD A6 F9  201           LDA   FMT2,X     ;INDEX INTO PRINT FORMAT TABLE
F8AD: 85 2E     202           STA   FORMAT     ;SAVE FOR ADR FIELD FORMATTING
F8AF: 29 03     203           AND   #$03       ;MASK FOR 2-BIT LENGTH
                204                            ; (P=1 BYTE, 1=2 BYTE, 2=3 BYTE)
F8B1: 85 2F     205           STA   LENGTH
F8B3: 98        206           TYA              ;OPCODE
F8B4: 29 8F     207           AND   #$8F       ;MASK FOR 1XXX1010 TEST
F8B6: AA        208           TAX              ; SAVE IT
F8B7: 98        209           TYA              ;OPCODE TO A AGAIN
F8B8: A0 03     210           LDY   #$03
F8BA: E0 8A     211           CPX   #$8A
F8BC: F0 0B     212           BEQ   MNNDX3
F8BE: 4A        213  MNNDX1   LSR
F8BF: 90 08     214           BCC   MNNDX3     ;FORM INDEX INTO MNEMONIC TABLE
F8C1: 4A        215           LSR
F8C2: 4A        216  MNNDX2   LSR              ;1) 1XXX1010->00101XXX
F8C3: 09 20     217           ORA   #$20       ;2) XXXYYY01->00111XXX
F8C5: 88        218           DEY              ;3) XXXYYY10->00110XXX
F8C6: D0 FA     219           BNE   MNNDX2     ;4) XXXYY100->00100XXX
F8C8: C8        220           INY              ;5) XXXXX000->000XXXXX
F8C9: 88        221  MNNDX3   DEY
F8CA: D0 F2     222           BNE   MNNDX1
F8CC: 60        223           RTS

F8D0: 20 82 F8  225  INSTDSP  JSR   INSDS1     ;GEN FMT, LEN BYTES
F8D3: 48        226           PHA              ;SAVE MNEMONIC TABLE INDEX
F8D4: B1 3A     227  PRNTOP   LDA   (PCL),Y
F8D6: 20 DA FD  228           JSR   PRBYTE
F8D9: A2 01     229           LDX   #$01       ;PRINT 2 BLANKS
F8DB: 20 4A F9  230  PRNTBL   JSR   PRBL2
F8DE: C4 2F     231           CPY   LENGTH     ;PRINT INST (1-3 BYTES)
F8E0: C8        232           INY              ;IN A 12 CHR FIELD
F8E1: 90 F1     233           BCC   PRNTOP
F8E3: A2 03     234           LDX   #$03       ;CHAR COUNT FOR MNEMONIC PRINT
F8E5: C0 04     235           CPY   #$04
F8E7: 90 F2     236           BCC   PRNTBL
F8E9: 68        237           PLA              ;RECOVER MNEMONIC INDEX
F8EA: A8        238           TAY
F8EB: B9 C0 F9  239           LDA   MNEML,Y
F8EE: 85 2C     240           STA   LMNEM      ;FETCH 3-CHAR MNEMONIC
F8F0: B9 00 FA  241           LDA   MNEMR,Y    ;  (PACKED IN 2-BYTES)
F8F3: 85 2D     242           STA   RMNEM
F8F5: A9 00     243  PRMN1    LDA   #$00
F8F7: A0 05     244           LDY   #$05
F8F9: 06 2D     245  PRMN2    ASL   RMNEM      ;SHIFT 5 BITS OF
F8FB: 26 2C     246           ROL   LMNEM      ;  CHARACTER INTO A
F8FD: 2A        247           ROL              ;    (CLEARS CARRY)
F8FE: 88        248           DEY
F8FF: D0 F8     249           BNE   PRMN2
F901: 69 BF     250           ADC   #$BF       ;ADD "?" OFFSET
F903: 20 ED FD  251           JSR   COUT       ;OUTPUT A CHAR OF MNEM
F906: CA        252           DEX
F907: D0 EC     253           BNE   PRMN1
F909: 20 48 F9  254           JSR   PRBLNK     ;OUTPUT 3 BLANKS
F90C: A4 2F     255           LDY   LENGTH
F90E: A2 06     256           LDX   #$06       ;CNT FOR 6 FORMAT BITS
F910: E0 03     257  PRADR1   CPX   #$03
F912: F0 1C     258           BEQ   PRADR5     ;IF X=3 THEN ADDR.
F914: 06 2E     259  PRADR2   ASL   FORMAT
F916: 90 0E     260           BCC   PRADR3
F918: BD B3 F9  261           LDA   CHAR1-1,X
F91B: 20 ED FD  262           JSR   COUT
F91E: BD B9 F9  263           LDA   CHAR2-1,X
F921: F0 03     264           BEQ   PRADR3
F923: 20 ED FD  265           JSR   COUT
F926: CA        266  PRADR3   DEX
F927: D0 E7     267           BNE   PRADR1
F929: 60        268           RTS
F930: A5 2E     272  PRADR5   LDA   FORMAT
F932: C9 E8     273           CMP   #$E8       ;HANDLE REL ADR MODE
F934: B1 3A     274           LDA   (PCL),Y    ;SPECIAL (PRINT TARGET,
F936: 90 F2     275           BCC   PRADR4     ;  NOT OFFSET)
F938: 20 56 F9  276  RELADR   JSR   PCADJ3
F93B: AA        277           TAX              ;PCL,PCH+OFFSET+1 TO A,Y
F93C: E8        278           INX
F93D: D0 01     279           BNE   PRNTYX     ;+1 TO Y,X
F93F: C8        280           INY
F940: 98        281  PRNTYX   TYA
F941: 20 DA FD  282  PRNTAX   JSR   PRBYTE     ;OUTPUT TARGET ADR
F944: 8A        283  PRNTX    TXA              ;  OF BRANCH AND RETURN
F945: 4C DA FD  284           JMP   PRBYTE
F948: A2 03     285  PRBLNK   LDX   #$03       ;BLANK COUNT
F94A: A9 A0     286  PRBL2    LDA   #$A0       ;LOAD A SPACE
F94C: 20 ED FD  287  PRBL3    JSR   COUT       ;OUTPUT A BLANK
F94F: CA        288           DEX
F950: D0 F8     289           BNE   PRBL2      ;LOOP UNTIL COUNT=0
F952: 60        290           RTS

F954: A5 2F     292  PCADJ2   LDA   LENGTH     ;  2=3-BYTE
F956: A4 3B     293  PCADJ3   LDY   PCH
F958: AA        294           TAX              ;TEST DISPLACEMENT SIGN
F959: 10 01     295           BPL   PCADJ4     ;  (FOR REL BRANCH)
F95B: 88        296           DEY              ;EXTEND NEG BY DEC PCH
F95C: 65 3A     297  PCADJ4   ADC   PCL
F95E: 90 01     298           BCC   RTS2       ;PCL+LENGTH(OR DISPL)+1 TO A
F960: C8        299           INY              ;  CARRY INTO Y (PCH)
F961: 60        300  RTS2     RTS
301  * FMT1 BYTES:    XXXXXXY0 INSTRS
                302  * IF Y=0         THEN LEFT HALF BYTE
                303  * IF Y=1         THEN RIGHT HALF BYTE
                304  *                   (X=INDEX)
F962: 04 20 54  305  FMT1     DFB   $04,$20,$54,$30,$0D
F965: 30 0D
F967: 80 04 90  306           DFB   $80,$04,$90,$03,$22
F96A: 03 22
F96C: 54 33 0D  307           DFB   $54,$33,$0D,$80,$04
F96F: 80 04
F971: 90 04 20  308           DFB   $90,$04,$20,$54,$33
F974: 54 33
F976: 0D 80 04  309           DFB   $0D,$80,$04,$90,$04
F979: 90 04
F97B: 20 54 3B  310           DFB   $20,$54,$3B,$0D,$80
F97E: 0D 80
F980: 04 90 00  311           DFB   $04,$90,$00,$22,$44
F983: 22 44
F985: 33 0D C8  312           DFB   $33,$0D,$C8,$44,$00
F988: 44 00
F98A: 11 22 44  313           DFB   $11,$22,$44,$33,$0D
F98D: 33 0D
F98F: C8 44 A9  314           DFB   $C8,$44,$A9,$01,$22
F992: 01 22
F994: 44 33 0D  315           DFB   $44,$33,$0D,$80,$04
F997: 80 04
F999: 90 01 22  316           DFB   $90,$01,$22,$44,$33
F99C: 44 33
F99E: 0D 80 04  317           DFB   $0D,$80,$04,$90
F9A1: 90
F9A2: 26 31 87  318           DFB   $26,$31,$87,$9A ;$ZZXXXY01 INSTR'S
F9A5: 9A
F9A6: 00        319  FMT2     DFB   $00        ;ERR
F9A7: 21        320           DFB   $21        ;IMM
F9A8: 81        321           DFB   $81        ;Z-PAGE
F9A9: 82        322           DFB   $82        ;ABS
F9AA: 00        323           DFB   $00        ;IMPLIED
F9AB: 00        324           DFB   $00        ;ACCUMULATOR
F9AC: 59        325           DFB   $59        ;(ZPAG,X)
F9AD: 4D        326           DFB   $4D        ;(ZPAG),Y
F9AE: 91        327           DFB   $91        ;ZPAG,X
F9AF: 92        328           DFB   $92        ;ABS,X
F9B0: 86        329           DFB   $86        ;ABS,Y
F9B1: 4A        330           DFB   $4A        ;(ABS)
F9B2: 85        331           DFB   $85        ;ZPAG,Y
F9B3: 9D        332           DFB   $9D        ;RELATIVE
F9B4: AC A9 AC  333  CHAR1    ASC   ",),#($"
F9B7: A3 A8 A4
F9BA: D9 00 D8  334  CHAR2    DFB   $D9,$00,$D8,$A4,$A4,$00
F9BD: A4 A4 00
                335  *CHAR2: "Y",0,"X$$",0
                336  * MNEML IS OF FORM:
                337  *  (A) XXXXX000
                338  *  (B) XXXYY100
                339  *  (C) 1XXX1010
                340  *  (D) XXXYYY10
                341  *  (E) XXXYYY01
                342  *      (X=INDEX)
F9C0: 1C 8A 1C  343  MNEML    DFB   $1C,$8A,$1C,$23,$5D,$8B
F9C3: 23 5D 8B
F9C6: 1B A1 9D  344           DFB   $1B,$A1,$9D,$8A,$1D,$23
F9C9: 8A 1D 23
F9CC: 9D 8B 1D  345           DFB   $9D,$8B,$1D,$A1,$00,$29
F9CF: A1 00 29
F9D2: 19 AE 69  346           DFB   $19,$AE,$69,$A8,$19,$23
F9D5: A8 19 23
F9D8: 24 53 1B  347           DFB   $24,$53,$1B,$23,$24,$53
F9DB: 23 24 53
F9DE: 19 A1     348           DFB   $19,$A1    ;(A) FORMAT ABOVE
F9E0: 00 1A 5B  349           DFB   $00,$1A,$5B,$5B,$A5,$69
F9E3: 5B A5 69
F9E6: 24 24     350           DFB   $24,$24    ;(B) FORMAT
F9E8: AE AE A8  351           DFB   $AE,$AE,$A8,$AD,$29,$00
F9EB: AD 29 00
F9EE: 7C 00     352           DFB   $7C,$00    ;(C) FORMAT
F9F0: 15 9C 6D  353           DFB   $15,$9C,$6D,$9C,$A5,$69
F9F3: 9C A5 69
F9F6: 29 53     354           DFB   $29,$53    ;(D) FORMAT
F9F8: 84 13 34  355           DFB   $84,$13,$34,$11,$A5,$69
F9FB: 11 A5 69
F9FE: 23 A0     356           DFB   $23,$A0    ;(E) FORMAT
FA00: D8 62 5A  357  MNEMR    DFB   $D8,$62,$5A,$48,$26,$62
FA03: 48 26 62
FA06: 94 88 54  358           DFB   $94,$88,$54,$44,$C8,$54
FA09: 44 C8 54
FA0C: 68 44 E8  359           DFB   $68,$44,$E8,$94,$00,$B4
FA0F: 94 00 B4
FA12: 08 84 74  360           DFB   $08,$84,$74,$B4,$28,$6E
FA15: B4 28 6E
FA18: 74 F4 CC  361           DFB   $74,$F4,$CC,$4A,$72,$F2
FA1B: 4A 72 F2
FA1E: A4 8A     362           DFB   $A4,$8A    ;(A) FORMAT
FA20: 00 AA A2  363           DFB   $00,$AA,$A2,$A2,$74,$74
FA23: A2 74 74
FA26: 74 72     364           DFB   $74,$72    ;(B) FORMAT
FA28: 44 68 B2  365           DFB   $44,$68,$B2,$32,$B2,$00
FA2B: 32 B2 00
FA2E: 22 00     366           DFB   $22,$00    ;(C) FORMAT
FA30: 1A 1A 26  367           DFB   $1A,$1A,$26,$26,$72,$72
FA33: 26 72 72
FA36: 88 C8     368           DFB   $88,$C8    ;(D) FORMAT
FA38: C4 CA 26  369           DFB   $C4,$CA,$26,$48,$44,$44
FA3B: 48 44 44
FA3E: A2 C8     370           DFB   $A2,$C8    ;(E) FORMAT
FA40: FF FF FF  371           DFB   $FF,$FF,$FF
FA43: 20 D0 F8  372  STEP     JSR   INSTDSP    ;DISASSEMBLE ONE INST
FA46: 68        373           PLA              ;  AT (PCL,H)
FA47: 85 2C     374           STA   RTNL       ;ADJUST TO USER
FA49: 68        375           PLA              ;  STACK. SAVE
FA4A: 85 2D     376           STA   RTNH       ;  RTN ADR.
FA4C: A2 08     377           LDX   #$08
FA4E: BD 10 FB  378  XQINIT   LDA   INITBL-1,X ;INIT XEQ AREA
FA51: 95 3C     379           STA   XQT,X
FA53: CA        380           DEX
FA54: D0 F8     381           BNE   XQINIT
FA56: A1 3A     382           LDA   (PCL,X)    ;USER OPCODE BYTE
FA58: F0 42     383           BEQ   XBRK       ;SPECIAL IF BREAK
FA5A: A4 2F     384           LDY   LENGTH     ;LEN FROM DISASSEMBLY
FA5C: C9 20     385           CMP   #$20
FA5E: F0 59     386           BEQ   XJSR       ;HANDLE JSR, RTS, JMP,
FA60: C9 60     387           CMP   #$60       ;  JMP (), RTI SPECIAL
FA62: F0 45     388           BEQ   XRTS
FA64: C9 4C     389           CMP   #$4C
FA66: F0 5C     390           BEQ   XJMP
FA68: C9 6C     391           CMP   #$6C
FA6A: F0 59     392           BEQ   XJMPAT
FA6C: C9 40     393           CMP   #$40
FA6E: F0 35     394           BEQ   XRTI
FA70: 29 1F     395           AND   #$1F
FA72: 49 14     396           EOR   #$14
FA74: C9 04     397           CMP   #$04       ;COPY USER INST TO XEQ AREA
FA76: F0 02     398           BEQ   XQ2        ;  WITH TRAILING NOPS
FA78: B1 3A     399  XQ1      LDA   (PCL),Y    ;CHANGE REL BRANCH
FA7A: 99 3C 00  400  XQ2      STA   XQT,Y      ;  DISP TO 4 FOR
FA7D: 88        401           DEY              ;  JMP TO BRANCH OR
FA7E: 10 F8     402           BPL   XQ1        ;  NBRANCH FROM XEQ.
FA80: 20 3F FF  403           JSR   RESTORE    ;RESTORE USER REG CONTENTS.
FA83: 4C 3C 00  404           JMP   XQT        ;XEQ USER OP FROM RAM
FA86: 85 45     405  IRQ      STA   ACC        ;  (RETURN TO NBRANCH)
FA88: 68        406           PLA
FA89: 48        407           PHA              ;**IRQ HANDLER
FA8A: 0A        408           ASL
FA8B: 0A        409           ASL
FA8C: 0A        410           ASL
FA8D: 30 03     411           BMI   BREAK      ;TEST FOR BREAK
FA8F: 6C FE 03  412           JMP   (IRQLOC)   ;USER ROUTINE VECTOR IN RAM
FA92: 28        413  BREAK    PLP
FA93: 20 4C FF  414           JSR   SAV1       ;SAVE REG'S ON BREAK
FA96: 68        415           PLA              ;  INCLUDING PC
FA97: 85 3A     416           STA   PCL
FA99: 68        417           PLA
FA9A: 85 3B     418           STA   PCH
FA9C: 20 82 F8  419  XBRK     JSR   INSDS1     ;PRINT USER PC.
FA9F: 20 DA FA  420           JSR   RGDSP1     ;  AND REG'S
FAA2: 4C 65 FF  421           JMP   MON        ;GO TO MONITOR
FAA5: 18        422  XRTI     CLC
FAA6: 68        423           PLA              ;SIMULATE RTI BY EXPECTING
FAA7: 85 48     424           STA   STATUS     ;  STATUS FROM STACK, THEN RTS
FAA9: 68        425  XRTS     PLA              ;RTS SIMULATION
FAAA: 85 3A     426           STA   PCL        ;  EXTRACT PC FROM STACK
FAAC: 68        427           PLA              ;  AND UPDATE PC BY 1 (LEN=0)
FAAD: 85 3B     428  PCINC2   STA   PCH
FAAF: A5 2F     429  PCINC3   LDA   LENGTH     ;UPDATE PC BY LEN
FAB1: 20 56 F9  430           JSR   PCADJ3
FAB4: 84 3B     431           STY   PCH
FAB6: 18        432           CLC
FAB7: 90 14     433           BCC   NEWPCL
FAB9: 18        434  XJSR     CLC
FABA: 20 54 F9  435           JSR   PCADJ2     ;UPDATE PC AND PUSH
FABD: AA        436           TAX              ;  ONTO STACH FOR
FABE: 98        437           TYA              ;  JSR SIMULATE
FABF: 48        438           PHA
FAC0: 8A        439           TXA
FAC1: 48        440           PHA
FAC2: A0 02     441           LDY   #$02
FAC4: 18        442  XJMP     CLC
FAC5: B1 3A     443  XJMPAT   LDA   (PCL),Y
FAC7: AA        444           TAX              ;LOAD PC FOR JMP,
FAC8: 88        445           DEY              ;  (JMP) SIMULATE.
FAC9: B1 3A     446           LDA   (PCL),Y
FACB: 86 3B     447           STX   PCH
FACD: 85 3A     448  NEWPCL   STA   PCL
FACF: B0 F3     449           BCS   XJMP
FAD1: A5 2D     450  RTNJMP   LDA   RTNH
FAD3: 48        451           PHA
FAD4: A5 2C     452           LDA   RTNL
FAD6: 48        453           PHA
FAD7: 20 8E FD  454  REGDSP   JSR   CROUT      ;DISPLAY USER REG
FADA: A9 45     455  RGDSP1   LDA   #ACC       ;  CONTENTS WITH
FADC: 85 40     456           STA   A3L        ;  LABELS
FADE: A9 00     457           LDA   #ACC/256
FAE0: 85 41     458           STA   A3H
FAE2: A2 FB     459           LDX   #$FB
FAE4: A9 A0     460  RDSP1    LDA   #$A0
FAE6: 20 ED FD  461           JSR   COUT
FAE9: BD 1E FA  462           LDA   RTBL-$FB,X
FAEC: 20 ED FD  463           JSR   COUT
FAEF: A9 BD     464           LDA   #$BD
FAF1: 20 ED FD  465           JSR   COUT
FAF4: B5 4A     466           LDA   ACC+5,X
FAF6: 20 DA FD  467           JSR   PRBYTE
FAF9: E8        468           INX
FAFA: 30 E8     469           BMI   RDSP1
FAFC: 60        470           RTS

FB19: C1        486  RTBL     DFB   $C1
FB1A: D8        487           DFB   $D8
FB1B: D9        488           DFB   $D9
FB1C: D0        489           DFB   $D0
FB1D: D3        490           DFB   $D3
FB1E: AD 70 C0  491  PREAD    LDA   PTRIG      ;TRIGGER PADDLES
FB21: A0 00     492           LDY   #$00       ;INIT COUNT
FB23: EA        493           NOP              ;COMPENSATE FOR 1ST COUNT
FB24: EA        494           NOP
FB25: BD 64 C0  495  PREAD2   LDA   PADDL0,X   ;COUNT Y-REG EVERY
FB28: 10 04     496           BPL   RTS2D      ;  12 USEC
FB2A: C8        497           INY
FB2B: D0 F8     498           BNE   PREAD2     ;  EXIT AT 255 MAX
FB2D: 88        499           DEY
FB2E: 60        500  RTS2D    RTS
FBC1: 48        577  BASCALC  PHA              ;CALC BASE ADR IN BASL,H
FBC2: 4A        578           LSR              ;  FOR GIVEN LINE NO
FBC3: 29 03     579           AND   #$03       ;  0<=LINE NO.<=$17
FBC5: 09 04     580           ORA   #$04       ;ARG=000ABCDE, GENERATE
FBC7: 85 29     581           STA   BASH       ;  BASH=000001CD
FBC9: 68        582           PLA              ;  AND
FBCA: 29 18     583           AND   #$18       ;  BASL=EABAB000
FBCC: 90 02     584           BCC   BSCLC2
FBCE: 69 7F     585           ADC   #$7F
FBD0: 85 28     586  BSCLC2   STA   BASL
FBD2: 0A        587           ASL
FBD3: 0A        588           ASL
FBD4: 05 28     589           ORA   BASL
FBD6: 85 28     590           STA   BASL
FBD8: 60        591           RTS
FBF0: A4 24     603  STOADV   LDY   CH         ;CURSOR H INDEX TO Y-REG
FBF2: 91 28     604           STA   (BASL),Y   ;STORE CHAR IN LINE
FBF4: E6 24     605  ADVANCE  INC   CH         ;INCREMENT CURSOR H INDEX
FBF6: A5 24     606           LDA   CH         ;  (MOVE RIGHT)
FBF8: C5 21     607           CMP   WNDWDTH    ;BEYOND WINDOW WIDTH?
FBFA: B0 66     608           BCS   CR         ;  YES CR TO NEXT LINE
FBFC: 60        609  RTS3     RTS              ;  NO,RETURN
FBFD: C9 A0     610  VIDOUT   CMP   #$A0       ;CONTROL CHAR?
FBFF: B0 EF     611           BCS   STOADV     ;  NO,OUTPUT IT.
FC01: A8        612           TAY              ;INVERSE VIDEO?
FC02: 10 EC     613           BPL   STOADV     ;  YES, OUTPUT IT.
FC04: C9 8D     614           CMP   #$8D       ;CR?
FC06: F0 5A     615           BEQ   CR         ;  YES.
FC08: C9 8A     616           CMP   #$8A       ;LINE FEED?
FC0A: F0 5A     617           BEQ   LF         ;  IF SO, DO IT.
FC0C: C9 88     618           CMP   #$88       ;BACK SPACE? (CNTRL-H)
FC0E: D0 C9     619           BNE   BELL1      ;  NO, CHECK FOR BELL.
FC10: C6 24     620  BS       DEC   CH         ;DECREMENT CURSOR H INDEX
FC12: 10 E8     621           BPL   RTS3       ;IF POS, OK. ELSE MOVE UP
FC14: A5 21     622           LDA   WNDWDTH    ;SET CH TO WNDWDTH-1
FC16: 85 24     623           STA   CH
FC18: C6 24     624           DEC   CH         ;(RIGHTMOST SCREEN POS)
FC1A: A5 22     625  UP       LDA   WNDTOP     ;CURSOR V INDEX
FC1C: C5 25     626           CMP   CV
FC1E: B0 0B     627           BCS   RTS4       ;IF TOP LINE THEN RETURN
FC20: C6 25     628           DEC   CV         ;DEC CURSOR V-INDEX
FC22: A5 25     629  VTAB     LDA   CV         ;GET CURSOR V-INDEX
FC24: 20 C1 FB  630  VTABZ    JSR   BASCALC    ;GENERATE BASE ADR
FC27: 65 20     631           ADC   WNDLFT     ;ADD WINDOW LEFT INDEX
FC29: 85 28     632           STA   BASL       ;TO BASL
FC2B: 60        633  RTS4     RTS

FD8E: A9 8D     815  CROUT    LDA   #$8D
FD90: D0 5B     816           BNE   COUT
FD92: A4 3D     817  PRA1     LDY   A1H        ;PRINT CR,A1 IN HEX
FD94: A6 3C     818           LDX   A1L
FD96: 20 8E FD  819  PRYX2    JSR   CROUT
FD99: 20 40 F9  820           JSR   PRNTYX
FD9C: A0 00     821           LDY   #$00
FD9E: A9 AD     822           LDA   #$AD       ;PRINT '-'
FDA0: 4C ED FD  823           JMP   COUT
FDA3: A5 3C     824  XAM8     LDA   A1L
FDA5: 09 07     825           ORA   #$07       ;SET TO FINISH AT
FDA7: 85 3E     826           STA   A2L        ;  MOD 8=7
FDA9: A5 3D     827           LDA   A1H
FDAB: 85 3F     828           STA   A2H
FDAD: A5 3C     829  MODSCHK  LDA   A1L
FDAF: 29 07     830           AND   #$07
FDB1: D0 03     831           BNE   DATAOUT
FDB3: 20 92 FD  832  XAM      JSR   PRA1
FDB6: A9 A0     833  DATAOUT  LDA   #$A0
FDB8: 20 ED FD  834           JSR   COUT       ;OUTPUT BLANK
FDBB: B1 3C     835           LDA   (A1L),Y
FDBD: 20 DA FD  836           JSR   PRBYTE     ;OUTPUT BYTE IN HEX
FDC0: 20 BA FC  837           JSR   NXTA1
FDC3: 90 E8     838           BCC   MODSCHK    ;CHECK IF TIME TO,
FDC5: 60        839  RTS4C    RTS              ;  PRINT ADDR

FDDA: 48        852  PRBYTE   PHA              ;PRINT BYTE AS 2 HEX
FDDB: 4A        853           LSR              ;  DIGITS, DESTROYS A-REG
FDDC: 4A        854           LSR
FDDD: 4A        855           LSR
FDDE: 4A        856           LSR
FDDF: 20 E5 FD  857           JSR   PRHEXZ
FDE2: 68        858           PLA
FDE3: 29 0F     859  PRHEX    AND   #$0F       ;PRINT HEX DIG IN A-REG
FDE5: 09 B0     860  PRHEXZ   ORA   #$B0       ;  LSB'S
FDE7: C9 BA     861           CMP   #$BA
FDE9: 90 02     862           BCC   COUT
FDEB: 69 06     863           ADC   #$06
FDED: 6C 36 00  864  COUT     JMP   (CSWL)     ;VECTOR TO USER OUTPUT ROUTINE
FDF0: C9 A0     865  COUT1    CMP   #$A0
FDF2: 90 02     866           BCC   COUTZ      ;DON'T OUTPUT CTRL'S INVERSE
FDF4: 25 32     867           AND   INVFLG     ;MASK WITH INVERSE FLAG
FDF6: 84 35     868  COUTZ    STY   YSAV1      ;SAV Y-REG
FDF8: 48        869           PHA              ;SAV A-REG
FDF9: 20 FD FB  870           JSR   VIDOUT     ;OUTPUT A-REG AS ASCII
FDFC: 68        871           PLA              ;RESTORE A-REG
FDFD: A4 35     872           LDY   YSAV1      ;  AND Y-REG
FDFF: 60        873           RTS              ;  THEN RETURN

FF3F: A5 48     1026 RESTORE  LDA   STATUS     ;RESTORE 6502 REG CONTENTS
FF41: 48        1027          PHA              ;  USED BY DEBUG SOFTWARE
FF42: A5 45     1028          LDA   ACC
FF44: A6 46     1029 RESTR1   LDX   XREG
FF46: A4 47     1030          LDY   YREG
FF48: 28        1031          PLP
FF49: 60        1032          RTS

FF4C: 86 46     1034 SAV1     STX   XREG
FF4E: 84 47     1035          STY   YREG
FF50: 08        1036          PHP
FF51: 68        1037          PLA
FF52: 85 48     1038          STA   STATUS
FF54: BA        1039          TSX
FF55: 86 49     1040          STX   SPNT
FF57: D8        1041          CLD
FF58: 60        1042          RTS

FF65: D8        1047 MON      CLD              ;MUST SET HEX MODE!
FF66: 20 3A FF  1048          JSR   BELL
FF69: A9 AA     1049 MONZ     LDA   #$AA       ;'*' PROMPT FOR MON
FF6B: 85 33     1050          STA   PROMPT
FF6D: 20 67 FD  1051          JSR   GETLNZ     ;READ A LINE
FF70: 20 C7 FF  1052          JSR   ZMODE      ;CLEAR MON MODE, SCAN IDX
FF73: 20 A7 FF  1053 NXTITM   JSR   GETNUM     ;GET ITEM, NON-HEX
FF76: 84 34     1054          STY   YSAV       ;  CHAR IN A-REG
FF78: A0 17     1055          LDY   #$17       ;  X-REG=0 IF NO HEX INPUT
FF7A: 88        1056 CHRSRCH  DEY
FF7B: 30 E8     1057          BMI   MON        ;NOT FOUND, GO TO MON
FF7D: D9 CC FF  1058          CMP   CHRTBL,Y   ;FIND CMND CHAR IN TEL
FF80: D0 F8     1059          BNE   CHRSRCH
FF82: 20 BE FF  1060          JSR   TOSUB      ;FOUND, CALL CORRESPONDING
FF85: A4 34     1061          LDY   YSAV       ;  SUBROUTINE
FF87: 4C 73 FF  1062          JMP   NXTITM
FF8A: A2 03     1063 DIG      LDX   #$03
FF8C: 0A        1064          ASL
FF8D: 0A        1065          ASL              ;GOT HEX DIG,
FF8E: 0A        1066          ASL              ;  SHIFT INTO A2
FF8F: 0A        1067          ASL
FF90: 0A        1068 NXTBIT   ASL
FF91: 26 3E     1069          ROL   A2L
FF93: 26 3F     1070          ROL   A2H
FF95: CA        1071          DEX              ;LEAVE X=$FF IF DIG
FF96: 10 F8     1072          BPL   NXTBIT
FF98: A5 31     1073 NXTBAS   LDA   MODE
FF9A: D0 06     1074          BNE   NXTBS2     ;IF MODE IS ZERO
FF9C: B5 3F     1075          LDA   A2H,X      ; THEN COPY A2 TO
FF9E: 95 3D     1076          STA   A1H,X      ; A1 AND A3
FFA0: 95 41     1077          STA   A3H,X
FFA2: E8        1078 NXTBS2   INX
FFA3: F0 F3     1079          BEQ   NXTBAS
FFA5: D0 06     1080          BNE   NXTCHR
FFA7: A2 00     1081 GETNUM   LDX   #$00       ;CLEAR A2
FFA9: 86 3E     1082          STX   A2L
FFAB: 86 3F     1083          STX   A2H
FFAD: B9 00 02  1084 NXTCHR   LDA   IN,Y       ;GET CHAR
FFB0: C8        1085          INY
FFB1: 49 B0     1086          EOR   #$B0
FFB3: C9 0A     1087          CMP   #$0A
FFB5: 90 D3     1088          BCC   DIG        ;IF HEX DIG, THEN
FFB7: 69 88     1089          ADC   #$88
FFB9: C9 FA     1090          CMP   #$FA
FFBB: B0 CD     1091          BCS   DIG
FFBD: 60        1092          RTS