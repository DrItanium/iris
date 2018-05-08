\ z80 assembler words
\ taken from the Threaded Interpretive Languages book
: cconstant create c, does> c@ ;

: <builds 0 constant ;

get-current ( )
vocabulary z80 also z80 definitions
: 8* ( a -- 8a ) 2* 2* 2* ;
0x0 cconstant B
0x1 cconstant C
0x2 cconstant D
0x3 cconstant E
0x4 cconstant H
0x5 cconstant L
0x6 cconstant M
0x7 cconstant A
0x00 cconstant BC
0x10 cconstant DE
0x20 cconstant HL
0x30 cconstant AF
0x30 cconstant SP
0x00 cconstant NZ
0x08 cconstant Z
0x10 cconstant NC
0x18 cconstant CY
0x20 cconstant PO
0x28 cconstant PE
0x30 cconstant P
0x38 cconstant N

: @X ( -- 8007 ) 0xDD c, 0x8007 ;
: @Y ( -- 8007 ) 0xFD c, 0x8007 ;

: IX ( -- HL ) 0xDD c, HL ;
: IY ( -- HL ) 0xFD c, HL ;



: 1byte <builds c, does> c@ c, ;
0x3f 1byte ccf, \ complement carry flag
0xaf 1byte cla, \ clear accumulator { xor A }
0x2f 1byte cpl,  \ complement accumulator (1's complement)
0x27 1byte daa, \ decimal adjust accumulator
0xf3 1byte dsi, \ disable interrupts
0xfb 1byte eni  \ enable interrupts
0x76 1byte hlt, \ halt
0x00 1byte nop, \ nop
0xa7 1byte rcf, \ reset carry flag
0x37 1byte scf, \ set carry flag
0xc9 1byte ret, \ return from subroutine
0x08 1byte xaa, \ exchange af and af'
0xd9 1byte xal, \ exchange all three register pairs
0xeb 1byte xdh, \ exchange de and hl

: 2bytes <builds c, does> ED c, c@ c, ;

0x46 2bytes im0, \ set interrupt mode 0
0x56 2bytes im1, \ set interrupt mode 1
0x5e 2bytes im2, \ set interrupt mode 2
0x44 2bytes neg, \ complement A (2's)
0x4d 2bytes rti, \ return from interrupt
0x45 2bytes rtn, \ return from non-maskable interrupt
0x6f 2bytes rld, \ rotate left digit
0x67 2bytes rrd, \ rotate right digit
0x57 2bytes lai, \ a = i
0x5F 2bytes lar, \ a = r
0x4f 2bytes lra, \ r = a
0x47 2bytes lia, \ i = a

set-current
previous
