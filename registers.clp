; iris
; Copyright (c) 2013-2019, Joshua Scoggins and Contributors
; All rights reserved.
; 
; Redistribution and use in source and binary forms, with or without
; modification, are permitted provided that the following conditions are met:
;     * Redistributions of source code must retain the above copyright
;       notice, this list of conditions and the following disclaimer.
;     * Redistributions in binary form must reproduce the above copyright
;       notice, this list of conditions and the following disclaimer in the
;       documentation and/or other materials provided with the distribution.
; 
; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
; ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(definstances registers
              (r0 of register) (r1 of register) (r2 of register)
              (r3 of register) (r4 of register) (r5 of register)
              (r6 of register) (r7 of register) (r8 of register)
              (r9 of register) (r10 of register) (r11 of register)
              (r12 of register) (r13 of register) (r14 of register)
              (r15 of register) (r16 of register) (r17 of register)
              (r18 of register) (r19 of register) (r20 of register)
              (r21 of register) (r22 of register) (r23 of register)
              (r24 of register) (r25 of register) (r26 of register)
              (r27 of register) (r28 of register) (r29 of register)
              (r30 of register) (r31 of register) (r32 of register)
              (r33 of register) (r34 of register) (r35 of register)
              (r36 of register) (r37 of register) (r38 of register)
              (r39 of register) (r40 of register) (r41 of register)
              (r42 of register) (r43 of register) (r44 of register)
              (r45 of register) (r46 of register) (r47 of register)
              (r48 of register) (r49 of register) (r50 of register)
              (r51 of register) (r52 of register) (r53 of register)
              (r54 of register) (r55 of register) (r56 of register)
              (r57 of register) (r58 of register) (r59 of register)
              (r60 of register) (r61 of register) (r62 of register)
              (r63 of register) (r64 of register) (r65 of register)
              (r66 of register) (r67 of register) (r68 of register)
              (r69 of register) (r70 of register) (r71 of register)
              (r72 of register) (r73 of register) (r74 of register)
              (r75 of register) (r76 of register) (r77 of register)
              (r78 of register) (r79 of register) (r80 of register)
              (r81 of register) (r82 of register) (r83 of register)
              (r84 of register) (r85 of register) (r86 of register)
              (r87 of register) (r88 of register) (r89 of register)
              (r90 of register) (r91 of register) (r92 of register)
              (r93 of register) (r94 of register) (r95 of register)
              (r96 of register) (r97 of register) (r98 of register)
              (r99 of register) (r100 of register) (r101 of register)
              (r102 of register) (r103 of register) (r104 of register)
              (r105 of register) (r106 of register) (r107 of register)
              (r108 of register) (r109 of register) (r110 of register)
              (r111 of register) (r112 of register) (r113 of register)
              (r114 of register) (r115 of register) (r116 of register)
              (r117 of register) (r118 of register) (r119 of register)
              (r120 of register) (r121 of register) (r122 of register)
              (r123 of register) (r124 of register) (r125 of register)
              (r126 of register) (r127 of register) (r128 of register)
              (r129 of register) (r130 of register) (r131 of register)
              (r132 of register) (r133 of register) (r134 of register)
              (r135 of register) (r136 of register) (r137 of register)
              (r138 of register) (r139 of register) (r140 of register)
              (r141 of register) (r142 of register) (r143 of register)
              (r144 of register) (r145 of register) (r146 of register)
              (r147 of register) (r148 of register) (r149 of register)
              (r150 of register) (r151 of register) (r152 of register)
              (r153 of register) (r154 of register) (r155 of register)
              (r156 of register) (r157 of register) (r158 of register)
              (r159 of register) (r160 of register) (r161 of register)
              (r162 of register) (r163 of register) (r164 of register)
              (r165 of register) (r166 of register) (r167 of register)
              (r168 of register) (r169 of register) (r170 of register)
              (r171 of register) (r172 of register) (r173 of register)
              (r174 of register) (r175 of register) (r176 of register)
              (r177 of register) (r178 of register) (r179 of register)
              (r180 of register) (r181 of register) (r182 of register)
              (r183 of register) (r184 of register) (r185 of register)
              (r186 of register) (r187 of register) (r188 of register)
              (r189 of register) (r190 of register) (r191 of register)
              (r192 of register) (r193 of register) (r194 of register)
              (r195 of register) (r196 of register) (r197 of register)
              (r198 of register) (r199 of register) (r200 of register)
              (r201 of register) (r202 of register) (r203 of register)
              (r204 of register) (r205 of register) (r206 of register)
              (r207 of register) (r208 of register) (r209 of register)
              (r210 of register) (r211 of register) (r212 of register)
              (r213 of register) (r214 of register) (r215 of register)
              (r216 of register) (r217 of register) (r218 of register)
              (r219 of register) (r220 of register) (r221 of register)
              (r222 of register) (r223 of register) (r224 of register)
              (r225 of register) (r226 of register) (r227 of register)
              (r228 of register) (r229 of register) (r230 of register)
              (r231 of register) (r232 of register) (r233 of register)
              (r234 of register) (r235 of register) (r236 of register)
              (r237 of register) (r238 of register) (r239 of register)
              (r240 of register) (r241 of register) (r242 of register)
              (r243 of register) (r244 of register) (r245 of register)
              (r246 of register) (r247 of register) (r248 of register)
              (r249 of register) (r250 of register) (r251 of register)
              (r252 of register) (r253 of register) (r254 of register)
              (r255 of register))

