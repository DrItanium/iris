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
(defrule process-register-translation
         (stage (current parse-knowledge-graph))
         ?f <- (decl $?before ?a -> ?b)
         =>
         (retract ?f)
         (if (> (length$ ?before) 0) then
           (assert (decl $?before ?a)))
         (make-instance ?a of tagged-component
                        (target ?b)
                        (tags)))

(deffacts register-translations
          (decl zero -> r0)
          (decl t0 -> tmp0 -> temp0 -> temporary0 -> r1)
          (decl t1 -> tmp1 -> temp1 -> temporary1 -> r2)
          (decl t2 -> tmp2 -> temp2 -> temporary2 -> r3)
          (decl t3 -> tmp3 -> temp3 -> temporary3 -> r4)
          (decl t4 -> tmp4 -> temp4 -> temporary4 -> r5)
          (decl t5 -> tmp5 -> temp5 -> temporary5 -> r6)
          (decl t6 -> tmp6 -> temp6 -> temporary6 -> r7)
          (decl t7 -> tmp7 -> temp7 -> temporary7 -> r8)
          (decl stack-pointer -> sp -> r9)
          (decl pred-bits -> all-preds -> r10)
          (decl ctr -> count -> r11)
          (decl loc0 -> local0 -> r12)
          (decl loc1 -> local1 -> r13)
          (decl loc2 -> local2 -> r14)
          (decl loc3 -> local3 -> r15)
          (decl loc4 -> local4 -> r16)
          (decl loc5 -> local5 -> r17)
          (decl loc6 -> local6 -> r18)
          (decl loc7 -> local7 -> r19)
          (decl link-backup -> lrbak -> r20)
          (decl sys0 -> system0 -> r248) 
          (decl sys1 -> system1 -> r249) 
          (decl sys2 -> system2 -> r250) 
          (decl sys3 -> system3 -> r251) 
          (decl sys4 -> system4 -> r252) 
          (decl sys5 -> system5 -> r253)
          (decl sys6 -> system6 -> r254)
          (decl sys7 -> system7 -> r255)
          (decl pfalse -> p1)
          (decl ptrue -> p0))
