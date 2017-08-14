;-----------------------------------------------------------------------------
; An implementation of the language specified by Charles H. Moore's book on
; a Problem orientied langauge.
;-----------------------------------------------------------------------------
; Fixed registers
;	- InstructionPoiner : r255 (aka ip)
;	- LinkRegister: r254 (aka lr)
;	- StackPointer: r253 (aka sp)
;	- Parameter Stack Top: r252 (aka ptop)
;	- Parameter Stack End: r251 (aka pend)
;	- Return Stack Top: r250 (aka rtop)
;	- Return Stack End: r249 (aka rend)
;	These are used internally for passing arguments around to leaf functions
;	- Safe Param 0: r248 (spar0)
;	- Safe Param 1: r247 (spar1)
;	- Safe Param 2: r246 (spar2)
;	- Safe Param 3: r245 (spar3)
;	- Safe Result 0: r244 (sres0)
;	- Safe Result 1: r243 (sres1)
;	- Safe Result 2: r242 (sres2)
;	- Safe Result 3: r241 (sres3)
;	- Safe Temporary 0: r240 (stmp0)
;	- Safe Temporary 1: r239 (stmp1)
;	- Safe Temporary 2: r238 (stmp2)
;	- Safe Temporary 3: r237 (stmp3)
;	- ASCII Space: r236 (space)
;	- Safe Temporary 4: r235 (stmp4)
;	- Safe Temporary 5: r234 (stmp5)
;	- Safe Temporary 6: r233 (stmp6)
;	- Max Word Length: r232 (wlen)
;	- Safe Temporary 7: r231 (stmp7)
;	- Zero register: r230 (zero)
;	- FETCH / DEPOSIT current: r229 (fdcur)
;	- input pointer: r228 (inptr)
;	- output pointer: r227 (outptr)
;	- is negative : r226
;	- nil registers : r225
;-----------------------------------------------------------------------------
; Parameter passing conventions
;  If the given subroutine calls another subroutine then you need to save
;  the current lr onto the return stack, otherwise you can just use it.
;  lr will always be populated in the case of subroutine calls
;-----------------------------------------------------------------------------
; setup the parameter and return stack pointers inside stack memory
;-----------------------------------------------------------------------------
; First setup the memory addresses using the org directive so that it is easy
; to keep track of
;-----------------------------------------------------------------------------
; Startup the machine and perform initial partitioning and register layout
; The above register layouts are a suggestion not exact, I'll rewrite them as I go on!
;-----------------------------------------------------------------------------
(alias r0 as scratch-register0)
(alias r1 as scratch-register1)
(alias r2 as scratch-register2)
(alias r3 as scratch-register3)
(alias r4 as scratch-register4)
(alias r5 as scratch-register5)
(alias r6 as scratch-register6)
(alias r7 as scratch-register7)

(alias scratch-register0 as scratch0)
(alias scratch-register1 as scratch1)
(alias scratch-register2 as scratch2)
(alias scratch-register3 as scratch3)
(alias scratch-register4 as scratch4)
(alias scratch-register5 as scratch5)
(alias scratch-register6 as scratch6)
(alias scratch-register7 as scratch7)

(alias r32 as global-register0)
(alias r33 as global-register1)
(alias r34 as global-register2)
(alias r35 as global-register3)

(alias global-register0 as g0)
(alias global-register1 as g1)
(alias global-register2 as g2)
(alias global-register3 as g3)

(alias r192 as internal-register0)
(alias r191 as internal-register1)
(alias r190 as internal-register2)
(alias r189 as internal-register3)
(alias r187 as internal-register4)
(alias r186 as internal-register5)
(alias r185 as internal-register6)

(alias internal-register0 as fixed-purpose-register0)
(alias internal-register1 as fixed-purpose-register1)
(alias internal-register2 as fixed-purpose-register2)
(alias internal-register3 as fixed-purpose-register3)
(alias internal-register4 as fixed-purpose-register4)
(alias internal-register5 as fixed-purpose-register5)
(alias internal-register6 as fixed-purpose-register6)

(alias fixed-purpose-register0 as fetch-deposit-current)
(alias fetch-deposit-current as fdcurr)

(alias fixed-purpose-register1 as input-pointer)
(alias input-pointer as inptr)

(alias fixed-purpose-register2 as output-pointer)
(alias output-pointer as outptr)

(alias fixed-purpose-register3 as ascii-space)

(alias fixed-purpose-register4 as max-word-length)
(alias max-word-length as wlen)

; Used during the dictionary search
(alias fixed-purpose-register5 as context-register)
(alias context-register as context)

; Used to walk any code segments we are curious about
(alias fixed-purpose-register6 as interpreter-location)
(alias interpreter-location as il)

; implied register aliases
(alias arg0 as sarg0)
(alias arg1 as sarg1)
(alias arg2 as sarg2)
(alias arg3 as sarg3)
(alias ret0 as sres0)
(alias ret1 as sres1)
(alias ret2 as sres2)
(alias ret3 as sres3)
(alias scratch-register0 as safe-temp0)
(alias scratch-register1 as safe-temp1)
(alias scratch-register2 as safe-temp2)
(alias scratch-register3 as safe-temp3)
(alias safe-temp0 as stmp0)
(alias safe-temp1 as stmp1)
(alias safe-temp2 as stmp2)
(alias safe-temp3 as stmp3)

(alias cond0-true as is-number)
(alias cond0-false as is-not-number)
(alias cond1-true as scratch-true)
(alias cond1-false as scratch-false)


(alias scratch0 as top)
(alias scratch1 as second)

(let InternalStackStart be 0xFFFF)
(let DataStackStart be 0x3FFF)
(let CallStackStart be 0x7FFF)
(let SpaceChar be 0x20)
(let WordLength be 62)


(section code
         (org ZeroAddress
              (label Startup
                     (set zero
                          0x0000)
                     (set sp
                          InternalStackStart)
                     (set cs
                          CallStackStart)
                     (set ds
                          DataStackStart)
                     (clear context)
                     ; TODO: more registers to setup
                     )
              (loop DONE
                    ; top of our control loop
                     (set sarg0
                          WordBuffer)  ; load the front fo the word buffer
                     (NUMBER)     ; Check and see if we got a number from this input
                     (when is-not-number
                           goto
                           ParseWord) ; If the is-not-number predicate register is true then it must be a word
                     (goto PrintResult)        ; Print the number
                     (label ParseWord
                            (set sarg0
                                 WordBuffer)
                            (WORD)) ; read the next word
                     (label PrintResult
                            (Print)       ; print it out
                            (set sarg0
                                 NewlineChar)
                            (Print)))       ; add a newline
              (func Print
                    ;-----------------------------------------------------------------------------
                    ; Prints a string character by character until we see a \0
                    ; Inputs:
                    ;	sarg0 - what to print out
                    ;-----------------------------------------------------------------------------
                    (loop PrintLoop
                           (ld scratch0
                               sarg0)        ; load the current char from memory
                           (is-zero scratch-true
                                    scratch-false
                                    scratch0) ; first check to see if we should stop printing (zero means stop)
                           (when scratch-true
                             goto
                             PrintDone)
                           ; this will cause many more cycles to be performed on unnecessary sets but it is more readable so that wins out
                           (putc scratch0) ; write it into io memory at the PutC port
                           (incr sarg0))
                    (label PrintDone))
              (func AtVerb
                     ;-----------------------------------------------------------------------------
                     ; Treat the top of the stack as an address and loads its contents in place of
                     ; the original address
                     ;-----------------------------------------------------------------------------
                     (pop top ds)   ; get the top element
                     (ld top
                         top)
                     (push ds
                           top))
              (func EqualsVerb
                     ;-----------------------------------------------------------------------------
                     ; Store second in memory at the address stored in top
                     ;-----------------------------------------------------------------------------
                     (pop top
                          ds) ; get the top element (address)
                     (pop second
                          ds) ; get the second element (value)
                     (st top
                         second)) ; data[top] = second
              (label ERROR
                     ;-----------------------------------------------------------------------------
                     ; Takes in the offending word, an error message, prints them, then clears the
                     ; stacks and then calls DONE.
                     ; Inputs are on r32, r33 for this routine
                     ;   sarg0 - word that did a bad thing
                     ;   sarg1 - error type
                     ;-----------------------------------------------------------------------------
                     ; error routine when something goes wrong!
                     (set ds
                          DataStackStart) ; overwrite the current parameter stack location with the bottom
                     (set cs
                          CallStackStart) ; overwrite the current return stack location with the bottom
                     (Print)          ; print the offending word that did the bad thing
                     (copy sarg0
                           sarg1)         ; need to print the error type so setup the arguments
                     (Print)
                     (goto DONE))          ; we're done
              (label Shutdown
                     ; End the program and shutdown the machine
                     (label Die
                            (terminate)))
              (func WORD
                    ;-----------------------------------------------------------------------------
                    ; WORD: Read the next word in the input
                    ;	sarg0 - pointer to temporary storage to save the current word
                    ;-----------------------------------------------------------------------------
                    (move scratch2 sarg0)                  ; Copy the pointer address to temporary storage so we can mess with it
                    (set scratch3 WORD_read_data)          ; where to jump if we see a space
                    (set scratch4 WORD_reassign_jumps)     ; where to jump to when wanting to handle storage
                    (clear scratch5)                        ; The number of characters in the word
                    (loop WORD_read_data
                          (getc scratch0)
                          (eq scratch-true
                              scratch-false
                              scratch0
                              ascii-space)                ; Are we looking at a space?
                          ; If we are looking at a space then goto WORD_read_data (start the loop up again). This is done to
                          ; "trim" the input of any number of spaces preceeding it
                          ; If we aren't looking at a space then we need to rebuild the jump table and then
                          (if scratch-true
                            scratch3
                            scratch4)
                          (label WORD_reassign_jumps
                                 ; this code should only be executed once. We now terminate if we see another space at this point!
                                 (set scratch3
                                      WORD_done_reading)
                                 (set scratch4
                                      WORD_store_word))
                          (label WORD_store_word
                                 ; the actual save operation,
                                 (st scratch2
                                     scratch0)   ; store the extracted character into the character buffer
                                 (incr scratch2) ; next character
                                 (gt scratch-true
                                     scratch-false
                                     scratch5
                                     wlen) ; did we go over the maximum word length?
                                 (when scratch-true
                                   goto
                                   WORD_too_large_word_ERROR) ; welp, this is fucked get out of here!
                                 (incr scratch5)))                 ; increment the word length count since we didn't error out
                    (label WORD_too_large_word_ERROR
                           ; we need to setup the pointers for error states since we got here!
                           (st scratch2
                               zero)     ; rewrite zero to the end of word entry
                           (set sarg1
                                errmsg_WORD_too_large_word) ; load the error message
                           (bi ERROR))
                    (label WORD_done_reading
                           (st scratch2
                               zero)))                ; put a zero in the current cell, or the last one
              (func NUMBER
                    ;-----------------------------------------------------------------------------
                    ; SUBROUTINE
                    ; Parse an unsigned hexadecimal number and construct a number out of it
                    ;-----------------------------------------------------------------------------
                    (using (save-to sp)
                           (lr inptr)
                           (move inptr
                                 sarg0)
                           ; be super lazy and just load all six characters
                           (Fetch) ; Load the first character and see if we're looking at a x or X
                           (eq scratch-true
                               scratch-false
                               fdcurr
                               0x58) ; are we looking at an X?
                           (when scratch-true goto HEXPARSE_LOOP) ; we are so parse the number!
                           (label NATURAL
                                  (loop NATURAL_CHECK_CHARACTER
                                         (Fetch)
                                         (subi scratch0
                                               fdcurr
                                               0x30)
                                         (gti is-number
                                              is-not-number
                                              scratch0
                                              0x9) ; if the result is greater than 9 (unsigned wraparound)
                                         (when is-not-number goto END_NATURAL)
                                         (muli scratch1
                                               scratch1
                                               10)
                                         (add scratch1
                                              scratch1
                                              scratch0)))
                           (label END_NATURAL
                                  (move sres0
                                        scratch1)
                                  (goto NUMBER_CHECK))
                           ; we aren't looking at any of that
                           (loop HEXPARSE_LOOP
                                  ; let's start parsing the hex loop and looking at four digits (must be four digits)
                                  (Fetch) ; get the most significant digit
                                  (subi scratch0
                                        fdcurr
                                        0x30)
                                  (lti is-number
                                       is-not-number
                                       scratch0
                                       0xA)  ; is it a natural digit?
                                  (when is-number goto COMBINE_NUMBER) ; it was successful so save it
                                  (subi scratch0
                                        fdcurr
                                        0x41) ; see if it is a capital letter
                                  (lti is-number
                                       is-not-number
                                       scratch0
                                       0x6)
                                  (when is-not-number goto CHECK_FOR_NOT_NUMBER_STATUS) ; we found an upper case digit
                                  ; add 10 (0xA) to the number since it is a digit
                                  (addi scratch0
                                        scratch0
                                        0xA) ;
                                  (bi COMBINE_NUMBER)
                                  (label CHECK_FOR_NOT_NUMBER_STATUS
                                         (when is-not-number goto NUMBER_END))
                                  (label COMBINE_NUMBER
                                         (shli scratch1
                                               scratch1
                                               0x4)
                                         (or scratch1
                                             scratch0
                                             scratch1)))
                           (label NUMBER_CHECK
                                  (eq is-number
                                      is-not-number
                                      fdcurr
                                      ascii-space)
                                  (move sres0
                                        scratch1))
                           (label NUMBER_END
                                  (move sres0
                                        scratch1))))
              ; TODO: Numeric Output Conversion (3.4.3)
              (func Fetch
                     ;-----------------------------------------------------------------------------
                     ; SUBROUTINE
                     ; Load the character defined by the input pointer into the fdcur register,
                     ; then advance inptr by one
                     ;-----------------------------------------------------------------------------
                     (ld fdcurr
                         inptr)
                     (incr inptr))

              (func Deposit
                     ;-----------------------------------------------------------------------------
                     ; SUBROUTINE
                     ; Store the character in fdcur in the address described by outptr, then
                     ; advance outptr by one
                     ;-----------------------------------------------------------------------------
                     (st outptr
                         inptr)
                     (incr outptr))

              (label WordDrop
                     ;-----------------------------------------------------------------------------
                     ; SUBROUTINE
                     ; Pop a word off the top of the parameter stack
                     ;-----------------------------------------------------------------------------
                     (pop top
                          sp)
                     (blr))

              (label WordDup
                     ;-----------------------------------------------------------------------------
                     ; SUBROUTINE
                     ; duplicate the top of the parameter stack
                     ;-----------------------------------------------------------------------------
                     (pop top
                          ds)
                     (push ds
                           top)
                     (push ds
                           top)
                     (blr))

              (label WordSwap
                     ;-----------------------------------------------------------------------------
                     ; SUBROUTINE
                     ; swap the top of the parameter stack with the lower word
                     ;-----------------------------------------------------------------------------
                     (pop top
                          ds)
                     (pop second
                          ds)
                     (push ds
                           top)
                     (push ds
                           second)
                     (blr))

              (label WordOver
                     ;-----------------------------------------------------------------------------
                     ; SUBROUTINE
                     ; push the lower word on the stack onto the stack (a b -- a b a)
                     ;-----------------------------------------------------------------------------
                     (pop top
                          ds) ; get the top and second
                     (pop second
                          ds)
                     (push ds
                           second) ; push lower
                     (push ds
                           top)    ; push top
                     (push ds
                           second) ; push lower
                     (blr))

              (label NEXT
                     ;-----------------------------------------------------------------------------
                     ; Goto the next instruction in the token list (outer execution list, il)
                     ;-----------------------------------------------------------------------------
                     (incr il)
                     (ld scratch0
                         il)
                     (move ip
                           scratch0))
              (label DOLIT
                     ;-----------------------------------------------------------------------------
                     ; Load a literal onto the stack from the ip list. This is necessary since the
                     ; ip list contains a set of instructions instead of the usual
                     ;-----------------------------------------------------------------------------
                     (ld top
                         il)
                     (push ds
                           top)
                     (incr il)
                     (bi NEXT))
              (label DOCON
                     ;-----------------------------------------------------------------------------
                     ; Run time routine for CONSTANT, VARIABLE, and CREATE.
                     ;-----------------------------------------------------------------------------
                     (pop top
                          cs)
                     (ld top
                         top)
                     (push ds
                           top)
                     (bi NEXT))
              (label DOLST
                     ;-----------------------------------------------------------------------------
                     ; Process colon list
                     ;-----------------------------------------------------------------------------
                     (move scratch0
                           il)
                     (pop il
                          cs)
                     (push cs
                           scratch0)
                     (bi NEXT))
              (label EXIT
                     ;-----------------------------------------------------------------------------
                     ; Terminate a colon list
                     ;-----------------------------------------------------------------------------
                     (pop il
                          cs)
                     (bi NEXT))
              (label EXECUTE
                     ;-----------------------------------------------------------------------------
                     ; Execute the top of the data stack
                     ;-----------------------------------------------------------------------------
                     (pop top
                          ds)
                     (b top))
              (label EXECUTE_INDIRECT
                     ;-----------------------------------------------------------------------------
                     ; Load the value from data memory using the address stored in the data stack
                     ; then jump to this loaded value
                     ;-----------------------------------------------------------------------------
                     (pop top
                          ds)
                     (ld top
                         top) ; load the address out of memory
                     (b top))
              (label BRANCH
                     ;-----------------------------------------------------------------------------
                     ; Unconditional branch to address
                     ;-----------------------------------------------------------------------------
                     (ld il
                         il)
                     (incr il)
                     (bi NEXT))
              (label COND_BRANCH
                     ;-----------------------------------------------------------------------------
                     ; Branch if flag is zero
                     ;-----------------------------------------------------------------------------
                     (pop top
                          ds)
                     (is-zero scratch-true
                              scratch-false
                              top)
                     (when scratch-true goto BRANCH)
                     (bi SKIP))
              )
         )
(let DictionaryBase be 0x7FFF)
(section data
         (org DictionaryBase
              (label DictionaryDrop
                     (word 0x0000)
                     (word 0x0104)
                     (string DROP)
                     (word WordDrop))
              (label DictionaryDup
                     (word DictionaryDrop)
                     (word 0x0103)
                     (string DUP)
                     (word WordDup))
              (label DictionarySwap
                     (word DictionaryDup)
                     (word 0x0104)
                     (string SWAP)
                     (word WordSwap))
              (label DictionaryOver
                     (word DictionarySwap)
                     (word 0x0104)
                     (string OVER)
                     (word WordOver))))
