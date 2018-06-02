: lfa ( pfa -- lfa ) 
    \ convert the parameter field address to link field address
4 - ; 
: cfa ( pfa -- cfa ) 
    \ convert the parameter field address to code field address
    2 - ;
: (abort) ( -- ) ABORT ;
: error ( n -- in blk )
  warning @ 0<
  if (abort) endif 
  here count type 
  ." ?"
  message
  SP!
  in @
  blk @
  quit ;
: ?ERROR ( f n -- ) swap if error else drop endif ;
: latest ( -- addr )
  \ leave the name field address of the last word defined in the current vocabulary
  current @ @ ;

: ' ( -- pfa )
  \ locate a word in the dictionary. Form is ' cccc
  -find \ get cccc and search the dictionary, first the context and then the 
        \ current vocabularies
  0= 0 ?error \ not found, issue error message
  drop 
  [compile] \ compile the next immediate word literal to compile the parameter field address at run-time
  literal ; immediate


: create ( -- ) 
  bl word
  here
  dup c@
  width @
  min
  1+ allot
  dup 0x0a0 toggle
  here 1- 0x80 toggle
  latest,
  current @ !
  here 2+ , ;

: code ( -- ) create [compile] assembler ;

: expect ( addr n -- )
over + over 
do 
key 
dup 0xE +origin = 
if 
drop 0x8 over I= dup r> 0x2 - + r> - 
else 
dup 0x0D = 
if 
  leave drop bl 0 
else 
  dup 
endif 
I C! 0 I 1+ ! 
endif 
emit 
loop 
drop ; 

: query TIB 0x50 expect 0 in ! ;

: word ( c ------ )
\ work horse of the interpreter
blk @ if blk @ block else tib @ endif 
in @ + swap enclose here 0x22 blanks in +! over - >r R here C! + 
here !+
R> 
;  \ move the string from input buffer to top of dictionary

: type  ( addr n -- )
  -dup 
  if 
    over + 
    swap 
    do 
        I C@
        emit
    loop
  else 
    drop
  endif ;

: count ( addr n -- ) dup 1+ swap ;

: -trailing ( addr n1 -- addr n2 )
  dup 0
  do
  over over
  + 1 - 
  c@ bl - 
  if leave else 1- endif loop ;
      

: (.") \ runtime procedure compiled by ." to type inline text
  R \ copy IP from the return stack, which points to the beginning of the inline text string
  count \ get the length byte of the string, preparing for TYPE
  dup 1+ \ length + 1
  r> + >r \ increment ip on the return stack by length +1, thus skipping the 
          \ text string and pointing to the next word after ", which is the 
          \ next word to be executed
  type    \ now type out the text string 
  ;

: ." 0x22 state @ if compile 
(.")
  word here C@ 1+ allot else word here endif ; immediate
: id. ( nfa -- ) 
  \ print an entry's name from its name field address on stack.
  pad
  0x20 \ ascii blank
  0x5F fill \ fill pad with 95 blanks
  dup pfa lfa \ find the link field address
  over - 
  pad swap cmove \ move the entire name with the length to PAD
  pad count \ prepare string for output
  0x1f and \ no more than 31 characters
  type \ type out the name
  space \ append a space
  ;
: (line) ( line scr -- addr count )
  >r c/l b/buf */mod \ calculate the character offset and the screen offset
                     \ members from the line number, characters/line, and bytes/buffer
  r> b/scr * +
  block
  + 
  c/l
  ;
: .line ( line scr -- )
  \ print on the terminal a line of text from disc by its line number and 
  \ screen number scr given on stack
  \ trailing blanks are also supported
  (line) \ runtime procedure to convert the line number
  -trailing type ;

: list ( n -- )
  \ display the ascii text of screen n on the terminal
  decimal cr
  dup scr !
  ." SCR #" . \ print th escreen number first
  0x10 0 do 
    cr I 3 .R space
    I SCR @ .line
    loop cr ;
: hex 0x10 base ! ;
: octal 0x8 base ! ;
: decimal 0x0a base ! ; 

: (number) ( d1 addr1 -- d2 addr2 )
  \ runtime routine of number conversion. Convert an ascii text beginning at addr1 + 1
  \ according to base. The result is accumulated with d1 to become d2. Addr2 is
  \ the address of the first unconvertable digit
  begin
    1+ dup >r 
    c@ 
    base @
    digit
    while
    swap 
    base @ U* 
    drop 
    rot 
    base @ U*
    D+
    dpl @ 1+
    if 
      1 dpl +!
      endif 
      r>
      repeat
      r>
      ;

: number ( addr -- d )
  0 0 rot \ push the two zero's on stack as the initial value of d
  dup 1+ c@ \ get the first digit
  0x2D = \ is it a -sign ?
  dup >R \ save the flag on return stack
  + \ if the first digit is -, the flag is 1, and addr + 1 points to the second digit
    \ addr + 0 remains the same, pointing to the first digit
  -1 \ The initial value of dpl
  begin 
    dpl ! \ store the decimal point counter
    (number) \ convert one digit after another until an invalid char occurs.
             \ result is accumulated into d.
    dup c@   \ fetch the invalid digit
    bl - \ is it a blank?
  while  \ not a blank, see if it is a decimal point
    dup c@  \ get the digit sign
    0x2e - \ is it a decimal point?
    0 ?error \ not a decimal point, it is an illegal character for a number.
             \ issue an error message and quit
    0 \ a decimal point was found. set DPL to 0 the next time
    repeat \ exit here if a blank was detected. Otherwise repeat the conversion
           \ process
    drop \ discard addr on stack
    r>   \ pop the flag of - sign back
    if dminus \ negate d if the first digit is a - sign
    endif 
;

: <# ( -- )
  pad \ pad is the scratch pad address for text output, 68 bytes above the dictionary head HERE
  HLD ! \ hld is a user variable holding the address of last character in the output text string
  ;
: erase ( addr u -- ) 0 fill ;
: blanks ( addr u -- ) bl fill ;
\ when the machine starts from cold
\ COLD calls ABORT
\ ABORT calls QUIT
: cold 
empty-buffers
0 density !
first use !
dr0 0 eprint !
orig 0x12 + 
up @ 6 + 
0x10 cmove 
orig 0x0c + @ forth 
6 + ! abort ;

: -find ( -- [ pfa b tf ] or [ f ] )
  \ accept the next word delimited by blanks in the input stream to here, and
  \ search the context and then the current vocabularies for a matching name.
  \ If found, the entry's parameter field address, a length byte, and a flag
  \ are left on stack. Otherwise only boolean false flag is left
  bl word \ move text string delimited by blanks from input string to the top of dictionary here
  here \ the address of text to be matched
  context @ @ \ fetch the name field address of the last word defined in the context
              \ vocabulary and begin the dictionary search
  (find) \ a primitive. search the dictionary starting at the address on stack for 
         \ a name matching the text at the address second on stack. Return
         \ the parameter field address of the matching name, its length byte,
         \ and a boolean true flag on stack for a match. If no match is possible
         \ then only a boolean false flag is left on stack
  dup 0= \ look at flag on stack
  if  \ no match in context vocabulary
    drop \ discard the false flag
    here \ get the address of text again
    latest \ the name field address of the last word defined in the current
           \ vocabulary
    (find) \ search again through the current vocabulary
  endif
;

: vocabulary ( -- )
  <builds 0xa081 , \ a dummy header at vocabulary intersection
  current @ \ fetch the parameter field address pointing to the last word defined
            \ in the current vocabulary
  cfa ,     \ store its code field address in the second cell in the parameter field
  here \ address of vocabulary link
  voc-link @ , \ fetch the user variable voc-link and insert it in the dictionary
  voc-link ! \ update boc-link with the link in this vocabulary
  does> \ this is the end in defining cccc vocabulary
  2 + context ! \  when cccc is invoked, the second cell in its parameter field will
  \ be stored into the variable context. the next dictionary search will begin with the cccc vocabulary
  ;

: definitions ( -- )
  \ used in the form: cccc definitions 
  \ make cccc vocabulary the current vocabulary.
  \ new definitions will be added to the cccc vocabulary
  context @ current ! ;
\ dictionary entry stuff
: traverse ( a1 n -- a2 )
  \ move across the name field of a variable length name field.
  \ a1 is the address of either the length byte or the last character
  \ if n == 1, the motion is towards high memory; 
  \ if n == -1, the motion is towards low memory
  \ a2 is the address of the other end of the name field
  swap \ get a1 to the top of the stack
  begin
    over + \ copy n and add to addr, pointing to the next character
    0x7F \ test number for the eighth bit of a character
    over c@ \ fetch the character
    < \ if it is greater than 127, the end is reached
  until \ loop back if not the end
  swap drop \ discard n
  ;
: nfa ( pfa -- nfa )
    \ convert the parameter field address to name field address
    5 - 
    -1 traverse ;
: forget ( -- )
  \ special form, forget cccc
  \ delete definitions defined after and including the word cccc. The current and
  \ context vocabulary must be the same

  current @ context @ - 0x12 ?error \ compare current with context, if not the same, issue an error
  [compile] ' \ locate cccc in the dictionary
  dup \ copy the parameter field address
  fence @ \ compare with the context in the user variable fence
  < 0xF ?error \ if cccc is less than fence, do not forget. Fence guards the trunk forth vocabulary 
               \ from being accidentally forgotten.
  dup nfa \ fetch teh name field address of cccc and,
  dp !    \ store in the dictionary pointer DP. Now the top of the dictionary is redefined to be the first byte of cccc
          \ in effect deleting all definitions above cccc.
  lfa @ \ get the link field address of cccc pointing to the word just below it.
  current @ ! \ store it in the current vocabulary, adjusting the current vocabulary to the fact that
              \ all definitions above (including) cccc no longer exist
  ;
