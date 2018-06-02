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

: .line ( line scr -- )
  \ print on the terminal a line of text from disc by its line number and 
  \ screen number scr given on stack
  \ trailing blanks are also supported





