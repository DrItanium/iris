( enum like support )
( duplicate the top of the stack after incrementing )
: enum-start ( -- 0 0 ) 0 dup ;
: enum-next  ( n1 -- n2 n2 ) 1 + dup ;
: enum-done  ( n1 -- ) drop ;

close-input-file
