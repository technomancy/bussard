: 2dup over over ;

: -rot rot rot ;

: >= 2dup = -rot > or ;

: <= 2dup = -rot < or ;

: forward_for 1 swap do forward loop ;
