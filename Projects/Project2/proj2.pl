% COMP30020 Declarative Programming - Project 2

% File:    proj2.pl
% Author:  Emmanuel Macario <macarioe> <831659>
% Origin:  17:00 Tues 3 Oct 2017
% Purpose: To implement a Maths Puzzle solver.


:- ensure_loaded(library(clpfd)).



/* Useful predicates
integer/1
var/1
nonvar/1
ins
ground/1
sort/2
msort/2
keysort/2
between/3
*/


puzzle_solution(Puzzle) :-
    length(Puzzle, N),
    (N #= 2 ; N #= 3; N #= 4),
    maplist(same_length(Puzzle), Puzzle),
    Puzzle = [[_,W,X],
              [Y,A,B],
              [Z,C,A]],
    Puzzle = [R1, [R2H|R2T], [R3H|R3T]],

    % Check that all values are in the domain.
    append([R2T, R3T], Vs), Vs ins 1..9,
    

    maplist(all_distinct, [R2T, R3T]),
    transpose([R2T, R3T], Columns),
    maplist(all_distinct, Columns), 
    

    (Y #= A + B ; Y #= A * B),
    (Z #= C + A ; Z #= C * A),
    (W #= C + A ; W #= C * A),
    (X #= B + A ; X #= B * A),
    maplist(writeln, Puzzle).




% sum_list/2 is a TRO predicate to sum a list.
sum_list(L, Sum) :-
    sum_list(L, 0, Sum).
sum_list([], A, A).
sum_list([N|Ns], A0, Sum) :- 
    A1 is A0 + N,
    sum_list(Ns, A1, Sum).


% Or could use this fold to do the same thing.
sum(Ns, Sum) :-
    foldl(plus, Ns, 0, Sum).



% multiply/3 is a TRO predicate to multiply two integers.
multiply(X, Y, XY) :-
    multiply(X, Y, 0, XY).
multiply(X, Y, A, XYA) :-
    ( X = 0 ->
        XYA = A
    ;   X1 is X-1,
        A1 is A+Y,
        multiply(X1, Y, A1, XYA)
    ).

% product_list/2 calculates the product of all integers in a list.
product_list(Ns, Product) :-
    foldl(multiply, Ns, 1, Product).


% Gets the first element of a list.
first_element([E|_], E).


sum_eq(Sum, List) :- sum(List, #=, Sum).

square_diagonal(Rows, Ds) :- foldl(diagonal, Rows, Ds, [], _).

diagonal(Row, D, Prefix0, Prefix) :-
        append(Prefix0, [D|_], Row),
        same_length([_|Prefix0], Prefix).




/* Test Cases (2 x 2)

% true.

puzzle_solution([[0,11,36],
                 [13,_,_],
                 [18,_,_]]).

puzzle_solution([[0,11,36],
                 [13,9,4],
                 [18,2,9]]).


% Should get 9,4,2,9 which is true.
puzzle_solution([[0,45,72],
                 [72,_,_],
                 [14,_,_]]).

puzzle_solution([[0,45,72],
                 [72,9,8],
                 [14,5,9]]).


puzzle_solution([[0,13,8],[6,_,_],[13,_,_]]).
puzzle_solution([[0,13,8],[6,4,2],[13,9,4]]).


puzzle_solution([[0,16,13],[42,_,_],[16,_,_]]).
puzzle_solution([[0,16,13],[42,7,6],[16,9,7]]).


puzzle_solution([[0,15,32],[32,_,_],[56,_,_]]).
puzzle_solution([[0,15,32],[32,8,4],[56,7,8]]).


puzzle_solution([[0,14,27],[27,_,_],[45,_,_]]).
puzzle_solution([[0,14,27],[27,9,3],[45,5,9]]).

*/

