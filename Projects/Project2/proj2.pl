% COMP30020 Declarative Programming - Project 2

% File:    proj2.pl
% Author:  Emmanuel Macario <macarioe> <831659>
% Origin:  17:00 Tues 3 Oct 2017
% Purpose: To implement a Maths Puzzle solver.


:- ensure_loaded(library(clpfd)).

/* Note to self: Check out labelling/2 or label/1 for the unground variable shit */

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
    maplist(same_length(Puzzle), Puzzle),
    square_diagonal(Puzzle, Ds), 
    check_diagonal(Ds),
    Puzzle = [_|Rows],
    maplist(check_row, Rows),
    transpose(Puzzle, TPuzzle),
    TPuzzle = [_|Cols],
    maplist(check_row, Cols),
    maplist(writeln, Puzzle).



/* TRO Multiply predicate. */

% multiply/3 is a TRO predicate to multiply two integers.
multiply(X, Y, XY) :-
    multiply(X, Y, 0, XY).
multiply(X, Y, A, XYA) :-
    ( X #= 0 ->
        XYA #= A
    ;   X1 #= X-1,
        A1 #= A+Y,
        multiply(X1, Y, A1, XYA)
    ).


/* Row checking predicates (for sum and product). */
check_row([N|Ns]) :-
    all_different(Ns),
    Ns ins 1..9,
    ( ground([N|Ns]) ->
        (row_product_eq([N|Ns]) ; row_sum_eq([N|Ns]))
    ;
        labeling([ff], Ns),
        (row_product_eq([N|Ns]) ; row_sum_eq([N|Ns]))
    ).
    

product_eq(Product, List) :-
    foldl(multiply, List, 1, Product).

row_product_eq([N|Ns]) :-
    product_eq(N, Ns).

sum_eq(Sum, List) :- 
    sum(List, #=, Sum).

row_sum_eq([N|Ns]) :- 
    sum_eq(N, Ns).



/* Diagonal checking. */
check_diagonal([_|Ns]) :- 
    all_same(Ns),
    Ns ins 1..9.

square_diagonal(Rows, Ds) :- 
    foldl(diagonal, Rows, Ds, [], _).

diagonal(Row, D, Prefix0, Prefix) :-
        append(Prefix0, [D|_], Row),
        same_length([_|Prefix0], Prefix).


/* Drop predicate straight from lectures. */
drop(N, List, Back) :- length(Front, N),
     append(Front, Back, List).



/* All same predicate from tutes. */
% Implement the predicate all_same(List) such that every element of
% List is identical.  This should hold for empty and single element
% lists, as well.

all_same([]).
all_same([_H]).
all_same([H|T]) :- 
    T = [H|_T1],
    all_same(T).


/* Tail recursive sum list predicate. */
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


/* Magic hexagon shit. */
sum38(Vs) :- sum(Vs, #=, 38).

magic_hexagon(Vs) :-
        Vs = [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S],
        Vs ins 1..19,
        all_different(Vs),
        maplist(sum38, [[A,B,C], [D,E,F,G], [H,I,J,K,L], [M,N,O,P], [Q,R,S],
                        [H,D,A], [M,I,E,B], [Q,N,J,F,C], [R,O,K,G], [S,P,L],
                        [C,G,L], [B,F,K,P], [A,E,J,O,S], [D,I,N,R], [H,M,Q]]).




/******************************************************************************/
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


/* Test cases (3x3)
puzzle_solution([[0,11,54,45],[16,_,_,9],[35,_,_,_],[27,_,_,_]]).
puzzle_solution([[0, 11,54,45],
                 [16, 1, 6, 9],
                 [35, 7, 1, 5],
                 [27, 3, 9, 1]]).


puzzle_solution([[0,252,35,168],[11,_,_,_],[19,_,_,_],[21,_,_,_]]). 
puzzle_solution([[0,252,35,168],
                 [11, 7, 1, 3],
                 [19, 4, 7, 8],
                 [21, 9, 5, 7]]).

puzzle_solution([[0,14,18,48],[20,_,_,_],[9,_,_,_],[126,_,_,_]]). 
puzzle_solution([[0, 14,18,48],
                 [20, 3, 9, 8],
                 [9,  4, 3, 2],
                 [126,7, 6, 3]]).

puzzle_solution([[0,72,336,23],[22,_,_,_],[17,_,_,_],[18,_,_,_]]). 
puzzle_solution([[0, 72,336,23],
                 [22, 6, 7, 9],
                 [17, 3, 6, 8],
                 [18, 4, 8, 6]]).

puzzle_solution([[0,14,140,40],[56,_,_,_],[140,_,_,_],[60,_,_,_]]). 
puzzle_solution([[0, 14,140,40],
                 [56, 4, 7, 2],
                 [140,7, 4, 5],
                 [60, 3, 5, 4]]).

puzzle_solution([[0,14,16,18],[189,_,_,_],[17,_,_,_],[60,_,_,_]]). 
puzzle_solution([[0,  14,16,18],
                 [189, 3, 9, 7],
                 [17,  6, 3, 8],
                 [60,  5, 4, 3]]).

puzzle_solution([[0,42,16,9],[48,_,_,_],[24,_,_,_],[105,_,_,_]]). 
puzzle_solution([[0, 42,16,9],
                 [48, 3, 8,2],
                 [24, 2, 3,4],
                 [105,7, 5,3]]).

puzzle_solution([[0,14,189,20],[14,_,_,_],[18,_,_,_],[63,_,_,_]]). 
puzzle_solution([[0, 14,189,20],
                 [14, 7,  3, 4],
                 [18, 2,  7, 9],
                 [63, 1,  9, 7]]).

puzzle_solution([[0,11,9,162],[14,_,_,_],[18,_,_,_],[84,_,_,_]]). 
puzzle_solution([[0, 11, 9, 162],
                 [14, 3, 2,  9],
                 [18, 1, 3,  6],
                 [84, 7, 4,  3]]).
*/

