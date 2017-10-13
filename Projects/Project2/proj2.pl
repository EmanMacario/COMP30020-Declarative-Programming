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
    ( N #= 3 ->
        puzzle_solution2(Puzzle)
    ; N #= 4 ->
        puzzle_solution3(Puzzle)
    ;   puzzle_solution4(Puzzle)
    ).

    % Write out the puzzle solution.
    %maplist(writeln, Puzzle).
    



% Predicate to solve 2x2 puzzles
puzzle_solution2(Puzzle) :-
    Puzzle = [[_,W,X],
              [Y,A,B],
              [Z,C,A]],
    Puzzle = [_, [_|R2T], [_|R3T]],

    % Check that all values are in the domain.
    append([R2T, R3T], Vs), Vs ins 1..9,
    
    % Check each row and column is distinct.
    maplist(all_distinct, [R2T, R3T]),
    transpose([R2T, R3T], Columns),
    maplist(all_distinct, Columns), 
    
    % Set arithmetic constraints.
    (sum_eq(Y, R2T) ; Y #= A * B),
    (sum_eq(Z, R3T) ; Z #= C * A),
    (sum_eq(W, R3T) ; W #= C * A),
    (sum_eq(X, [B,A]) ; X #= B * A).



% Predicate to solve 3x3 puzzles.
puzzle_solution3(Puzzle) :-
    Puzzle = [[_, A, B, C],
              [D, X, G, H],
              [E, I, X, J],
              [F, K, L, X]],
    Puzzle = [_, [_|R2T], [_|R3T], [_|R4T]],

    % Check that all values are in the domain.
    append([R2T, R3T, R4T], Vs), Vs ins 1..9,

    % Check each row and column is distinct.
    maplist(all_distinct, [R2T, R3T, R4T]),
    transpose([R2T, R3T, R4T], Columns),
    maplist(all_distinct, Columns),

    % Set arithmetic constraints.
    (D #= X + G + H ; D #= X * G * H),
    (E #= I + X + J ; E #= I * X * J),
    (F #= K + L + X ; F #= K * L * X),
    (A #= X + I + K ; A #= X * I * K),
    (B #= G + X + L ; B #= G * X * L),
    (C #= H + J + X ; C #= H * J * X).



% Predicate to solve 4x4 puzzles.
puzzle_solution4(Puzzle).



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
product_eq(Product, List) :-
    foldl(multiply, List, 1, Product).

row_product_eq([N|Ns]) :-
    product_eq(N, Ns).


sum_eq(Sum, List) :- sum(List, #=, Sum).

row_sum_eq([N|Ns]) :- sum_eq(N, Ns).



square_diagonal(Rows, Ds) :- foldl(diagonal, Rows, Ds, [], _).

diagonal(Row, D, Prefix0, Prefix) :-
        append(Prefix0, [D|_], Row),
        same_length([_|Prefix0], Prefix).





% Set the variable E.g. A in 1..2.
% Then test a row or column exactly after it is finished.





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

