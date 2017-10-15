% COMP30020 Declarative Programming - Project 2

% File:    proj2.pl
% Author:  Emmanuel Macario <macarioe> <831659>
% Origin:  17:00 Tues 3 Oct 2017
% Purpose: To implement a Maths Puzzle solver.


:- ensure_loaded(library(clpfd)).


puzzle_solution(Puzzle) :-
    length(Puzzle, N),
    maplist(same_length(Puzzle), Puzzle),
    square_diagonal(Puzzle, Ds), 
    check_diagonal(Ds),
    Puzzle = [_|Rows],
    maplist(check_row, Rows),
    transpose(Puzzle, TPuzzle),
    TPuzzle = [_|Cols],
    maplist(check_row, Cols).
    %maplist(writeln, Puzzle).


/* TRO Multiply predicate. */
% multiply/3 is a TRO predicate to multiply two integers.
multiply(X, Y, XY) :-
    multiply(X, Y, 0, XY).
multiply(X, Y, A, XYA) :-
    ( X #= 0 ->
        XYA #= A
    ;   
        X1 #= X-1,
        A1 #= A+Y,
        multiply(X1, Y, A1, XYA)
    ).


/* Row checking predicates (for sum and product). */
check_row([N|Ns]) :-
    ground(N),
    all_different(Ns),
    Ns ins 1..9,
    ( ground(Ns) ->
        (row_product_eq([N|Ns]) ; row_sum_eq([N|Ns]))
    ;
        labeling([ffc], Ns),
        (row_product_eq([N|Ns]) ; row_sum_eq([N|Ns]))
    ).


/* TRO predicate to get the product of all numbers in a list. */
product_eq(Product, List) :-
    ( List = [] ->
        Product #= 0
    ;
        product_eq(Product, 1, List)
    ).

product_eq(A, A, []).
product_eq(Product, A0, [N|Ns]) :-
    A1 #= A0 * N,
    product_eq(Product, A1, Ns). 

/* Checks if the first element of a list is equal
   to the product of all its elements. */
row_product_eq([N|Ns]) :-
    product_eq(N, Ns).


/* Get sum of all integers in a list */
sum_eq(Sum, List) :-
    sum_eq(List, 0, Sum).
sum_eq([], A, A).
sum_eq([N|Ns], A0, Sum) :- 
    A1 #= A0 + N,
    sum_eq(Ns, A1, Sum).

/* Checks if the first element of a list is equal
   to the sum of all elements in the tail. */
row_sum_eq([N|Ns]) :- 
    sum_eq(N, Ns).



/* Diagonal checking. */
check_diagonal([_|Ns]) :- 
    all_same(Ns),
    Ns ins 1..9.

/*
square_diagonal(Rows, Ds) :- 
    foldl(diagonal, Rows, Ds, [], _).

diagonal(Row, D, Prefix0, Prefix) :-
        append(Prefix0, [D|_], Row),
        same_length([_|Prefix0], Prefix).
*/

/*
square_diagonal(Rows, Ds) :-
    square_diagonal(Rows, _, [], Ds).

square_diagonal([], _, Ds, Ds).
square_diagonal([R|Rs], D0, P0, POut) :-
    append(P0, [D0|_], R),
    same_length([_|P0], P1),
    square_diagonal(Rs, D0, P1, POut).
    */



% Another try
square_diagonal(Rows, Ds) :-
    square_diagonal(Rows, [], [], Ds).


% Base case
square_diagonal([], _, Ds, Ds).
square_diagonal([R|Rs], PrefixIn, DIn, Ds) :-
    append(PrefixIn, [D|_], R),
    same_length([_|PrefixIn], PrefixOut),
    append(DIn, [D], DOut),
    square_diagonal(Rs, PrefixOut, DOut, Ds).


/* All same predicate from tutes. */
% Implement the predicate all_same(List) such that every element of
% List is identical.  This should hold for empty and single element
% lists, as well.

all_same([]).
all_same([_H]).
all_same([H|T]) :- 
    T = [H|_T1],
    all_same(T).



/* Create tuples of size n (e.g. A-B-C) */
n_tuples([_,N|Ns], Tuples) :-
    n_tuples(Ns, N, Tuples).

n_tuples([], A, A).
n_tuples([N|Ns], A0, AOut) :-
    A1 = A0-N,
    n_tuples(Ns, A1, AOut).


/* E.g.

    exclude(ground, Rows, UngroundRows).

    Z = [72, _, _], n_tuples(Z, X), bagof(X, check_row(Z), L).
*/


/* Predicate that filters unground rows (or columns). */
