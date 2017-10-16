/* ============================================================================             

                COMP30020: Declarative Programming - Project 2

   ============================================================================

    File:    proj2.pl
    Author:  Emmanuel Macario <macarioe@student.unimelb.edu.au>
    Origin:  17:00 Tues 3 Oct 2017
    Purpose: To implement a 'Maths Puzzle' solver.


    This code aims to solve a 'Maths Puzzle' of size 2 x 2, 3 x 3, or 4 x 4, 
    whereby the goal of the puzzle is to fill in all the squares such that 
    the puzzle satisfies a set of mathematical properties and constraints.

    The program achieves this by imposing a set of domain constraints, then
    systematically trying out values for unfilled variables in the puzzle, 
    until it either succeeds (has a valid solution), or fails.

    Efforts have been made to increase efficiency, such as implementing
    tail-recursion optimisation where possible. More details on optimisation
    are explained below.


   ========================================================================== */


% ============================================================================ %
% Additional libraries used.
% ============================================================================ %

:- ensure_loaded(library(clpfd)).


% ============================================================================ %
% Main predicate to solve the puzzle.
% ============================================================================ %

/*
** Takes a Puzzle, represented as a proper list of proper lists,
** and either validates the puzzle, finds a solution to the puzzle, 
** or fails indicating that there exists no solution.
**
** Key assumptions placed on the input Puzzle:
**     1. All row/column header values are bounded to integers.
**     2. The 'inner squares' may be bounded or unbounded. 
**     3. The top left corner of the puzzle (first element of the 
**        first list) can be ignored, since it is non-meaningful.
**     4. The puzzle is square.
*/
puzzle_solution(Puzzle) :-
    % Ensure the puzzle is square.
    maplist(same_length(Puzzle), Puzzle),

    % Get all diagonal elements.
    square_diagonal(Puzzle, Ds), 

    % Check all diagonal elements are the same.
    check_diagonal(Ds),

    % Check that rows satisfy puzzle constraints.
    Puzzle = [_|Rows],
    maplist(check_row, Rows),

    % Get the columns of the puzzle.
    transpose(Puzzle, TPuzzle),

    % Check that columns satisfy puzzle constraints.
    TPuzzle = [_|Cols],
    maplist(check_row, Cols).


% ============================================================================ %
% Predicates to check the diagonal.
% ============================================================================ %

/*
** Takes a square matrix (list of lists) Rows, and gets 
** the list Ds, containing all elements in the diagonal 
** of the matrix (top-left to bottom-right diagonal).
*/
square_diagonal(Rows, Ds) :-
    square_diagonal(Rows, [], [], Ds).

/*
** This predicate works by generating a longer and 
** longer prefix in each recursive step, such that it 
** can identify the diagonal element in a respective row.
** Diagonal elements are appended to an accumulating
** list throughout the recursion, and the final state
** of this accumulator is achieved when there are no
** more rows to be considered (i.e. empty list).
*/
square_diagonal([], _, Ds, Ds).
square_diagonal([R|Rs], PrefixIn, DIn, Ds) :-
    % Find diagonal element, D, of a row.
    append(PrefixIn, [D|_], R),

    % Generate the next prefix.
    same_length([_|PrefixIn], PrefixOut),

    % Add diagonal element to the list so far.
    append(DIn, [D], DOut),

    % Now, get the rest of the diagonal 
    % elements from the remaining rows.
    square_diagonal(Rs, PrefixOut, DOut, Ds).


/*
** Takes a list as input, and succeeds if the
** list is made up of identical elements. Holds
** for empty, and single element lists.
*/
all_same([]).
all_same([_]).
all_same([H|T]) :- 
    T = [H|_],
    all_same(T).


/*
** Takes the diagonal generated from the puzzle,
** and checks to see that all elements in the
** diagonal are identical, and are in the
** specified domain.
**
** Note: Since the diagonal generated from the puzzle 
** contains the top left element, we can ignore it here.
*/
check_diagonal([_|Ns]) :- 
    all_same(Ns),
    Ns ins 1..9.


% ============================================================================ %
% Predicates to check the rows and columns.
% ============================================================================ %

/* 
** Multiplies two integers X and Y, 
** to get their product, XY.
*/
multiply(X, Y, XY) :-
    multiply(X, Y, 0, XY).
multiply(X, Y, A, XY) :-
    ( X #= 0 ->
        XY #= A
    ;   
        X1 #= X-1,
        A1 #= A+Y,
        multiply(X1, Y, A1, XY)
    ).


/*
** Takes a List of integers and calculates the 
** Product of all the integers in the list. 
**
** Note: Works in any mode where List is ground.
*/
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


/*
** Takes a list of integers, and checks if the head
** of the list is equal to the product of all the 
** elements in the tail of the list.
*/
row_product_eq([N|Ns]) :-
    product_eq(N, Ns).


/* 
** Takes a List of integers and calculates the
** Sum of all the integers in the list.
*/
sum_eq(Sum, List) :-
    sum_eq(List, 0, Sum).
sum_eq([], A, A).
sum_eq([N|Ns], A0, Sum) :- 
    A1 #= A0 + N,
    sum_eq(Ns, A1, Sum).


/* 
** Takes a list of integers, and checks if the head
** of the list is equal to the sum of all the
** elements in the tail of the list.
*/
row_sum_eq([N|Ns]) :- 
    sum_eq(N, Ns).


/*
** Takes a row (or column) of the puzzle, and checks
** to see if it satisfies the puzzle constraints.
** If a row is ground, just checks to see if row
** constraints are satisfied. Otherwise, attempts 
** to enumerate a possible solution for the row.
**
** Note: Since we check a row for validity as soon 
** as it is generated, this decreases the size
** of the search space enormously.
*/
check_row([N|Ns]) :-
    % Check the row (or column) header value is ground.
    ground(N),

    % Check that all numbers in a row are 
    % distinct (excluding header value).
    all_different(Ns),

    % Set domain contraint for all row values
    % (excluding header value).
    Ns ins 1..9,

    % Two cases to consider:
    ( ground(Ns) ->
        % Row is ground, so just test constraints.
        (row_product_eq([N|Ns]) ; row_sum_eq([N|Ns]))
    ;   
        % Else, row is unground. Of the unlabeled row variables, 
        % label them in priority of smallest restricted domain, 
        % then in order of leftmost, participating in the most 
        % constraints. Then, check if row constraints satisfied.
        labeling([ffc], Ns),
        (row_product_eq([N|Ns]) ; row_sum_eq([N|Ns]))
    ).