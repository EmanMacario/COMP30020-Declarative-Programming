% Question 1
list_of(_X, []).
list_of(X, [X | Xs]) :- list_of(X, Xs).

% Question 2
/* Implement the predicate all_same(List) such that every element of
List is identical.  This should hold for empty and single element
lists, as well. */

all_same([]).
all_same([_H]).
all_same([H|T]) :-
    T = [H|T1],
    all_same([H|T1]).

% Question 3
/* Implement the predicate adjacent(E1, E2, List) such that E1 appears
immediately before E2 in List.  Implement it by a single call to
append/3.  What modes should and does this work in? */

adjacent(E1, E2, [E1, E2 | T]).
adjacent(E1, E2, [H | T]) :- 
    adjacent(E1, E2, T).


adjacent2(E1, E2, List) :-
    append(_, [E1, E2 | _], List).


% Question 4