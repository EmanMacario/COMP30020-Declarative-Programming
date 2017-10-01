% COMP30020 Week 8 - Workshop 7 (13/09/17)

% Question 1
list_of(_X, []).
list_of(X, [X | Xs]) :- 
    list_of(X, Xs).

% This predicate works in any mode.


% Question 2
% Implement the predicate all_same(List) such that every element of
% List is identical.  This should hold for empty and single element
% lists, as well.

all_same1([]).
all_same1([_H]).
all_same1([H|T]) :- 
    T = [H|_T1],
    all_same1(T).

% Or we could use predicate defined above.
all_same2(List) :- 
    list_of(_, List).



% Question 3
% Implement the predicate adjacent(E1, E2, List) such that E1 appears
% immediately before E2 in List.  Implement it by a single call to
% append/3.  What modes should and does this work in?

adjacent1(E1, E2, List) :-
    append(_, [E1, E2|_], List).

% This predicate works in any mode.




% Question 4
% Reimplement the adjacent(E1, E2, List) predicate as a recursive
% predicate that calls no other predicate but itself.

adjacent2(E1, E2, [E1, E2|_T]).
adjacent2(E1, E2, [_H|T]) :- 
    adjacent2(E1, E2, T).



% Question 5
% Implement the predicate before(E1, E2, List) such that E1 and E2 are
% both elements of List, where E2 occurs after E1 on List.
before(E1, E2, [E1|Tail]) :- 
    member(E2, Tail).

before(E1, E2, [_|Tail]) :-
    before(E1, E2, Tail).



% Question 6
intset_member(N, tree(_, N, _)).
intset_member(N, tree(L, V, R)) :-
    ( N < V ->
        intset_member(N, L)
      ; intset_member(N, R)
    ).


intset_insert(N, empty, tree(empty, N, empty)).
intset_insert(N, tree(L, N, R), tree(L, N, R)).
intset_insert(N, tree(L0, N0, R), tree(L, N0, R)) :-
    N < N0,
    intset_insert(N, L0, L).
intset_insert(N, tree(L, N0, R0), tree(L, N0, R)) :-
    N > N0,
    intset_insert(N, R0, R).