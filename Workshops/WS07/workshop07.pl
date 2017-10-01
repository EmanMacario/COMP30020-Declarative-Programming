% COMP30020 Week 8 - Workshop 7 (13/09/17)

% Question 1
list_of(_X, []).
list_of(X, [X | Xs]) :- list_of(X, Xs).


% Question 2
% Implement the predicate all_same(List) such that every element of
% List is identical.  This should hold for empty and single element
% lists, as well.

all_same1([]).
all_same1([_H]).
all_same1([H|T]) :- T = [H|T1],
                    all_same1([H|T1]).

% Or we could use predicate defined above.
all_same2(List) :- list_of(_, List).



% Question 3
% Implement the predicate adjacent(E1, E2, List) such that E1 appears
% immediately before E2 in List.  Implement it by a single call to
% append/3.  What modes should and does this work in?

adjacent1(E1, E2, List) :-
    append(_, [E1, E2 | _], List).


% Question 4
% Reimplement the adjacent(E1, E2, List) predicate as a recursive
% predicate that calls no other predicate but itself.

adjacent2(E1, E2, [E1, E2 | _T]).
adjacent2(E1, E2, [_H | T]) :- adjacent2(E1, E2, T).