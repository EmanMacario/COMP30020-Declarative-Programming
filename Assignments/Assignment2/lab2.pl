% COMP30020 Declarative Programming - Assignment 2

% File:    lab2.pl
% Author:  Emmanuel Macario <macarioe> <831659>
% Origin:  20:00 Sun 1 Oct 2017
% Purpose: To implement some simple Prolog predicates.


% Question 1
correspond(E1, [E1|_], E2, [E2|_]).
correspond(E1, [_|T1], E2, [_|T2]) :-
    correspond(E1, T1, E2, T2).


% Question 2

/* interleave([], []).
interleave([[H|T]|Tail], List) :- 
    interleave(Tail, Rest),
    interleave(T, Rest2),
    append([H|Rest], Rest2, List).
*/



interleave([],[]).
interleave([[]|Tail],Rest) :- interleave([],Rest).
interleave([[Head1|Tail1]|Tail],[Head1|Rest]) :-
append(Tail,[Tail1],RestA),
interleave(RestA,Rest).


interleaving_join([], []).

interleaving_join([[]|X], Y):- 
    interleaving_join(X, Y).

interleaving_join([[H|T]|X], [H|Y]):- 
    append(X, [T], X2),
    interleaving_join(X2, Y).




% Question 3 - use number(X) and atom(X)

partial_eval(Var, Var, Val, Val).


partial_eval(E1 + E2, Var, Val, Expr) :-
    partial_eval(E1, Var, Val, E3),
    partial_eval(E2, Var, Val, E4),
    ( number(E3), number(E4) ->
        Expr is E3 + E4
      ; Expr = E3 + E4
    ).

partial_eval(E1 - E2, Var, Val, Expr) :-
    partial_eval(E1, Var, Val, E3),
    partial_eval(E2, Var, Val, E4),
    ( number(E3), number(E4) ->
        Expr is E3 - E4
      ; Expr = E3 - E4
    ).

partial_eval(E1 * E2, Var, Val, Expr) :-
    partial_eval(E1, Var, Val, E3),
    partial_eval(E2, Var, Val, E4),
    ( number(E3), number(E4) ->
        Expr is E3 * E4
      ; Expr = E3 * E4
    ).

partial_eval(E1 / E2, Var, Val, Expr) :-
    partial_eval(E1, Var, Val, E3),
    partial_eval(E2, Var, Val, E4),
    ( number(E3), number(E4) ->
        Expr is E3 / E4
      ; Expr = E3 / E4
    ).

partial_eval(E1 // E2, Var, Val, Expr) :-
    partial_eval(E1, Var, Val, E3),
    partial_eval(E2, Var, Val, E4),
    ( number(E3), number(E4) ->
        Expr is E3 // E4
      ; Expr = E3 // E4
    ).


partial_eval(Expr0, Var, Val, Expr) :-
    ( number(Expr0) ->
        Expr is Expr0
      ; Expr = Expr0
    ).
/* Sample calls
partial_eval(6*7, x, 2, E).
partial_eval(6*(3+x*x), x, 2, E).
partial_eval(x*(3+y*y), y, 2, E).
partial_eval((x*0+6)*(x-x+3+y*y), y, 2, E).
*/
