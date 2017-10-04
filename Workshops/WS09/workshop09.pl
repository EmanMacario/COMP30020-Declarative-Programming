% COMP30020 Week 10 - Workshop 9

% Question 1 - O(nm)
same_elements(L1, L2) :-
    all_in(L1, L2),
    all_in(L2, L1).

all_in([], _).
all_in([X|Xs], Y) :-
    member(X, Y),
    all_in(Xs, Y).


% Question 2 - O(nlogn)
% Note: You can compare lists with equals in Prolog.
%       sort/2 removes duplicates.
same_elements1(L1, L2) :-
    sort(L1, Sorted),
    sort(L2, Sorted).


% Question 3 - Use divmod/4 for this question instead of div and //
% dividend(+Dividend, +Divisor, -Quotient, -Remainder).
times(W,X,Y,Z) :-
    ( integer(W), integer(X), integer(Y) ->
        Z is W*X + Y
    ;   
      integer(Z) ->
        ( integer(X) ->
            divmod(Z, X, W, Y)
        ; 
          integer(W) ->
            divmod(Z, W, X, Y)
        ;
          throw(error(instantiation_error, context(times/4,_)))
        )
    ).


% Question 4
% Note: It is a 'convention' to write tuples in Prolog as [A-B].
containers(Steps) :-
        containers(3, 5, 0, 0, _, 4, [0-0], Steps).

containers(_, _, V1, V2, V1, V2, _, []).
containers(C1, C2, V1, V2, T1, T2, Hist, [Move|Steps]) :-
        move(C1, C2, V1, V2, N1, N2, Move),
        State = N1-N2,
        \+ member(State, Hist),
        containers(C1, C2, N1, N2, T1, T2, [State|Hist], Steps).

move(C1, _, _, V2, 0, V2, empty(C1)).
move(_, C2, V1, _, V1, 0, empty(C2)).
move(C1, _, _, V2, C1, V2, fill(C1)).
move(_, C2, V1, _, V1, C2, fill(C2)).
move(C1, C2, V1, V2, N1, N2, pour(C1,C2)) :-
        pour(C2, V1, V2, N1, N2).
move(C1, C2, V1, V2, N1, N2, pour(C2,C1)) :-
        pour(C1, V2, V1, N2, N1).

pour(C2, V1, V2, N1, N2) :-
        (   V1 + V2 > C2 ->
                N1 is V1 - (C2 - V2),
                N2 is C2
        ;   N1 = 0,
            N2 is V1 + V2
        ).