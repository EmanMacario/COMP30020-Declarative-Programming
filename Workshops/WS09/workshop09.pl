% COMP30020 Week 10 - Workshop 9

% Question 1
same_elements(L1, L2) :-
    all_in(L1, L2),
    all_in(L2, L1).

all_in([], _).
all_in([X|Xs], Y) :-
    member(X, Y),
    all_in(Xs, Y).



% Question 2
same_elements(L1, L2) :-
    


% Question 3
times(W,X,Y,Z) :-
    ( integer(Z) ->
        
    )


% Question 4