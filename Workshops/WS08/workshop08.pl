% COMP30020 Week 9 - Workshop 8

% Question 1

sumlist(L, Sum) :-
    sumlist_acc(L, Sum, 0).

sumlist_acc([], Acc, Acc).
sumlist_acc([N|Ns], Sum, Acc) :- 
    NewAcc is Acc + N,
    sumlist_acc(Ns, Sum, NewAcc).


% Question 2
tree(empty).
tree(node(Left, _, Right)) :-
    tree(Left),
    tree(Right).

tree_list(empty, []).
tree_list(node(L, V, R), List) :-
    tree_list(L, LeftList),
    tree_list(R, RightList),
    append(LeftList, [V|RightList], List).


% Better Implementation.
tree_list1(empty, Acc, Acc).
tree_list1(node(L, V, R), AccIn, AccOut) :-
    tree_list1(R, AccIn, Acc1),
    Acc2 = [V|Acc1],
    tree_list1(L, Acc2, AccOut).


% T = node(node(empty,1,empty), 2, node(empty,3,empty)).