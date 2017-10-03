% COMP30020 Week 9 - Workshop 8

% Question 1
sumlist(L, Sum) :-
    sumlist_acc(L, 0, Sum).

sumlist_acc([], Acc, Acc).
sumlist_acc([N|Ns], Acc, Sum) :- 
    NewAcc is Acc + N,
    sumlist_acc(Ns, NewAcc, Sum).


% Predicate tree/1 checks if a BST is 'proper'.
tree(empty).
tree(node(Left, _, Right)) :-
    tree(Left),
    tree(Right).


% Question 2
tree_list(empty, []).
tree_list(node(Left, N, Right), List) :-
    tree_list(Left, L),
    tree_list(Right, R),
    append(L, [N|R], List).



% Question 3 - Better TRO Implementation.
tree_list(empty, Acc, Acc).
tree_list(node(L, V, R), AccIn, AccOut) :-
    tree_list(R, AccIn, AccOutRight),
    AccInLeft = [V|AccOutRight],
    tree_list(L, AccInLeft, AccOut).


% T = node(node(empty,1,empty), 2, node(empty,3,empty)).



% Question 4 - list_tree/2 recursively constructs a BST from a list
% starting from the middle element as the root node.
list_tree([], empty).
list_tree([E|List], node(Left,Elt,Right)) :-
    length(List, Len),
    Len2 is Len // 2,
    length(Front, Len2),
    append(Front, [Elt|Back], [E|List]),
    list_tree(Front, Left),
    list_tree(Back, Right).




