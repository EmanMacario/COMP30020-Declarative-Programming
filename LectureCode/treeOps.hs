-- File: treeOps.hs
-- Author: Emmanuel Macario <macarioe>
-- Origin: 15:30 Sun 20 Aug 2017

-- Purpose: This is a module that contains functions for 
-- manipulating and searching BSTs


-- Binary Search Tree is either a 'Leaf', or a 'Node' that contains a string
-- key and an integer value.
data Tree = Leaf | Node String Int Tree Tree
     deriving (Show)


-- Function 'countnodes' accepts a BST as input and returns an integer
-- that is the total number of nodes in the tree.
countnodes :: Tree -> Int
countnodes Leaf = 0
countnodes (Node _ _ l r) =
    1 + (countnodes l) + (countnodes r)


-- Function 'search_bst' takes a BST and key as input, and searches the tree to
-- see if it contains the key. If search succeeds, function returns the searched
-- for value, otherwise returns nothing.
search_bst :: Tree -> String -> Maybe Int
search_bst Leaf _ = Nothing
search_bst (Node k v l r) sk =
    if sk == k then
        Just v
    else if sk < k then
        search_bst l sk
    else
        search_bst r k



-- Function 'insert_bst' takes a BST and insertion key/value pair, and updates
-- the BST by inserting the key/value pair in its rightful position. If the 
-- insertion key is already in the table, then just replaces the old value with
-- the new insertion value.
insert_bst :: Tree -> String -> Int -> Tree
insert_bst Leaf ik iv = Node ik iv Leaf Leaf
insert_bst (Node k v l r) ik iv =
    if ik == k then
        Node ik iv l r
    else if ik < k then
        Node k v (insert_bst l ik iv) r
    else
        Node k v l (insert_bst r ik iv)



-- Function 'assoc_list_to_bst' takes a list of (String, Int) i.e. key/value 
-- pairs, and inserts them into an initially empty BST
assoc_list_to_bst :: [(String, Int)] -> Tree
assoc_list_to_bst [] = Leaf
assoc_list_to_bst ((hk ,hv):kvs) =
    let t0 = assoc_list_to_bst kvs
    in insert_bst t0 hk kv
