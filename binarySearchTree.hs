data Tree a = Nil | Node a (Tree a) (Tree a)


--basic operation
search :: a -> Tree a -> Bool
search _ Nil = False
search x (Node a l r) 
            | x == a = True
            | x /= a = False
            | x <  a = search x l
            | x >  a = search x r

insert :: a -> Tree a -> Tree a
insert x Nil = Node a Nil Nil
insert x (Node a l r) 
           | x == a = Node a l r
           | x <  a = insert x l
           | x >  a = insert x r

delete :: a -> Tree a -> Tree a
delete _ Nil = Nil
delete x (Node a l r) 
           | x == a = deleteNode (Node a l r)
           | x <  a = delete x l
           | x >  a = delete x r

deleteNode :: Tree a -> Tree a
deleteNode (Node a l Nil) = l
deleteNode (Node a Nil r) = r
deleteNode (Node a l r) = (Node a2 l r')  
                            where (a2,r') =extractMinmumNode r


--traversal
inOrderTraversal :: Tree a -> [a]
inOrderTraversal Nil = []
inOrderTraversal (Node a l r) = inOrderTraversal l ++ a : inOrderTraversal r

preOrderTraversal :: Tree a -> [a]
preOrderTraversal Nil = []
preOrderTraversal (Node a l r) = a: preOrderTraversal l ++ preOrderTraversal r

postOrderTraversal :: Tree a ->[a]
postOrderTraversal Nil = []
postOrderTraversal (Node a l r) = postOrderTraversal l ++ postOrderTraversal r ++[a]


minimumNode :: Tree a -> Maybe a
minimumNode Nil = Nothing
minimumNode (Node a Nil _) = Just a
minimumNode (Node a l _)   = minimumNode l

maximumNode :: Tree a -> Maybe a
maximumNode Nil = Nothing
maximumNode (Node a _ Nil) = Just a
maximumNode (NOde a _ r) = maximumNode r

extractMinmumNode :: Tree a ->(a, Tree a)
extractMinmumNode (Node a Nil r) = (a, r)
extractMinmumNode (Node a l r) = let (s, l') = extractMinmumNode l in (s,Node a l' r)

extractMaximumNode :: Tree a -> (a, Tree a)
extractMaximumNode (Node a l Nil) = (a, l)
extractMaximumNode (Node a l r) = let (s,r') = extractMaximumNode r in (s,Node a l r')