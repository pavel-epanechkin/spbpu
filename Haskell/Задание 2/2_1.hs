data BinaryTree = EmptyTree
				  | Leaf { intValue :: Integer }
				  | Node { intValue :: Integer, lst :: BinaryTree, rst :: BinaryTree } deriving(Show,Eq)

insert :: BinaryTree -> Integer -> BinaryTree
insert EmptyTree key = Leaf key
insert (Leaf k) key = insert (Node k EmptyTree EmptyTree) key
insert (Node k l r) key | key < k = Node k (insert l key) r
						| key > k = Node k l (insert r key)
						| k == key = Node k l r

remove :: BinaryTree -> Integer -> BinaryTree
remove EmptyTree _ = EmptyTree
remove (Leaf k) key = if (k == key) then EmptyTree else Leaf k
remove (Node k (Leaf lk) EmptyTree) key | lk == key = Leaf k 
										| k == key = Leaf lk
										| otherwise = Node k (Leaf lk) EmptyTree
remove (Node k EmptyTree (Leaf lk)) key | lk == key = Leaf k
										| k == key = Leaf lk 
										| otherwise = Node k EmptyTree (Leaf lk)
remove (Node k l r) key | l == EmptyTree && k == key = r
						| r == EmptyTree && k == key = l
						| key < k = Node k (remove l key) r
						| key > k = Node k l (remove r key)
						| k == key = Node (getMaxFromTree l) (remove l (getMaxFromTree l)) r
	  where getMaxFromTree tree = case tree of
	  		(Leaf k) 				-> k
	  		(Node k l EmptyTree) 	-> k
	  		(Node k l r)  			-> getMaxFromTree r

infixl 9 `insert`, `remove`

emptyTree :: BinaryTree
emptyTree = EmptyTree

containsElement :: BinaryTree -> Integer -> Bool
containsElement EmptyTree _ = False
containsElement (Leaf k) key = (k == key)
containsElement (Node k l r) key | k == key = True
								 | key < k = containsElement l key
								 | key > k = containsElement r key

nearestGE :: BinaryTree -> Integer -> Integer
nearestGE EmptyTree _ = error "Not found"
nearestGE binTree val =
		let iter tree v nGE = case tree of 
			EmptyTree 		-> nGE
			(Leaf k) 		-> if v <= k then [k] else nGE
			(Node k l r) 	-> if v == k then [k] else (if v < k then iter l v [k] else iter r v nGE) in
		filterRes (iter binTree val [])
		where filterRes res = case res of 
			[]			-> error "Not found"
			(h : t) 	-> h

treeFromList :: [Integer] -> BinaryTree
treeFromList list = foldl insert EmptyTree list

--listFromTree (обход в глубину)
listFromTree :: BinaryTree -> [Integer]
listFromTree binTree = 
	let iter tree list = case tree of
			EmptyTree 		-> list
			(Leaf k) 		-> k : list
			(Node k l r)	-> iter r (iter l (k : list)) in
    reverse (iter binTree [])

--listFromTree (обход в ширину)
--listFromTree :: BinaryTree -> [Integer]
--listFromTree binTree = 
--	let iter list [] = list;
--		iter list (h : t) = case h of
--			EmptyTree 		-> iter list t
--			(Leaf k) 		-> iter (k : list) t
--			(Node k l r)	-> iter (k : list) (t ++ [l] ++ [r]) in
--    reverse (iter [] [binTree])