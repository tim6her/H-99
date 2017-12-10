data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (List []) = []
flatten (Elem x) = [x]
flatten (List ((Elem x):xs)) = x : flatten (List xs)
flatten (List ((List xs):ys)) = flatten (List xs) ++ flatten (List ys)
