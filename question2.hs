myButLast :: [a] -> a
myButLast [] = error "No one but last element for empty lists!"
myButLast [x] = error "No one but last element for singelton"
myButLast [x, _] = x
myButLast (_ : xs) = myButLast xs
