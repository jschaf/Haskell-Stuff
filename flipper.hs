

tuple :: a -> b -> c -> (a,b,c)
tuple = (,,)

plus4 a b c d = a : b : c : d : []
plus3 a b c = a:b:c:[]