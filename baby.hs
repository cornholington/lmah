foo x = x + x

bar x y = foo x + foo y

baz x = if x > 100 then x else x*2

mytake x y = if null x || y == 0 then [] else [ (head x) ] ++ mytake (tail x) (y-1)

mycycle x = x ++ mycycle x
