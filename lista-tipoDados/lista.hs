data Triple a b c = Triple a b c deriving(Eq, Show)

tripleFst (Triple a b c) = a
tripleSnd (Triple a b c) = b
tripleThr (Triple a b c) = c

data Quadruple a b = Quadruple a a b b deriving(Eq, Show)

firstTwo (Quadruple a b c d) = (a,b)
secondTwo (Quadruple a b c d) = (c,d)

data Tuple a b c d = Tuple1 a | Tuple2 a b | Tuple3 a b c | Tuple4 a b c d deriving(Eq, Show)

tuple1 (Tuple1 a) = Just a
tuple1 (Tuple2 a b) = Just a
tuple1 (Tuple3 a b c) = Just a
tuple1 (Tuple4 a b c d) = Just a

tuple2 (Tuple2 a b) = Just b
tuple2 (Tuple3 a b c) = Just b
tuple2 (Tuple4 a b c d) Just b
tuple2 _ = Nothing

tuple3 (Tuple3 a b c) = Just c
tuple3 (Tuple4 a b c d) = Just c
tuple3 _ = Nothing

tuple4 (Tuple4 a b c d) = Just d
tuple4 _ = Nothing




