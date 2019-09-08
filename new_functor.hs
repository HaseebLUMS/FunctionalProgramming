-- defining a new data type
data IsGood a = Bad | Good a deriving (Show, Ord, Eq, Read)

-- making it a functor
instance Functor IsGood where
    fmap f (Good x) = Good (f x)
    fmap f Bad = Bad