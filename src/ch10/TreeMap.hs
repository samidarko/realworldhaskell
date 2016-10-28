--import Prelude(Functor)

data Tree a = Node (Tree a) (Tree a)
            | Leaf a
              deriving (Show)

--treeLengths (Leaf s) = Leaf (length s)
--treeLengths (Node l r) = Node (treeLengths l) (treeLengths r)

treeMap :: (a -> b) -> Tree a -> Tree b
treeMap f (Leaf a)   = Leaf (f a)
treeMap f (Node l r) = Node (treeMap f l) (treeMap f r)

--class Functor f where
--    fmap :: (a -> b) -> f a -> f b

instance Functor Tree where
    fmap = treeMap

--let tree = Node (Leaf "foo") (Node (Leaf "x") (Leaf "quux"))
--fmap length tree