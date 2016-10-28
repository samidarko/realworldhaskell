module Main where

type CustomerID = Int
type ReviewBody = String
data Book = Book Int String [String] deriving (Show)
data BookReview = BookReview Book CustomerID String
data BetterReview = BetterReview Book CustomerID ReviewBody
type BookRecord = (Book, BookReview)

mySwap :: (a,b) -> (b, a)
mySwap (a, b) = (b, a)

type Address = [String]
data Customer = Customer {
      customerID      :: CustomerID
    , customerName    :: String
    , customerAddress :: Address
    } deriving (Show)

data List a = Cons a (List a)
            | Nil
              deriving (Show)

fromList (x:xs) = Cons x (fromList xs)
fromList []     = Nil

toList (Cons x xs) = x : toList xs
toList Nil     = []

data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

single :: a -> Tree a
single x = Node x Empty Empty

insert :: (Ord a) => Tree a -> a -> Tree a
insert Empty x = single x
insert (Node n left right) x
    | x == n = Node n left right
    | x < n = Node n (insert left x) right
    | x > n = Node n left (insert right x)

data MaybeTree a = MaybeNode a (Maybe a) (Maybe a) deriving (Show)

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort [a | a <- xs, a > x]
    in  smallerSorted ++ [x] ++ biggerSorted

safeSecond :: [a] -> Maybe a
safeSecond (_:x:_) = Just x
safeSecond _ = Nothing

quux a = let a = "foo"
         in a ++ "eek!"

bar = let a = 1; b = 2
       in a + b

length' :: [a] -> Int
length' (x:xs) = 1 + length' xs
length' [] = 0

-- mean :: [Double] -> Double
-- mean :: (Num a) => [a] -> a
mean xs = num / den
    where
    num = sum xs
    den = fromIntegral $ length xs

palindrome :: (Ord a) => [a] -> [a]
palindrome xs = forward ++ backward
    where
    forward = quicksort xs
    backward = reverse forward

isOrdered :: (Ord a, Eq a) => [a] -> Bool
isOrdered [] = True
isOrdered (x:[]) = True
isOrdered (x:y:zs) | x <= y = isOrdered $ y:zs
isOrdered _ = False

palintest xs = if isEven && isBalanced xs then True else False
    where
    isEven = even $ length $ xs
    isBalanced ys = let (a, b) = splitAt (length ys `div` 2) ys in
        isOrdered a && a == reverse b

intersperse' :: Char -> [String] -> String
intersperse' c [] = ""
intersperse' c (x:xs) = x ++ [c] ++ intersperse' c xs

findIndex :: Char -> [Char] -> Maybe Int
findIndex c [] = Nothing
findIndex c xs = fn 0 xs
    where
        fn pos [] = Nothing
        fn pos (y:ys) = if y == c then (Just pos) else fn (pos+1) ys

alphaUpper :: Char -> Char
alphaUpper c = upper $ findIndex c ['a'..'z']
    where
        upper Nothing = c
        upper (Just n) = ['A'..'Z'] !! n

safeHead :: [a] -> Maybe a
safeHead (x:xs) = Just(x)
safeHead _ = Nothing

safeTail :: [a] -> Maybe [a]
safeTail (x:xs) = Just(xs)
safeTail _ = Nothing

safeLast :: [a] -> Maybe a
safeLast xs
    | length xs > 0 = Just (last xs)
    | otherwise = Nothing

safeInit :: [a] -> Maybe [a]
safeInit xs
    | length xs > 0 = Just (init xs)
    | otherwise = Nothing

oddList :: [Int] -> [Int]

oddList (x:xs)
    | odd x     = x : oddList xs
    | otherwise = oddList xs
oddList _                  = []

mySum xs = helper 0 xs
    where helper acc (x:xs) = helper (acc + x) xs
          helper acc _      = acc

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f [] = []
myFilter f (x:xs)
    | f x = x : myFilter f xs
    | otherwise = myFilter f xs

myFilterFoldr p xs = foldr step [] xs
    where step x ys | p x       = x : ys
                    | otherwise = ys

--myFilterFoldl p xs = foldr step [] xs
--    where step ys x | p x       = x : ys
--                    | otherwise = ys

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f z xs = foldr step id xs z
    where step x g a = g (f a x)