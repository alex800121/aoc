{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module MyLib where

import Control.Monad (guard, mplus)
import Data.Bits (xor)
import Data.Char (chr, digitToInt, intToDigit, isHexDigit, ord)
import Data.Foldable (Foldable (foldr'), toList)
import Data.List (delete, foldl', group, nub, tails, uncons)
import Data.List.Split (chunksOf)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, mapMaybe, maybeToList)
import Data.Proxy (Proxy (..))
import qualified Data.Sequence as S
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Void (Void)
import Debug.Trace
import Text.Megaparsec
import Text.Megaparsec.Char (space)
import Text.Megaparsec.Char.Lexer (decimal, signed)

drawASCII :: (Integral a) => [a] -> String
drawASCII = map (chr . fromIntegral)

pickAnySplit :: [a] -> [(a, [a])]
pickAnySplit = f id
  where
    f _ [] = []
    f g (x : xs) = (x, g xs) : f (g . (x :)) xs

(+&) :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
(a, b) +& (c, d) = (a + c, b + d)

(-&) :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
(a, b) -& (c, d) = (a - c, b - d)

data Tree a = Leaf a | Branch a [Tree a] deriving (Show, Eq, Ord)

instance Functor Tree where
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Branch a xs) = Branch (f a) (map (fmap f) xs)

instance Foldable Tree where
  foldMap f (Leaf a) = f a
  foldMap f (Branch a xs) = f a `mappend` foldMap (foldMap f) xs

instance Traversable Tree where
  traverse f (Leaf a) = Leaf <$> f a
  traverse f (Branch a xs) = Branch <$> f a <*> traverse (traverse f) xs

data Direction = North | East | South | West deriving (Show, Eq, Ord)

instance Enum Direction where
  fromEnum North = 0
  fromEnum East = 1
  fromEnum South = 2
  fromEnum West = 3
  toEnum n = case n `mod` 4 of
    0 -> North
    1 -> East
    2 -> South
    3 -> West

type Parser = Parsec Void String

emcd :: (Integral a) => a -> a -> (a, a, a)
emcd a 0 = (a, 1, 0)
emcd a b = (g, s, t - (q * s))
  where
    (q, r) = divMod a b
    (g, t, s) = emcd b r

firstRepeat :: (Eq a) => [a] -> Maybe (Int, a)
firstRepeat = g 0 []
  where
    g _ _ [] = Nothing
    g i s (x : xs) = if x `elem` s then Just (i, x) else g (i + 1) (x : s) xs

firstRepeat' :: (Ord a) => [a] -> Maybe (Int, a)
firstRepeat' = g 0 Set.empty
  where
    g _ _ [] = Nothing
    g i s (x : xs) = if x `Set.member` s then Just (i, x) else g (i + 1) (Set.insert x s) xs

firstRepeatBy :: (a -> a -> Bool) -> [a] -> Maybe (Int, a)
firstRepeatBy f = g 0 []
  where
    g _ _ [] = Nothing
    g i s (x : xs) = if any (f x) s then Just (i, x) else g (i + 1) (x : s) xs

stablizedBy :: (a -> a -> Bool) -> [a] -> Maybe (Int, a)
stablizedBy f = go 0
  where
    go i (x : y : xs) = if f x y then Just (i + 1, y) else go (i + 1) (y : xs)
    go _ _ = Nothing

stablized :: (Eq a) => [a] -> Maybe (Int, a)
stablized = stablizedBy (==)

signedInteger :: Parser Int
signedInteger = signed space decimal

mapFirst :: (a -> b) -> (a, c) -> (b, c)
mapFirst f (a, c) = (f a, c)

third :: (a, b, c) -> c
third (_, _, x) = x

data KnotList = KnotList {focus :: Int, list :: S.Seq Int} deriving (Show)

twistKnotList :: Int -> KnotList -> KnotList
twistKnotList i (KnotList f l)
  | i + f > len =
      let excess = i + f - len
          (left, right) = S.splitAt f (l S.>< S.take excess l)
          (l', newHead) = S.splitAt len (left S.>< S.reverse right)
       in KnotList f (newHead S.>< S.drop excess l')
  | otherwise =
      let (left, right') = S.splitAt f l
          (mid, right) = S.splitAt i right'
       in KnotList f (left S.>< S.reverse mid S.>< right)
  where
    len = S.length l

knotHash :: String -> String
knotHash s =
  let inputSuffix = [17, 31, 73, 47, 23]
      initSeq = KnotList 0 $ S.fromList [0 .. 255]
      moveFocus :: Int -> KnotList -> KnotList
      moveFocus i l = l {focus = (l.focus + i) `mod` S.length l.list}
      step :: Int -> Int -> KnotList -> KnotList
      step skip i = moveFocus (skip + i) . twistKnotList i
      run :: [Int] -> KnotList -> KnotList
      run i k = foldl (flip (uncurry step)) k l
        where
          l = zip [0 ..] i
      process :: S.Seq Int -> String
      process = map intToDigit . concatMap ((\x -> [x `div` 16, x `mod` 16]) . foldl1 xor) . chunksOf 16 . toList
      l = map ord s ++ inputSuffix
      list = take (length l * 64) $ cycle l
      k = run list initSeq
   in process k.list

baseNToInteger :: Integer -> String -> Integer
baseNToInteger n = foldl' (\acc x -> (n * acc) + fromIntegral (digitToInt x)) 0

baseNToInt :: Int -> String -> Int
baseNToInt n = fromIntegral . baseNToInteger (fromIntegral n)

intToBits :: Int -> String
intToBits x
  | x == 0 = "0"
  | x > 0 = f x []
  | x < 0 = '-' : intToBits (negate x)
  where
    f 0 = id
    f x = f (x `div` 2) . (intToDigit (x `mod` 2) :)

hexTo4Bits :: Char -> Maybe String
hexTo4Bits l
  | isHexDigit l = let l' = intToBits $ digitToInt l in Just $ replicate (4 - length l') '0' ++ l'
  | otherwise = Nothing

(!?) :: [a] -> Int -> Maybe a
l !? i = if i < 0 || i >= length l then Nothing else Just (l !! i)

drawMap :: (a -> Maybe b) -> [[a]] -> Map (Int, Int) b
drawMap convert l = f 0 0
  where
    f x y = case l !? y of
      Nothing -> Map.empty
      Just y' -> case y' !? x of
        Nothing -> f 0 (y + 1)
        Just a -> case convert a of
          Nothing -> f (x + 1) y
          Just b -> Map.insert (x, y) b (f (x + 1) y)

drawMapWithKey :: ((Int, Int) -> a -> Maybe b) -> [[a]] -> Map (Int, Int) b
drawMapWithKey convert l = f 0 0
  where
    f x y = case l !? y of
      Nothing -> Map.empty
      Just y' -> case y' !? x of
        Nothing -> f 0 (y + 1)
        Just a -> case convert (x, y) a of
          Nothing -> f (x + 1) y
          Just b -> Map.insert (x, y) b (f (x + 1) y)

drawGraph :: (Maybe a -> b) -> Map (Int, Int) a -> [[b]]
drawGraph convert m = f minY
  where
    k = Map.keys m
    minX = minimum $ map fst k
    maxX = maximum $ map fst k
    minY = minimum $ map snd k
    maxY = maximum $ map snd k
    f y
      | y > maxY = []
      | otherwise = [convert (m Map.!? (x, y)) | x <- [minX .. maxX]] : f (y + 1)

sumVariants :: (Num a, Eq a, Ord a) => a -> [a] -> [[a]]
sumVariants target choices
  | target == 0 = [[]]
  | target /= 0 && null choices = []
  | otherwise = do
      (a : t) <- init $ tails $ choices
      b <- sumVariants (target - a) t
      return (a : b)

primeFactors :: (Integral a) => a -> [a]
primeFactors a = f a primes []
  where
    primes = takeWhile (<= sqrtCeiling a) primeSeive
    f 1 _ ls = ls
    f x [] ls = x : ls
    f x (y : ys) ls
      | x `mod` y == 0 = f (x `div` y) (y : ys) (y : ls)
      | otherwise = f x ys ls

primeFactors' :: (Integral a) => a -> [(Int, a)]
primeFactors' = map ((,) <$> length <*> head) . group . primeFactors

factors :: (Integral a) => a -> [a]
factors x =
  let p = primeFactors' x
      f [] = [1]
      f ((n, a) : xs) = (*) <$> map (a ^) [0 .. n] <*> f xs
   in f p

sqrtCeiling :: (Integral a) => a -> a
sqrtCeiling = ceiling . sqrt . fromIntegral

primeSeive :: (Integral a) => [a]
primeSeive = f [2 ..]
  where
    f (x : xs) = x : f (filter ((/= 0) . (`mod` x)) xs)

data Nat = Z | S Nat deriving (Eq, Ord)

data SNat (n :: Nat) where
  SZ :: SNat Z
  SS :: SNat n -> SNat (S n)

data Vec (n :: Nat) a where
  Nil :: Vec 'Z a
  Cons :: a -> Vec n a -> Vec (S n) a

deriving instance (Eq a) => Eq (Vec n a)

deriving instance (Ord a) => Ord (Vec n a)

deriving instance Functor (Vec n)

deriving instance Foldable (Vec n)

instance Show Nat where
  show n = show' 0 n
    where
      show' n Z = "Nat" ++ show n
      show' n (S x) = show' (n + 1) x

-- instance Eq a => Eq (Vec 'Z a) where
--   Nil == Nil = True
-- instance (Eq a, Eq (Vec n a)) => Eq (Vec ('S n) a) where
--   Cons x xs == Cons y ys = x == y && xs == ys

instance (Show a) => Show (Vec n a) where
  show Nil = "<>"
  show (Cons x xs) = '<' : show x ++ show' xs
    where
      show' :: (Show a) => Vec n a -> String
      show' Nil = ">"
      show' (Cons y ys) = ',' : show y ++ show' ys

-- instance Functor (Vec 'Z) where
--   fmap _ Nil = Nil

-- instance Functor (Vec n) => Functor (Vec ('S n)) where
--   fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative (Vec 'Z) where
  pure _ = Nil
  Nil <*> Nil = Nil

instance (Applicative (Vec n)) => Applicative (Vec ('S n)) where
  pure a = Cons a (pure a)
  Cons f fs <*> Cons x xs = Cons (f x) (fs <*> xs)

instance (Num a) => Num (Vec 'Z a) where
  Nil + Nil = Nil
  Nil * Nil = Nil
  abs Nil = Nil
  signum Nil = Nil
  fromInteger _ = Nil
  negate Nil = Nil

instance (Num a, Num (Vec n a), Applicative (Vec n)) => Num (Vec ('S n) a) where
  Cons x xs + Cons y ys = Cons (x + y) (xs + ys)
  Cons x xs * Cons y ys = Cons (x * y) (xs * ys)
  abs (Cons x xs) = Cons (abs x) (abs xs)
  signum (Cons x xs) = Cons (signum x) (signum xs)
  fromInteger = pure . fromInteger
  negate (Cons x xs) = Cons (negate x) (negate xs)

manhattan :: (Num a) => Vec n a -> a
manhattan = sum . fmap abs

overlapEucVec :: (Ord a) => Vec n (a, a) -> Vec n (a, a) -> Maybe (Vec n (a, a))
overlapEucVec Nil Nil = pure Nil
overlapEucVec (Cons (a, b) xs) (Cons (c, d) ys)
  | maxOfSmall < minOfBig = do
      rest <- overlapEucVec xs ys
      pure (Cons (maxOfSmall, minOfBig) rest)
  | otherwise = Nothing
  where
    maxOfSmall = max a c
    minOfBig = min b d

subtractEucVec :: (Ord a) => Vec n (a, a) -> Vec n (a, a) -> [Vec n (a, a)]
subtractEucVec Nil Nil = []
subtractEucVec (Cons (a, b) xs) (Cons (c, d) ys) =
  [Cons (x, y) ys | (x, y) <- [(max b c, d), (c, min a d)], y > x]
    ++ [Cons (x', y') rest | (x', y') <- [(max a c, min b d)], y' > x', rest <- subtractEucVec xs ys]

subtractEucVecs' :: (Ord a) => Vec n (a, a) -> [Vec n (a, a)] -> [Vec n (a, a)]
subtractEucVecs' x = concatMap (subtractEucVec x)

subtractEucVecs :: (Ord a) => [Vec n (a, a)] -> [Vec n (a, a)] -> [Vec n (a, a)]
subtractEucVecs xs ys = foldr' subtractEucVecs' ys xs

jointEucVec :: (Ord a) => Vec n (a, a) -> Vec n (a, a) -> [Vec n (a, a)]
jointEucVec x y = x : subtractEucVec x y

jointEucVecs' :: (Ord a) => [Vec n (a, a)] -> Vec n (a, a) -> [Vec n (a, a)]
jointEucVecs' xs y = y : concatMap (subtractEucVec y) xs

jointEucVecs :: (Ord a) => [Vec n (a, a)] -> [Vec n (a, a)]
jointEucVecs = foldl' jointEucVecs' []

fromVec :: Vec n a -> [a]
fromVec Nil = []
fromVec (Cons x xs) = x : fromVec xs

toVec :: SNat n -> [a] -> Vec n a
toVec SZ _ = Nil
toVec (SS _) [] = error "Given Vec length is longer than given list"
toVec (SS n) (x : xs) = Cons x $ toVec n xs

vZipWith :: (a -> b -> c) -> Vec n a -> Vec n b -> Vec n c
vZipWith _ Nil Nil = Nil
vZipWith f (Cons x xs) (Cons y ys) = Cons (f x y) $ vZipWith f xs ys

vTail :: Vec (S n) a -> Vec n a
vTail (Cons _ xs) = xs

pick :: Int -> [a] -> [[a]]
pick n l
  | n <= 0 = pure []
  | otherwise = do
      (x, xs) <- mapMaybe uncons $ tails l
      (x :) <$> pick (n - 1) xs

extEuc :: (Integral a) => a -> a -> (a, a, a)
extEuc a b = go a 1 0 b 0 1
  where
    go r0 s0 t0 r1 s1 t1
      | r2 == 0 = (s1, t1, gcd r0 r1)
      | otherwise = go r1 s1 t1 r2 s2 t2
      where
        (q1, r2) = r0 `divMod` r1
        s2 = s0 - (q1 * s1)
        t2 = t0 - (q1 * t1)
