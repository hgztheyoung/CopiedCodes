--Raw FingerTree Without Measurements

{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}
module  Main where

main :: IO ()
main = putStrLn "Hello World"

class Reduce f where
  reducer :: (a->b->b) -> (f a->b->b)
  reducel :: (b->a->b) -> (b->f a->b)

instance Reduce [] where
  reducer f x z = foldr f z x
  reducel  = foldl

toList :: (Reduce f) => f a -> [a]
toList s = consbar s [] where
  consbar = reducer (:)

data Node a = Node2 a a | Node3 a a a
  deriving Show

data FingerTree a =
  Empty
  | Single a
  | Deep (Digit a) (FingerTree (Node a)) (Digit a)
  deriving Show

type Digit a = [a]

instance Reduce Node where
  reducer (-<) (Node2 a b) z = a -< (b -< z)
  reducer (-<) (Node3 a b c) z = a -< (b -< (c -< z))
  reducel (>-) z (Node2 b a) = (z >- b) >- a
  reducel (>-) z (Node3 c b a) = ((z >- c) >- b) >- a

instance Reduce FingerTree where
  reducer _ Empty z = z
  reducer (-<) (Single x) z = x -< z
  reducer (-<) (Deep pr m sf ) z = pr -<< (m -<<< (sf -<< z))
    where (-<<) = reducer (-<)
          (-<<<) = reducer (reducer (-<))
  reducel _ z Empty = z
  reducel (>-) z (Single x) = z >- x
  reducel (>-) z (Deep pr m sf ) = ((z >>- pr) >>>- m) >>- sf
    where (>>-) = reducel (>-)
          (>>>-) = reducel (reducel (>-))

infixr 5 <|
(<|) :: a -> FingerTree a -> FingerTree a
a <| Empty = Single a
a <| Single b = Deep [a] Empty [b]
a <| Deep [b,c,d,e] m sf = Deep [a,b] (Node3 c d e <| m) sf
a <| Deep pr m sf = Deep ([a] ++ pr) m sf

infixl 5 |>
(|>)  :: FingerTree a -> a -> FingerTree a
Empty               |> a = Single a
Single b            |> a = Deep [b] Empty [a]
Deep pr m [e,d,c,b] |> a = Deep pr (m |> Node3 e d c) [b,a]
Deep pr m sf        |> a = Deep pr m (sf ++ [a])

(<||) :: (Reduce f) => f a -> FingerTree a -> FingerTree a
(<||) = reducer (<|)

(||>) :: (Reduce f) => FingerTree a -> f a -> FingerTree a
(||>) = reducel (|>)

toTree :: (Reduce f) => f a -> FingerTree a
toTree s = s <|| Empty

--We need
--FingerTree (Node a) -> Digit a -> Digit a -> FingerTree (Node a)-> FingerTree (Node a) for catenating two Deep
-- Try to implement Digit a -> Digit a -> [Node a] or [a] -> [Node a]
-- and FingerTree a -> [a] -> FingerTree

data ViewL s a = NilL | ConsL a (s a)
data ViewR s a = NilR | ConsR (s a) a

viewL :: FingerTree a -> ViewL FingerTree a
viewL Empty = NilL
viewL (Single x) = ConsL x Empty
viewL (Deep pr m sf) = ConsL (head pr) (deepL (tail pr) m sf)

viewR :: FingerTree a -> ViewR FingerTree a
viewR Empty = NilR
viewR (Single x) = ConsR Empty x
viewR (Deep pr m sf) = ConsR (deepR pr m (init sf)) (last sf)

deepL :: [a] -> FingerTree (Node a) -> Digit a -> FingerTree a
deepL [] m sf  =
  case viewL m of
    NilL -> toTree sf
    ConsL a mbar -> Deep (toList a) mbar sf
deepL pr m sf = Deep pr m sf

deepR :: [a] -> FingerTree (Node a) -> Digit a -> FingerTree a
deepR pr m [] =
  case viewR m of
    NilR -> toTree pr
    ConsR mbar a -> Deep pr mbar (toList a)
deepR pr m sf = Deep pr m sf

isEmpty :: FingerTree a -> Bool
isEmpty x = case viewL x of
  NilL -> True
  ConsL _ _ -> False

headL :: FingerTree a -> a
headL x = case viewL x of ConsL a _ -> a

tailL :: FingerTree a -> FingerTree a
tailL x = case viewL x of ConsL _ x' -> x'

lastR :: FingerTree a -> a
lastR x = case viewR x of ConsR _ a -> a

initR :: FingerTree a -> FingerTree a
initR x = case viewR x of ConsR x' _ -> x'

--In a lazy language like Haskell, the tail part of the view is not computed unless it is used. In a strict language, it might be more useful to provide the three separate functions as primitives.

app3 :: FingerTree a -> [a] -> FingerTree a -> FingerTree a
app3 Empty ts xs = ts <|| xs
app3 xs ts Empty = xs ||> ts
app3 (Single x) ts xs = x <| (ts <|| xs)
app3 xs ts (Single x) = (xs ||> ts) |> x
app3 (Deep pr1 m1 sf1) ts (Deep pr2 m2 sf2) =
  Deep pr1 (app3 m1 (nodes (sf1 ++ ts ++ pr2)) m2) sf2

nodes :: [a] -> [Node a]
nodes [a,b] = [Node2 a b]
nodes [a,b,c] = [Node3 a b c]
nodes [a,b,c,d] = [Node2 a b,Node2 c d]
nodes (a:b:c:xs) = Node3 a b c : nodes xs

(><) :: FingerTree a -> FingerTree a -> FingerTree a
xs >< ys = app3 xs [] ys


instance Monoid (FingerTree a) where
  mempty = Empty
  mappend = (><)
