{-# LANGUAGE BlockArguments #-}

module Main where

import Control.Category
import Data.Foldable (toList)
import Prelude hiding (id, (.))

newtype a -&> b = H {invoke :: (b -&> a) -> b}

run :: (a -&> a) -> a
run h = invoke h id

instance Category (-&>) where
  id = H run
  f . g = H \k -> invoke f (g . k)

(!>) :: (i -> o) -> (i -&> o) -> (i -&> o)
a !> b = H \k -> a (invoke k b)

rep :: (i -> o) -> (i -&> o)
rep f = f !> rep f

zipping :: (Foldable f, Monoid o) => f a -> (a -> o) -&> o
zipping = foldr (\y t -> ($ y) !> t) (rep mempty)

zipWith4 ::
  (Foldable f, Foldable g, Foldable h, Foldable i) =>
  (x -> y -> z -> w -> o) ->
  (f x -> g y -> h z -> i w -> [o])
zipWith4 o xs ys zs ws =
  run $
    zipping xs
      . zipping ys
      . zipping zs
      . zipping ws
      . rep \r w z y x -> o x y z w : r

data Daisy f g c where Grow :: f a -> g b -> (a -> b -> c) -> Daisy f g c

instance (Foldable f, Foldable g) => Foldable (Daisy f g) where
  foldMap h (Grow fa gb k) = run $ zipping fa . zipping gb . rep \r b a -> h (k a b) <> r

data DaisyChain fs c where
  Link :: c -> DaisyChain '[] c
  (:=:) :: DaisyChain fs (a -> c) -> f a -> DaisyChain (f ': fs) c

infixl 5 :=:

instance Foldable (DaisyChain '[]) where
  foldMap h (Link c) = h c

instance (Foldable f) => Foldable (DaisyChain '[f]) where
  foldMap h (Link k :=: fa) = foldMap (h . k) fa

instance (Foldable f, Foldable (DaisyChain (ff ': fs))) => Foldable (DaisyChain (f ': ff ': fs)) where
  foldMap h (fs :=: fa) = run $ zipping fs . zipping fa . rep \r c a -> h (a c) <> r

main :: IO ()
main = do
  print $ zipWith4 (,,,) [1, 2, 3 :: Int] ['a', 'b', 'c', 'd'] [True, False, True] [(), (), ()]
  print $ toList (Grow [1, 2, 3 :: Int] ['a', 'b', 'c', 'd'] (,))
  print $ toList (Link (,,) :=: [1, 2, 3 :: Int] :=: ['a', 'b', 'c', 'd'] :=: [True, False, True])

-- $> main
-- [(1,'a',True,()),(2,'b',False,()),(3,'c',True,())]
-- [(1,'a'),(2,'b'),(3,'c')]
-- [(1,'a',True),(2,'b',False),(3,'c',True)]
