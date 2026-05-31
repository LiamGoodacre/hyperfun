module Main where

import Control.Applicative.Free (Ap)
import Control.Applicative.Free qualified as Ap
import Control.Category
import Control.Monad.Fix (fix)
import Data.Coerce (coerce)
import Data.Foldable (toList)
import Data.Function ((&))
import Data.Functor.Compose (Compose (..))
import Data.List (tails)
import Data.Profunctor
import Prelude hiding (id, (.))

newtype a -&> b = H {invoke :: (b -&> a) -> b}

run :: (a -&> a) -> a
run h = invoke h id
{-# INLINE run #-}

instance Category (-&>) where
  id = H \k -> invoke k id
  {-# INLINE id #-}
  f . g = H \k -> invoke f (g . k)
  {-# INLINE (.) #-}

unroll :: (a -&> b) -> ((a -&> b) -> a) -> b
unroll (H e) k = e (H k)
-- unroll = coerce
{-# INLINE unroll #-}

roll :: (((a -&> b) -> a) -> b) -> (a -&> b)
roll = coerce
{-# INLINE roll #-}

ana :: (x -> (x -> a) -> b) -> x -> (a -&> b)
ana psi = f where f x = H \z -> psi x (invoke z . f)
{-# INLINE ana #-}

cata :: (((x -> a) -> b) -> x) -> (a -&> b) -> x
cata phi = f where f h = phi $ \g -> unroll h (g . f)
{-# INLINE cata #-}

instance Profunctor (-&>) where
  dimap f g h = H $ g . invoke h . dimap g f
  {-# INLINE dimap #-}
  lmap f h = H $ invoke h . rmap f
  {-# INLINE lmap #-}
  rmap f h = H $ f . invoke h . lmap f
  {-# INLINE rmap #-}

instance Functor ((-&>) a) where
  fmap = rmap
  {-# INLINE fmap #-}

instance Applicative ((-&>) a) where
  pure a = H $ \_ -> a
  {-# INLINE pure #-}
  p <* _ = p
  {-# INLINE (<*) #-}
  _ *> p = p
  {-# INLINE (*>) #-}
  (<*>) = curry $ ana $ \(i, j) fga ->
    unroll i (\i' -> fga (i', j)) $ unroll j (\j' -> fga (i, j'))
  {-# INLINE (<*>) #-}

(<***>) :: forall z a b. (z -&> (a -> b)) -> (z -&> a) -> z -&> b
(<***>) z_ab z_a = H \bz -> do
  let f = invoke z_ab (H \z_ab' -> invoke bz (z_ab' <***> z_a))
  let x = invoke z_a (H \z_a' -> invoke bz (z_ab <***> z_a'))
  f x

instance Monad ((-&>) a) where
  return = pure
  {-# INLINE return #-}
  m >>= f = cata (\g -> roll $ \k -> unroll (f (g k)) k) m
  {-# INLINE (>>=) #-}

(!>) :: (i -> o) -> (i -&> o) -> (i -&> o)
a !> b = H \k -> a (invoke k b)
{-# INLINE (!>) #-}

infixr 5 !>

send :: i -> ((i -> o) -&> o) -> (i -> o) -&> o
send x t = ($ x) !> t
{-# INLINE send #-}

fold :: [a] -> (a -> b -> c) -> c -> (b -&> c)
fold xs o z = foldr (\y t -> o y !> t) (pure z) xs
{-# INLINE fold #-}

build :: (forall b c. (a -> b -> c) -> c -> (b -&> c)) -> [a]
build g = run (g (:) [])
{-# INLINE build #-}

rep :: (i -> o) -> (i -&> o)
rep f = f !> rep f

unsafeCycleReps :: [i -> o] -> (i -&> o)
unsafeCycleReps f = foldr (!>) undefined (cycle f)
{-# INLINE unsafeCycleReps #-}

pass :: v -> (v -> o) -&> o
pass v = rep ($ v)
{-# INLINE pass #-}

together :: (a -&> b) -> (a -&> b) -> (a -&> b)
together f g = H \k -> invoke f (H \j -> invoke k (together g j))
{-# INLINE together #-}

shortInterleave :: forall a f g. (Foldable f, Foldable g) => f a -> g a -> [a]
shortInterleave = do
  let use a b = H \k -> a : invoke k b
  \l r -> run (foldr use (pure []) l . foldr use (pure []) r)
{-# INLINE shortInterleave #-}

shortInterleaving :: forall a f g. (Foldable f, Foldable g) => f (g a) -> [a]
shortInterleaving = do
  let use a b = H \k -> a : invoke k b
  let useAll = foldr use (pure [])
  run . foldr (\xs r -> useAll xs . r) id
{-# INLINE shortInterleaving #-}

longInterleave :: forall a f g. (Foldable f, Foldable g) => f a -> g a -> [a]
longInterleave = do
  let use a b = H \k _ -> a : invoke k b True
  let kickit j = invoke j (H \k b -> if b then kickit k else []) False
  \l r -> kickit (foldr use id l . foldr use id r)
{-# INLINE longInterleave #-}

longInterleaving :: forall a f g. (Foldable f, Foldable g) => f (g a) -> [a]
longInterleaving = do
  -- We have a boolean flag passed along through the processes.
  -- The flag starts out as False from kickit.
  -- When a process has something to contribute, it replaces it with True.
  -- When kickit's process retrieves the flag after a cycle through,
  -- it can end if no process contributed, or keep going otherwise.
  let use a b = H \k _ -> a : invoke k b True
  let useAll = foldr use id
  let kickit j = invoke j (H \k b -> if b then kickit k else []) False
  kickit . foldr (\xs r -> useAll xs . r) id
{-# INLINE longInterleaving #-}

apFold ::
  forall x f g h.
  (Functor f, Functor g, Functor h) =>
  Ap f x ->
  (forall a b. f a -> g (a -> b) -> h b) ->
  (x -> h x) ->
  g x -&> h x
apFold ap c n = case ap of
  Ap.Pure x -> pure (n x)
  Ap.Ap @_ @y h t -> do
    _implement_me

apBuild ::
  ( forall g h.
    ( forall a.
      f a ->
      g (a -> x) ->
      h x
    ) ->
    (x -> h x) ->
    (g x -&> h x)
  ) ->
  Ap f x
apBuild g = run (g Ap.Ap Ap.Pure)

-- longInterleavingAp :: (x -> y -> z) -> Ap f x -> Ap g y -> Ap f z
-- longInterleavingAp = do
--   let useAll = undefined
--   \l r -> run (
--   run . fix \me -> \case
--     Ap.Ap h t -> useAll h . me t
--     Ap.Pure x -> id x

{- {{{ longInterleaving trace-ish

longInterleaving
  [ [1, 4],
    [2, 5, 7, 8, 9],
    [3, 6]
  ]

kickit
  ( useAll [1, 4] .
    useAll [2, 5, 7, 8, 9] .
    useAll [3, 6] .
    id
  )

invoke
  ( use 1 (use 4 id) .
    use 2 (use 5 (use 7 (use 8 (use 9 id)))) .
    use 3 (use 6 id) .
    id
  )
  (H \k b -> if b then kickit k else [])
  False

invoke
  (H \k ->
    invoke
      (use 1 (use 4 id))
      ( use 2 (use 5 (use 7 (use 8 (use 9 id)))) .
        use 3 (use 6 id) .
        id .
        k
      )
  )
  (H \k b -> if b then kickit k else [])
  False

invoke
  (use 1 (use 4 id))
  ( use 2 (use 5 (use 7 (use 8 (use 9 id)))) .
    use 3 (use 6 id) .
    id .
    (H \k b -> if b then kickit k else [])
  )
  False

  1 :
    invoke
      ( use 2 (use 5 (use 7 (use 8 (use 9 id)))) .
        use 3 (use 6 id) .
        id .
        (H \k b -> if b then kickit k else [])
      )
      (use 4 id)
      True

  1 :
    invoke
      (H \k ->
        invoke
          (use 2 (use 5 (use 7 (use 8 (use 9 id)))))
          ( use 3 (use 6 id) .
            id .
            (H \k b -> if b then kickit k else []) .
            k
          )
      )
      (use 4 id)
      True

  1 :
    invoke
      (use 2 (use 5 (use 7 (use 8 (use 9 id)))))
      ( use 3 (use 6 id) .
        id .
        (H \k b -> if b then kickit k else []) .
        (use 4 id)
      )
      True

  1 : 2 :
    invoke
      ( use 3 (use 6 id) .
        id .
        (H \k b -> if b then kickit k else []) .
        (use 4 id)
      )
      (use 5 (use 7 (use 8 (use 9 id))))
      True

  1 : 2 :
    invoke
      (use 3 (use 6 id))
      ( id .
        (H \k b -> if b then kickit k else []) .
        use 4 id .
        use 5 (use 7 (use 8 (use 9 id)))
      )
      True

  1 : 2 : 3 :
    invoke
      ( id .
        (H \k b -> if b then kickit k else []) .
        use 4 id .
        use 5 (use 7 (use 8 (use 9 id)))
      )
      (use 6 id)
      True

  1 : 2 : 3 :
    invoke
      ( (H \k b -> if b then kickit k else []) .
        use 4 id .
        use 5 (use 7 (use 8 (use 9 id))) .
        use 6 id
      )
      id
      True

  1 : 2 : 3 :
    invoke
      (H \k b -> if b then kickit k else [])
      ( use 4 id .
        use 5 (use 7 (use 8 (use 9 id))) .
        use 6 id .
        id
      )
      True

  1 : 2 : 3 :
    if True
      then
        kickit
          ( use 4 id .
            use 5 (use 7 (use 8 (use 9 id))) .
            use 6 id .
            id
          )
      else []

  1 : 2 : 3 :
    kickit
      ( use 4 id .
        use 5 (use 7 (use 8 (use 9 id))) .
        use 6 id .
        id
      )

  1 : 2 : 3 :
    invoke
      ( use 4 id .
        use 5 (use 7 (use 8 (use 9 id))) .
        use 6 id .
        id
      )
      (H \k b -> if b then kickit k else [])
      False

  1 : 2 : 3 :
    invoke
      (use 4 id)
      ( use 5 (use 7 (use 8 (use 9 id))) .
        use 6 id .
        id .
        (H \k b -> if b then kickit k else [])
      )
      False

  1 : 2 : 3 : 4 :
    invoke
      ( use 5 (use 7 (use 8 (use 9 id))) .
        use 6 id .
        id .
        (H \k b -> if b then kickit k else [])
      )
      id
      True

  1 : 2 : 3 : 4 : 5 :
    invoke
      ( use 6 id .
        id .
        (H \k b -> if b then kickit k else []) .
        id
      )
      (use 7 (use 8 (use 9 id)))
      True

  1 : 2 : 3 : 4 : 5 :
    invoke
      (use 6 id)
      ( id .
        (H \k b -> if b then kickit k else []) .
        id .
        use 7 (use 8 (use 9 id))
      )
      True

  1 : 2 : 3 : 4 : 5 : 6 :
    invoke
      ( id .
        (H \k b -> if b then kickit k else []) .
        id .
        use 7 (use 8 (use 9 id))
      )
      id
      True

  1 : 2 : 3 : 4 : 5 : 6 :
    if True
      then
        kickit
          ( id .
            use 7 (use 8 (use 9 id)) .
            id .
            id
          )
      else []

  1 : 2 : 3 : 4 : 5 : 6 : 7 :
    invoke
      ( id .
        id .
        (H \k b -> if b then kickit k else []) .
        id
      )
      (use 8 (use 9 id))
      True

  1 : 2 : 3 : 4 : 5 : 6 : 7 :
    kickit
      ( id .
        use 8 (use 9 id) .
        id .
        id
      )

  1 : 2 : 3 : 4 : 5 : 6 : 7 : 8 :
    kickit
      ( id .
        use 9 id .
        id .
        id
      )

  1 : 2 : 3 : 4 : 5 : 6 : 7 : 8 : 9 :
    kickit
      ( id .
        id .
        id .
        id
      )

  1 : 2 : 3 : 4 : 5 : 6 : 7 : 8 : 9 :
    invoke
      ( id .
        id .
        id .
        id
      )
      (H \k b -> if b then kickit k else [])
      False

  1 : 2 : 3 : 4 : 5 : 6 : 7 : 8 : 9 : []

}}} -}

zipping :: (Foldable f, Monoid o) => f a -> (a -> o) -&> o
zipping = foldr send (rep mempty)
{-# INLINE zipping #-}

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
{-# INLINE zipWith4 #-}

data Daisy f g c where Grow :: f a -> g b -> (a -> b -> c) -> Daisy f g c

instance (Foldable f, Foldable g) => Foldable (Daisy f g) where
  foldMap h (Grow fa gb k) = run $ zipping fa . zipping gb . rep \r b a -> h (k a b) <> r
  {-# INLINE foldMap #-}

data DaisyChain fs c where
  Link :: c -> DaisyChain '[] c
  (:=:) :: DaisyChain fs (a -> c) -> f a -> DaisyChain (f ': fs) c

infixl 5 :=:

instance Foldable (DaisyChain '[]) where
  foldMap h (Link c) = h c
  {-# INLINE foldMap #-}

instance (Foldable f) => Foldable (DaisyChain '[f]) where
  foldMap h (Link k :=: fa) = foldMap (h . k) fa
  {-# INLINE foldMap #-}

instance (Foldable f, Foldable (DaisyChain (ff ': fs))) => Foldable (DaisyChain (f ': ff ': fs)) where
  foldMap h (fs :=: fa) = run $ zipping fs . zipping fa . rep \r c a -> h (a c) <> r
  {-# INLINE foldMap #-}

hybase :: IO ()
hybase = do
  print $ zipWith4 (,,,) [1, 2, 3 :: Int] ['a', 'b', 'c', 'd'] [True, False, True] [(), (), ()]
  print $ toList (Grow [1, 2, 3 :: Int] ['a', 'b', 'c', 'd'] (,))
  print $ toList (Link (,,) :=: [1, 2, 3 :: Int] :=: ['a', 'b', 'c', 'd'] :=: [True, False, True])
  print $ zipWith2 (,) [1, 2, 3 :: Int] ['a', 'b', 'c', 'd']
  print @[Int] $ -- [1,2,3]
    invoke
      (rep (1 :) . rep (2 :) . rep (3 :))
      (pure [])
  print @[Int] $ -- [1,2,3,4,1,2,3]
    invoke
      (rep (1 :) . rep (2 :) . rep (3 :))
      ((4 :) !> pure [])
  print @[Int] $ -- [1,2,3,4,1,2,3,5,1,2,3]
    invoke
      (rep (1 :) . rep (2 :) . rep (3 :))
      ((4 :) !> (5 :) !> pure [])
  print @(Int, Int, Int) $ -- (1,2,3)
    invoke
      (pass 1 . pass 2 . pass 3)
      (pure (\z y x -> (x, y, z)))
  print @[(Int, Int, Int)] $ -- [(2,4,6), (1,2,3)]
    invoke
      (pass 1 . pass 2 . pass 3)
      ((\t z y x -> (x * 2, y * 2, z * 2) : t) !> pure (\z y x -> [(x, y, z)]))
  print @[Int] $ -- [1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2]
    take 20 $
      run (rep (1 :) . rep (2 :) . rep (3 :))
  print @[(Int, Int, Int)] $ -- [(2,4,6),(2,4,6),(2,4,6)]
    take 3 $
      run (pass 1 . pass 2 . pass 3 . rep (\t z y x -> (x * 2, y * 2, z * 2) : t))
  print @[(Int, Int)] $ -- [(0,1000),(102,1001),(204,1002)]
    take 3 $
      run
        ( rep (\next -> next 100)
            . rep (\next prev -> next (prev + 2))
            . rep (\next -> next 1000)
            . rep (\next prev1 prev2 s -> (prev2 * s, prev1 + s) : next (s + 1))
        )
        (0 :: Int)
  print @[(Int, Int, Int)] $ -- [(2,3,0),(22,3,1),(2,3,2),(22,3,3),(2,3,4),(22,3,5),(2,3,6),(22,3,7),(2,3,8),(22,3,9)]
    take 10 $
      run
        ( rep (\next -> next (,,))
            . unsafeCycleReps [\next prev -> next (prev 2), \next prev -> next (prev 22)]
            . rep (\next prev -> next (prev 3))
            . rep (\next prev s -> prev s : next (s + 1))
        )
        (0 :: Int)
  print @[Int] $ -- [0,1,2,3,6,7,14,15,30,31]
    take 10 $
      run
        ( rep (\next s -> s : next (s + 1))
            . rep (\next s -> s : next (s * 2))
        )
        (0 :: Int)
  print @[Int] $
    take 20 $
      run
        ( together
            (rep (0 :))
            ( together
                (rep (1 :))
                (rep (2 :))
            )
        )
  mempty

zipWith2 ::
  (Foldable f, Foldable g) =>
  (x -> y -> o) ->
  (f x -> g y -> [o])
zipWith2 o xs ys =
  run $ zipping xs . zipping ys . rep \r y x -> o x y : r
{-# INLINE zipWith2 #-}

-- {{{
bongos :: ()
bongos = do
  let _ = zipWith2 (,) [1, 2, 3 :: Int] ['a', 'b', 'c', 'd']
  let _ = run $ zipping [1, 2, 3 :: Int] . zipping ['a', 'b', 'c', 'd'] . rep \r y x -> (x, y) : r
  let _ = invoke (zipping [1, 2, 3 :: Int] . zipping ['a', 'b', 'c', 'd'] . rep \r y x -> (x, y) : r) id
  let _ = invoke (zipping [1, 2, 3 :: Int] . zipping ['a', 'b', 'c', 'd'] . rep \r y x -> (x, y) : r) (H run)
  let _ = invoke (zipping [1, 2, 3 :: Int] . (zipping ['a', 'b', 'c', 'd'] . rep \r y x -> (x, y) : r)) (H run)
  let _ = invoke (H \k -> invoke (zipping [1, 2, 3 :: Int]) (zipping ['a', 'b', 'c', 'd'] . rep (\r y x -> (x, y) : r) . k)) (H run)
  let _ = invoke (zipping [1, 2, 3 :: Int]) (zipping ['a', 'b', 'c', 'd'] . rep (\r y x -> (x, y) : r) . H run)
  let _ = invoke (foldr (\y t -> ($ y) !> t) (rep mempty) [1, 2, 3 :: Int]) (zipping ['a', 'b', 'c', 'd'] . rep (\r y x -> (x, y) : r) . H run)
  let _ = invoke ((\y t -> ($ y) !> t) (1 :: Int) (foldr (\y t -> ($ y) !> t) (rep mempty) [2, 3 :: Int])) (zipping ['a', 'b', 'c', 'd'] . rep (\r y x -> (x, y) : r) . H run)
  let _ = invoke (($ (1 :: Int)) !> foldr (\y t -> ($ y) !> t) (rep mempty) [2, 3 :: Int]) (zipping ['a', 'b', 'c', 'd'] . rep (\r y x -> (x, y) : r) . H run)
  let _ = invoke (($ (1 :: Int)) !> ($ (2 :: Int)) !> ($ (3 :: Int)) !> rep mempty) (zipping ['a', 'b', 'c', 'd'] . rep (\r y x -> (x, y) : r) . H run)
  let _ = invoke (H \k -> ($ (1 :: Int)) (invoke k $ ($ (2 :: Int)) !> ($ (3 :: Int)) !> rep mempty)) (zipping ['a', 'b', 'c', 'd'] . rep (\r y x -> (x, y) : r) . H run)
  let _ = (\k -> ($ (1 :: Int)) (invoke k $ ($ (2 :: Int)) !> ($ (3 :: Int)) !> rep mempty)) (zipping ['a', 'b', 'c', 'd'] . rep (\r y x -> (x, y) : r) . H run)
  let _ =
        ($ (1 :: Int))
          ( invoke
              (zipping ['a', 'b', 'c', 'd'] . rep (\r y x -> (x, y) : r) . H run)
              $ ($ (2 :: Int)) !> ($ (3 :: Int)) !> rep mempty
          )
  let _ =
        invoke
          (zipping ['a', 'b', 'c', 'd'] . rep (\r y x -> (x, y) : r) . H run)
          (($ (2 :: Int)) !> ($ (3 :: Int)) !> rep mempty)
          (1 :: Int)
  let _ =
        invoke
          ( H \k ->
              invoke
                (zipping ['a', 'b', 'c', 'd'])
                (rep (\r y x -> (x, y) : r) . H run . k)
          )
          (($ (2 :: Int)) !> ($ (3 :: Int)) !> rep mempty)
          (1 :: Int)
  let _ =
        invoke
          (zipping ['a', 'b', 'c', 'd'])
          (rep (\r y x -> (x, y) : r) . H run . (($ (2 :: Int)) !> ($ (3 :: Int)) !> rep mempty))
          (1 :: Int)
  let _ =
        invoke
          (foldr (\y t -> ($ y) !> t) (rep mempty) ['a', 'b', 'c', 'd'])
          (rep (\r y x -> (x, y) : r) . H run . (($ (2 :: Int)) !> ($ (3 :: Int)) !> rep mempty))
          (1 :: Int)
  let _ =
        invoke
          (($ 'a') !> ($ 'b') !> ($ 'c') !> rep mempty)
          (rep (\r y x -> (x, y) : r) . H run . (($ (2 :: Int)) !> ($ (3 :: Int)) !> rep mempty))
          (1 :: Int)
  let _ =
        invoke
          (H \k -> ($ 'a') (invoke k (($ 'b') !> ($ 'c') !> rep mempty)))
          (rep (\r y x -> (x, y) : r) . H run . (($ (2 :: Int)) !> ($ (3 :: Int)) !> rep mempty))
          (1 :: Int)
  let _ =
        ($ 'a')
          (invoke (rep (\r y x -> (x, y) : r) . H run . (($ (2 :: Int)) !> ($ (3 :: Int)) !> rep mempty)) (($ 'b') !> ($ 'c') !> rep mempty))
          (1 :: Int)
  let _ =
        ( invoke
            (rep (\r y x -> (x, y) : r) . H run . (($ (2 :: Int)) !> ($ (3 :: Int)) !> rep mempty))
            (($ 'b') !> ($ 'c') !> rep mempty)
        )
          'a'
          (1 :: Int)
  let _ =
        ( invoke
            ( H \k ->
                invoke
                  (rep (\r y x -> (x, y) : r))
                  ((H run . (($ (2 :: Int)) !> ($ (3 :: Int)) !> rep mempty)) . k)
            )
            (($ 'b') !> ($ 'c') !> rep mempty)
        )
          'a'
          (1 :: Int)
  let _ =
        ( invoke
            (rep (\r y x -> (x, y) : r))
            ((H run . (($ (2 :: Int)) !> ($ (3 :: Int)) !> rep mempty)) . (($ 'b') !> ($ 'c') !> rep mempty))
        )
          'a'
          (1 :: Int)
  let _ =
        ( invoke
            ((\r y x -> (x, y) : r) !> rep (\r y x -> (x, y) : r))
            ((H run . (($ (2 :: Int)) !> ($ (3 :: Int)) !> rep mempty)) . (($ 'b') !> ($ 'c') !> rep mempty))
        )
          'a'
          (1 :: Int)
  let _ =
        ( invoke
            ( H \k ->
                (\r y x -> (x, y) : r) (invoke k (rep (\r y x -> (x, y) : r)))
            )
            ((H run . (($ (2 :: Int)) !> ($ (3 :: Int)) !> rep mempty)) . (($ 'b') !> ($ 'c') !> rep mempty))
        )
          'a'
          (1 :: Int)
  let _ =
        ( invoke
            (H \k yy xx -> (xx, yy) : (invoke k (rep (\r y x -> (x, y) : r))))
            ((H run . (($ (2 :: Int)) !> ($ (3 :: Int)) !> rep mempty)) . (($ 'b') !> ($ 'c') !> rep mempty))
        )
          'a'
          (1 :: Int)
  let _ =
        ( \yy xx ->
            (xx, yy) : (invoke ((H run . (($ (2 :: Int)) !> ($ (3 :: Int)) !> rep mempty)) . (($ 'b') !> ($ 'c') !> rep mempty)) (rep (\r y x -> (x, y) : r)))
        )
          'a'
          (1 :: Int)
  let _ =
        (1 :: Int, 'a')
          : (invoke ((H run . (($ (2 :: Int)) !> ($ (3 :: Int)) !> rep mempty)) . (($ 'b') !> ($ 'c') !> rep mempty)) (rep (\r y x -> (x, y) : r)))
  let _ =
        (1 :: Int, 'a')
          : ( invoke
                ( H run
                    . (($ (2 :: Int)) !> ($ (3 :: Int)) !> rep mempty)
                    . (($ 'b') !> ($ 'c') !> rep mempty)
                )
                (rep (\r y x -> (x, y) : r))
            )
  let _ =
        (1 :: Int, 'a')
          : ( invoke
                ( H \k ->
                    invoke
                      (H run)
                      ( (($ (2 :: Int)) !> ($ (3 :: Int)) !> rep mempty)
                          . (($ 'b') !> ($ 'c') !> rep mempty)
                          . k
                      )
                )
                (rep (\r y x -> (x, y) : r))
            )
  let _ =
        (1 :: Int, 'a')
          : ( invoke
                (H run)
                ( (($ (2 :: Int)) !> ($ (3 :: Int)) !> rep mempty)
                    . (($ 'b') !> ($ 'c') !> rep mempty)
                    . rep (\r y x -> (x, y) : r)
                )
            )
  let _ =
        (1 :: Int, 'a')
          : ( invoke
                (H \k -> invoke k id)
                ( (($ (2 :: Int)) !> ($ (3 :: Int)) !> rep mempty)
                    . (($ 'b') !> ($ 'c') !> rep mempty)
                    . rep (\r y x -> (x, y) : r)
                )
            )
  let _ =
        (1 :: Int, 'a')
          : ( invoke
                ( (($ (2 :: Int)) !> ($ (3 :: Int)) !> rep mempty)
                    . (($ 'b') !> ($ 'c') !> rep mempty)
                    . rep (\r y x -> (x, y) : r)
                )
                id
            )
  let _ =
        (1 :: Int, 'a')
          : ( invoke
                ( (($ (2 :: Int)) !> ($ (3 :: Int)) !> rep mempty)
                    . (($ 'b') !> ($ 'c') !> rep mempty)
                    . rep (\r y x -> (x, y) : r)
                )
                (H run)
            )
  ()

-- }}}

-- invoke (rep x . rep y . rep z) id
-- invoke (rep x) (rep y . rep z . id)
-- x $ invoke (rep y . rep z . id) (rep x)
-- x $ invoke (rep y) (rep z . id . rep x)
-- x $ y $ invoke (rep z . id . rep x) (rep y)
-- x $ y $ invoke (rep z) (id . rep x . rep y)
-- x $ y $ z $ invoke (id . rep x . rep y) (rep z)
-- x $ y $ z $ invoke id (rep x . rep y . rep z)
-- x $ y $ z $ invoke (rep x . rep y . rep z) id
-- x $ y $ z $ x $ y $ z $ ...

main :: IO ()
main = do
  hybase
  print @[Int] $ build (fold [1, 2, 3, 4, 5])
  print $ lockstepH id [1, 2 :: Int] "ABC"
  xs <-
    lockstepA
      ( \case
          Both a b -> do
            let x = show (a, b)
            putStrLn x
            pure x
          This a -> do
            let x = show a
            putStrLn x
            pure x
          That b -> do
            let x = show b
            putStrLn x
            pure x
      )
      [1, 2 :: Int]
      "ABC"
  print xs

  print $ shortInterleave @Int [1, 3, 5, 0, 0] [2, 4] -- [1,2,3,4,5]
  print $ longInterleave @Int [1, 3, 5] [2, 4, 6, 7] -- [1,2,3,4,5,6,7]
  print $ shortInterleaving @Int [[1, 4, 7, 0], [2, 5], [3, 6, 0, 0]] -- [1,2,3,4,5,6,7]
  print $ take 10 $ longInterleaving @Int [[1, 2, 3], repeat 0, [4, 5]] -- [1,0,4,2,0,5,3,0,0,0]
  print $ take 10 $ longInterleaving @Int [[1 ..], [10 ..], [100 ..]] -- [1,10,100,2,11,101,3,12,102,4]
  print $ take 10 $ longInterleaving @Int (repeat [1]) -- [1,1,1,1,1,1,1,1,1,1]
  print $ take 10 $ longInterleaving @Int (tails [1 ..]) -- [1,2,3,4,5,6,7,8,9,10]
  print $ longInterleaving @Int @[] @[] [] -- []
  print $ longInterleaving @Int [[]] -- []
  print $ longInterleaving @Int [[], []] -- []
  -- print $ take 1 $ longInterleaving @Int (repeat []) -- non-termination

  --
  mapM_ (print @(Int, Int)) $
    take 20 do
      0 & do
        run
          ( (fix \me -> H \k n -> (0, n) : (invoke k me $! n + 1))
              . (fix \me -> H \k n -> (1, n * 100) : (invoke k me $! n + 2))
              . (fix \me -> H \k n -> (2, n * 100) : (invoke k me $! n + 3))
          )

  -- print $ grun (gsingle 1 :: G Int Int Int)
  mempty
-- print $ bloink [1, 2, 3, 4, 5 :: Int]
{-# INLINE main #-}

data Product t s c = Product (t c) (s c)

-- t (a, Maybe b -> f c) -> s (b, Maybe a -> f c) -> f (Product t s c)

newtype Clown s b c x = Clown {getClown :: (Maybe b -> c) -> s b -> (s c, x)}
  deriving
    (Functor, Applicative)
    via (->) (Maybe b -> c)
      `Compose` (->) (s b)
      `Compose` (,) (s c)

-- bloink :: forall t. (Traversable t) => t Int -> (t [Int], t [Int])
-- bloink ts = do
--   let _ppt = for ts \a -> rep (a :)
--   let _pps = for ts \b -> rep (b * 10 :)
--
--   let _pph = do
--         l <- _ppt
--         r <- _pps
--         pure (l, r)
--
--   let _m = invoke _pph (pure [])
--   _m

-- lockstep ::
--   (Traversable t, Traversable s) =>
--   (Maybe a -> Maybe b -> c) -> t a -> s b -> (t c, s c)
-- lockstep f ts ss = do
--   let ppt = getCompose $ ts & traverse \a -> Compose (send (Just a))
--   let pt = ppt (pass Nothing)
--
--   -- let pps = getCompose $ ss & traverse \b -> Compose (send (Just b))
--   -- let ps = pps (pass Nothing)
--
--   -- let done = H \k ->
--   --       _ $
--   --         invoke pt $ H \j ->
--   --           _ $ invoke ps $ H \i ->
--   --                 _ f k j i
--   -- run done
--
--
--   run $ pt . rep _

data FunList a c r
  = Done r
  | More a (FunList a c (c -> r))

instance Functor (FunList a c) where
  fmap f (Done r) = Done (f r)
  fmap f (More a k) = More a (fmap (f .) k)

instance Applicative (FunList a c) where
  pure = Done

  Done f <*> x = fmap f x
  More a k <*> x = More a ((flip <$> k) <*> x)

single :: a -> FunList a c c
single a = More a (Done id)

walk :: (Traversable t) => t a -> FunList a c (t c)
walk = traverse single

data These a b = This a | That b | Both a b
  deriving stock (Show)

lockstep ::
  forall t s a b c.
  (Traversable t, Traversable s) =>
  (These a b -> c) ->
  t a ->
  s b ->
  (t c, s c)
lockstep f ts ss = go (walk ts) (walk ss)
  where
    go :: FunList a c r -> FunList b c q -> (r, q)
    go (Done tc) (Done sc) = (tc, sc)
    go (More a kt) (More b ks) =
      let c = f (Both a b)
       in case go kt ks of
            (tc, sc) -> (tc c, sc c)
    go (More a kt) (Done sc) =
      let c = f (This a)
       in case go kt (Done sc) of
            (tc, sc') -> (tc c, sc')
    go (Done tc) (More b ks) =
      let c = f (That b)
       in case go (Done tc) ks of
            (tc', sc) -> (tc', sc c)

lockstepH ::
  (Traversable t, Traversable s) =>
  (These a b -> c) ->
  t a ->
  s b ->
  (t c, s c)
lockstepH f ts ss = run (rep go) (walk ss) (walk ts)
  where
    go next ss' ts' = case (ts', ss') of
      (Done tc, Done sc) -> (tc, sc)
      (More a kt, More b ks) ->
        let c = f (Both a b)
         in next (fmap ($ c) ks) (fmap ($ c) kt)
      (More a kt, Done sc) ->
        let c = f (This a)
         in next (Done sc) (fmap ($ c) kt)
      (Done tc, More b ks) ->
        let c = f (That b)
         in next (fmap ($ c) ks) (Done tc)

lockstepA ::
  forall f t s a b c.
  (Applicative f, Traversable t, Traversable s) =>
  (These a b -> f c) ->
  t a ->
  s b ->
  f (t c, s c)
lockstepA f ts ss = go (walk ts) (walk ss)
  where
    go :: FunList a c r -> FunList b c q -> f (r, q)
    go (Done tc) (Done sc) = pure (tc, sc)
    go (More a kt) (More b ks) =
      (\c (tc, sc) -> (tc c, sc c))
        <$> f (Both a b)
        <*> go kt ks
    go (More a kt) (Done sc) =
      (\c (tc, sc') -> (tc c, sc'))
        <$> f (This a)
        <*> go kt (Done sc)
    go (Done tc) (More b ks) =
      (\c (tc', sc) -> (tc', sc c))
        <$> f (That b)
        <*> go (Done tc) ks

-- newtype G c a b = G {runG :: G c (c -> b) (c -> a) -> b}
--
-- instance Profunctor (G c) where
--   dimap f g h = G $ g . runG h . dimap (fmap g) (fmap f)
--   {-# INLINE dimap #-}
--
-- instance Applicative (G c a) where
--   pure = gpure
--   gf <*> gx = G \k -> do
--     let f =
--           runG gf $ G \gf' -> do
--             runG k $ G \hhh -> do
--               let lol :: (c -> c -> x -> b) -> (c -> c -> x) -> c -> c -> b
--                   lol x y z w = x z w (y z w)
--               let _x = runG gx $ G \gx' -> runG k (liftA2 lol gf' gx')
--               _ hhh _x
--     let x = runG gx $ G \gx' -> runG k (dimap undefined undefined gf <*> dimap undefined undefined gx')
--     f x
--   {-# INLINE (<*>) #-}
--
-- gpure :: b -> G c a b
-- gpure v = G \_ -> v

-- gsingle :: b -> G b a a
-- gsingle b = G \k -> runG k (gsingle b) b
--
-- grun :: G c c b -> b
-- grun g = runG g (gpure id)
