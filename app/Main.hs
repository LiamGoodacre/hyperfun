module Main where

import Control.Category
import Data.Coerce (coerce)
import Data.Function ((&))
import Data.Profunctor
import Prelude hiding (id, (.))

newtype a -&> b = H {invoke :: (b -&> a) -> b}

type Process a = a -&> a

run :: Process a -> a
run h = invoke h id
{-# INLINE run #-}

(!>) :: (i -> o) -> (i -&> o) -> (i -&> o)
a !> b = H \k -> a (invoke k b)
{-# INLINE (!>) #-}

fold_with ::
  (Foldable f, Category cat) =>
  (y -> cat a a -> cat a a) ->
  (x -> y) ->
  f x ->
  cat a a
fold_with o t = foldr (\a r -> o (t a) r) id
{-# INLINE fold_with #-}

longInterleaving :: forall a f g. (Foldable f, Foldable g) => f (g a) -> [a]
longInterleaving fga = do
  let make_each_process :: Process (Bool -> [a])
      make_each_process =
        fga & fold_with (.) \ga ->
          ga & fold_with (!>) \a next ->
            \_continuing -> a : next True

  let loop_or_stop :: Process (Bool -> [a])
      loop_or_stop = rep \next ->
        \continuing -> if continuing then next False else []

  run (make_each_process . loop_or_stop) False
{-# INLINE longInterleaving #-}

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

-- (<***>) :: forall z a b. (z -&> (a -> b)) -> (z -&> a) -> z -&> b
-- (<***>) z_ab z_a = H \bz -> do
--   let f = invoke z_ab (H \z_ab' -> invoke bz (z_ab' <***> z_a))
--   let x = invoke z_a (H \z_a' -> invoke bz (z_ab <***> z_a'))
--   f x

instance Monad ((-&>) a) where
  return = pure
  {-# INLINE return #-}
  m >>= f = cata (\g -> roll $ \k -> unroll (f (g k)) k) m
  {-# INLINE (>>=) #-}

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

longInterleavingOld :: forall a f g. (Foldable f, Foldable g) => f (g a) -> [a]
longInterleavingOld = do
  -- We have a boolean flag passed along through the processes.
  -- The flag starts out as False from kickit.
  -- When a process has something to contribute, it replaces it with True.
  -- When kickit's process retrieves the flag after a cycle through,
  -- it can end if no process contributed, or keep going otherwise.
  let use a b = H \k _ -> a : invoke k b True
  let useAll = foldr use id
  let kickit j = invoke j (H \k b -> if b then kickit k else []) False
  kickit . foldr (\xs r -> useAll xs . r) id
{-# INLINE longInterleavingOld #-}

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
  -- print $ zipWith4 (,,,) [1, 2, 3 :: Int] ['a', 'b', 'c', 'd'] [True, False, True] [(), (), ()]
  -- print $ toList (Grow [1, 2, 3 :: Int] ['a', 'b', 'c', 'd'] (,))
  -- print $ toList (Link (,,) :=: [1, 2, 3 :: Int] :=: ['a', 'b', 'c', 'd'] :=: [True, False, True])
  -- print $ zipWith2 (,) [1, 2, 3 :: Int] ['a', 'b', 'c', 'd']
  -- print @[Int] $ -- [1,2,3]
  --   invoke
  --     (rep (1 :) . rep (2 :) . rep (3 :))
  --     (pure [])
  -- print @[Int] $ -- [1,2,3,4,1,2,3]
  --   invoke
  --     (rep (1 :) . rep (2 :) . rep (3 :))
  --     ((4 :) !> pure [])
  -- print @[Int] $ -- [1,2,3,4,1,2,3,5,1,2,3]
  --   invoke
  --     (rep (1 :) . rep (2 :) . rep (3 :))
  --     ((4 :) !> (5 :) !> pure [])
  -- print @(Int, Int, Int) $ -- (1,2,3)
  --   invoke
  --     (pass 1 . pass 2 . pass 3)
  --     (pure (\z y x -> (x, y, z)))
  -- print @[(Int, Int, Int)] $ -- [(2,4,6), (1,2,3)]
  --   invoke
  --     (pass 1 . pass 2 . pass 3)
  --     ((\t z y x -> (x * 2, y * 2, z * 2) : t) !> pure (\z y x -> [(x, y, z)]))
  -- print @[Int] $ -- [1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2]
  --   take 20 $
  --     run (rep (1 :) . rep (2 :) . rep (3 :))
  -- print @[(Int, Int, Int)] $ -- [(2,4,6),(2,4,6),(2,4,6)]
  --   take 3 $
  --     run (pass 1 . pass 2 . pass 3 . rep (\t z y x -> (x * 2, y * 2, z * 2) : t))
  -- print @[(Int, Int)] $ -- [(0,1000),(102,1001),(204,1002)]
  --   take 3 $
  --     run
  --       ( rep (\next -> next 100)
  --           . rep (\next prev -> next (prev + 2))
  --           . rep (\next -> next 1000)
  --           . rep (\next prev1 prev2 s -> (prev2 * s, prev1 + s) : next (s + 1))
  --       )
  --       (0 :: Int)
  -- print @[(Int, Int, Int)] $ -- [(2,3,0),(22,3,1),(2,3,2),(22,3,3),(2,3,4),(22,3,5),(2,3,6),(22,3,7),(2,3,8),(22,3,9)]
  --   take 10 $
  --     run
  --       ( rep (\next -> next (,,))
  --           . unsafeCycleReps [\next prev -> next (prev 2), \next prev -> next (prev 22)]
  --           . rep (\next prev -> next (prev 3))
  --           . rep (\next prev s -> prev s : next (s + 1))
  --       )
  --       (0 :: Int)
  -- print @[Int] $ -- [0,1,2,3,6,7,14,15,30,31]
  --   take 10 $
  --     run
  --       ( rep (\next s -> s : next (s + 1))
  --           . rep (\next s -> s : next (s * 2))
  --       )
  --       (0 :: Int)
  -- print @[Int] $
  --   take 20 $
  --     run
  --       ( together
  --           (rep (0 :))
  --           ( together
  --               (rep (1 :))
  --               (rep (2 :))
  --           )
  --       )
  mempty

zipWith2 ::
  (Foldable f, Foldable g) =>
  (x -> y -> o) ->
  (f x -> g y -> [o])
zipWith2 o xs ys =
  run $ zipping xs . zipping ys . rep \r y x -> o x y : r
{-# INLINE zipWith2 #-}

-- $> main

main :: IO ()
main = do
  hybase
  -- print @[Int] $ build (fold [1, 2, 3, 4, 5])
  -- print $ lockstep id [1, 2 :: Int] "ABC"
  -- print $ lockstepH id [1, 2 :: Int] "ABC"
  -- xs <-
  --   lockstepA
  --     ( \case
  --         Both a b -> do
  --           let x = show (a, b)
  --           putStrLn x
  --           pure x
  --         This a -> do
  --           let x = show a
  --           putStrLn x
  --           pure x
  --         That b -> do
  --           let x = show b
  --           putStrLn x
  --           pure x
  --     )
  --     [1, 2 :: Int]
  --     "ABC"
  -- print xs

  -- print $ shortInterleave @Int [1, 3, 5, 0, 0] [2, 4] -- [1,2,3,4,5]
  -- print $ longInterleave @Int [1, 3, 5] [2, 4, 6, 7] -- [1,2,3,4,5,6,7]
  -- print $ shortInterleaving @Int [[1, 4, 7, 0], [2, 5], [3, 6, 0, 0]] -- [1,2,3,4,5,6,7]
  -- print $ take 10 $ longInterleavingOld @Int [[1, 2, 3], repeat 0, [4, 5]] -- [1,0,4,2,0,5,3,0,0,0]
  -- print $ take 10 $ longInterleavingOld @Int [[1 ..], [10 ..], [100 ..]] -- [1,10,100,2,11,101,3,12,102,4]
  -- print $ take 10 $ longInterleavingOld @Int (repeat [1]) -- [1,1,1,1,1,1,1,1,1,1]
  -- print $ take 10 $ longInterleavingOld @Int (tails [1 ..]) -- [1,2,3,4,5,6,7,8,9,10]
  -- print $ longInterleavingOld @Int @[] @[] [] -- []
  -- print $ longInterleavingOld @Int [[]] -- []
  -- print $ longInterleavingOld @Int [[], []] -- []
  -- print $ take 1 $ longInterleavingOld @Int (repeat []) -- non-termination

  print $ take 10 $ longInterleaving @Int ([] :: [[Int]])
  print $ take 10 $ longInterleaving @Int [[]]
  print $ take 10 $ longInterleaving @Int [[1]]
  print $ take 10 $ longInterleaving @Int [[1, 2]]
  print $ take 10 $ longInterleaving @Int [[1], [2]]
  print $ take 10 $ longInterleavingOld @Int [[1, 2, 3], repeat 0, [4, 5]] -- [1,0,4,2,0,5,3,0,0,0]
  print $ take 10 $ longInterleaving @Int [[1, 2, 3], repeat 0, [4, 5]] -- [1,0,4,2,0,5,3,0,0,0]

  --
  -- mapM_ (print @(Int, Int)) $
  --   take 20 do
  --     0 & do
  --       run
  --         ( (fix \me -> H \k n -> (0, n) : (invoke k me $! n + 1))
  --             . (fix \me -> H \k n -> (1, n * 100) : (invoke k me $! n + 2))
  --             . (fix \me -> H \k n -> (2, n * 100) : (invoke k me $! n + 3))
  --         )

  --
  -- print $ longInterleave @Int [1, 3, 5] [2, 4, 6, 7] -- [1,2,3,4,5,6,7]
  -- print $ grun (gsingle 1 :: G Int Int Int)
  mempty
{-# INLINE main #-}

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

{-
newtype IxH g h x = IxH
  { elimIxH ::
      forall r.
      (x -> r) ->
      ( forall a.
        (forall b. g (a -> b) -> h b) ->
        IxH g h (a -> x) ->
        r
      ) ->
      r
  }

runIxH :: (forall a. a -> g a) -> IxH g g x -> g x
runIxH n h = elimIxH h n \step t -> step (runIxH n t)
{-# INLINE runIxH #-}

ipure :: x -> IxH g h x
ipure x = IxH \p _ -> p x
{-# INLINE ipure #-}

icons :: (forall b. g (a -> b) -> h b) -> IxH g h (a -> x) -> IxH g h x
icons step t = IxH \_ c -> c step t
{-# INLINE icons #-}

ixMap :: (x -> y) -> IxH g h x -> IxH g h y
ixMap f h = elimIxH h (ipure . f) \step t -> icons step (ixMap (f .) t)
{-# INLINE ixMap #-}

ixLift2 :: (x -> y -> z) -> IxH g h x -> IxH g h y -> IxH g h z
ixLift2 f = goL f
  where
    goL :: (x -> y -> z) -> IxH g h x -> IxH g h y -> IxH g h z
    goL k l r =
      elimIxH
        l
        (\x -> ixMap (k x) r)
        (\step lt -> icons step (goR (\ax y a -> k (ax a) y) lt r))

    goR :: (x -> y -> z) -> IxH g h x -> IxH g h y -> IxH g h z
    goR k l r =
      elimIxH
        r
        (\y -> ixMap (\x -> k x y) l)
        (\step rt -> icons step (goL (\x ay a -> k x (ay a)) l rt))
{-# INLINE ixLift2 #-}

class IxCategory k where
  ixId :: x -> k g h x
  ixComp :: (x -> y -> z) -> k f g x -> k h f y -> k h g z

-- instance IxCategory IxH where
--   ixId = ipure
--   ixComp = _

apFold ::
  forall x f g h.
  Ap f x ->
  (forall a b. f a -> g (a -> b) -> h b) ->
  IxH g h x
apFold ap c = case ap of
  Ap.Pure x -> ipure x
  Ap.Ap h t -> icons (c h) (apFold t c)

apBuild ::
  (forall g h. (forall a b. f a -> g (a -> b) -> h b) -> IxH g h x) ->
  Ap f x
apBuild g = runIxH Ap.Pure (g Ap.Ap)

longInterleaveAp :: forall f z y x. (x -> y -> z) -> Ap f x -> Ap f y -> Ap f z
longInterleaveAp f l r = runIxH Ap.Pure (ixLift2 f (apFold l Ap.Ap) (apFold r Ap.Ap))
{-# INLINE longInterleaveAp #-}

longInterleavingAp :: forall f x. Ap (Ap f) x -> Ap f x
longInterleavingAp ap = go (apFold ap Ap.Ap)
  where
    go :: IxH (Ap (Ap f)) (Ap (Ap f)) y -> Ap f y
    go q =
      elimIxH q Ap.Pure \step rest ->
        processFront (step (Ap.Pure id)) rest

    processFront ::
      forall a y.
      Ap (Ap f) a ->
      IxH (Ap (Ap f)) (Ap (Ap f)) (a -> y) ->
      Ap f y
    processFront front rest = case front of
      Ap.Pure y -> go (ixMap ($ y) rest)
      Ap.Ap inner outerTail -> case inner of
        Ap.Pure y -> processFront (($ y) <$> outerTail) rest
        Ap.Ap h t ->
          Ap.Ap h $
            go $
              ixAppendEffect
                (\r ay a -> r (ay a))
                (ixAppendApply (apFold outerTail Ap.Ap) (ixMap (\r k a -> r (k a)) rest))
                t

    ixAppendApply ::
      forall u v.
      IxH (Ap (Ap f)) (Ap (Ap f)) u ->
      IxH (Ap (Ap f)) (Ap (Ap f)) (u -> v) ->
      IxH (Ap (Ap f)) (Ap (Ap f)) v
    ixAppendApply l r =
      elimIxH
        l
        (\x -> ixMap ($ x) r)
        ( \step rest ->
            icons step $
              ixAppendApply rest (ixMap (\xy ax -> xy . ax) r)
        )

    ixAppendEffect ::
      forall a y z.
      (y -> a -> z) ->
      IxH (Ap (Ap f)) (Ap (Ap f)) y ->
      Ap f a ->
      IxH (Ap (Ap f)) (Ap (Ap f)) z
    ixAppendEffect f q e =
      elimIxH
        q
        (\y -> icons (Ap.Ap e) (ipure (f y)))
        ( \step rest ->
            icons step $
              ixAppendEffect (\ay a x -> f (ay x) a) rest e
        )
{-# INLINE longInterleavingAp #-}

longInterleavingAp' :: forall f x. Ap (Ap f) x -> Ap f x
longInterleavingAp' = \case
  Ap.Pure x -> Ap.Pure x
  Ap.Ap inner rest -> case inner of
    Ap.Pure y -> longInterleavingAp' (($ y) <$> rest)
    Ap.Ap h t ->
      Ap.Ap h $
        longInterleavingAp' $
          (\k ay a -> k (ay a)) <$> rest <*> Ap.liftAp t
{-# INLINE longInterleavingAp' #-}

mainAp = do
  print @[(Int, Int)] $
    Ap.retractAp
      ( apBuild
          ( apFold
              ( (,)
                  <$> Ap.liftAp [1, 2, 3]
                  <*> Ap.liftAp [1, 2, 3]
              )
          )
      )

  print @(String, (Int, Int)) $
    Ap.retractAp $
      longInterleaveAp
        (,)
        (Ap.Ap ("L1", 1 :: Int) (Ap.Ap ("L2", 10 :: Int) (Ap.Pure (\a b -> a + b))))
        (Ap.Ap ("R1", 100 :: Int) (Ap.Ap ("R2", 1000 :: Int) (Ap.Ap ("R3", 10000 :: Int) (Ap.Pure (\a b c -> a + b + c)))))

  print @(String, (Int, Int, Int)) $
    Ap.retractAp $
      longInterleavingAp $
        Ap.Ap
          (Ap.Ap ("A1", 1 :: Int) (Ap.Ap ("A2", 10 :: Int) (Ap.Pure (\a b -> a + b))))
          ( Ap.Ap
              (Ap.Ap ("B1", 100 :: Int) (Ap.Pure id))
              ( Ap.Ap
                  (Ap.Ap ("C1", 1000 :: Int) (Ap.Ap ("C2", 10000 :: Int) (Ap.Pure (\a b -> a + b))))
                  (Ap.Pure (\a b c -> (a, b, c)))
              )
          )

-}
