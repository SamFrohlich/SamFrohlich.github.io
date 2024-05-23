{-

Ungenerators are the dual of generators, they take a generated value and maybe produce
the randomness that led to it.

Using this framework, they can be defined at the same time as generators:
  Fwd =   generate : rand -> a
  Bwd = ungenerate : a    -> Maybe rand

Poster: https://harrisongoldste.in/papers/icfpsrc21.pdf
Code: https://github.com/hgoldstein95/ungenerators

This file embeds Harrison's work into our framework for the sake of comparison.
Code is copied from his, then adjusted to work with our existing code and embellished
with examples and comments for understanding how it compares / fits with our stuff.

-}

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-} -- Gives us our cheeky forall in BUG
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}

module Ungenerators where

-- base
import           Control.Applicative    (Alternative(..))
import           Control.Monad
import           Data.Bifunctor
import           Data.Maybe
import           GHC.Generics (Generic)

-- containers
import           Data.Map               (Map)
import qualified Data.Map               as Map

-- generic-arbitrary
import           Test.QuickCheck.Arbitrary.Generic

-- logict
import           Control.Monad.Logic

-- profunctors
import           Data.Profunctor        hiding (Choice)

-- QuickCheck
import           Test.QuickCheck        hiding (generate, classify)
import           Test.QuickCheck        (genericShrink)

-- local
import           BidirectionalProfunctors
import           PartialProfunctors

-- Implementation:
-- -----------------------------------------------------------------------------

-- Unlike our other examples, where we create different prof monads that do different
-- things, Ungenerators have a more general implementation: as a subclass of Profmonad
class Profmonad g => BiUngen g where
  -- Reminds me of my need for foldable in 'BiNonDet' / 'RelativeRecovery'
  select
    :: Eq a
    => String  -- ^ Human friendly label of what kind of choices the list encapsulates
    -> [g a a] -- ^ Choice of BiUngens
    -> g a a   -- ^ Choice
  uniform :: [a] -> g a a -- Some arbitrary lift and choice.
  -- Often defined msum :: (Foldable t, MonadPlus m) => t (m a) -> m a

-- We can wrap up all aligned BiUngen things into this type:
type BUG a = forall g. BiUngen g => g a a

-- This implementation works in conjunction with ':*:'
instance (BiUngen g, BiUngen u) => BiUngen (g :*: u) where
  select s cs = select s (pfst <$> cs) :*: select s (psnd <$> cs)
  uniform xs = uniform xs :*: uniform xs

-- constructor: making an instance of the BiUngen class

-- deconstructors: instead of unpacking the directions from a monadic prof made up
-- with :*:, Fwd and Bwd, the different directions come from different instances
-- of the type class.

-- Extra Profunctor Toys:
-- -----------------------------------------------------------------------------

-- Type of partial isomorphisms
type Iso a b = (a -> b, b -> Maybe a)

-- We can use our partial isos to change the carrier of aligned monadic profs
(<$$>) :: forall p a b. (ProfunctorPartial p, Profunctor p, Functor (p a)) => Iso a b -> p a a -> p b b
(f, b) <$$> p = comap b (fmap f p)

infixr 0 <$$>

-- Tupling two aligned profunctors within the profunctor
(<**>) :: forall p a b. (Profmonad p, ProfunctorPartial p) => p a a -> p b b -> p (a, b) (a, b)
px <**> py = do
  x <- comap mfst px
  y <- comap msnd py
  return (x, y)
  where
    mfst (x, _) = Just x
    msnd (_, y) = Just y

infixl 4 <**>

-- <$$> and <**> can be used similarly to <$> and <*> when programming in applicative
-- style, but where the "fmapped" constructor's args are all tupled up (by <**>)
-- This really comes into its own in the Expr generation example he has
-- Ironically tho this brings us back towards the point free style that this work
-- was trying to avoid - I like point free tho!

-- Instances:
-- -----------------------------------------------------------------------------

-- Check / Gen
-- ------------

-- Check:
-- u -> Maybe v
-- This instance is our check: it checks that b adheres to the properties.
-- See my replication of the bst example
-- (he calls it PureGen)

newtype PureGen b a = PureGen {runPureGen :: b -> Maybe a}

instance Functor (PureGen b) where
  fmap f m = PureGen $ fmap f . runPureGen m

instance Applicative (PureGen b) where
  pure = return
  (<*>) = ap

instance Monad (PureGen b) where
  return x = PureGen $ const (Just x)
  mx >>= f = PureGen $ \b ->
    runPureGen mx b >>= \x -> runPureGen (f x) b

instance Alternative (PureGen b) where
  empty = PureGen $ const Nothing
  mx <|> my = PureGen $ \b -> runPureGen mx b <|> runPureGen my b

instance MonadPlus (PureGen b)

instance Profunctor PureGen where
  dimap :: (a -> b) -> (c -> d) -> PureGen b c -> PureGen a d
                                -- b -> Maybe c   a -> Maybe d
  dimap f h g = PureGen (fmap h . runPureGen g . f)

instance Profmonad PureGen

instance BiUngen PureGen where
  uniform = msum . map pure
  select sid gs = PureGen $ \b ->
    msum [if a == Just b then a else Nothing | g <- gs, let a = runPureGen g b]

purify :: (forall g. (ProfunctorPartial g, BiUngen g) => g u v) -> u -> Maybe v
purify = runPureGen -- makes any bug a pure bug

check :: (forall g. (ProfunctorPartial g, BiUngen g) => g u v) -> u -> Maybe v
check = purify -- specialising to this instance is equiv to our check deconstructor

instance ProfunctorPartial PureGen where
  toFailureP :: PureGen u v -> PureGen (Maybe u) v
             -- u -> Maybe v   Maybe u -> Maybe v
  toFailureP g = PureGen $ join . fmap (runPureGen g)

-- Gen:
-- A generator using the Gen Monad
-- Called EnumGen' in his code:

newtype EnumGen' b a = EnumGen' {runEnumGen' :: Gen a}
  deriving (Functor, Applicative, Monad) -- , MonadGen)

instance Profunctor EnumGen' where
  dimap :: (a -> b) -> (c -> d) -> EnumGen' b c -> EnumGen' a d
                                -- Gen c        -- Gen d
  dimap f h g = EnumGen' $  fmap h (runEnumGen' g)
  -- comap _ g = EnumGen' (runEnumGen' g)

instance ProfunctorPartial EnumGen' where
  toFailureP :: EnumGen' u v -> EnumGen' (Maybe u) v
             -- Gen v        -- Gen v
  toFailureP g = EnumGen' (runEnumGen' g)

instance Profmonad EnumGen'

instance BiUngen EnumGen' where
  uniform  = EnumGen' . oneof . fmap pure
  select _ = EnumGen' . oneof . fmap runEnumGen'
  -- oneOf :: [Gen a] -> Gen a
  -- Randomly uses one of the given generators. The input list must be non-empty.

enumerate' :: (forall g. (ProfunctorPartial g, BiUngen g) => g u v) -> Gen v
enumerate' = runEnumGen'

generate = enumerate' -- specialising to this instance is equiv to our generator deconstructor

-- Logic:
-- We could also generate in the Logic monad

newtype LogicGen b a = LogicGen {runLogicGen :: Logic a}
  deriving (Functor, Applicative, Alternative, MonadPlus, MonadLogic)

instance Monad (LogicGen b) where
  return = LogicGen . pure
  mx >>= f = LogicGen (runLogicGen mx >>- runLogicGen . f)

instance Profunctor LogicGen where
  dimap :: (a -> b) -> (c -> d) -> LogicGen b c -> LogicGen a d
  dimap _ f l = LogicGen $ f <$> runLogicGen l
  -- comap _ g = LogicGen (runLogicGen g)

instance Profmonad LogicGen

instance BiUngen LogicGen where
  uniform = msum . fmap pure
  select _ = msum

instance ProfunctorPartial LogicGen where
  toFailureP g = LogicGen (runLogicGen g)

enumerate :: (forall g. (ProfunctorPartial g, BiUngen g) => g u v) -> [v]
enumerate = observeAll . runLogicGen

-- Gen / Ungen
-- ------------

-- Where the randomness is a sequence of choices
--   Fwd  : [Choices] -> a
--   Bwd  : a    -> Maybe [Choices]
-- Actual types more complex (see SeqGen for fwd, and SeqUngen for bwds)
type Choice = (String, Int)

-- Gen (fwd instance):
                                       -- Choices to generate bases on
newtype SeqGen b a = SeqGen {runSeqGen :: [Choice] -> Maybe (a, [Choice])}
                                                   -- Maybe a generated value
                                                   -- and the list of changes
                                                   -- still to be used
                                                   -- (generating value consumes choices)

instance Functor (SeqGen b) where
  fmap f x = SeqGen $ (first f <$>) . runSeqGen x

instance Applicative (SeqGen b) where
  pure = return
  (<*>) = ap

instance Monad (SeqGen b) where
  return x = SeqGen $ \cs -> Just (x, cs)
  mx >>= f =
    SeqGen $
      runSeqGen mx >=> \(x, cs') -> runSeqGen (f x) cs'

instance Profunctor SeqGen where
                                              -- [Choice] -> Maybe (d, [Choice])
  dimap :: (a -> b) -> (c -> d) -> SeqGen b c -> SeqGen a d
                                -- [Choice] -> Maybe (c, [Choice])
  dimap _ f g = SeqGen $ \cs -> first f <$> runSeqGen g cs

instance Profmonad SeqGen

instance BiUngen SeqGen where
  uniform = pure . head -- lifts the head of the list to be the first elem of our tuple
  select sid gs = do
    (s, i) <- choice
    -- (s, i) :: Choice
    -- remember Choice = (String, Int)
    if sid /= s -- Ensure it matches the string arg
      then SeqGen (const Nothing) -- if not, Nothing
      else gs !! i                -- upon match, extract the correct index from gs
    where
             -- [Choice] -> Maybe (Choice, [Choice])
      choice :: SeqGen a Choice
      choice = SeqGen $ \case
        [] -> Nothing
        (c : cs) -> Just (c, cs) -- Takes first choice, and pairs with rest of list

instance ProfunctorPartial SeqGen where
  toFailureP :: SeqGen u v -> SeqGen (Maybe u) v
  toFailureP = SeqGen . runSeqGen -- Easy peasy to change the phantom type

-- You can see examples of this instance in action for genTree and bst

-- Ungen (bwd instance):
                                             -- Value to inform choices on
newtype SeqUnGen b a = SeqUnGen {runSeqUnGen :: b -> Maybe (a, [Choice])}
                                                  -- a and the Choices that would lead to it

instance Functor (SeqUnGen b) where
  fmap f x = SeqUnGen $ fmap (first f) . runSeqUnGen x

instance Applicative (SeqUnGen b) where
  pure = return
  (<*>) = ap

instance Monad (SeqUnGen b) where
  return x = SeqUnGen $ const (Just (x, [])) -- No choices lead to x. We were just given x.
  mx >>= f = SeqUnGen $ \b -> -- why we not in monad?
    case runSeqUnGen mx b of
      Nothing -> Nothing
      Just (x, cs) -> case runSeqUnGen (f x) b of
        Nothing -> Nothing
        Just (y, cs') -> Just (y, cs ++ cs') -- collect together the choices that led us here

instance Alternative (SeqUnGen b) where
  -- unravels to be basically alternative of maybe monad
  empty = SeqUnGen $ const Nothing
  ux <|> uy = SeqUnGen $ \b ->
    let extractMaybe = ($ b) . runSeqUnGen -- unpack and apply to b
     in extractMaybe ux <|> extractMaybe uy

instance MonadPlus (SeqUnGen b)

instance Profunctor SeqUnGen where
                                                -- a -> Maybe (d, [Choice])
  dimap :: (a -> b) -> (c -> d) -> SeqUnGen b c -> SeqUnGen a d
                                -- b -> Maybe (c, [Choice])
  dimap f h g = SeqUnGen $ \a -> first h <$> runSeqUnGen g (f a)
  -- comap f ug = SeqUnGen $ f >=> runSeqUnGen ug

instance Profmonad SeqUnGen

instance ProfunctorPartial SeqUnGen where
                             -- Maybe u -> Maybe (v, [Choice])
  toFailureP :: SeqUnGen u v -> SeqUnGen (Maybe u) v
              -- u -> Maybe (v, [Choice])
  toFailureP g = SeqUnGen $ \mu -> join $ runSeqUnGen g <$> mu

instance BiUngen SeqUnGen where
  select s = msum . zipWith recordChoice [0 ..]
    where
      tell x = SeqUnGen $ \b -> Just ((), x)
      recordChoice i d = do
        tell [(s, i)]
        x <- d
        comap (\y -> if x == y then Just x else Nothing) (pure x)
  uniform = msum . map pure

-- Properties:
-- -----------------------------------------------------------------------------

-- His current round tripping specialised to Gen/check.

-- generate :: (forall g. (ProfunctorPartial g, BiUngen g) => g u v) -> Gen v
-- check :: (forall g. (ProfunctorPartial g, BiUngen g) => g u v) -> u -> Maybe v

-- When we specialise to Gen, everyone contained in the Gen must equal Just x when
-- specialised to the pure generator
sound :: (Eq u, Show u) => (forall g. (ProfunctorPartial g, BiUngen g) => g u u) -> Property
sound bg = forAll (enumerate' bg) $ \x -> -- forAll :: (Show a, Testable prop) => Gen a -> (a -> prop) -> Property from QuickCheck
                   -- specialises to be Gen
  purify bg x === Just x
  -- specialises to be pure

-- soundWeak :: (Eq u, Show u) => (forall g. (ProfunctorPartial g, BiUngen g) => g u v) -> Property
-- soundWeak bg = forAll (enumerate' bg) $ \x -> -- forAll :: (Show a, Testable prop) => Gen a -> (a -> prop) -> Property from QuickCheck
--   purify bg x === Just x

-- Prop in action:
-- *Ungenerators> quickCheck $ sound genTree
-- +++ OK, passed 100 tests.
-- *Ungenerators> quickCheck $ sound badGenTree
-- *** Failed! Falsified (after 4 tests)..
-- *** Failed! Falsified (after 4 tests):
-- Node (Node (Node Leaf 9 Leaf) 1 (Node Leaf 4 Leaf)) 6 Leaf
-- Nothing /= Just (Node (Node (Node Leaf 9 Leaf) 1 (Node Leaf 4 Leaf)) 6 Leaf)

-- Following the same reasoning as in the paper, you won't be able to prove this
-- property in a quasicompsitional manner:

-- First up, this property is aligned so it would have to be weakened to stand a
-- chance of being compositional with respect to bind, which we cannot because check
-- wants a b, but we only can make as using generate.
-- Second, say you did have some weakened soundness property, we have a counter
-- example where the quasi-comp would claim sound for something we know to be unsound.

-- Counter example:

-- Building blocks for counter example:

-- Happily the testy nature of this prob catches the unsoundness
-- counter example from paper:
bool :: forall g. (ProfunctorPartial g, BiUngen g) => g Bool Bool
bool = select "Bool" [pure True, pure False]

isTrue :: Bool -> Maybe Bool
isTrue True = Just True
isTrue False = Nothing

-- Now we will see two example gs:
--   ex1 = clearly unsound as it can generate Trues and Falses, but only accepts Trues
--   ex2 = sound because it only generates and accepts Trues.

ex1 :: forall g. (ProfunctorPartial g, BiUngen g) => g Bool Bool
ex1 = comap isTrue bool -- unsound
-- >>> quickCheck (sound ex1)
-- *** Failed! Falsified (after 1 test):
-- False
-- Nothing /= Just False

ex2 :: forall g. (ProfunctorPartial g, BiUngen g) => g Bool Bool
ex2 = comap isTrue (return True)

-- Now, let's trace through our quasi-comp reasoning, focussing on the comap case.
-- Here assume:
--   * return True upholds our soundness (we'd hope so cos return should to be comp)
--   * some good programmer has proved the atomic bool to fulfil our weak sound prop
-- The comp game is reducing the comap expression to the atomic g that fulfils the
-- prop. Cos both of these g's share the same projection, they should be acted on
-- by comap in the same way, but that would result in our quasi-comp proof reporting
-- that both are sound, which they are not :-(
-- => there is no soundness prop that is quasicomp

-- whereas weak completeness works just like in comp bx monadically:


-- Weak (unaligned) complete
-- >>> quickCheck $ property (\u -> complete genTree u)
-- >>> quickCheck $ property (complete (uniform [1,2,3]))
complete :: Eq v => (forall g. (ProfunctorPartial g, BiUngen g) => g u v) -> u -> Bool
complete g x = case check g x of
  Just y  -> y `elem` enumerate g
  Nothing -> True

-- This is compositional with respect to BiUngens / ProfunctorPartials
-- (proved for interesting cases, with proofs for other cases alluded to)

(====) :: a -> a -> a
(====) = undefined

-- return
completeCompReturn :: forall u v . (Eq u, Eq v) => v -> u -> Bool
completeCompReturn y x =
  case check (return y) x of
  Just y  -> y `elem` enumerate (return y)
  Nothing -> True
  ==== {- only consider relevant case -}
  let Just y' = check (return y) x
  in (y' `elem` enumerate (return y))
  ==== {- def. enumerate -}
  (y' `elem` (observeAll . runLogicGen) (return y))
  ==== {- def. runLogicGen -}
  (y' `elem` observeAll (return y))
  ==== {- def. observeAll (return z) = [z] -}
  (y' `elem` [y])
  ==== {- def. yIsy' -}
  True
    where
      yIsy =
        check (return y) x
        ==== {- def check -}
        purify (return y) x
        ==== {- def purify -}
        runPureGen (return y) x
        ==== {- def return_PureGen -}
        runPureGen (PureGen $ const (Just y)) x
        ==== {- def runPureGen -}
        const (Just y) x
        ==== {- def runPureGen -}
        Just y

-- bind
completeCompBind
  :: Eq w
  => (forall g. (ProfunctorPartial g, BiUngen g) => g u v)
  -> (v -> forall g. (ProfunctorPartial g, BiUngen g) => g u w)
  -> u
  -> v
  -> Bool
completeCompBind g k x y' =
  -- ASS 1: check g x == Just y => y `elem` g
  -- ASS 2: forall v. check (k v) x == Just y => y `elem` (k v)
  -- We can use the consequent of these by assuming our antecedent (that it is a Just),
  -- which we have done below, and knowing that how bind for Maybes works, the result
  -- of g and kv must also have been Justs to get us into the Just case
  case check (g >>= k) x of
    Just y  -> y `elem` enumerate (return y)
    Nothing -> True
  ==== {- only consider relevant case -}
  let Just y = check (g >>= k) x -- ASS 3
  in (y `elem` enumerate (g >>= k))
  ==== {- enumerate -}
  (y `elem` (observeAll . runLogicGen) (g >>= k))
  ==== {- runLogicGen -}
  (y `elem` observeAll (runLogicGen g >>= runLogicGen <$> k))
  ==== {- runLogicGen -}
  (y `elem` ((observeAll . runLogicGen) g >>= observeAll . runLogicGen . k))
  ====
  let gs = (observeAll . runLogicGen) g
  in (y `elem` (gs >>= observeAll . runLogicGen . k))
  ==== {- >>=_[] -}
  (y `elem` concatMap (observeAll . runLogicGen . k) gs)
  ==== {- concatMap -} -- Will be non-empty as long as gs is, and we know that gs is non-empty cos y is an elem (consequent ASS 2)
  (y `elem` concat (undefined ++ [(observeAll . runLogicGen . k) y'] ++ undefined)) -- our mapped function gets applied to all elems, we will focus on some y'
  ==== {- repackage -}
  (y `elem` concat (undefined ++ [enumerate (k y')] ++ undefined))
  ==== {- ASS 3 -}
  (y `elem` concat (undefined ++ [undefined ++ [y] ++ undefined] ++ undefined))
  ==== {- concat -}
  (y `elem` (undefined ++ undefined ++ [y] ++ undefined ++ undefined))
  ==== {- tidy -}
  (y `elem` (undefined ++ [y] ++ undefined))
  ==== {- elem -}
  True

-- (made by copying my proofs from Relative Recover => I think the rest of the cases will also follow)
-- dimap
-- toFailure
-- ^ left for reader to convert from my Relative Recovery proofs (different deets same big steps)
-- The interesting case is the new functions from the BiUngen class...

-- uniform
completeCompUniform :: Eq v => [v] -> v -> Bool
completeCompUniform as x =
  case check (uniform as) x of
    Just y  -> y `elem` enumerate (uniform as)
    Nothing -> True
  ==== {- only consider relevant case -}
  let Just y' = check (uniform as) x
  in (y' `elem` enumerate (uniform as))
  ==== {- def. enumerate -}
  (y' `elem` (observeAll . runLogicGen) (uniform as))
  ==== {- def. uniform_LogicGen -}
  (y' `elem` (observeAll . runLogicGen) ((msum . fmap pure) as))
  ==== {- def. pure_LogicGen -}
  (y' `elem` (observeAll . runLogicGen) ((msum . fmap (LogicGen . pure)) as))
  ==== {- def. behaviour of msum for logic and lists -}
  (y' `elem` as)
  ==== {- lemma -}
  (head as `elem` as)
  ==== {- trivial -}
  True
  where
    -- check (uniform as) == Just (head as)
    lemma =
      check (uniform as) x
      ==== {- check / purify -}
      runPureGen (uniform as) x
      ==== {- uniform_PureGen -}
      runPureGen ((msum . map pure) as) x
      ==== {- pure_PureGen -}
      runPureGen ((msum . map (\a -> PureGen $ const (Just a))) as) x
      ==== {- ... -}
      msum (Just <$> as)
      ==== {- msum takes first Just, and since they are all Justs thats the head -}
      (Just . head) as

-- select:
completeCompSelect
  :: forall u
   . Eq u
  => String
  -> (forall g. (ProfunctorPartial g, BiUngen g) => [g u u]) -- select forces alignment
  -> u
  -> Bool
completeCompSelect s gs x =
  -- ASS: each of the gs upholds the property i.e
  --      forall x, g `elem` gs. check g x == Just y => y `elem` g
  case check (select s gs) x of
    Just y  -> y `elem` enumerate (select s gs)
    Nothing -> True
  ==== {- only consider relevant case -}
  let Just y = check (select s gs) x
  in (y `elem` enumerate (select s gs))
  ==== {- enumerate -}
  (y `elem` (observeAll . runLogicGen) (select s gs))
  ==== {- select_Logic -}
  (y `elem` (observeAll . runLogicGen) (msum gs))
  -- Our assumption gives:
  -- forall x, g `elem` gs. check g x == Just y => y `elem` g
  -- I pick g to be the first item of gs that results in Just when applied to x.
  -- As we are in the Just case, I know one of these must exist in gs
  -- The lemma below provides us the antecedent for this ass
  -- => y `elem` g
  -- Because msum for Logic collates all the results, we only need y to be in one
  -- of the component parts, which it it. It is in g.
  ====
  True
  where
    -- When select falls into the just case that is because at least one g in its
    -- list resulted in a Just when checking, and it is the Just y of the first one
    -- in the list that does that that is our result:
    lemma
      :: (forall g. (ProfunctorPartial g, BiUngen g) => g u u)
      -> u
      -> Bool
    lemma g y =
      -- GOAL: (check (select s gs) x) == Just y => (check g x) == Just y
      (check (select s gs) x == Just y)
      ==== {- check / purify -}
      (runPureGen (select s gs) x == Just y)
      ==== {- select_PureGen -}
      (runPureGen (msum gs) x == Just y)
      ==== {- msum -}
      (runPureGen ((foldr (<|>) empty) gs) x == Just y)
      ==== {- msum -}
      (runPureGen (undefined <|> g <|> undefined) x == Just y)
      ==== {- (<|>) left assoc -}
      (runPureGen ((undefined <|> g) <|> undefined) x == Just y)
      ==== {- (<|>)_PureGen -}
      (runPureGen ((PureGen $ \b -> undefined <|> runPureGen g b) <|> undefined) x == Just y)
      -- to result in a Just, one of gs must have resulted in Just,
      -- and it is the first of these whose result is Just y,
      -- lets assume that is the one we called g
      ====
      (runPureGen ((PureGen $ \b -> runPureGen g b)) x == Just y)
      ==== {- runPureGen -}
      ((\b -> runPureGen g b) x == Just y)
      ==== {- beta -}
      (runPureGen g x == Just y)
      ==== {- check / purify -}
      (check g x == Just y) -- Where g is the first item of gs that resulted in
                            -- a Just when applied to x

-- Feedback on Harrison's soundness prop:
--   * Nice that the testing is build in, gives confidence
--   * Will be hard to formally prove
--       - doesn't decompose (as above)
--       - might have to prove instance by instance, BiUngen is too high an abstraction
--         to formally prove properties, well atleast the way I proved, cos I always
--         unpacked to provide equational reasoning proof
-- => As Comp BX Monadically says soundness is not a key thing in testing, this might
--    be a sufficient property

-- General thoughts on a prop for ungenerators:
-- Ungenerators sing back to me the choices I made to generate a particular example.
-- I don't want them to do that when the example was invalid.
-- e.g.
-- >>> runSeqUnGen (bst (0,2)) (Node Leaf 5 Leaf)
-- Not sure there is anything else that I would want from an ungenerator?
-- Not sure what soundness and completeness tell us about ungenerators because
-- they specialise to the more traditional gen instance, maybe specialising to
-- the simple SeqGen one will be more representative?

-- runSeqGen :: [Choice] -> Maybe (a, [Choice])
-- runSeqUnGen :: b -> Maybe (a, [Choice])

-- Fwd: If I generate something from these choices, it better be the case that
-- the choices reported to have generated it are indeed those I fed in (cs)
fwd :: Eq u => (forall g. (ProfunctorPartial g, BiUngen g) => g u u) -> [Choice] -> Bool
fwd g [] = True
fwd g cs = case runSeqGen g cs of
  Just (v, cs') -> runSeqUnGen g v == Just (v,choicesUsed cs cs') -- The choices I used to gen v are difference between cs and cs'
  Nothing     -> True
  where
    -- choices before -> choices after -> choices used
    choicesUsed [] [] = []
    choicesUsed [] _ = undefined -- if there were no choices before there can only be no choices after
    choicesUsed cs [] = cs -- if there are all choices after, all of them must have been used
    choicesUsed cs cs'
      | length cs == length cs' = [] -- none where consumed
      | length cs > length cs' = take (length cs - length cs') cs
      | length cs < length cs' = undefined -- cannot be, you shouldnt end up with more choices

-- We cannot make this unaligned because there is nothing that we can feed into
-- => same story as soundness
-- weakFwd :: (forall g. (ProfunctorPartial g, BiUngen g) => g u v) -> [Choice] -> Bool
-- weakFwd g cs = case runSeqGen g cs of
--   Just (v, cs') -> runSeqUnGen g v == Just (_,cs)

-- Even if we could, and there were some weak property that corresponds to this
-- I wonder if a similar counter example to that of soundness applies.

-- Now they both pass
-- >>> quickCheck (fwd ex1)
-- +++ OK, passed 100 tests.
-- >>> quickCheck (fwd ex2)
-- +++ OK, passed 100 tests.
-- BUT THIS IS WRONG. I have a counter example for fwd and ex1:
-- >>> fwd ex1 [("Bool", 1)]
-- False
-- Clearly this is not a good testing property cos choices are too general to generate
-- making it unlikely that [("Bool", 1)] comes up in the 100 tests, we should have
-- a more restrictive input

-- ex1:
-- true trip (works as expected)
-- >>> runSeqGen ex1 [("Bool", 0)]
-- Just (True,[])
-- >>> runSeqUnGen ex1 True
-- Just (True,[("Bool",0)])
-- false trip
-- >>> runSeqGen ex1 [("Bool", 1)]
-- Just (False,[])
-- >>> runSeqUnGen ex1 False
-- Nothing
-- ^ not as expected! Cos the choices that its happy to generate with dont match
-- the choices it can generate
-- => counter example to fwds round trip

-- ex2:
-- true trip (works as expected)
-- >>> runSeqGen ex2 [("Bool", 0)]
-- Just (True,[("Bool",0)])
-- ^ remember that return doesnt consume any choices
-- >>> runSeqUnGen ex2 True
-- Just (True,[])
-- ^ it knows no choices were consumed

-- So it is the same story where one does fulfil and one doesnt but both atomic
-- biungens were fine and the same comap was applied

-- => we could have a fwds prop more specific to the ungen context, but it will
-- suffer the same provability issues as sound, altho we could combat this with
-- the testing if we made the input type more constrained.

-- Backwards: if i extrapolate choices that made something, those choices better
-- make it
bwd :: Eq u => (forall g. (ProfunctorPartial g, BiUngen g) => g u u) -> u -> Bool
bwd g x = case runSeqUnGen g x of
  Just (_, cs) -> runSeqGen g cs == Just (x, []) -- all choices should be consumed
  Nothing -> True

-- We can easily weaken this:
bwdWeak :: Eq v => (forall g. (ProfunctorPartial g, BiUngen g) => g u v) -> u -> Bool
bwdWeak g x = case runSeqUnGen g x of
  Just (x', cs) -> runSeqGen g cs == Just (x', []) -- all choices should be consumed
  Nothing       -> True

-- And my guess is that it would be compositional.
-- TODO prove?

-- Examples:
-- -----------------------------------------------------------------------------

data Tree
  = Leaf
  | Node Tree Int Tree
  deriving (Show, Eq, Generic)

instance Arbitrary Tree where
  arbitrary = genericArbitrary
  shrink = genericShrink

-- node :: Iso ((Tree, Int), Tree) Tree
-- node = (\((l, x), r) -> Node l x r, \case Node l x r -> Just ((l, x), r); _ -> Nothing)

nodeVal :: Tree -> Maybe Int
nodeVal (Node _ x _) = Just x
nodeVal _ = Nothing

nodeLeft :: Tree -> Maybe Tree
nodeLeft (Node l _ _) = Just l
nodeLeft _ = Nothing

nodeRight :: Tree -> Maybe Tree
nodeRight (Node _ _ r) = Just r
nodeRight _ = Nothing

-- basic tree generator, makes a tree of max depth 4
genTree :: forall g. (ProfunctorPartial g, BiUngen g) => g Tree Tree
genTree = aux 4
  where
    aux 0 = pure Leaf
    aux n =
      select
        "Tree"
        [ pure Leaf,
          do
            x <- comap nodeVal genInt
            l <- comap nodeLeft (aux (n - 1))
            r <- comap nodeRight (aux (n - 1))
            pure (Node l x r)
        ]
    genInt = select "Int" [pure x | x <- [0 .. 10]]

-- check:
-- Ungenerators> runPureGen genTree Leaf
-- Just Leaf
-- Ungenerators> runPureGen genTree (Node Leaf 3 Leaf)
-- Just (Node Leaf 3 Leaf)
-- Nothings will be returned when nodes values not in range 0 .. 10, or when dept
-- of tree is bigger than 4 (i.e. this generator is sound)

-- Gen from choices:
-- >>> runSeqGen genTree [("Tree",0)]
-- Just (Leaf,[])
-- ^ because I asked for index 0 of the Tree list: a Leaf
-- >>> runSeqGen genTree [("Tree",1) ]
-- Nothing
-- ^ Is upset cos I never said that an Int was chosen, so we cani chose the value
--   of the node index 1 node
-- Example from poster:
-- >>> runSeqGen genTree [("Tree",1), ("Int", 5), ("Tree",0), ("Tree",0) ]
-- Just (Node Leaf 5 Leaf,[])
-- ^ Must have three tree choices to work
-- Bigger example:
-- >>> runSeqGen genTree [("Tree",1), ("Int", 2), ("Tree",1),("Int", 3), ("Tree",0), ("Tree",0),("Tree",0)  ]

-- Ungen:
-- >>> runSeqUnGen genTree (Node Leaf 5 Leaf)
-- Just (Node Leaf 5 (Node Leaf 2 Leaf),[("Tree",1),("Int",5),("Tree",0),("Tree",1),("Int",2),("Tree",0),("Tree",0)])
-- ^^ lists them the "wrong" way round

-- Faulty version of genTree with a mixup in the def
-- NOTE:- this only adversely affects the check instance
badGenTree :: forall g.  (ProfunctorPartial g, BiUngen g) => g Tree Tree
badGenTree = aux 4
  where
    aux 0 = pure Leaf
    aux n =
      select
        "Tree"
        [ pure Leaf,
          do
            x <- comap nodeVal genInt
            -- left and rights mixed up
            l <- comap nodeRight (aux (n - 1))
            r <- comap nodeLeft (aux (n - 1))
            pure (Node l x r)
        ]
    genInt = select "Int" [pure x | x <- [0 .. 10]]

-- check:

-- Should work for symmetrical trees:
-- *Ungenerators> check badGenTree Leaf
-- Just Leaf
-- *Ungenerators> check badGenTree (Node Leaf 2 Leaf)
-- Just (Node Leaf 2 Leaf)
-- *Ungenerators> check badGenTree (Node (Node Leaf 4 Leaf) 2 (Node Leaf 4 Leaf))
-- Just (Node (Node Leaf 4 Leaf) 2 (Node Leaf 4 Leaf))

-- but fails when the on non symmetric ones:
-- *Ungenerators> check badGenTree (Node (Node Leaf 3 Leaf) 2 (Node Leaf 4 Leaf))
-- Nothing
-- nodeRight / nodeLeft generate the nothing, which shows itself in the pure bug
-- as the comapped maybe is the result

-- Ungen:
-- >>> runSeqUnGen genTree (Node Leaf 5 (Node Leaf 2 Leaf))
-- Just (Node Leaf 5 Leaf,[("Tree",1),("Int",5),("Tree",0),("Tree",0)])
-- >>> runSeqUnGen genTree (Node Leaf 11 Leaf)
-- Nothing
-- ^ fails because 11 is out of the generate range

-- bst example:

-- dud bool generator that is always generates False, and the check passes
-- >>> check boolF True
-- Nothing
-- >>> check boolF False
-- Just False
-- >>> sample $ generate boolF
-- False
boolF :: forall g.  (ProfunctorPartial g, BiUngen g) => g Bool Bool
boolF = comap (\b -> if b == False then Just False else Nothing) (pure False)

isLeaf :: Tree -> Bool
isLeaf Leaf = True
isLeaf (Node _ _ _) = False

nodeValue :: Tree -> Maybe Int
nodeValue (Node _ n _) = Just n
nodeValue _            = Nothing

leaf :: forall g.  (ProfunctorPartial g, BiUngen g) => g Tree Tree
leaf = pure Leaf

inRange :: (Int, Int) -> forall g.  (ProfunctorPartial g, BiUngen g) => g Int Int
inRange (min, max) = select "Int" [pure x | x <- [min .. max]]

-- gen:
-- >>> sample $ generate (bst (0,2))
-- Node (Node (Node Leaf 0 Leaf) 1 Leaf) 2 Leaf

-- check:
-- >>> check (bst (0,2)) (Node (Node (Node Leaf 0 Leaf) 1 Leaf) 2 Leaf)
-- Just (Node (Node (Node Leaf 0 Leaf) 1 Leaf) 2 Leaf)
-- >>> check (bst (0,2)) (Node (Node Leaf 1 Leaf) 2 (Node Leaf 6 Leaf)) -- values go out of range
-- Nothing
-- >>> check (bst (0,6)) (Node (Node Leaf 6 Leaf) 2 (Node Leaf 1 Leaf)) -- values on wrong side
-- Nothing

-- gen from choices:
-- >>> runSeqGen (bst (2,0)) []
-- Just (Leaf,[])
-- >>> runSeqGen (bst (0,1)) [("Int", 0), ("Int", 0)]
-- Just (Node Leaf 0 (Node Leaf 1 Leaf),[])

-- ungen:
-- >>> runSeqUnGen (bst (0,6)) (Node Leaf 5 Leaf)
-- Just (Node Leaf 5 Leaf,[("Bool",1),("Int",5),("Bool",0),("Bool",0)])
-- >>> runSeqUnGen (bst (0,2)) (Node Leaf 6 Leaf)
-- Nothing
-- ^ cos 6 out of range -- In range Nothings, which is good for completeness
-- >>> runSeqUnGen (bst (0,7)) (Node (Node Leaf 6 Leaf) 1 Leaf)
-- Nothing
-- ^ cos not a valid bst -- Again the nothing is triggered by inRange, but on the second level
-- >>> runSeqUnGen (bst (0,7)) (Node (Node Leaf 9 Leaf) 0 Leaf)
-- Just (Node Leaf 0 Leaf,[("Bool",1),("Int",0),("Bool",0)])
-- Interestingly, because the min > max case results in a leaf this stops with leaf
-- at that case. It only gets Nothing upset when a select goes wrong
-- This is a result of the way it has been defined to stop when min and max cross.
-- This reminds me of the two levels of failure that my Recovery, but instead of
-- the state you can compare it to what you put in.
-- => a toPredicate wrapper about check that looks to see if it is a just is not
-- appropriate:
-- >>> check (bst (0,7)) (Node (Node Leaf 9 Leaf) 0 Leaf)
-- Just (Node Leaf 0 Leaf)
-- I think this is okay for an ungenerator tho. It is good that they take my example
-- and "fix it"
bst :: (Int, Int) -> forall g.  (ProfunctorPartial g, BiUngen g) => g Tree Tree
bst (min, max) | min > max = leaf
bst (min, max) = do
  isLeaf' <- comap (Just . isLeaf) bool
  if isLeaf' then return Leaf
  else do
    n <- comap nodeValue (inRange (min, max))
    l <- comap nodeLeft  (bst (min, n - 1))
    r <- comap nodeRight (bst (n + 1, max))
    return (Node l n r)

-- CtxGen:
-- -----------------------------------------------------------------------------

-- This is just 'BiUngen' except with an extra "context" argument c.
class Profmonad g => CtxGen c g where
  selectCtx :: Eq a => String -> c -> [g a a] -> g a a
  uniformCtx :: c -> [g a a] -> g a a

-- Add x to list, then take first four elems of list
-- Used for remembering the latest decisions so far, e.g. what side of the tree we are
-- and the values above us
(<:>) :: a -> [a] -> [a]
(<:>) x = take 4 . (x :)

-- Partial isomorphism between the components of a node, and a node
node :: Iso ((Tree, Int), Tree) Tree
node = ( \((l, x), r) -> Node l x r
       , \case Node l x r -> Just ((l, x), r); _ -> Nothing)

-- Just constructs the context, remember the instances decide what is done with it.
ctxGenTree :: forall g. (ProfunctorPartial g, CtxGen [String] g) => g Tree Tree
ctxGenTree = aux []
  where
    aux ctx =
      selectCtx
        "Tree"
        ctx
        [ pure Leaf,
          do
            -- we contextualise with four strings on info describing the tree
            -- just above us with what side of the tree we are ('L'/'R') and what
            -- value is above us
            i <- comap nodeVal (genInt ctx)
            let ctx' = show i <:> ctx -- Add new value to list, forgetting the oldest value
            node <$$> aux ("L" <:> ctx') <**> pure i <**> aux ("R" <:> ctx')
            -- create a node from its component parts
        ]
    genInt ctx = selectCtx "Int" ctx [pure x | x <- [0 .. 10]]

-- >>> sample $ runRLGen ctxGenTree (Map.singleton ("Tree", []) [1000,0] :: RLModel [String])
-- Leaf
-- >>> sample $ runRLGen ctxGenTree (Map.empty :: RLModel [String])
-- spins because the default context forces the node choice

-- usage:

-- He used the contextualisation for his RLCheck example which implements an algorithm
-- to create a generator of interesting inputs. His implementation using the BX
-- infrastructure allows the algorithm to be pre-trained, improving its initial choices.

-- The two directions of this are still generate (from an RLModel) and ungenerate,
-- but we the use choices reported by the ungenerator to reward and improve the RL model

-- Type of an RLModel (Source of "randomness"):
type RLModel c
  = Map
      (String, c) -- String describing choice made in context c
      [Int]       -- Frequency that an index appears, where the frequency occurs
                  -- at the index in question

-- generate:
newtype RLGen c b a = RLGen {runRLGen :: RLModel c -> Gen a}

instance Functor (RLGen c b) where
  fmap f x = RLGen $ fmap f . runRLGen x

instance Applicative (RLGen c b) where
  pure = return
  (<*>) = ap

instance Monad (RLGen c b) where
  return x = RLGen $ \_ -> pure x
  mx >>= f = RLGen $ \b -> do
    x <- runRLGen mx b
    runRLGen (f x) b

instance Profunctor (RLGen g) where
                                -- RLModel g -> Gen c
  dimap :: (a -> b) -> (c -> d) -> RLGen g b c -> RLGen g a d
  dimap g f rl = RLGen $ \m -> f <$> runRLGen rl m
  --comap _ = RLGen . runRLGen

instance Profmonad (RLGen c)

instance ProfunctorPartial (RLGen g) where
  toFailureP :: RLGen g u v -> RLGen g (Maybe u) v
  toFailureP = RLGen . runRLGen

instance Ord c => CtxGen c (RLGen c) where
  uniformCtx _ gs = RLGen $ \m -> oneof $ map (`runRLGen` m) gs
    -- ignores context, then does the standard uniform thing where we just pick
    -- one from the list and then lift
  selectCtx s c gs = RLGen $ \m -> do
    -- We making a Gen thing:
    -- Selected index is based on context and string
    -- Current context and string is used to retrieve frequencies of indices
    let fs = Map.findWithDefault
              [1 ..] -- default
              (s, c) -- desired key
              m
    i <- frequency (zip fs (pure <$> [0 ..])) -- frequencies informs choice of i
    runRLGen (gs !! i) m -- i selected

-- So the frequencies is like the reward we give.
-- (not gonna copy the rest of the implementation as this was just to demo how
-- the context can be used)

-- Manual vs automatic defining
-- -----------------------------------------------------------------------------


-- One could consider the use of the combinators such as (<**>) or (<$$>) as
-- "automatically" defining the backwards dir, as you do not have to specify it
-- via comap.

-- I wonder if this is related to my discussion with Eddie, where we resolved that
-- "the correct choice of comap is derivable from a prism (it is match of hte constructor in question)"

-- Prisms

-- > A lens describes something isomorphic to a product with some extra context. A lens from s to a indicates there exists c such that s is isomorphic to (c, a).
-- > On the other hand, a prism from s to a indicates there exists c such that s is isomorphic to (Either c a).
--   - Edward Kmett

-- prism ops:
-- match :: s -> Maybe a
-- rebuild :: a -> s

-- in lens lib, a prism is an iso that is partial in on dir, so it does seem like these things are very connected

-- example of how the comap is the match

-- Prisms can be thought of as deconstructing sum types
-- (where as lenses do product types)

-- Tree is a sum type:
-- data Tree = Leaf | Node Tree Int Tree

data Prism s a = Prism
  { match   :: s -> Maybe a
  , rebuild :: a -> s
  }

-- prism for nodevalue
nodeValP :: Prism Tree Int
nodeValP = Prism m r
  where
    m :: Tree -> Maybe Int
    m Leaf = Nothing
    m (Node _ n _) = Just n
    -- ^^ this is exactly the same as nodeVal:
    -- nodeVal :: Tree -> Maybe Int
    -- nodeVal (Node _ x _) = Just x
    -- nodeVal _ = Nothing
    r :: Int -> Tree
    r n = Node Leaf n Leaf

-- => what to provide the comap with is just the match of the prism for that part

-- does this relate to the automatic style?
-- Well Iso in this file, is basically a prism!
-- type Iso a b = (a -> b, b -> Maybe a) -- where b is source
-- type Pri a b = (a -> s, s -> Maybe a) -- (rebuild, match)

-- and if we look at the def of <$$> it literally uses the match

-- this is a special case tho, potentially could be more generally done?
