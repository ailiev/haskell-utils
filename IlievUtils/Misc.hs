{-# LANGUAGE OverlappingInstances, MultiParamTypeClasses, TypeSynonymInstances #-}

module IlievUtils.Misc (
		 (.&&),
                 (.||),
		 (.*),
                 (...),

--                 StreamShow (..),
--                 (<<),

                 (>>==),

        Stack (..),

        comp2_1,

        notp,
	takeMiddle,
	log2,

         integerize,
         integerize2,

	 ilog2,
         isqrt,
         divUp,
         subtr,

         hex,

         tr,
         trs,

--         mkList,

         findInStack,
         maybeLookup,
         maybeMapAdjust,
         fromJustMsg,
         maybeApply,
         MaybeT, runMaybeT,
         maybeM,
                    
        modifyListHead,
        mapOnTail,
        interleave,

        applyWithDefault,

        splice,
        runSumFrom0,
        filterList,
        breakList,
        spanList,

        mapOne,

		 pair2,
		 pair3,

                 mapTuple2,

                 mapTupleM2,

                 projSnd,
                 projFst,

        tuple2list2,

        myLiftM,
        liftArgM,
        scanM,
        unfoldrM,
        replicateM,
        iterateM,
        sumM,
        iterateWhileM,
        takeWhileM,
        repeatM,
        concatMapM,

        mapAccumDupsBy,
        extractOne,

        strictList,
        strictEval,

        iterateList,

		 factorial,
                 choose,
                 sumOp,
                 mapInputs2,
                
                tup3_get1, tup3_get2, tup3_get3,
                tup3_proj1, tup3_proj2, tup3_proj3,

                tup4_get1, tup4_get2, tup4_get3, tup4_get4, 
                tup4_proj_1,  tup4_proj_2,  tup4_proj_3,  tup4_proj_4,

                tup5_get1,

                expand,
                proj_tup2,
                StreamShow(..),
                (<<),

                DocAble(..),

                splice,
                spliceInIf,
                updateSome,
                mapSplice,
                sublist,
                modifyFirst,
                split,
                joinLists,
                getBits,
                bitMask,

compareWith,
nubOrds,
mapAccumDupsBy,
extractOne,

                bool2int,
                int2bool

		)
    where

import List (isPrefixOf, union, intersperse, mapAccumL, partition, group, sort)

import Maybe                                    (isNothing, fromJust)

import Monad (MonadPlus, mzero, mplus, msum, liftM)

import Control.Monad.Error (Error, noMsg, ErrorT, runErrorT, MonadError(..))

import Control.Monad.Trans (MonadTrans, lift)

import Numeric                      (showHex)

import Data.Bits            ((.&.))
import qualified Data.Bits                      as Bits

import qualified Data.Map as Map

import qualified    Text.PrettyPrint            as PP



-- | give a predicate which is an 'and' of two predicates
infixr 3 .&&
(.&&) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
f .&& g = \x -> f x && g x

-- | give a predicate which is an 'or' of two predicates
infixr 4 .||
f .|| g = \x -> f x || g x

enumAll :: (Enum a) => [a]
enumAll = undefined



-- instance (Eq a) => Eq (a -> a) where
--      f == g = (\x -> (f x) == (g x))
--      f /= g = (\x -> (f x) /= (g x))

-- instance (Num a) => Num (a -> a) where
--     f + g = \a b -> f a + g b
--     f - g = \a b -> f a - g b
--     f * g = \a b -> f a * g b
--     negate f = negate . f
--     abs f    = abs . f
--     signum f = signum . f
--     fromInteger f = fromInteger . f


-- | join two arithmetic operations with a multiply
infixl 7 .*
(.*) :: (Num a) => (a -> a) -> (a -> a) -> (a -> a)
f .* g = \x -> f x * g x


-- | return a predicate which is the 'not' of another predicate
notp :: (a -> Bool) -> (a -> Bool)
notp f = not . f


-- | a cousin of the 'Show' class specially for building strings
class StreamShow a where
    strShows :: a -> ShowS
    strShow  :: a -> String
    -- and default definitions
    strShows x = ((strShow x) ++)
    strShow  x = strShows x ""

instance StreamShow String where
    strShows s = (s ++)
    strShow  s = s

instance StreamShow Int     where strShows = showsPrec 0
instance StreamShow Integer where strShows = showsPrec 0
instance StreamShow Bool    where strShows = showsPrec 0


instance (StreamShow a) => StreamShow [a] where
    strShows = foldl1 (.) . intersperse (", " ++) . map strShows

instance (StreamShow a, StreamShow b) => StreamShow (a,b) where
    strShows (x,y) =    strShows "(" .
                        strShows x . strShows ", " . strShows y .
                        strShows ")"


-- | append two streamable values together.
(<<) :: (StreamShow a, StreamShow b) => (a -> b -> String)
x << y = strShow x ++ strShow y
--    where cleanup = (filter ((/= '"')))



-- | like >>= except the function is outside the monad, so this puts in a "return" for us
infixl 1  >>==
(>>==) :: (Monad m) => m a -> (a -> b) -> m b
k >>== f    = k >>= return . f


-- | using the subtraction operator applied to one arg is tricky (needs a flip, and
-- something like (- x) is taken as (negate x)), hence:
subtr x = \y -> y - x


hex :: Integral a => a -> String
hex x = ("0x"++) . Numeric.showHex x $ ""

{-
instance Show String where
    -- try to deal with surrounding quotes
    show ('"' : chars)  = init chars
    show x              = ('"':x) ++ ['"']
-}


-- | compose a function on 2 args with a function on one arg.
-- same fixity as (.)
infixr 9 ...
(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
f ... g = \x y -> f (g x y)

f `comp2_1` g = \x y -> f (g x) (g y)


{-
takeWhileList :: ([a] -> Bool) -> [a] -> [a]
takeWhileList p [] = []
takeWhileList p l@(x:xs) =
    | p l = x : takeWhileList p xs
    | otherwise = []

dropWhileList p [] = []
dropWhileList p xs@(x:xs')
    | p xs      =  dropWhileList p xs'
    | otherwise =  xs
-}

spanList p []            = ([],[])
spanList p xs@(x:xs') 
            | p xs      =  (x:ys,zs) 
            | otherwise =  ([],xs)
    where (ys,zs) = spanList p xs'

breakList p = spanList (not . p)


-- | filter some sublist out of a list
filterList :: (Eq a) => [a] -> [a] -> [a]
filterList _   [] = []
filterList bad xs = let (pre,badL) = breakList (bad `isPrefixOf`) xs
                        postBad    = drop (length bad) badL
                    in pre ++ (filterList bad postBad)


-- | iterate a function which produces a finite list, by re-applying it on each output, and
-- then concatenating
iterateList :: (a -> [a]) -> a -> [a]
iterateList f x = let fx = f x
                  in 
                    fx ++ concatMap (iterateList f) fx



-- | a version of unfoldr for use in a Monad
unfoldrM :: (Monad m)  =>  ( b -> m (Maybe (a, b)) )  ->  b  ->  m [a]
unfoldrM f x  = do maybe_res <- f x
                   case maybe_res of
                                  (Just (y,x')) -> do rest <- unfoldrM f x'
                                                      return (y : rest)
                                  (Nothing)     -> return []

replicateM :: (Monad m) => Int -> m a -> m [a]
replicateM = sequence ... replicate

-- | iterate a monadic function infinitely.
-- not very useful as monadically produced lists seem to be usually not lazy.
iterateM :: (Monad m) => (a -> m a) -> a -> m [a]
iterateM f x = do y     <- f x
                  zs    <- iterateM f y
                  return (y:zs)

-- | iterate a monadic function while a predicate holds
iterateWhileM :: (Monad m) => (a -> m Bool) -> (a -> m a) -> a -> m [a]
iterateWhileM p f x     = do p_val  <- p x
                             if p_val
                              then do y    <- f x
                                      rest <- iterateWhileM p f y
                                      return (y:rest)
                              else return []

-- | take the prefix of a list while elements satisfy a monadic predicate.
takeWhileM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
takeWhileM p (x:xs)     = do p_val  <- p x
                             if p_val
                              then do rest <- takeWhileM p xs
                                      return (x:rest)
                              else return []
takeWhileM _ []         = return []


-- | repeat a monadic action to infinity.
repeatM :: (Monad m) => m a -> m [a]
repeatM = sequence . repeat



{-
fooM :: (Monad m) => (a -> m b) -> [m a] -> m [b]
fooM f xs = 
-}

sumM ::  (Num a, Monad m) => [a] -> m a
sumM = myLiftM sum


concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f = (liftM concat) . mapM f

-- | remove duplicates in a list of 'Ord' instance (ie. can be sorted). should be
-- | much more efficient than 'List.nub', which is O(n^2)
nubOrds :: (Ord a) => [a] -> [a]
nubOrds = map head . List.group . List.sort

{-
-- set difference for Ord instances, using a Map
diffOrds :: (Ord a) => [a] -> [a] -> [a]
diffOrds x y    = let [x', y'] = map sort [x, y]
 -}                    


compareWith f x y = compare (f x) (f y)


-- apply a function to all second and further instances of an item in a list, with a given
-- equality predicate
{-
mapDupsBy :: (a -> a -> Bool) -> (a -> a) -> [a] -> [a]
mapDupsBy eq f xs = let (ys, _) = foldl g ([],[]) xs
                    in
                      reverse ys
    where g (ys, uniqs) x = let (y, uniqs') = if any (eq x) uniqs -- have already seen x
                                              then (f x, uniqs)
                                              else (x  , x:uniqs)
                            in
                              (y:ys, -- this is efficient, but will build the list in
                                     -- reverse
                               uniqs')
-}


-- | mapAccum a function on equivalent values in a list, presumably to make them
-- different via numbering or such. 'f' will be applied to the first instance of every
-- value too, with 'start' as the accumulator value.
mapAccumDupsBy :: (a -> a -> Bool)
               -> (acc_t -> a -> (acc_t,b))
               -> acc_t
               -> [a]
               -> [b]
mapAccumDupsBy eq f start xs = snd $ mapAccumL g [] xs
-- the accumulator is an assoc list of type (a, acc_t)
    where -- g :: [(a, acc_t)] -> a -> ( [(a, acc_t)], a )
          g accums x = let (mb_accum, rest) = extractOne (eq x . fst) accums
                           (acc_val', y)    = maybe (f start x)
                                                    (\acc -> f (snd acc) x)
                                                    mb_accum
                           in
                             ((x,acc_val') : rest, -- new accumulator
                              y -- new list element
                             )

-- | extract one of a list's elements, which matches some predicate; and the rest of the
-- list.
extractOne :: (a -> Bool) -> [a] -> (Maybe a, [a])
extractOne   p  xs = let (matches, rest) = partition p xs
                     in
                       case matches of
                         []            -> (Nothing, xs)
                         [m]           -> (Just m, rest)
                         (m1:m_rest)   -> (Just m1, m_rest++rest)


-- | substitute a sublist for another list
tr :: (Eq a) => ([a],[a]) -> [a] -> [a]
tr _         [] = []
tr (from,to) xs = let (pre,badL)        = breakList (from `isPrefixOf`) xs
                      postBad           = drop (length from) badL
                  in  if ((length pre) == (length xs)) -- 'from' was not found
                      then xs
                      else pre ++ to ++ (tr (from,to) postBad)

trs :: (Eq a) => [([a],[a])] -> [a] -> [a]
trs = foldl1 (.) . map tr



-- from http://haskell.org/hawiki/LicensedPreludeExts
-- Evaluates every element of the list when any part of the list is examined.
-- Someone modified (improved) this on the unlicensed PreludeExts page.
strictList :: [a] -> [a]
strictList ls@(a : more) = seq a $ seq (strictList more) $ ls
strictList ls = ls

-- | force a strict evaluation of a value, returning it
strictEval :: a -> a
strictEval x = x `seq` x

-- | apply f to (Maybe x), using def if x is Nothing
applyWithDefault :: (a -> a) -> a -> Maybe a -> a
applyWithDefault f def x = case x of
                               Just x'  -> f x'
                               Nothing  -> def


maybeApply :: Maybe (a -> a) -> a -> a
maybeApply (Just f) x = f x
maybeApply Nothing  x = x



-- NOTE: needs multi-parameter type classes, which is not Haskell 98, but should
-- be in the next spec apparently
class Stack s where
    peek :: s a -> a
    pop  :: s a -> s a
    push :: a -> s a -> s a
    modtop :: (a -> a) -> s a -> s a


instance Stack [] where
    peek = head
    pop  = tail
    push x s = (x:s)
    modtop = modifyListHead


-- | integer division with rounding *up*
infixl 7 `divUp`
divUp :: (Integral a) => a -> a -> a
divUp x y = let (d,m) = divMod x y
            in  d + (signum m)



-- | apply an operation successively down a list, until it does not fail, then
--   return that result.
--   (which is what msum does, provided that "fail" is mzero)
findInStack :: (MonadPlus m) => (a -> m b) -> [a] -> m b
findInStack f stack = msum (map f stack)



-- REMINDER: we have in Control.Monad.Error:
-- instance (Error e) => MonadPlus (Either e) where ...


-- | lift a function which injects into a Monad into one which has input and result in the
-- Monad. Have used it on mapM.
liftArgM :: (Monad m) => (a -> m b) -> (m a -> m b)
liftArgM f m_x = do x <- m_x
                    f x


-- | lift a function into a monad. The difference from 'liftM' is that the result of myLiftM
-- takes a value not in the monad, so it's useful on the RHS of >>= (among others)
myLiftM :: (Monad m) => (a -> b) -> (a -> m b)
myLiftM f x = return (f x)


-- | like 'scanl', but for monadic functions
scanM            :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m [a]
scanM f a []     =  return [a]
scanM f a (x:xs) =  do y    <- f a x
                       rest <- scanM f y xs
                       return (a:rest)



-- | force a Map.lookup in the Maybe monad
maybeLookup :: Ord k => k -> [Map.Map k a] -> Maybe a
maybeLookup key maps = findInStack (Map.lookup key) maps



-- | a version of Map.adjust which tells if it changed anything or not (ie.
-- whether the key was found)
maybeMapAdjust :: (MonadPlus m, Ord k) => (a -> a) -> k -> Map.Map k a -> m (Map.Map k a)
maybeMapAdjust f k m    = if Map.member k m
                          then return $ Map.adjust f k m
                          else mzero

-- | fromJust, with an error message in case of Nothing
fromJustMsg :: String -> Maybe a -> a
fromJustMsg msg (Just x) = x
fromJustMsg msg Nothing  = error $ "fromJust Nothing: " ++ msg



-- | this version of map is mostly the identity, but the first time that f returns
-- non-Nothing we'll actually use that result.
-- TODO: there must be a better way to do this with the Maybe monad
mapOne :: (a -> Maybe a) -> [a] -> [a]
mapOne f (x:xs) = case f x of
                           Nothing  -> x : mapOne f xs
                           Just x'  -> x': xs
mapOne _ [] = []


modifyListHead _ [] = error "modifyListHead on empty list!"
modifyListHead f (x:xs) = (f x : xs)

mapOnTail f []          = error "mapOnTail on empty list!"
mapOnTail f (x:xs)      = x : (map f xs)

interleave [] []            = []
interleave (x:xs) (y:ys)    = (x:y:interleave xs ys)



{-
-- shortcut isn't quite working for now...

-- newtype MaybeT m a = ET (ErrorT () m a)
type MaybeT = ErrorT ()


runMaybeT :: (Monad m) => MaybeT m a -> m (Maybe a)
runMaybeT c = do let (ET c') = c
                 v <- runErrorT c'
                 return (either (const Nothing) Just v)
runMaybeT c = do v <- runErrorT c
                 return (either (const Nothing) Just v)
-}

-- not quite sure about this, but can be useful
instance MonadError () (Maybe) where
    throwError _            = Nothing
    Nothing `catchError` h  = h ()
    Just x  `catchError` _  = Just x


newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance (Monad m) => Monad (MaybeT m) where
    return a    = MaybeT $ return (Just a)
    m >>= k     = MaybeT $ do a <- runMaybeT m
                              case a of
                                     Nothing -> return Nothing
                                     Just x  -> runMaybeT (k x)
    fail _      = MaybeT $ return Nothing


instance (Monad m) => MonadPlus (MaybeT m) where
    mzero       = MaybeT $ return Nothing
    m `mplus` n = MaybeT $ do a <- runMaybeT m
                              case a of
                                     Nothing    -> runMaybeT n
                                     Just x     -> return (Just x)

instance MonadTrans MaybeT where
    lift m      = MaybeT $ do a <- m
                              return $ Just a



{-
class (Show a) => Appendable a where
    (<<)   :: String -> a -> String
    s << x = s ++ (show x)

instance Appendable String where
    (<<) = (++)

instance (Show a) => Appendable a where
    s << x = s ++ (show x)
-}



-- | if a list has predicate results: [F, F, F, T, T, ..., T, F, F,...],
-- return the middle part that is True
takeMiddle :: (a -> Bool) -> [a] -> [a]
takeMiddle p = (takeWhile p) . (dropWhile (not . p))

log2 :: (Floating a) => a -> a
log2 = logBase 2


-- | wrap a numeric function so it takes and returns an 'Integral' type
-- by taking a 'ceiling'
integerize :: (Num a, RealFrac b, Integral a_int, Integral b_int) =>
              (a -> b) -> (a_int -> b_int)
integerize f = ceiling . f . fromIntegral

-- | same for function on 2 params
integerize2 f x y = ceiling $ f (fromIntegral x) (fromIntegral y) 


ilog2 x 
    | x > 0     = integerize log2 x
    | otherwise = 0

isqrt = integerize sqrt


-- | given 2 functions 'f' and 'g', produce one function 'h' s.t. h x = (f x, g x)
pair2 :: (a -> b) -> (a -> c) -> (a -> (b,c))
pair2 f g = \x -> (f x, g x)

pair3 :: (a -> b) -> (a -> c) -> (a -> d) -> (a -> (b,c,d))
pair3 f g h = \x -> (f x, g x, h x)


-- | map a function over a pair, returning the resulting pair
mapTuple2 :: (a -> b) -> (a,a) -> (b,b)
mapTuple2 f (x,y) = (f x, f y)

-- | map a monadic function over a pair, returning the result pair in the Monad
mapTupleM2 :: (Monad m) => (a -> m b) -> (a,a) -> m (b,b)
mapTupleM2 f (x,y) = do x' <- f x
                        y' <- f y
                        return (x', y')

-- | turn a pair of functions to a function on pairs
proj_tup2 :: ((a -> b), (c -> d)) -> (a,c) -> (b,d)
proj_tup2 (f,g) (x,y) = (f x, g y)


-- | project a function onto first of a pair
projFst :: (a -> b) -> (a, c) -> (b, c)
projFst f (x,y) = (f x, y  )
-- | project a function onto second of a pair
projSnd :: (a -> b) -> (c, a) -> (c, b)
projSnd f (x,y) = (x  , f y)



factorial :: Integral a => a -> a
factorial n = product [1..n]

-- | binomial operation.
-- using 'quot' operator to make sure we get an integral result (which
-- should always be the case anyhow)
choose :: (Integral a) => a -> a -> a
choose n k = factorial n `quot` (factorial k * factorial (n-k))

fib             = 1 : 1 : [ a+b | (a,b) <- zip fib (tail fib) ]

-- | simple encapsulation for a summation with limit 'a' to 'b' and
-- summed function 'f'
sumOp :: (Enum ctr, Num b) => (ctr -> b) -> ctr -> ctr -> b
sumOp f a b = sum . map f $ [a..b]


runSumFrom0 :: (Num a) => [a] -> [a]
runSumFrom0 = init . scanl (+) 0


-- | update a range (offset and length) of a list.
-- more precisely, replace the range (offset,len) with news (regardless of its length, ie
-- result length may differ from input length.)
splice :: (Int, Int) -> [a] -> [a] -> [a]
splice (offset,len) news l = (take offset l) ++
                             news ++
                             (drop (offset + len) l)

-- | Splice in new values into a list (using a modifier function) where some predicate
-- succeeds; if predicate fails, apply another function.
spliceInIf :: (a -> Bool)       -- ^ Do we splice in at this element?
           -> (a -> c -> b)     -- ^ Function used to update the value with a new one
           -> (a -> b)          -- ^ Function to apply if predicate false
           -> [c]               -- ^ The new values, should be as many as predicate
                                -- returns True on
           -> [a]               -- ^ The list
           -> [b]

spliceInIf p f g ns (x:xs)
    | p x       = case ns of (n:ns')    -> f x n : recurse ns' xs
                             []         -> error "spliceInIf: ran out of replacement values"
    | otherwise = g x : recurse ns xs
    where recurse = spliceInIf p f g
spliceInIf _ _ _ ns []  = case ns of [] -> []
                                     _  -> error "WARNING: spliceInIf had replacements left over at the end" []


-- | Update some of a list's elements, which pass a predicate, with a list of replacement
-- values.
updateSome :: (a -> Bool)       -- ^ Which elements to replace?
           -> [a]               -- ^ The replacement values
           -> [a]               -- ^ The list
           -> [a]
updateSome p = spliceInIf p (\old new -> new) id




-- | map a function onto a sublist of a list, and another function on the rest.
mapSplice :: (a -> b)           -- ^ map onto the sublist
          -> (a -> b)           -- ^ map onto the rest
          -> (Int,Int)          -- ^ the sublist location; offset and length.
          -> [a]                -- ^ the list
          -> [b]
mapSplice f g (offset,len) l = let before = take offset l
                                   during = take len $ drop offset l
                                   after  = drop (offset + len) l
                               in  map g before ++ map f during ++ map g after

                               



sublist off len = take len . drop off


-- | modify a list at the first element where a Maybe function succeeds; leave all
-- other elements the same.
-- returns Nothing if f fails everywhere.
modifyFirst :: (a -> Maybe a) -> [a] -> Maybe [a]
modifyFirst f xs =  let xsys            = zip xs (map f xs)
                        (part1,part2)   = span (isNothing . snd) xsys
                    in  if null part2
                        then Nothing
                        else Just $ map fst part1 ++
                                    [fromJust . snd . head $ part2] ++
                                    map fst (tail part2)

-- | Split a list at some separator element. like a parametrized 'lines'.
split :: (Eq a) => a -> [a] -> [[a]]
split sep s = let (l, s') = break (== sep) s
              in  l : case s' of
                        []      -> []
                        (_:s'') -> split sep s''

-- | join a list of lists with some separator
joinLists :: [a] -> [[a]] -> [a]
joinLists sep xs = concat $ intersperse sep xs




--
-- some bit manipulation routines.
--

-- | Get the value of a range of bits (inclusive) in an instance of Bits.
getBits :: Bits.Bits i => i -> (Int,Int) -> i
i `getBits` (a,b) = ( i .&. (bitMask (a,b)) ) `Bits.shiftR` a

-- | Get a bitmask for a range of bits (inclusive)
bitMask :: Bits.Bits i => (Int,Int) -> i
bitMask (i,j)     = ( Bits.complement ((Bits.complement 0) `Bits.shiftL` (j-i+1)) )  `Bits.shiftL` i


-- | make a list of 2 elements from a tuple.
tuple2list2 :: (a,a) -> [a]
tuple2list2 (x,y) = [x,y]


-- | have a list of functions on 2 params, run them over a fixed pair of
-- parameters
mapInputs2 :: a -> b -> [a -> b -> c] -> [c]
mapInputs2 x y fs = map (\f -> f x y) fs


-- | Monad version of Prelude.maybe
maybeM :: (Monad m) => m b -> (a -> m b) -> Maybe a -> m b
maybeM def f x = case x of Just x' -> f x'
                           Nothing -> def


data OneOf3 a b c = Num1 a | Num2 b | Num3 c

oneof3 f1 f2 f3 x = case x of Num1 x -> f1 x
                              Num2 x -> f2 x
                              Num3 x -> f3 x



-- | take a function f and 3 params, and apply f on the params
apply3 :: (a -> b -> c -> d) -> a -> b -> c -> d
apply3 = ((($).).)


-- a version of scanl that throws away its last element
--scanl

-- helpers for 3-tuples!
tup3_get1 (x,_,_) = x
tup3_get2 (_,x,_) = x
tup3_get3 (_,_,x) = x

tup3_proj1 f (x,y,z) = (f x , y   , z  )
tup3_proj2 f (x,y,z) = (x   , f y , z  )
tup3_proj3 f (x,y,z) = (x   , y   , f z)


-- and for 4-tuples!
tup4_get1 (x1,x2,x3,x4) = x1
tup4_get2 (x1,x2,x3,x4) = x2
tup4_get3 (x1,x2,x3,x4) = x3
tup4_get4 (x1,x2,x3,x4) = x4

tup5_get1 (x1,x2,x3,x4,x5)  = x1
tup5_get2 (x1,x2,x3,x4,x5)  = x2
tup5_get3 (x1,x2,x3,x4,x5)  = x3
tup5_get4 (x1,x2,x3,x4,x5)  = x4
tup5_get5 (x1,x2,x3,x4,x5)  = x5

tup4_proj_1 f (x1,x2,x3,x4) = (f x1, x2  , x3  , x4  )
tup4_proj_2 f (x1,x2,x3,x4) = (x1  , f x2, x3  , x4  )
tup4_proj_3 f (x1,x2,x3,x4) = (x1  , x2  , f x3, x4  )
tup4_proj_4 f (x1,x2,x3,x4) = (x1  , x2  , x3  , f x4)

-- | Take an assoc list of (key, values), and expand it to a (longer) list of
-- (key,value), where each 'values' has been expanded to one value per element.
expand :: [(a, [b])] -> [(a,b)]
-- use foldr to avoid quadratic blowup with (++)
expand xs = foldr f [] xs
    where f (a,bs) dones = [(a,b) | b <- bs] ++ dones

-- | class of types which are convertable to a 'Text.PrettyPrint.Doc'
-- for pretty-printing.
class DocAble a where
    doc     :: a -> PP.Doc

bool2int :: (Integral b) => Bool -> b
bool2int = fromIntegral . fromEnum

int2bool :: (Integral a) => a -> Bool
int2bool = toEnum . fromIntegral
