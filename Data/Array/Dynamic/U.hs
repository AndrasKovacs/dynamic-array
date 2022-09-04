{-# language
   RankNTypes, LambdaCase, KindSignatures, RoleAnnotations, BangPatterns,
   GeneralizedNewtypeDeriving, UndecidableInstances #-}

module Data.Array.Dynamic.U  (
    empty
  , Array(..)
  , capacity
  , clear
  , push
  , pop
  , Data.Array.Dynamic.U.read
  , Data.Array.Dynamic.U.show
  , size
  , unsafeRead
  , unsafeWrite
  , write
  , unsafeLast
  , Data.Array.Dynamic.U.last
  , isEmpty
  , foldl'
  , foldlIx'
  , foldr'
  , foldrIx'
  , fromList
  , freeze
  , Data.Array.Dynamic.U.any
  , Data.Array.Dynamic.U.all
  , allIx
  , anyIx
  , for
  , forIx
  ) where

import Data.Unlifted
import Data.Array.UndefElem
import Data.Kind

import qualified Data.Ref.UU   as RUU
import qualified Data.Ref.F    as RF
import qualified Data.Array.UM as UM
import qualified Data.Array.UI as UI

type role Array representational
newtype Array (a :: Type) = Array (RUU.Ref (RF.Ref Int) (UM.Array a))
  deriving Unlifted

defaultCapacity :: Int
defaultCapacity = 5
{-# inline defaultCapacity #-}

empty :: forall a. Unlifted a => IO (Array a)
empty = do
  sizeRef <- RF.new 0
  arrRef  <- UM.new defaultCapacity defaultElem
  Array <$> RUU.new sizeRef arrRef
{-# inline empty #-}

capacity :: Array a -> IO Int
capacity (Array r) = do
  elems <- RUU.readSnd r
  pure $! UM.size elems
{-# inline capacity #-}

unsafeRead :: Unlifted a => Array a -> Int -> IO a
unsafeRead (Array r) i = do
  elems <- RUU.readSnd r
  UM.read elems i
{-# inline unsafeRead #-}

read :: Unlifted a => Array a -> Int -> IO a
read (Array r) i = do
  elems <- RUU.readSnd r
  sizeRef <- RUU.readFst r
  size <- RF.read sizeRef
  if 0 <= i && i < size then
    UM.read elems i
  else
    error "Data.Array.Dynamic.U.read: out of bounds"
{-# inline read #-}

unsafeWrite :: Unlifted a => Array a -> Int -> a -> IO ()
unsafeWrite (Array r) i a = do
  elems <- RUU.readSnd r
  UM.write elems i a
{-# inline unsafeWrite #-}

write :: Unlifted a => Array a -> Int -> a -> IO ()
write (Array r) i ~a = do
  s <- RF.read =<< RUU.readFst r
  if 0 <= i && i < s
    then unsafeWrite (Array r) i a
    else error "Data.Array.Dynamic.U.write: out of bounds"
{-# inline write #-}

push :: Unlifted a => Array a -> a -> IO ()
push (Array r) ~a = do
  sizeRef <- RUU.readFst r
  elems   <- RUU.readSnd r
  size    <- RF.read sizeRef
  let cap = UM.size elems
  RF.write sizeRef (size + 1)
  if (size == cap) then do
    let cap' = 2 * cap
    elems' <- UM.new cap' undefElem
    UM.copySlice elems 0 elems' 0 size
    UM.write elems' size a
    RUU.writeSnd r elems'
  else do
    UM.write elems size a
{-# inline push #-}

pop :: Unlifted a => Array a -> IO (Maybe a)
pop (Array r) = do
  sizeRef <- RUU.readFst r
  size    <- RF.read sizeRef
  case size of
    0    -> pure Nothing
    size -> do
      elems <- RUU.readSnd r
      let size' = size - 1
      a <- UM.read elems size'
      UM.write elems size' undefElem
      RF.write sizeRef size'
      pure $! Just a
{-# inline pop #-}

fromList :: Unlifted a => [a] -> IO (Array a)
fromList as = do
  let size = length as
      cap  = size + defaultCapacity
  sizeRef <- RF.new size
  arrRef  <- UM.new cap defaultElem
  arr     <- RUU.new sizeRef arrRef
  let go !i []     = pure ()
      go i  (a:as) = UM.write arrRef i a >> go (i + 1) as
  go 0 as
  pure (Array arr)

freeze :: Unlifted a => Array a -> IO (UI.Array a)
freeze (Array arr) = do
  sizeRef <- RUU.readFst arr
  elems   <- RUU.readSnd arr
  size    <- RF.read sizeRef
  tgt     <- UM.new size defaultElem
  UM.copySlice elems 0 tgt 0 size
  UM.unsafeFreeze tgt

clear :: Unlifted a => Array a -> IO ()
clear (Array r) = do
  (`RF.write` 0) =<< RUU.readFst r
  RUU.writeSnd r =<< UM.new defaultCapacity undefElem
{-# inline clear #-}

size :: Array a -> IO Int
size (Array r) = RF.read =<< RUU.readFst r
{-# inline size #-}

unsafeLast :: Unlifted a => Array a -> IO a
unsafeLast arr = do
  i <- size arr
  Data.Array.Dynamic.U.unsafeRead arr (i - 1)
{-# inline unsafeLast #-}

isEmpty :: Array a -> IO Bool
isEmpty arr = (==0) <$> size arr
{-# inline isEmpty #-}

last :: Unlifted a => Array a -> IO a
last arr = do
  i <- size arr
  isEmpty arr >>= \case
    True -> error "Data.Array.Dynamic.last: empty array"
    _    -> unsafeRead arr (i - 1)
{-# inline last #-}

show :: (Show a, Unlifted a) => Array a -> IO String
show (Array r) = do
  elems  <- RUU.readSnd r
  size <- RF.read =<< RUU.readFst r
  elems' <- UM.freezeSlice elems 0 size
  pure (Prelude.show elems')
{-# inlinable show #-}

foldl' :: Unlifted a => (b -> a -> b) -> b -> Array a -> IO b
foldl' f b = \arr -> do
  s <- size arr
  let go i b | i == s    = pure b
             | otherwise = do
                 a <- unsafeRead arr i
                 go (i + 1) $! f b a
  go 0 b
{-# inline foldl' #-}

foldlIx' :: Unlifted a => (Int -> b -> a -> b) -> b -> Array a -> IO b
foldlIx' f b = \arr -> do
  s <- size arr
  let go i b | i == s    = pure b
             | otherwise = do
                 a <- unsafeRead arr i
                 go (i + 1) $! f i b a
  go 0 b
{-# inline foldlIx' #-}

foldr' :: Unlifted a => (a -> b -> b) -> b -> Array a -> IO b
foldr' f b = \arr -> do
  s <- size arr
  let go i b | i == (-1) = pure b
             | otherwise = do
                 a <- unsafeRead arr i
                 go (i - 1) $! f a b
  go (s - 1) b
{-# inline foldr' #-}

foldrIx' :: Unlifted a => (Int -> a -> b -> b) -> b -> Array a -> IO b
foldrIx' f b = \arr -> do
  s <- size arr
  let go i b | i == (-1) = pure b
             | otherwise = do
                 a <- unsafeRead arr i
                 go (i - 1) $! f i a b
  go (s - 1) b
{-# inline foldrIx' #-}

-- TODO: any + all with lazy fold
any :: Unlifted a => (a -> Bool) -> Array a -> IO Bool
any f = foldl' (\b a -> f a || b) False
{-# inline any #-}

all :: Unlifted a => (a -> Bool) -> Array a -> IO Bool
all f = foldl' (\b a -> f a && b) True
{-# inline all #-}

anyIx :: Unlifted a => (Int -> a -> Bool) -> Array a -> IO Bool
anyIx f = foldlIx' (\i b a -> f i a || b) False
{-# inline anyIx #-}

allIx :: Unlifted a => (Int -> a -> Bool) -> Array a -> IO Bool
allIx f = foldlIx' (\i b a -> f i a && b) True
{-# inline allIx #-}

for :: Unlifted a => Array a -> (a -> IO b) -> IO ()
for arr f = go (0 :: Int) where
  go i = do
    s <- size arr
    if i == s then pure () else do {x <- unsafeRead arr i; f x; go (i + 1)}
{-# inline for #-}

forIx :: Unlifted a => Array a -> (Int -> a -> IO b) -> IO ()
forIx arr f = go (0 :: Int) where
  go i = do
    s <- size arr
    if i == s then pure () else do {x <- unsafeRead arr i; f i x; go (i + 1)}
{-# inline forIx #-}
