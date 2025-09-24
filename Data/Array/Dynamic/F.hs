
{-# language
   RankNTypes, LambdaCase, KindSignatures, RoleAnnotations, BangPatterns,
   GeneralizedNewtypeDeriving, UndecidableInstances #-}

{-|
Arrays of unboxed (flat) elements.
-}

module Data.Array.Dynamic.F  (
    empty
  , Array(..)
  , capacity
  , clear
  , push
  , pop
  , Data.Array.Dynamic.F.read
  , Data.Array.Dynamic.F.show
  , size
  , unsafeRead
  , unsafeWrite
  , write
  , unsafeLast
  , Data.Array.Dynamic.F.last
  , isEmpty
  , Data.Array.Dynamic.F.foldl'
  , foldlIx'
  , foldr'
  , foldrIx'
  , fromList
  , freeze
  , for
  , forIx
  ) where

import Data.Unlifted
import Data.Internal.Errors
import Data.Kind

import Data.Flat (Flat)

import qualified Data.Ref.UU   as RUU
import qualified Data.Ref.F    as RF
import qualified Data.Array.FM as FM
import qualified Data.Array.FI as FI

type role Array representational
newtype Array (a :: Type) = Array (RUU.Ref (RF.Ref Int) (FM.Array a))
  deriving Unlifted

defaultCapacity :: Int
defaultCapacity = 5
{-# inline defaultCapacity #-}

empty :: forall a. Flat a => IO (Array a)
empty = do
  sizeRef <- RF.new 0
  arrRef  <- FM.new defaultCapacity
  Array <$> RUU.new sizeRef arrRef
{-# inline empty #-}

capacity :: Flat a => Array a -> IO Int
capacity (Array r) = do
  elems <- RUU.readSnd r
  pure $! FM.size elems
{-# inline capacity #-}

unsafeRead :: Flat a => Array a -> Int -> IO a
unsafeRead (Array r) i = do
  elems <- RUU.readSnd r
  FM.read elems i
{-# inline unsafeRead #-}

read :: Flat a => Array a -> Int -> IO a
read (Array r) i = do
  elems <- RUU.readSnd r
  sizeRef <- RUU.readFst r
  size <- RF.read sizeRef
  if 0 <= i && i < size then
    FM.read elems i
  else
    error "Data.Array.Dynamic.U.read: out of bounds"
{-# inline read #-}

unsafeWrite :: Flat a => Array a -> Int -> a -> IO ()
unsafeWrite (Array r) i a = do
  elems <- RUU.readSnd r
  FM.write elems i a
{-# inline unsafeWrite #-}

write :: Flat a => Array a -> Int -> a -> IO ()
write (Array r) i ~a = do
  s <- RF.read =<< RUU.readFst r
  if 0 <= i && i < s
    then unsafeWrite (Array r) i a
    else error "Data.Array.Dynamic.U.write: out of bounds"
{-# inline write #-}

push :: Flat a => Array a -> a -> IO ()
push (Array r) ~a = do
  sizeRef <- RUU.readFst r
  elems   <- RUU.readSnd r
  size    <- RF.read sizeRef
  let cap = FM.size elems
  RF.write sizeRef (size + 1)
  if (size == cap) then do
    let cap' = 2 * cap
    elems' <- FM.new cap'
    FM.copySlice elems 0 elems' 0 size
    FM.write elems' size a
    RUU.writeSnd r elems'
  else do
    FM.write elems size a
{-# inline push #-}

pop :: Flat a => Array a -> IO (Maybe a)
pop (Array r) = do
  sizeRef <- RUU.readFst r
  size    <- RF.read sizeRef
  case size of
    0    -> pure Nothing
    size -> do
      elems <- RUU.readSnd r
      let size' = size - 1
      a <- FM.read elems size'
      FM.write elems size' undefElem
      RF.write sizeRef size'
      pure $! Just a
{-# inline pop #-}

fromList :: Flat a => [a] -> IO (Array a)
fromList as = do
  let size = length as
      cap  = size + defaultCapacity
  sizeRef <- RF.new size
  arrRef  <- FM.new cap
  arr     <- RUU.new sizeRef arrRef
  let go !i []     = pure ()
      go i  (a:as) = FM.write arrRef i a >> go (i + 1) as
  go 0 as
  pure (Array arr)

freeze :: Flat a => Array a -> IO (FI.Array a)
freeze (Array arr) = do
  sizeRef <- RUU.readFst arr
  elems   <- RUU.readSnd arr
  size    <- RF.read sizeRef
  tgt     <- FM.new size
  FM.copySlice elems 0 tgt 0 size
  FM.unsafeFreeze tgt

clear :: Flat a => Array a -> IO ()
clear (Array r) = do
  (`RF.write` 0) =<< RUU.readFst r
  RUU.writeSnd r =<< FM.new defaultCapacity
{-# inline clear #-}

size :: Array a -> IO Int
size (Array r) = RF.read =<< RUU.readFst r
{-# inline size #-}

unsafeLast :: Flat a => Array a -> IO a
unsafeLast arr = do
  i <- size arr
  Data.Array.Dynamic.F.unsafeRead arr (i - 1)
{-# inline unsafeLast #-}

isEmpty :: Array a -> IO Bool
isEmpty arr = (==0) <$> size arr
{-# inline isEmpty #-}

last :: Flat a => Array a -> IO a
last arr = do
  i <- size arr
  isEmpty arr >>= \case
    True -> error "Data.Array.Dynamic.U.last: empty array"
    _    -> unsafeRead arr (i - 1)
{-# inline last #-}

show :: (Show a, Flat a) => Array a -> IO String
show (Array r) = do
  elems  <- RUU.readSnd r
  size <- RF.read =<< RUU.readFst r
  elems' <- FM.freezeSlice elems 0 size
  pure (Prelude.show elems')
{-# inlinable show #-}

foldl' :: Flat a => (b -> a -> b) -> b -> Array a -> IO b
foldl' f b = \arr -> do
  s <- size arr
  let go i b | i == s    = pure b
             | otherwise = do
                 a <- unsafeRead arr i
                 go (i + 1) $! f b a
  go 0 b
{-# inline foldl' #-}

foldlIx' :: Flat a => (Int -> b -> a -> b) -> b -> Array a -> IO b
foldlIx' f b = \arr -> do
  s <- size arr
  let go i b | i == s    = pure b
             | otherwise = do
                 a <- unsafeRead arr i
                 go (i + 1) $! f i b a
  go 0 b
{-# inline foldlIx' #-}

foldr' :: Flat a => (a -> b -> b) -> b -> Array a -> IO b
foldr' f b = \arr -> do
  s <- size arr
  let go i b | i == (-1) = pure b
             | otherwise = do
                 a <- unsafeRead arr i
                 go (i - 1) $! f a b
  go (s - 1) b
{-# inline foldr' #-}

foldrIx' :: Flat a => (Int -> a -> b -> b) -> b -> Array a -> IO b
foldrIx' f b = \arr -> do
  s <- size arr
  let go i b | i == (-1) = pure b
             | otherwise = do
                 a <- unsafeRead arr i
                 go (i - 1) $! f i a b
  go (s - 1) b
{-# inline foldrIx' #-}

for :: Flat a => Array a -> (a -> IO b) -> IO ()
for arr f = go (0 :: Int) where
  go i = do
    s <- size arr
    if i == s then pure () else do {x <- unsafeRead arr i; f x; go (i + 1)}
{-# inline for #-}

forIx :: Flat a => Array a -> (Int -> a -> IO b) -> IO ()
forIx arr f = go (0 :: Int) where
  go i = do
    s <- size arr
    if i == s then pure () else do {x <- unsafeRead arr i; f i x; go (i + 1)}
{-# inline forIx #-}
