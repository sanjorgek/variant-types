{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_HADDOCK show-extensions #-}
{-|
Module      : Types
Description : Variant Types
Copyright   : (c) Jorge Santiago Alvarez Cuadros, 2017
License     : GPL-3
Maintainer  : sanjorgek@ciencias.unam.mx
Stability   : experimental
Portability : portable

Variant types,
-}
module Test.QuickCheck.Variant.Types where

import qualified Data.Map                    as Map
import qualified Data.Set                    as Set
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.Hspec.Variant
import           Test.QuickCheck
import           Test.QuickCheck.Variant

{-|
Auxiliar generator
-}
selectOneOf :: [a] -> Gen a
selectOneOf = oneof . map return

{-|
Variant Void
-}
instance Variant () where
  invalid = return ()
  valid = return ()

{-|
Variant maybe

Nothing can't be invalid.
-}
instance (Variant a) => Variant (Maybe a) where
  valid = do
    x <- valid
    selectOneOf [Just x, Nothing]
  invalid = do
    y <- invalid
    return $ Just y

{-|
Varaint array/list

Empty can't be invalid
-}
instance (Variant a) => Variant [a] where
  valid = do
    x <- valid
    xs <- valid
    selectOneOf [x:xs, []]
  invalid = do
    x <- invalid
    xs <- invalid
    y <- valid
    ys <- valid
    selectOneOf [[x], x:xs, x:ys, y:xs]

{-|
Variant either
-}
instance (Variant a, Variant b) => Variant (Either a b) where
  invalid = do
    x <- invalid
    y <- invalid
    selectOneOf [Left x, Right y]
  valid = do
    x <- valid
    y <- valid
    selectOneOf [Left x, Right y]

{-|
Variant 2-tuple

Cambine invalid data.
-}
instance (Variant a, Variant b) => Variant (a, b) where
  invalid = do
    x <- invalid
    y <- invalid
    z <- valid
    w <- valid
    selectOneOf [(x,y), (x,z), (w,y)]
  valid = do
    x <- valid
    y <- valid
    return (x, y)

{-|
Variant 3-tuple

Cambine invalid data.
-}
instance (Variant a, Variant b, Variant c) => Variant (a, b, c) where
  invalid = do
    a <- invalid
    b <- invalid
    c <- invalid
    a' <- valid
    b' <- valid
    c' <- valid
    selectOneOf [(a,b,c),(a',b,c),(a,b',c),(a,b,c'),(a',b',c),(a,b',c'),(a',b,c')]
  valid = do
    a <- valid
    b <- valid
    c <- valid
    return (a,b,c)

{-|
Variant set

Empty set can't be invalid
-}
instance (Ord a, Variant a) => Variant (Set.Set a) where
  invalid = do
    xs <- invalid
    return $ Set.fromList xs
  valid = do
    xs <- valid
    selectOneOf [Set.empty, Set.fromList xs]

{-|
Variant map

Empty map can't be invalid
-}
instance (Ord k, Variant k, Variant a) => Variant (Map.Map k a) where
  invalid = do
    xs <- invalid
    return $ Map.fromList xs
  valid = do
    xs <- valid
    selectOneOf [Map.empty, Map.fromList xs]
