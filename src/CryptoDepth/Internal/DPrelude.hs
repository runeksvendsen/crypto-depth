{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module CryptoDepth.Internal.DPrelude
( module Protolude
, module TypeLits
, module Prelude
, module EitherT
, sameSym
, trace
, Vector
, Map
, fmapL
, printf
, fail
, show'
, (NE.<|)
, throwBug
)
where

import Protolude hiding (trace, Show, show, Map)
import Prelude (String, Show, show, id, mod, lookup, error)
import Debug.Trace (trace)
import GHC.TypeLits as TypeLits ( Symbol, KnownSymbol, SomeSymbol(..)
                                , sameSymbol, symbolVal, someSymbolVal
                                )
import Control.Monad.Trans.Except as EitherT
import Control.Monad.Fail
import           Data.Vector  (Vector)
import Text.Printf
import Data.EitherR (fmapL)
import qualified Data.HashMap.Strict as Map
import qualified Data.List.NonEmpty as NE

type Map = Map.HashMap

show' :: Show a => a -> Text
show' = toS . show

sameSym :: (KnownSymbol a, KnownSymbol b) => Proxy a -> Proxy b -> Bool
sameSym a b = isJust (sameSymbol a b)

instance KnownSymbol sym => StringConv (Proxy sym) Text where
    strConv _ _ = toS $ symbolVal (Proxy :: Proxy sym)

throwBug :: Either String a -> a
throwBug = either (\str -> error $ "BUG: " ++ str) id
