{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}

module CryptoDepth.Exchange where

import CryptoDepth.Internal.DPrelude hiding (head)
import CryptoDepth.Internal.Util
import CryptoDepth.Types
import OrderBook.Types
import qualified OrderBook.Matching as Match
import qualified Money


marketSell :: KnownSymbol quote 
           => Money.Dense base 
           -> BuySide venue base quote 
           -> Money.Dense quote
marketSell qty = undefined  -- TODO: market order by base quantity
slippageSell slip = Match.resQuoteQty . flip Match.slippageSell slip


marketBuy :: KnownSymbol base 
          => Money.Dense quote 
          -> SellSide venue base quote 
          -> Money.Dense base
marketBuy qty = Match.resBaseQty . flip Match.marketBuy qty
slippageBuy slip = Match.resBaseQty . flip Match.slippageBuy slip


-- | Exchange by slippage through multiple orderbook sides
slippageExchangeMulti
    :: Rational                         -- ^ Slippage, in percent
    -> [SomeSide]                       -- ^ Orderbook sides to go through
    -> Either String Money.SomeDense    -- ^ Resulting amount (currency unit: destination currency of last SomeSide)
slippageExchangeMulti _ []                              = Left "Empty orderbook side list"
slippageExchangeMulti slip (SomeSide fstSideE:fstSides) =
    either (exchange fstSides . Money.toSomeDense . slippageSell slip) 
           (exchange fstSides . Money.toSomeDense . slippageBuy slip) 
           fstSideE 

exchange :: [SomeSide]
         -> Money.SomeDense
         -> Either String Money.SomeDense
exchange [] dense = Right dense
exchange (SomeSide sideE:sides) dense = 
    mkResult sideE dense >>= exchange sides

mkResult :: forall venue base quote.
            (KnownSymbol base, KnownSymbol quote)
            => Either (BuySide venue base quote) (SellSide venue base quote) 
            -> Money.SomeDense
            -> Either String Money.SomeDense
mkResult sideE sDense = Money.withSomeDense sDense $ \(dense :: Money.Dense src) ->
    let (pQuote,pBase) = (Proxy :: Proxy quote, Proxy :: Proxy base) 
        pSrc = Proxy :: Proxy src
    in case sideE of
        Left bs -> 
            case sameSymbol pSrc pBase of
                Nothing   -> Left  $ printf "incompatible src '%s' and BuySide base '%s" (symbolVal pSrc) (symbolVal pBase)
                Just Refl -> Right . Money.toSomeDense $ marketSell dense bs
        Right ss -> 
            case sameSymbol pSrc pQuote of
                Nothing   -> Left  $ printf "incompatible src '%s' and SellSide quote '%s" (symbolVal pSrc) (symbolVal pQuote)
                Just Refl -> Right . Money.toSomeDense $ marketBuy dense ss
