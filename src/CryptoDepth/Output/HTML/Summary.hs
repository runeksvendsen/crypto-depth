{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ParallelListComp #-}
module CryptoDepth.Output.HTML.Summary
( summary
)
where

import CryptoDepth.Internal.DPrelude
import CryptoDepth.Output.HTML.Common
import qualified CryptoDepth as CD
import CryptoDepth                (LiquidPaths(..))
import qualified Money
import Lucid
import System.IO (stdout, hSetEncoding, utf8)
import Data.Text.Lazy.IO as L


summary
  :: forall numeraire.
     KnownSymbol numeraire
  => Rational
  -> [(CD.Sym, Money.Dense numeraire, Money.Dense numeraire)]
  -> Html ()
summary slipPct symVolumes =
  htmlDoc titleStr (summaryTable symVolumes)
  where
  titleStr = printf "Sell/buy volume at %s%% slippage" (showRat slipPct) :: String

summaryTable
    :: forall numeraire.
       KnownSymbol numeraire
    => [(CD.Sym, Money.Dense numeraire, Money.Dense numeraire)]
    -> Html ()
summaryTable symVolumes = do
  let numeraire :: String
      numeraire = printf "(%s)" (symbolVal (Proxy :: Proxy numeraire))
  table_ [ class_ "table table-striped" ] $ do
    thead_ $ do
      tr_ $ do
        th_ [ scope_ "col" ] $ "#"
        th_ [ scope_ "col" ] $ "Symbol"
        th_ [ scope_ "col" ] $ toHtml ("Sell volume " <> numeraire)
        th_ [ scope_ "col" ] $ toHtml ("Buy volume " <> numeraire)
    tbody_ $
      forM_ (zip [1..] symVolumes) (uncurry summaryRow)

summaryRow
  :: KnownSymbol numeraire
  => Word
  -> (CD.Sym, Money.Dense numeraire, Money.Dense numeraire)
  -> Html ()
summaryRow pos (sym, buyVol, sellVol) =
  tr_ $ do
    th_ [ scope_ "row" ] $ toHtml (show pos)
    td_ $ a_ [ href_ (toS sym <> ".html") ] $ (toHtml sym)
    td_ (toHtml $ showDenseAmount sellVol)
    td_ (toHtml $ showDenseAmount buyVol)
