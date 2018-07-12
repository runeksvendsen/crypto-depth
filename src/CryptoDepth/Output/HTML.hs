{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ParallelListComp #-}
module CryptoDepth.Output.HTML
( htmlOut
)
where

import CryptoDepth.Internal.DPrelude
import qualified CryptoDepth.Paths as Paths

import qualified Money
import Lucid
import Lucid.Base (makeAttribute)
import System.IO (stdout, hSetEncoding, utf8)
import Data.Text.Lazy.IO as L


htmlOut :: KnownSymbol numeraire
     => [(Paths.Sym, Money.Dense numeraire, Money.Dense numeraire)]
     -> IO ()
htmlOut symVolumes = do
  hSetEncoding stdout utf8
  L.hPutStr stdout (renderText $ html symVolumes)

html :: forall numeraire.
        KnownSymbol numeraire
     => [(Paths.Sym, Money.Dense numeraire, Money.Dense numeraire)]
     -> Html ()
html symVolumes = do
  html_ $ do
    head_ $ do
      title_ "Crypto sell/buy volume at 5% slippage"
      boostrapCss
    body_ $
      table symVolumes

boostrapCss :: Html ()
boostrapCss =
    link_ [rel_ "stylesheet", href_ url, type_ "text/css", integrity_ hash, crossorigin_ "anonymous"]
  where
    url = "https://stackpath.bootstrapcdn.com/bootstrap/4.1.1/css/bootstrap.min.css"
    hash = "sha384-WskhaSGFgHYWDcbwN70/dfYBj47jz9qbsMId/iRN3ewGhXQFZCSftd1LZCfmhktB"
    crossorigin_ :: Text -> Attribute
    crossorigin_ = makeAttribute "crossorigin"
    -- <link rel="stylesheet" href={$url} integrity={$hash} crossorigin="anonymous">

table :: forall numeraire.
         KnownSymbol numeraire
      => [(Paths.Sym, Money.Dense numeraire, Money.Dense numeraire)]
      -> Html ()
table symVolumes = do
  let numeraire :: String
      numeraire = printf "(%s)" (symbolVal (Proxy :: Proxy numeraire))
  table_ [ class_ "table table-striped" ] $ do
    thead_ $ do
      tr_ $ do
        th_ [ scope_ "col" ] $ "#"
        -- th_ [ scope_ "col" ] $ "Cryptocurrency"
        th_ [ scope_ "col" ] $ "Symbol"
        th_ [ scope_ "col" ] $ toHtml ("Sell volume " <> numeraire)
        th_ [ scope_ "col" ] $ toHtml ("Buy volume " <> numeraire)
    tbody_ $
      forM_ (zip [1..] symVolumes) (uncurry mkRow)

mkRow :: KnownSymbol numeraire
      => Word
      -> (Paths.Sym, Money.Dense numeraire, Money.Dense numeraire)
      -> Html ()
mkRow pos (sym, buyVol, sellVol) =
  tr_ $ do
    th_ [ scope_ "row" ] $ toHtml (show pos)
    td_ (toHtml sym)
    td_ (toHtml $ showDenseAmount sellVol)
    td_ (toHtml $ showDenseAmount buyVol)




showSomeDense :: Money.SomeDense -> String
showSomeDense = (`Money.withSomeDense` showDense)

-- | Show amount WITH trailing symbol, e.g.: "1,234 USD"
showDense :: forall a. KnownSymbol a => Money.Dense a -> String
showDense =
    (++ " " ++ symbolVal (Proxy :: Proxy a)) . toS . showDenseAmount

-- | Show amount WITHOUT trailing symbol, e.g.: "1,234"
showDenseAmount :: KnownSymbol a => Money.Dense a -> Text
showDenseAmount =
    fromMaybe (error "denseToDecimal")
    . Money.denseToDecimal Money.Round False (Just ',') '.' 0 (1 % 1)

{-

template1 :: Html ()
template1 = do
  table_ [ class_ "table table-striped" ] $ do
    thead_ $ do
      tr_ $ do
        th_ [ scope_ "col" ] $ "#"
        th_ [ scope_ "col" ] $ "First"
        th_ [ scope_ "col" ] $ "Last"
        th_ [ scope_ "col" ] $ "Handle"
    tbody_ $ do
      tr_ $ do
        th_ [ scope_ "row" ] $ "1"
        td_ "Mark"
        td_ "Otto"
        td_ "@mdo"

<table class="table table-striped">
  <thead>
    <tr>
      <th scope="col">#</th>
      <th scope="col">First</th>
      <th scope="col">Last</th>
      <th scope="col">Handle</th>
    </tr>
  </thead>
  <tbody>

    <tr>
      <th scope="row">1</th>
      <td>Mark</td>
      <td>Otto</td>
      <td>@mdo</td>
    </tr>

  </tbody>
</table>

-}


