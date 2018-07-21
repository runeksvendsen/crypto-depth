{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ParallelListComp #-}
module CryptoDepth.Output.HTML.Crypto
( crypto
)
where

import CryptoDepth.Internal.DPrelude
import CryptoDepth.Output.HTML.Common
import qualified CryptoDepth            as CD
import Lucid
import qualified Data.Text as T


crypto
  :: forall numeraire.
     KnownSymbol numeraire
  => (CD.Sym, ([CD.PathInfo numeraire], [CD.PathInfo numeraire]))
  -> Html ()
crypto (sym, (buyPaths, sellPaths)) =
  htmlDoc titleStr $
    div_ [class_ "container-fluid"] $
      div_ [class_ "row"] $ do
          div_ [class_ "col-md"] $ do
            h2_ "Sell paths"
            cryptoTable sellPaths
          div_ [class_ "col-md"] $ do
            h2_ "Buy paths"
            cryptoTable buyPaths
  where
    titleStr = printf "%s path info" sym

cryptoTable
    :: forall numeraire.
       KnownSymbol numeraire
    => [CD.PathInfo numeraire]
    -> Html ()
cryptoTable pathInfos = do
  let numeraire :: String
      numeraire = printf "(%s)" (symbolVal (Proxy :: Proxy numeraire))
  table_ [ class_ "table table-striped" ] $ do
    thead_ $ do
      tr_ $ do
        th_ [ scope_ "col" ] $ "#"
        th_ [ scope_ "col" ] $ "Path"
        th_ [ scope_ "col" ] $ toHtml ("Volume " <> numeraire)
    tbody_ $
      forM_ (zip [1..] pathInfos) (uncurry cryptoRow)

cryptoRow
  :: KnownSymbol numeraire
  => Word
  -> CD.PathInfo numeraire
  -> Html ()
cryptoRow pos pi =
  tr_ $ do
    th_ [ scope_ "row" ] $ toHtml (show pos)
    td_ (htmlPath pi)
    td_ (toHtml $ showDenseAmount $ CD.piQty pi)

htmlPath
    :: forall numeraire.
       KnownSymbol numeraire
    => CD.PathInfo numeraire
    -> Html ()
htmlPath CD.PathInfo{..} =
      void . sequence
    $ intersperse (toHtml separator)
    $ foldr (flip symVenueHtml) [] (CD.groupVenues piPath)
  where
    separator :: T.Text
    separator = " → "
    symbolText :: [CD.Sym] -> Text
    symbolText [] = error "htmlPath: empty symbol list"
    symbolText [sym] = sym
    symbolText syms = T.concat ["(", T.concat $ intersperse separator syms, ")"]
    symVenueHtml :: [Html ()] -> (Text, [CD.Sym]) -> [Html ()]
    symVenueHtml pre (venue, syms) =
        (toHtml (symbolText syms) >> sub_ (toHtml venue)) : pre






