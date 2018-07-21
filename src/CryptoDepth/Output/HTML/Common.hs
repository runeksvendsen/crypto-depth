module CryptoDepth.Output.HTML.Common
( htmlDoc
, showRat
, showDenseAmount
)
where

import CryptoDepth.Internal.DPrelude
import qualified Money
import Lucid
import Lucid.Base (makeAttribute)


htmlDoc
  :: String
  -> Html ()
  -> Html ()
htmlDoc titleStr body = do
  html_ $ do
    head_ $ do
      title_ $ toHtml titleStr
      boostrapCss
    body_ body

showRat :: Rational -> String
showRat rat
    | denominator rat == 1 = show (numerator rat)
    | otherwise = printf "%d/%d" (denominator rat) (numerator rat)

boostrapCss :: Html ()
boostrapCss =
    link_ [rel_ "stylesheet", href_ url, type_ "text/css", integrity_ hash, crossorigin_ "anonymous"]
  where
    url = "https://stackpath.bootstrapcdn.com/bootstrap/4.1.1/css/bootstrap.min.css"
    hash = "sha384-WskhaSGFgHYWDcbwN70/dfYBj47jz9qbsMId/iRN3ewGhXQFZCSftd1LZCfmhktB"
    crossorigin_ :: Text -> Attribute
    crossorigin_ = makeAttribute "crossorigin"
    -- <link rel="stylesheet" href={$url} integrity={$hash} crossorigin="anonymous">

-- | Show amount WITHOUT trailing symbol, e.g.: "1,234"
showDenseAmount :: KnownSymbol a => Money.Dense a -> Text
showDenseAmount =
    fromMaybe (error "denseToDecimal")
    . Money.denseToDecimal Money.Round False (Just ',') '.' 0 (1 % 1)
