module CryptoDepth.Internal.Types.EdgePath where

import CryptoDepth.Internal.DPrelude
import CryptoDepth.Internal.Types
import qualified Data.List.NonEmpty  as NE

-- | A path through one or more markets.
--   Either starts or ends in "numeraire".
newtype EdgePath (numeraire :: Symbol) =
    EdgePath { someEdgeVenueList :: NonEmpty SomeEdgeVenue }
        deriving (Eq, Show, Generic)

pathDescr
    :: EdgePath numeraire
    -> NonEmpty SymVenue
pathDescr (EdgePath sev) = throwBug $
    snd <$> composeSS sev

srcSym
    :: EdgePath numeraire
    -> Sym
srcSym ep =
    let epList = groupVenues (pathDescr ep)
    in epSrc (NE.head epList)

dstSym
    :: EdgePath numeraire
    -> Sym
dstSym ep =
    let epList = groupVenues (pathDescr ep)
    in epDst (NE.last epList)
