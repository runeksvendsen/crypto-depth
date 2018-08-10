module CryptoDepth.Output.HTML
( htmlOut
) where

import CryptoDepth.Output.HTML.Summary as Summary
import CryptoDepth.Output.HTML.Crypto  as Crypto

import System.Directory                 (createDirectoryIfMissing)
import CryptoDepth.Internal.DPrelude
import qualified CryptoDepth            as CD
import qualified Data.HashMap.Strict    as Map
import Lucid
import System.IO                        (stderr, hSetEncoding, utf8, IOMode(WriteMode))
import Data.Text.Lazy.IO                as L
import System.IO                        (withFile)


htmlOut
    :: KnownSymbol numeraire
    => CD.Slippage
    -> Map CD.Sym ([CD.PathInfo numeraire], [CD.PathInfo numeraire])
    -> IO ()
htmlOut slipPct symVolumes = do
    let baseDir = "html"
    createDirectoryIfMissing False baseDir
    -- Write summary/main page
    let summaryHtml = summary slipPct $ CD.totals symVolumes
    writeHtmlFile baseDir "index.html" summaryHtml
    -- Write crypto pages
    -- TODO
    forM_ (Map.toList symVolumes) $ \item@(sym, _) ->
        writeHtmlFile baseDir (toS sym <> ".html") (Crypto.crypto item)


writeHtmlFile baseDir filename html = do
    let targetFile = baseDir <> "/" <> filename
    withFile targetFile WriteMode $ \handle -> do
        writeUtf8Html html handle
        L.hPutStrLn stderr $ "Wrote " <> toS targetFile

writeUtf8Html html handle = do
    hSetEncoding handle utf8
    L.hPutStr handle (renderText html)
