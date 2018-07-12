{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ParallelListComp #-}
module CryptoDepth.Output.CLI
( cliStdout
)
where

import Prelude (unlines)
import CryptoDepth.Internal.DPrelude
import qualified CryptoDepth.Paths as Paths
import qualified Money


cliStdout :: KnownSymbol numeraire
          => [(Paths.Sym, Money.Dense numeraire, Money.Dense numeraire)]
          -> IO ()
cliStdout symVolume = do
    let !outLines = map prettyPrint symVolume
    putStr initialLine
    putStr (unlines outLines)
    putStrLn delimiter
  where
    -- Output
    prettyPrint :: KnownSymbol numeraire
                => (Paths.Sym, Money.Dense numeraire, Money.Dense numeraire)
                -> String
    prettyPrint (nodeSym, buySum, sellSum) = printf formatStr
        (toS nodeSym :: String)
        (showDense buySum)
        (showDense sellSum)
    formatStr = "%s\t%17s\t%17s"
    delimiter = "-------------------------------------------------"
    initialLine = unlines
                  [ delimiter
                  , printf formatStr
                        ("sym" :: String)
                        ("buy_volume" :: String)
                        ("sell_volume" :: String)
                  , delimiter
                  ]


showSomeDense :: Money.SomeDense -> String
showSomeDense = (`Money.withSomeDense` showDense)

showDense :: forall a. KnownSymbol a => Money.Dense a -> String
showDense = (++ " " ++ symbolVal (Proxy :: Proxy a))
                . toS . fromMaybe (error "denseToDecimal")
                . Money.denseToDecimal Money.Round False (Just ',') '.' 0 (1 % 1)
