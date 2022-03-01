module Main where

import System.IO.Unsafe (unsafeInterleaveIO)
import Data.Functor (($>))

import DDnetFriends as DD
import Control.Concurrent (threadDelay)

-- NOTE: inherits unsafety from unsafeInterleaveIO.
-- be careful to not do any IO in `f` which might interfere with other IO!
iterateIO :: (a -> IO a) -> a -> IO [a]
iterateIO f cur = (cur:) <$> (unsafeInterleaveIO $ f cur >>= iterateIO f)

main = do
    let dd_up = DD.ddnet_update ["keksi", "Pixificial", "metamuffin"]
    dd_state <- DD.make_dd

    init <- dd_up dd_state

    let more (up, st) = dd_up st

    updates <- iterateIO (\a -> threadDelay 3000000 >> more a) init

    mapM (print . fst) updates
