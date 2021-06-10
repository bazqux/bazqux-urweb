import System.Environment
import Control.Monad

import qualified Search
import qualified Discovery

mains =
    [("ReindexAllPosts", Search.reindexAllPosts)
    ,("ReindexAllTags", Search.reindexAllTags)
    ,("PrepareIndexSubscriptionsData", Discovery.prepareIndexSubscriptionsData)
    ,("IndexSubscriptions", Discovery.indexSubscriptions)
    ]

main = do
    args <- getArgs
    case args of
        [cmd] | Just c <- lookup cmd mains ->
            c
        _ -> do
            putStrLn "Usage: "
            forM_ mains $ \ (c,_) ->
                putStrLn $ "  cabal run Index -- " <> c
