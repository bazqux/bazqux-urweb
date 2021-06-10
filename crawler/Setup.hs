import Distribution.Simple
import qualified Gen

main = do
    defaultMainWithHooks $ simpleUserHooks
        { preBuild = \ a b -> do
            putStrLn "Generating data types, I/O and FFI code"
            Gen.main
            putStrLn "done"
            preBuild simpleUserHooks a b
        }
