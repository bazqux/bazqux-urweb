import Control.Applicative ((<$>))
import Control.Exception (finally)
import Control.Monad (forM_)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Network.Riak.Connection (defaultClient)
import Network.Riak.Connection.Pool (Pool, create, withConnection)
import Network.Riak.Content (binary)
import Network.Riak.Types (Bucket, Key, Quorum(..))
import System.IO.Unsafe (unsafePerformIO)
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Arbitrary(..), (==>))
import Test.QuickCheck.Monadic (assert, monadicIO, run)
import qualified Data.ByteString.Lazy as L
import qualified Network.Riak.Basic as B
import qualified Network.Riak.JSON as J
import qualified Network.Riak.Value as V

instance Arbitrary L.ByteString where
    arbitrary     = L.pack `fmap` arbitrary

cruft :: IORef [(Bucket, Key)]
{-# NOINLINE cruft #-}
cruft = unsafePerformIO $ newIORef []

pool :: Pool
{-# NOINLINE pool #-}
pool = unsafePerformIO $
       create defaultClient 1 1 1

t_put_get b k v = notempty b && notempty k ==> monadicIO $ assert . uncurry (==) =<< run act
 where
  act = withConnection pool $ \c -> do
          modifyIORef cruft ((b,k):)
          p <- Just <$> B.put c b k Nothing (binary v) Default Default
          r <- B.get c b k Default
          return (p,r)
  notempty = not . L.null

main = defaultMain tests `finally` cleanup
  where
    cleanup = withConnection pool $ \c -> do
                bks <- readIORef cruft
                forM_ bks $ \(b,k) -> B.delete c b k Default

tests = [
  testProperty "t_put_get" t_put_get
 ]
