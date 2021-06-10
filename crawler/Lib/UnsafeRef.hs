-- | IORef с Show,Eq,Ord,Binary instance-ами, чтобы была возможность его
-- включать в генерируемые структуры данных
module Lib.UnsafeRef
    ( UnsafeRef(..)
    )
    where

import Control.DeepSeq
import Data.IORef
import Data.Binary

newtype UnsafeRef a = UnsafeRef { unsafeRef :: IORef a }

instance Show (UnsafeRef a) where
    show _ = "UnsafeRef"

instance Eq (UnsafeRef a) where
    (==) = error "Eq UnsafeRef?"

instance Ord (UnsafeRef a) where
    compare = error "compare UnsafeRef?"

instance Binary (UnsafeRef a) where
    put = error "put UnsafeRef?"
    get = error "get UnsafeRef?"

instance NFData (UnsafeRef a) where
    rnf (UnsafeRef a) = rnf a `seq` ()
