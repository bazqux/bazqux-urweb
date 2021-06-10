{-# LANGUAGE BangPatterns, ForeignFunctionInterface #-}
module Codec.RawZlib
    ( compress, decompress )
    where

import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Lazy.Internal as BL
import qualified Control.Exception as E
import System.IO.Unsafe

import Codec.Zlib
import Codec.Zlib.Lowlevel
import Foreign (plusPtr, castPtr, withForeignPtr)

foreign import ccall unsafe "free_z_stream_inflate"
    c_free_z_stream_inflate' :: ZStream' -> IO ()
foreign import ccall unsafe "free_z_stream_deflate"
    c_free_z_stream_deflate' :: ZStream' -> IO ()

rawWindowBits :: WindowBits
rawWindowBits = WindowBits (-15)
                -- defaultWindowBits
-- для raw формата window bits делаются отрицательными

decompress :: BL.ByteString -> BL.ByteString
decompress l = unsafePerformIO $
    E.bracket zstreamNew c_free_z_stream_inflate' $ \ zs -> do
    inflateInit2 zs rawWindowBits
    let feed acc [] = return $ BL.fromChunks $ reverse acc
        feed acc (B.PS fp offs len : next) = do
--            print ("feed", len)
            acc' <- withForeignPtr fp $ \ ptr -> do
                c_set_avail_in zs (ptr `plusPtr` offs) (toEnum len)
                go acc True
            feed acc' next
        go acc False = return acc
        go !acc True = do
            (b,n) <- B.createAndTrim' bufLen $ \ dst -> do
                c_set_avail_out zs (castPtr dst) (toEnum bufLen)
                _r <- c_call_inflate_noflush zs
                avail <- c_get_avail_out zs
--                 availIn <- c_get_avail_in zs
--                 print (r, avail, availIn)
                return (0, bufLen - fromEnum avail, avail == 0)
            go (b:acc) n
    feed [] $ BL.toChunks l
    where bufLen = BL.defaultChunkSize

compress :: BL.ByteString -> BL.ByteString
compress l = unsafePerformIO $
    E.bracket zstreamNew c_free_z_stream_deflate' $ \ zs -> do
    deflateInit2 zs 1 rawWindowBits 8 StrategyDefault
    let feed acc [] = return $ BL.fromChunks $ reverse acc
        feed acc (B.PS fp offs len : next) = do
--             print ("feed", len)
            acc' <- withForeignPtr fp $ \ ptr -> do
                c_set_avail_in zs (ptr `plusPtr` offs) (toEnum len)
                go acc True (null next)
            feed acc' next
        go acc False _ = return acc
        go !acc True finish = do
            (b,n) <- B.createAndTrim' bufLen $ \ dst -> do
                c_set_avail_out zs (castPtr dst) (toEnum bufLen)
                _r <-
                    if finish then c_call_deflate_finish zs
                    else c_call_deflate_noflush zs
                avail <- c_get_avail_out zs
--                 availIn <- c_get_avail_in zs
--                 print (r, avail, availIn)
                return (0, bufLen - fromEnum avail, avail == 0)
            go (b:acc) n finish
    feed [] $ BL.toChunks l
    where bufLen = BL.defaultChunkSize
