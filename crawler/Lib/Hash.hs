module Lib.Hash
    ( md5, hashData, urlSafe, urlSafe_sha1, base58T
    , base64_sha1, base64_sha1T, base64_sha3_512
    , base16_sha3_512, base16_md5
    , sha1_bs, sha3_512_bs, sha1_digest2bs, sha256_digest2bs
    , base16_sha256_hmac
    )
    where

import qualified Data.ByteArray as BA
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Base58 as Base58
import qualified Data.ByteString as B
import Lib.StringConversion
import Crypto.Hash
import qualified Data.Text as T
import qualified Crypto.MAC.HMAC as HMAC

sha1_digest2bs :: Digest SHA1 -> B.ByteString
sha1_digest2bs = B.pack . BA.unpack

sha256_digest2bs :: Digest SHA256 -> B.ByteString
sha256_digest2bs = B.pack . BA.unpack

sha3_512_digest2bs :: Digest SHA3_512 -> B.ByteString
sha3_512_digest2bs = B.pack . BA.unpack

md5_digest2bs :: Digest MD5 -> B.ByteString
md5_digest2bs = B.pack . BA.unpack

sha1_bs :: B.ByteString -> B.ByteString
sha1_bs = sha1_digest2bs . hash

sha3_512_bs :: B.ByteString -> B.ByteString
sha3_512_bs = sha3_512_digest2bs . hash

md5_bs :: B.ByteString -> B.ByteString
md5_bs  = md5_digest2bs . hash

base64_sha1 = Base64.encode . sha1_bs
base64_sha1T = base64T sha1_bs
base16_sha1 = Base16.encode . sha1_bs

urlSafe_sha1 = urlSafe sha1_bs
base64T h = bst . Base64.encode . h . tbs
base16T h = bst . Base16.encode . h . tbs

base58T h = bst . Base58.encodeBase58 Base58.flickrAlphabet . h . tbs

urlSafe h = T.takeWhile (/= '=') . T.map fix . base64T h
    where fix '+' = '-'
          fix '/' = '_'
          -- fix '=' = '.'
          fix x   = x

base64_sha3_512 = Base64.encode . sha3_512_bs
base16_sha3_512 = Base16.encode . sha3_512_bs

base64_md5 = Base64.encode . md5_bs
base64_md5T = base64T md5_bs
base16_md5 = Base16.encode . md5_bs
base16_md5T = base16T md5_bs

-- lowercase
md5 = base16_md5T

hashData = bst . base64_sha1

base16_sha256_hmac k m =
    Base16.encode $ sha256_digest2bs $ HMAC.hmacGetDigest $ HMAC.hmac k m
