-- Doesn't always resolve even with 30 seconds timeout.
-- Maybe some UDP packets lost.
--
-- TODO: пропатчить concurrent-dns-cache, чтобы Exception
-- "recv: does not exist (Connection refused)" ловил, когда BIND9 не запущен
-- Проверить, теперь есть ошибка NetworkFailure
--
{-# LANGUAGE ScopedTypeVariables, RecordWildCards, ViewPatterns,
             OverloadedStrings, TupleSections, BangPatterns #-}
module Lib.DnsCache
    ( -- * DNS cache
      DnsCache
    , withDnsCache

      -- * DNS lookup
    , resolveA, resolveCachedA

      -- * Utils
    , showHostAddress

--    , massResolve
    , resolveHostEntry
    , reservedHostAddress
    ) where

import Control.Monad
import qualified Data.Text as T
import Control.Concurrent
import qualified Control.Concurrent.MSem as MSem
import Data.IP
import Data.List
import Network.Socket
import Lib.Log
import Network.DNS hiding (lookup)
import Network.DNS.Cache
import qualified Data.ByteString.Char8 as B
import Data.Maybe
import Network.BSD
import System.IO.Unsafe
import System.Info
import qualified Control.Exception as E

-- | Show @HostAddress@ in standard 123.45.67.89 format.
--
-- Unlike 'inet_ntoa' this function is pure and thread-safe.
showHostAddress :: HostAddress -> String
showHostAddress = show . fromHostAddress


_test :: IO ()
_test = withDnsCache $ \ c -> do
    let r' f hn = do
          h <- f c hn
          logS $ hn ++ ": " ++ either T.unpack showHostAddress h
        r = r' resolveA
        rc = r' (\ c d -> fmap fromJust $ resolveCachedA c d)
        rn n = replicateM_ n . r
    r "blog.beta.bazqux.com"
    r "beta.bazqux.com"
    r "bazqux.com"
    r "3.dk"
    r "4.dk"
    r ".kontan.co.id"
    r "www.7kanal.co.il"
    r "blog.trade.gov"
    r "enjoyble.com"
    r "www.linuxfederation.com"
    r "blog.bogorad.eu"
    r "rssfriends.com"
    r "www.scorched.ru"
    r "127.0.0.1"
    r "cdn.bazqux.com"
    r "qwer.google.com"
    rn 10 "www.huffingtonpost.com"
    rn 5 "twitter.com"
    rn 5 "wordpress.com"
    rn 5 "feeds.feedburner.com"
    rn 10 "feedburner.com"
    rc "feedburner.com"
    rc "feedburner.com"
    rc "192.168.1.100"
    r "192.168.1.256"
--    r "celebutopia.net"
    s <- MSem.new 0
    let cnt = 100000
    replicateM_ cnt $ do
        forkIO $ void $ resolveA c "celebutopia.net" >> MSem.signal s
--        forkIO $ void $ resolveA c "google.com"
    replicateM_ cnt $ MSem.wait s
    rc "celebutopia.net"

-- Для запуска локального bind9
-- sudo launchctl load -w /Library/LaunchDaemons/org.macports.bind9.plist

rc  | os == "darwin" = RCFilePath "/etc/resolv.conf"
    | otherwise      = RCHostName "127.0.0.1"

cacheConf :: DNSCacheConf
cacheConf = DNSCacheConf {
    resolvConfs    = [
         defaultResolvConf { resolvInfo = rc, resolvTimeout = 5*1000*1000 }
       ]
  , maxConcurrency = 30 -- 20 скорость не меняет, 50 уже вызывает таймауты и не сильно шустрее работает
  -- очистка кеша BIND: sudo rndc flush
  -- на локальном 213.133.99.99 с concurrency 20 вылезали
  -- TestDnsCache: recv: does not exist (Connection refused)
  -- а вот с bind скорость меняется
  -- concurrency 100 + bind w/o forwarders 10k/34sec
  --   иногда вылезает TimeoutExpired (несколько раз на 10k доменов)
  -- concurrency 50 + bind w/o forwarders  10k/57sec
  --   TimeoutExpired на тех же серверах (видимо блокируют мои серваки)
  -- concurrency 20 -- ошибок столько же, но работает в 2 раза медленнее
  -- concurrency 100 + bind w/ hetzner dns 10k/30sec
  --   очень мало ошибок, но есть
  -- concurrency 50 + bind w/ hetzner dns 10k/30sec -- то же, что и 100
  --   ошибок, кажется, еще меньше (но больше, чем с google+hetzner, 50k 2min)
  --   concurrency 30 дает такой же результат по времени (50k 2min)
  --   причем для google+hetzner то же
  -- concurrency 170 + bind w/ hetzner dns 10k/30sec -- то же, что и 100
  --   но уже заметно чаще ServerFailure
  -- concurrency 170 + bind w/ google+hetzner dns 10k/19sec -- ощутимо быстрее
  --   к двум минутам уже 100k
  --   но уже заметно чаще ServerFailure
  -- concurrency 20 + bind w/ google+hetzner dns 10k/48sec (28k 2min) хуже
  -- concurrency 30 + bind w/ google+hetzner dns 10k/30sec (50k 2min) норм
  -- concurrency 50 + bind w/ google+hetzner dns 10k/28sec -- чуть быстрее
  --   к двум минутам 60k
  --   ошибок почти нет
  -- concurrency 60 + bind w/ google+hetzner dns 10k/23sec -- чуть быстрее
  --   ошибок мало, но есть
  -- concurrency 75 + bind w/ google+hetzner dns 10k/20sec -- почти как 170
  --   ошибки появляются
  --   порядок forwarders не влияет, видимо, bind их по кругу опрашивает
  --   1 hetzner + 2 google = 37sec (хуже, чем если все 3 hetzner)
  --    и с concurrency 50 39sec (незначительно, но хуже)
  -- ADNS 170 +1000ms delay, Google + Hetzner DNS (настройки из кравлера)
  -- медленнее 10k/37sec (37k 2min, вместо 51k)
  -- часто появляются ошибки
  --
  -- Показатели сильно меняются, если подождать подольше, чтобы кеши
  -- forwarder-ов почистились
  -- В целом 50 более-менее нормально, если больше, то появляются ошибки
  -- меньше -- медленнее, хотя до 30 не сильно
  -- Также важно иметь и Google и Hetzner, т.к. уменьшается число ошибок

  , minTTL         = 300
    -- у facebook минута и один IP, при запуске кравлера получаются
    -- неравномерные очереди
  , maxTTL         = 600
    -- 10 минут, т.к. все равно локальный bind есть, а домены часто повторяются
    -- при запуске кравлера (основная масса меньше минуты) и при добавлении
    -- комментариев -- достаточно быстро.
  , negativeTTL    = 60
  }

withDnsCache = withDNSCache cacheConf

type DnsCache = DNSCache

checkDomain f domain act
    | any (`elem` (":/" :: [Char])) domain =
        return $ f $ Left "Invalid domain name"
    | length domain > 253 = return $ f $ Left "Domain name too long"
    | any (\ t -> T.length t > 63) (T.split (== '.') $ T.pack domain) =
        return $ f $ Left "Domain name or component too long"
    | any (\ t -> T.length t == 0) (T.split (== '.') $ T.pack domain) =
        return $ f $ Left "Illegal domain name (empty label)"
    | isIPAddr domain = return $ f $ Right $ ipToWord32 domain
    | otherwise = act

isIPAddr :: HostName -> Bool
isIPAddr hn = length groups == 4 && all ip groups
    where groups = T.split (== '.') $ T.pack hn
          ip x = T.length x <= 3 && T.length x > 0 &&
                 T.all (\ e -> e >= '0' && e <= '9') x &&
                 read (T.unpack x) <= (255 :: Int)

ipToWord32 :: String -> HostAddress
ipToWord32 = toHostAddress . read

handleError :: (T.Text -> a) -> E.SomeException -> IO a
handleError f e = return $ f $ T.concat ["Exception (badly configured or non-working local BIND?): ", T.pack (show e)]

handle f a = a `E.catch` handleError f

resolveA :: DnsCache -> String -> IO (Either T.Text HostAddress)
resolveA c d =
    handle Left $ checkDomain id d $ fmap errorStr $ resolve c (B.pack d)

resolveCachedA :: DnsCache -> String -> IO (Maybe (Either T.Text HostAddress))
resolveCachedA c d =
    handle (Just . Left) $ checkDomain Just d $ fmap (fmap errorStr) $ resolveCache c (B.pack d)

errorStr (Left e) = Left $ case e of
    SequenceNumberMismatch -> "Sequence number mismatch?"
    TimeoutExpired -> "Timeout"
    UnexpectedRDATA -> "Unexpected RDATA (no domain found?)"
    IllegalDomain -> "Illegal domain name"
    FormatError -> "Name server was unable to interpret the query"
    ServerFailure -> "Name server returned failure"
    NameError -> "No such domain"
    NotImplemented -> "Not implemented (name server failuer)"
    OperationRefused -> "Name server refused operaion"
    BadOptRecord -> "Bad OPT record"
    RetryLimitExceeded -> "Retry limit exceeded"
    BadConfiguration -> "Bad configuration"
    NetworkFailure e -> T.pack $ "Network failure: " ++ show e
    DecodeError e -> T.pack $ "Decode error: " ++ e
    UnknownDNSError -> "Unknown DNS error"
    QuestionMismatch -> "Question mismatch"
    InvalidAXFRLookup -> "Invalid AXFR lookup"
errorStr (Right x) = Right $ case x of
    Hit a -> a
    Resolved a -> a
    Numeric a -> a

-- massResolve = withDNSCache cacheConf $ \ kcache -> AC.withDnsCacheSettings (AC.DnsCacheSettings 170 (Just 1000)) $ \ ac -> withDnsCache $ \ c -> do
--     s <- MSem.new 200
--     n <- newIORef 0
--     t <- getUrTime
--     flip E.catch (\ (e :: E.SomeException) -> return ()) $ forever $ do
--         hostName <- B.getLine
--         i <- readIORef n
--         writeIORef n (i+1)
--         when (i `mod` 1000 == 0) $ do
--             t' <- getUrTime
--             logS $ show i ++ " (" ++ showSecs (diffUrTime t' t) ++ ")"
--         MSem.wait s
--         void $ forkIO $ flip E.finally (MSem.signal s) $ do
--             (t, r) <- time $ resolve kcache hostName -- resolveA c hostName
--             case r of
--                 Left e -> do -- e@('(':_) -> do
--                     ar <- AC.resolveA ac (B.unpack hostName)
--                     case ar of
--                         Right _ -> logS $ B.unpack hostName ++ " " ++ showSecs t ++ " " ++ show e
--                         _ -> return ()
--                 _ -> return ()
-- --            return ()
-- --             logS $ "resolve: " ++ showSecs t ++ "  " ++ hostName ++ "\t" ++
-- --                  either (const "Fail") showHostAddress r

hostEntries = unsafePerformIO $ getHostEntries False
{-# NOINLINE hostEntries #-}

-- | аналог getHostByName, но без непонятных тормозов
-- и резольвит только /etc/hosts.
resolveHostEntry e = return $ go hostEntries
    where go [] = error $ "resolveFromHostEntry: can’t find " ++ e
          go (h:hs)
              | e `elem` hostName h : hostAliases h = hostAddress h
              | otherwise = go hs

-- resolveHostEntry e = do
--     AddrInfo { addrAddress = SockAddrInet _ r }:_ <-
--         getAddrInfo Nothing (Just e) Nothing
--     return r

-- https://en.wikipedia.org/wiki/Reserved_IP_addresses
-- использовать iproute
reservedHostAddress (fromHostAddress -> ip) =
    find (\(r,_,_) -> isMatchedTo ip r) reservedHostAddresses

reservedHostAddresses :: [(AddrRange IPv4, T.Text, T.Text)]
reservedHostAddresses =
    [ ("0.0.0.0/8", "Software", "Current network (only valid as source address).")
    , ("10.0.0.0/8", "Private network", "Used for local communications within a private network.")
    , ("100.64.0.0/10", "Private network", "Shared address space for communications between a service provider and its subscribers when using a carrier-grade NAT.")
    , ("127.0.0.0/8", "Host", "Used for loopback addresses to the local host.")
    , ("169.254.0.0/16", "Subnet", "Used for link-local addresses between two hosts on a single link when no IP address is otherwise specified, such as would have normally been retrieved from a DHCP server.")
    , ("172.16.0.0/12", "Private network", "Used for local communications within a private network.")
    , ("192.0.0.0/24", "Private network", "IETF Protocol Assignments.")
    , ("192.0.2.0/24", "Documentation", "Assigned as TEST-NET-1, documentation and examples.")
    , ("192.88.99.0/24", "Internet", "Reserved. Formerly used for IPv6 to IPv4 relay (included IPv6 address block 2002::/16).")
    , ("192.168.0.0/16", "Private network", "Used for local communications within a private network.")
    , ("198.18.0.0/15", "Private network", "Used for benchmark testing of inter-network communications between two separate subnets.")
    , ("198.51.100.0/24", "Documentation", "Assigned as TEST-NET-2, documentation and examples.")
    , ("203.0.113.0/24", "Documentation", "Assigned as TEST-NET-3, documentation and examples.")
    , ("224.0.0.0/4", "Internet", "In use for IP multicast (Former Class D network).")
    , ("255.255.255.255/32", "Subnet", "Reserved for the “limited broadcast” destination address.") -- совпадает с 240.0.0.0/4
    , ("240.0.0.0/4", "Internet", "Reserved for future use (Former Class E network).")
    ]
--     [ ("::/0", "Routing", "Default route.")
--     , ("::/128", "Software", "Unspecified address.")
--     , ("::1/128", "Host", "Loopback address to the local host.")
--     , ("::ffff:0:0/96", "Software", "IPv4 mapped addresses.")
--     , ("::ffff:0:0:0/96", "Software", "IPv4 translated addresses.")
--     , ("64:ff9b::/96", "Global Internet", "IPv4/IPv6 translation.")
--     , ("100::/64", "Routing", "Discard prefix.")
--     , ("2001::/32", "Global Internet", "Teredo tunneling.")
--     , ("2001:20::/28", "Software", "ORCHIDv2.")
--     , ("2001:db8::/32", "Documentation", "Addresses used in documentation and example source code.")
--     , ("2002::/16", "Global Internet", "The 6to4 addressing scheme (now deprecated).")
--     , ("fc00::/7", "Private network", "Unique local address.")
--     , ("fe80::/10", "Link", "Link-local address.")
--     , ("ff00::/8", "Global Internet", "Multicast address.")
--     ]
