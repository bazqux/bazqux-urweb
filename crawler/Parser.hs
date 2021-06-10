{-# LANGUAGE BangPatterns, OverloadedStrings,
             ViewPatterns, LambdaCase,
             RecordWildCards, TupleSections, MultiWayIf #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Parser
    ( parse, ParseResult(..), SwitchUrl(..), JSComments(..)
    , FeedMsg(..), Media(..), LinkKind(..), Parent(..)
    , transformURL, transformedUrl, transformedUrlHostName
    , test, parseUrl, parseUrlT
    , parseFile, perfTest
    , hasCustomParser
    , botRequestHeaders
    , parseFacebookError, isFacebookOAuthException, twitterErrors
    , redownloadOptionsLastModified
    , calculateTwitterRateLimitDelay
    ) where

import qualified Network.HTTP.Client as C
import qualified Data.CaseInsensitive as CI
import System.Environment
import System.Process (system)
import Control.Concurrent
import Control.Concurrent.Async
import qualified Control.Concurrent.MSem as MSem
import Data.String
import Data.Maybe
import Data.List
import Control.Monad
import Control.Applicative
import Text.HTML.TagSoup hiding (parseTags, renderTags)
import Text.HTML.TagSoup.Fast
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import URL
import Lib.Log
import Lib.Regex
import Lib.StringConversion
import Network.HTTP.Conduit.Downloader
import Lib.DnsCache
import Lib.Json
import Text.Printf
import Lib.UrTime
import qualified Data.Aeson as JSON
import qualified Data.HashSet as HS
--import Text.Show.Pretty (ppShow)
import Auth
import Parser.Types
import Parser.Custom
import Parser.Default
import qualified Parser.Disqus
import qualified Parser.Facebook
import           Parser.Facebook (parseFacebookError, isFacebookOAuthException)
import qualified Parser.Habr
import qualified Parser.LiveJournal
import qualified Parser.LtU
import qualified Parser.Reddit
import qualified Parser.Telegram
import           Parser.Twitter hiding (customParsers)
import qualified Parser.Twitter
import qualified Parser.VK
import qualified Parser.YouTube
import Config (addProxy, proxiedFeedDomain, botDownloaderSettings, botUserAgent, rmKeysFromUrl)

perfTest = do
--    hSetBuffering stdout NoBuffering
    args <- getArgs
    let (fn, n) = case args of
                    [] -> ("default", 10)
                    [f] -> (f, 10)
                    f:n:_ -> (f, read n)
    contents <- B.readFile fn
    withLogger $ \ l -> do
        st <- getTime
        w <- MSem.new 0
        let go 0 size = return size
            go i !size = do
                let p = parseTags $ ensureUtf8Xml s
                    s = B.concat $ [contents] ++ replicate i "<test>"
                    aLen [] !n = n
                    aLen ((a,v):as) !n = aLen as (n + B.length a + B.length v)
                    check [] tags text = do
                        logS $ show (i, tags, text)-- , length p)-- B.take 10 $ renderTags p)
                        MSem.signal w
                    check (TagOpen t a : ts) !tags !text =
                        check ts (tags+1) (text + B.length t + aLen a 0)
                    check (TagText t : ts) !tags !text =
                        check ts (tags+1) (text + B.length t)
                    check (_ : ts) !tags !text =
                        check ts (tags+1) text
--                 forkIO $ do
--                     d <- randomRIO (1,25000000)
--                     threadDelay d
                check p 0 0
                go (i-1) (size + B.length s)
        size <- go n 0
--        replicateM_ n $ MSem.wait w
        et <- getTime
        printf "%.3f MB/sec\n" (fromIntegral size / 1e6 / (et-st))
        -- где-то 40-80 с резгребанием текста
        -- и      80-210 без
        -- кажется файлы без лишних пробелов работают шустрее


test = fmap (length . show) $ parseFile "https://www.twitter.com/asdf" "parseUrlT.json"
    -- putStrLn . show =<< parseUrl
--        "http://ohnotheydidnt.livejournal.com/67246877.html?thread=11650230813&format=light&updplz442957"
--      "http://www.retronator.com/rss?9ace0530"
--       "http://habrahabr.ru/post/148769/"
--       "https://graph.facebook.com/comments/?ids=http://techcrunch.com/2012/07/28/why-platform-clouds-need-to-be-more-like-app-stores/"
--       "http://www.avadeanlewis.com/nutrition-quick-tips-to-reach-your-fitness-goals/feed/"
--       "http://milfuegos.blogspot.com/feeds/posts/default"
--       "http://www.thenervousbreakdown.com/feed/" -- большой

checkParents s n [] = n
checkParents s n ((Just g, fm):fms)
    | not (g `HS.member` s) = checkParents s n fms
checkParents s n ((_, fm):fms) =
    checkParents (HS.insert (fmGuid fm) s) (n+1) fms

saveDebugJSON step c = do
    logS $ "JSON detected. Saving in " ++ fn
    B.writeFile fn c
    void $ forkIO $ void $ system $
        "emacs -batch " ++ fn
        ++ " -f mark-whole-buffer -f json-pretty-print"
        ++ " -f save-buffer"
    where fn = "./dist-newstyle/parseUrlT" <> step <> ".json"

-- withLogger $ \ l -> logTime l "" (fmap B.length $ urlGetContentsT "http://navalny.livejournal.com/782706.html")
withDL url = withDownloaderSettings
    $ botDownloaderSettings { dsUserAgent = botUserAgent (T.pack url) 1 }
urlGetContentsT step = urlGetContentsT' step 5 Nothing
urlGetContentsPostT step d = urlGetContentsT' step 5 (Just d)

urlGetContentsT' step n post url = do
    r <- urlGetContentsT'' step n post url
    either (\ e -> fail $ "urlGetContentsT " ++ url ++ ": " ++ T.unpack e)
        return r
urlGetContentsT'' step n post url = withDL url $ \ d -> withDnsCache $ \ dnsCache -> do
    let hn = hostNameFromUrl url
    Right a <- resolveA dnsCache hn
    -- нужен свой IP, т.к. иначе может использоваться IPv6
--     putStrLn $ showHostAddress a
    let proxy = proxiedFeedDomain (T.pack url)
    putStrLn $ if proxy then "Proxied" else "Non-proxied"
    (r, rdr) <- rawDownload
        ((if proxy then addProxy Nothing else return)
         . signReq . socks . maybe id postRequest post) d url (Just a) []
    print (url, post)
--     mapM_ print (maybe [] rdrHeaders rdr)
    case r of
        DROK c _ -> do
            mapM_ (logS . ("Twitter rate limit delay: " <>) . show) $
                calculateTwitterRateLimitDelay (maybe [] rdrHeaders rdr)
            mapM_ (mapM_ (logS . ("Facebook rate limit delay: " <>) . show) .
                Parser.Facebook.parseFacebookUsageRdr) rdr
            when (isJust $ decodeJson c) $
                saveDebugJSON step c
            return $ Right c
        DRRedirect r | n > 0 -> do
            logS $ "redirect:"
            -- по идее, надо новый parseUrlT пускать,
            -- но пофиг
            logS $ "  " ++ r
            urlGetContentsT'' step (n-1) post r
        e -> do
            maybe (return ()) (B.putStrLn . rdrBody) rdr
            when ((isFacebookOAuthException . rdrBody <$> rdr) == Just True) $
                fail "Facebook authentication error"
            return $ Left $ T.pack $ show e
    where hostName = hostNameFromUrl url
          socks r =
              r { C.responseTimeout = C.responseTimeoutMicro 15000000 }
--                     { C.socksProxy = Just $ S.SocksConf (S.SocksAddress (S.SocksAddrIPV4 0x0100007f) 9050) S.SocksVer5
--                       }
          signReq
              | hostName == "api.twitter.com" = signTwitter
              | otherwise = id

parseFile :: URL -> FilePath -> IO ParseResult
parseFile u fn = do
    t <- getUrTime
    fmap (parse t u) $ B.readFile fn

botRequestHeaders u s c =
    c { C.requestHeaders =
        ("Accept-Language", "en;q=1.0, *;q=0.5") : map fix (C.requestHeaders c) }
    where fix ("User-Agent", _) = ("User-Agent", botUserAgent u s)
          fix x = x

debugParse :: URL -> IO ()
debugParse u = mapM_ dbg . take 10 . prFeedMsgs =<< parseUrlT u
    where dbg (FeedMsg {..}) = do
              putStrLn (replicate 78 '-')
              p "subj: " fmSubject
              p "guid: " fmGuid
              mapM_ (p "date: " . T.pack . showUrTime) fmPublishedTime
              p "author: " fmAuthor
              p "" fmBody
          p n x = B.putStrLn $ n <> tbs x

parseUrl :: URL -> IO ParseResult
parseUrl u = do
    t <- getUrTime
    fmap (parse t u) $ withDownloaderSettings botDownloaderSettings $ \ d -> do
        r <- download d u Nothing []
        case r of
            DROK dat _ -> return dat
            o -> error $ show o
         -- urlGetContentsT u

parseUrlT :: URL -> IO ParseResult
parseUrlT u = parseUrlT' [] u

parseUrlT' :: [(T.Text, T.Text)] -> URL -> IO ParseResult
parseUrlT' env (T.pack -> u) = do
    (tr, u', post) <- transformURL u $ \ k -> do
        let v = lookup k env
        logT $ "getenv " <> k <> " = " <> showT v
        return v
    t <- getUrTime
--    print (tr, u')
    dat <- case (tr, post) of
        ("", _) -> return ""
        (_, Just dat) -> do
--             B.putStrLn dat
            r <- urlGetContentsPostT "" dat (T.unpack tr)
--            B.putStrLn r
            return r
        (_, Nothing)
            | Just (u2, cat) <- u' ->
                liftM2 cat (urlGetContentsT "-a" (T.unpack tr))
                           (urlGetContentsT "-b" (T.unpack u2))
            | otherwise -> urlGetContentsT "" (T.unpack tr)
--    B.writeFile "parseUrlT.html" dat
    let processPR step env = \ case
            PRSetEnv setEnv pr -> do
                forM_ setEnv $ \ (k,v) ->
                    logT $ "setenv " <> k <> " = " <> v
                processPR step (setEnv <> env) pr
            PRAdditionalDownload w urls (CombineDownloads combine) -> do
                logS $ "PRAdditionalDownload " ++ T.unpack w
                sem <- MSem.new 20
                cs <- forM (zip [step..] urls) $ \ (s,u) -> async $ MSem.with sem $ do
                    logT $ "  " <> rmKeysFromUrl u
                    either (Left . (w <>)) Right
                        <$> urlGetContentsT'' ("-" <> show s) 0 Nothing (T.unpack u)
                cs <- mapM wait cs
                processPR (step + length urls) env $ combine cs
            PRRedirect r -> do
                logT "PRRedirect: "
                logT $ "  " <> rmKeysFromUrl r
                parseUrlT' env $ T.unpack r
            r@(PRFeed _ _ l) -> do
                logS $ "Feed with " ++ show (length l) ++ " posts"
                return r
            r ->
                return r
    processPR 1 env $ parse t (T.unpack u) dat

parseUrlTF :: URL -> FilePath -> IO ParseResult
parseUrlTF (T.pack -> u) fn = do
    (tr, u', post) <- transformURL u (const $ return Nothing)
    when (isJust u') $ fail "parseUrlTF: two urls requred"
    t <- getUrTime
    fmap (parse t (T.unpack u)) $ B.readFile fn

parseTrUrl :: TURL -> TURL -> IO ParseResult
parseTrUrl origU trU = do
    t <- getUrTime
    fmap (parse t (T.unpack origU)) $ urlGetContentsT "" (T.unpack trU)

-- | Пытается определить содержимое данных и разгребать
parse dlTime url = go (customParsers turl)
    where pt = parseTagsT . ensureUtf8Xml
          turl = T.pack url
          go [] dat = defaultParser turl (pt dat)
          go ((test, _, handler) : hs) dat
             | test turl =
                 case handler of
                     CPTags h -> h dlTime turl (pt dat)
                     CPBS h -> h dlTime turl dat
                     CPJSON h
--                        | Left e <- T.decodeUtf8' dat ->
--                              PRError $ T.pack $ "UTF-8 decode error: " ++ show e
--                        | otherwise
                         ->
                       case eitherDecodeJson dat of
                         Left e -> PRError $ "Can't decode JSON: " <> T.pack e
                         Right js -> h dlTime turl js
                     CPNone -> defaultParser turl (pt dat)
             | otherwise = go hs dat

------------------------------------------------------------------------------
-- Специальные парсеры

-- | Преобразование URL в подходящий для скачивания.
transformURL :: T.Text -> GetEnv -> IO TransformedUrl
transformURL url getEnv = go (customParsers url)
    where go [] = return (url, Nothing, Nothing)
          go ((test, transform, _) : ps)
             | test url  = transform url getEnv
             | otherwise = go ps

transformedUrl url = do
    (url_', _, _) <- transformURL url (const $ return Nothing)
    return $ if url_' == "" then url else url_'
transformedUrlHostName =
    fmap hostNameFromUrlT . transformedUrl

-- | Используется ли специализированный парсер, ID-шкам сообщений которого
-- можно доверять.
hasCustomParser url
    | Just (_,_,m) <- find (\ (t,_,_) -> t url) (customParsers url)
    = case m of
        CPNone -> False -- просто замена URL, стандартный RSS/Atom парсер
        _ -> True
    | otherwise = False

{-# NOINLINE customParsersDomainsRegex #-}
customParsersDomainsRegex :: Regex
customParsersDomainsRegex = fromString $ T.unpack $
    (\ x -> T.concat ["(", T.intercalate "|" x, ")"]) $
    map (T.replace "." "\\.") $ concat customParsersDomains

customParsers url
    | regexTest customParsersDomainsRegex url = concat customParsers'
      -- делаем предварительную проверку, чтобы не бегать лишний раз по всему
      -- списку
    | otherwise = []

(customParsersDomains, customParsers') =
    unzip
    [ Parser.Disqus.customParsers
    , Parser.Facebook.customParsers
    , Parser.Habr.customParsers
    , Parser.LiveJournal.customParsers
    , Parser.LtU.customParsers
    , Parser.Reddit.customParsers
    , Parser.Telegram.customParsers
    , Parser.Twitter.customParsers
    , Parser.VK.customParsers
    , Parser.YouTube.customParsers
    , otherCustomParsers
    ]

otherCustomParsers =
    mkCustomParsers
    ["www.google.com", "www.google.com/news", "news.google."]
    [ ( (oldGoogleNewsPrefix `T.isPrefixOf`), fixOldGoogleNews, CPNone )
    , ( ("http://news.google." `T.isPrefixOf`), mkHttps, CPNone )
    , ( ("http://www.google.com/reader" `T.isPrefixOf`), noScan, CPTags emptyHtml)
      -- все редиректятся на about, нет смысла сканировать
    ]
    where oldGoogleNewsPrefix = "https://www.google.com/news?"
          fixOldGoogleNews u _ = return
              (T.append "http://news.google.com/news/feeds?" $
               T.drop (T.length oldGoogleNewsPrefix) u, Nothing, Nothing)

emptyHtml _ _ _ = PRHtml [] [] []

------------------------------------------------------------------------------
-- Утилиты

redownloadOptionsLastModified =
    foldr (<|>) Nothing .
    map (parseHttpTime . T.unpack <=< T.stripPrefix "If-Modified-Since: ")

testCloudFlare = withDownloaderSettings botDownloaderSettings $ \ d -> do
    let printHdr prefix (n, v) =
            B.putStrLn $ B.concat [prefix, CI.original n, ": ", v]
        r req = do
            mapM_ (printHdr "> ") $ C.requestHeaders req
            return req
    (dr,mbRdr) <- rawDownload r d "https://www.videogamer.com/rss/allupdates.xml" Nothing []
    print dr
    print mbRdr
    mapM_ (printHdr "< ") $ rdrHeaders $ fromJust mbRdr

-- по кругу перенаправляет
--
testCloudFlare_ = withDownloaderSettings botDownloaderSettings $ \ d -> do
    let url = "http://hentaifromhell.org/feed/"
        addCookieJar rdr r =
            return $ r { C.cookieJar = Just (rdrCookieJar rdr) }
        go add u = do
            putStrLn u
            (dr,mbRdr) <- rawDownload add d u Nothing []
            case dr of
                DROK d _ ->
                    return d
                DRRedirect r
                    | Just rdr <- mbRdr -> do
                        putStrLn "Status redirect"
                        go (addCookieJar rdr) r
                DRError e
                    | Just rdr <- mbRdr
                    , Just redir <- lookup "refresh" (rdrHeaders rdr)
                    , r <- relUri (B.unpack $ B.dropWhile (/= '/') redir) url
                    -> do
                        putStrLn "Header redirect"
                        go (addCookieJar rdr) r
                _ -> do
                    print dr
                    print mbRdr
                    fail "Don’t know what to do"
    go return url
