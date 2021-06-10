{-# LANGUAGE OverloadedStrings, ViewPatterns, RecordWildCards, TupleSections,
             BangPatterns, ScopedTypeVariables, TransformListComp
#-}
module OPML
    ( userOPML, subscriptionsOPML, opmlSubscriptions, opmlSubscriptionsS
    ) where

import Control.Monad
import Control.Applicative
import Data.List
import Data.Ord
import Data.Char
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Internal as B
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Generated.DataTypes
import Lib.UrTime
import Data.Maybe
import URL
import Riak
import Generated.RiakIO
import qualified Data.Map.Strict as Map
import qualified Data.Text.Encoding as T
import qualified Data.Text.ICU.Convert as ICU
import System.IO.Unsafe
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as B
import GHC.Exts
import Text.HTML.TagSoup hiding (parseTags, renderTags)
import Text.HTML.TagSoup.Fast
import qualified Codec.Archive.Zip as Z
import Discovery
import Lib.StringConversion
import UsageFlags
import Subscriptions
import Mailer

testOPML = T.putStr =<< userOPML False "1"

opmlFromBackup u = do
    u <- readUserFromStatsUsers u
    t <- getUrTime
    T.putStr =<< (subscriptionsOPML t $ uSubscriptions u)

userOPML :: Bool -> Key User -> IO T.Text
userOPML fromTheWeb key = do
    when fromTheWeb $ usageFlag key UFExportOPML
    u <- readUser' key
    t <- getUrTime
    subscriptionsOPML t $ uSubscriptions u

subscriptionsOPML :: UrTime -> [Subscription] -> IO T.Text
subscriptionsOPML t subscriptions = do
    ps <- cachedReadManyPostss $ map sUrl subscriptions
    return $ subscriptionsOPML' t ps subscriptions

subscriptionsOPML' :: UrTime -> [Maybe Posts] -> [Subscription] -> T.Text
subscriptionsOPML' t ps subscriptions =
    let header = T.concat
            [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n\
              \<opml version=\"1.0\">\n\
              \    <head>\n\
              \        <title>BazQux Reader subscriptions</title>\n\
              \        <dateCreated>"
            , T.pack $ formatUrTimeRfc822 t
            , "</dateCreated>\n\
              \    </head>\n\
              \    <body>\n" ]
        footer =
            "    </body>\n\
            \</opml>\n"
        subs = zipWith (\s p -> s { sTitle = sTitle s <|>
                                             (postsTitle =<< p) })
                       subscriptions ps
        topLevelSubs = filter (null . sFolders) subs
        subGroups =
            [ (the folder, sub)
            | sub <- subs, folder <- sFolders sub
            , then group by folder using groupWith
            ]
        outFeed (Subscription {..}) =
            renderTagsT [ TagOpen "outline"
                                  [("text", t), ("title", t), ("type", "rss"),
                                   ("xmlUrl", sUrl)]
                        , TagClose "outline" ]
            where t = fromMaybe sUrl sTitle
        outFeeds =
            map outFeed . sortBy (comparing $ maybe "" T.toLower . sTitle)
        ident = map (T.append "    ")
        outGroup (f,subs) =
            renderTagsT [ TagOpen "outline" [("text", f), ("title", f)] ] :
            ident (outFeeds subs) ++ ["</outline>"]
        outGroups =
            concatMap outGroup $ sortBy (comparing $ T.toLower . fst) subGroups
    in T.concat $
           [ header
           , T.unlines $ ident $ ident $ outFeeds topLevelSubs ++ outGroups
           , footer]

-- | Decode XML to UTF-8 using @encoding@ attribute of @\<?xml\>@ tag.
ensureUtf8Opml :: B.ByteString -> B.ByteString
ensureUtf8Opml s
    | Right _ <- T.decodeUtf8' s = s
      -- first of all we try decode utf-8.
      -- Some sites specify non utf-8 encoding, while the text is in utf.
    | otherwise =
    case dropWhile isText $ parseTags s of
        --         ^ sometimes there is a space or junk text before <?xml>
        (TagOpen "?xml" attrs : TagClose _ :
         TagOpen "opml" _ : TagText _ : TagOpen "head" _ : TagOpen "title" _ :
         TagText "Slick RSS OPML Export"  : _)
            | Just "ISO-8859-1" <- lookup "encoding" attrs ->
                toUnicode "windows-1251"
        (TagOpen "?xml" attrs : _)
            | Just enc <- lookup "encoding" attrs
            , B.map toLower enc /= "utf-8" ->
                toUnicode $ fixEnc enc
        _ -> s
    where toUnicode enc =
              unsafePerformIO $
                  (do -- print ("asdf", fixEnc enc, "qwer")
                      c <- ICU.open (B.unpack enc) Nothing
                      return $! T.encodeUtf8 $ ICU.toUnicode c s)
                  `E.catch`
                  \ (e :: E.SomeException) -> do
                      -- print e
                      return s
                    -- in case of errors try process as utf-8
          isText (TagText _) = True
          isText _ = False
          -- http://www.w3.org/TR/2009/WD-html5-20090825/infrastructure.html#character-encodings-0
          fixEnc enc =
              fromMaybe enc $
              lookup (B.map toLower enc)
              [ e "x-sjis"      "windows-31J"
              , e "windows-932" "windows-31J"
              , e "x-x-big5"    "Big5"
              , e "EUC-KR"      "windows-949"
              , e "GB2312"      "GBK"
              , e "GB_2312-80"  "GBK"
              , e "ISO-8859-1"  "windows-1252" -- но это не 1251
              , e "ISO-8859-9"  "windows-1254"
              , e "ISO-8859-11" "windows-874"
              , e "KS_C_5601-1987"      "windows-949"
              , e "Shift_JIS"   "windows-31J"
              , e "TIS-620"     "windows-874"
              , e "US-ASCII"    "windows-1252" ]
              where e from to = (B.map toLower from, to)


testOPMLSubs =
    T.putStr =<< subscriptionsOPML (UrTime 0 0) . opmlSubscriptionsS
        =<< B.readFile "subscriptions.opml"
          -- "/Users/volodya/Downloads/google-reader-subscriptions.xml"

writeImportData user ext dat = return ()
-- writeImportData user ext dat = do
--     createDirectoryIfMissing True path
--     B.writeFile (path ++ "/" ++ encodeURIComponent (T.unpack user) ++ ext) dat
--     where path = "user_imports"

importGoogleReaderSubscriptions :: Key User -> [Subscription] -> IO ()
importGoogleReaderSubscriptions user s = do
    t <- getUrTime
    let opml = subscriptionsOPML' t (repeat Nothing) s
    writeImportData user ".xml" (T.encodeUtf8 opml)
    userAddSubscriptions user s
    -- слито, дабы лишний раз не кидать список подписок из urweb и обратно
    userEvent user ((if null s then T.append "Empty" else id) "Import") $
        T.concat
        [ "Subscriptions: ", T.pack (show $ length s), "\n\n"
        , maskUrlPasswordsInOpml $ tbs opml ]

opmlSubscriptions :: B.ByteString -> T.Text -> IO ()
opmlSubscriptions blob0 user = do
    usageFlag user UFOPML
    blob <- do
        s <- extractSubscriptionsXml blob0
        case s of
            Just s -> do
                writeImportData user ".zip" blob0
                -- Google Takeout тоже сохраняем
                return s
            Nothing -> return blob0
    writeImportData user ".xml" blob
    let s = opmlSubscriptionsS blob
    userAddSubscriptions user s
    userEvent user ((if null s then T.append "Empty" else id) "OPML") $
        T.concat
        [ "Subscriptions: ", T.pack (show $ length s), "\n\n"
        , maskUrlPasswordsInOpml blob ]

maskUrlPasswordsInOpml o
    | any badTag (parseTagsT o) =
      -- параноидально проверяем наличие запароленных URL не только в самих
      -- адресах фидов но и в htmlUrl, чтобы нигде не видеть пароли
      T.unlines
      [ "Some feed URLs has passwords and were masked"
      , subscriptionsOPML' (UrTime 0 0) (repeat Nothing) $
        map mask $ opmlSubscriptionsS o ]
    | otherwise = bst o
    where badTag (TagOpen _ as) = any badAttr as
          badTag _ = False
          badAttr (n,v) = n `elem` ["xmlurl", "htmlurl"] && urlHasAuthT v
          mask s = s { sUrl = maskUrlPasswordT (sUrl s) }

extractSubscriptionsXml dat = findSubscriptions `E.catch` exc
    where exc :: E.SomeException -> IO (Maybe B.ByteString)
          exc _ = return Nothing
          findSubscriptions =
              case find (isInfixOf "subscriptions.xml" . Z.eRelativePath) $
                   Z.zEntries $ Z.toArchive $ BL.fromStrict dat of
                  Just e | Z.eUncompressedSize e < 10000000 ->
                      let !r = BL.toStrict $ Z.fromEntry e in
                      return (Just r)
                  _ ->
                      return Nothing

opmlSubscriptionsS :: B.ByteString -> [Subscription]
opmlSubscriptionsS = go Nothing [] Map.empty . parseTagsT . ensureUtf8Opml
    where checkTitle _ Nothing = False
          checkTitle readers (Just r) = any (`T.isInfixOf` T.toLower r) readers
          go r folders acc [] = Map.elems acc
          go Nothing !f !acc (TagOpen "title" _ : TagText t : ts) =
              go (Just t) f acc ts
          go !r !f !acc (TagOpen "outline" atts : ts)
           --  | Just "rss" <- lookup "type" atts
             -- <title>:
             -- "RSS-потоки из Яндекс.Ленты"
             -- "Blogtrottr OPML export"
             -- "The Old Reader" -- вырезать subscriptions
             --
             -- TODO: Надо определять yandex/blogtrottr и использовать title,
             -- у всех остальных text более приоритетен.
             -- Также делать trim.
             --
             -- yandex:
             --    title название и text=feed.description,
             --    и description=feed.description
             --    в названиях папок только text
             -- Blogtrottr:
             --    title название и text=feed.description
             --    в названиях папок только text
             -- vienna:
             --    text название и description=feed.description
             --    в названиях папок только text
             -- Feeds subscribed by email@example.com ?? :
             --    text название, title исходное название
             -- feedly, newsblur, feeddler, я, google reader, digg, comma feed,
             -- inoreader, feedspot, g2reader:
             --    text и такой же title
             -- FeedDemon, FeedShow OMPLBuilder:
             --    text и такой же title
             --    в названиях папок только text
             -- livejournal, TT-RSS, outlook, QuiteRSS, Slick RSS, NewsFox,
             -- RSSOwl: text
             -- netvibes: title
             --
             | Just (T.strip -> t) <-
                (if checkTitle ["яндекс", "blogtrottr"] r then
                     lookup "title" atts <|> lookup "text" atts
                 else
                     lookup "text" atts <|> lookup "title" atts)
                 <|>
                 return "" -- у The Old Reader теперь нет title
             , Just u <- lookup "xmlurl" atts <|> lookup "htmlurl" atts
             , u /= "" =
                 go r ("" : f)
                    (Map.insertWith
                        (\a b -> a { sFolders = union (sFolders a) (sFolders b)
                                   })
                        u
                        (Subscription
                         { sUrl        = u
                         , sState      = SSAdded
                         , sEditsCount = 0
                         , sTitle      =
                             if t /= "" && t /= u then Just t else Nothing
                         , sFolders    =
                             let outIf reader what
                                     | checkTitle [reader] r =
                                         filter (`notElem` what)
                                     | otherwise = id
                             in
                             outIf "feedly"
                                 ["feedly.others"] $
                             outIf "the old reader"
                                 ["Subscriptions", "訂閱"] $
                             outIf "blogtrottr"
                                 ["Subscriptions"] $
                             outIf "netvibes"
                                 ["Без рубрики"] $
                             outIf "the rss aggregator (google chrome extension"
                                 ["Feeds"] $
                             filter (/= "") f
                         })
                        acc) ts
          go r f acc (TagOpen "outline" atts : ts)
             | Just t <- lookup "text" atts <|> lookup "title" atts
             , Just "rss" /= lookup "type" atts
                 = go r (t : f) acc ts
          go r f acc (TagOpen "outline" atts : ts) = go r ("" : f) acc ts
          go r (_:f) acc (TagClose "outline" : ts) = go r f acc ts
          go r f acc (_ : ts) = go r f acc ts
