{-# LANGUAGE ViewPatterns, RecordWildCards, BangPatterns, OverloadedStrings #-}
-- | Разбор ЖЖ
module Parser.LiveJournal
    ( customParsers
    , preprocessLJ, parseLJ
    ) where

import Data.Maybe
import Data.Char
import Network.HTTP.Conduit.Downloader
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as B
import qualified Data.Aeson as JSON
import qualified Data.HashMap.Lazy as HM
import qualified Data.Vector as V
import URL
import Parser.Types
import Parser.Custom
import Lib.UrTime
import Lib.ReadUtils
import Lib.Regex
import Lib.StringConversion
import Text.HTML.TagSoup hiding (parseTags, renderTags, (~/=))
import Text.HTML.TagSoup.Fast
import Control.Applicative
import System.Random

customParsers =
    mkCustomParsers ["livejournal.com"]
    [ ljNoJSON "livejournal\\.com/[0-9]+.html.*nojson"
    , ljNoJSON "users\\.livejournal\\.com/.*/[0-9]+.html.*nojson"
    , ljNoJSON "livejournal\\.com/[0-9]+.html.*thread"
    , ljNoJSON "users\\.livejournal\\.com/.*/[0-9]+.html.*thread"
    , ljPage "https?://[^\\.]+.livejournal\\.com/[0-9]+.html.*page=[0-9]+"
    , ljPage "https?://users\\.livejournal\\.com/.*/[0-9]+.html.*page=[0-9]+"
    , lj "livejournal\\.com/[0-9]+.html"
    , lj "users\\.livejournal\\.com/.*/[0-9]+.html"
    , ljRss "livejournal\\.com/?$"
    , ljRss "users\\.livejournal\\.com/[^/]+/?$"
    , ( rt "livejournal\\.com", mkHttps, CPNone )
    ]
    where ljNoJSON t = (rt t, changeUrlIO transformLJHtmlURL,
                        CPTags $ \ _ u -> preprocessLJ u . parseLJ u)
          lj t = (rt t, transformLJURL, CPJSON handlePageAndLJJSON)
          ljPage t = (regexTest t, changeUrlIO transformLJPageUrl,
                      CPJSON handleLJJSON)
          ljRss t = (rt t, noScan,
                  CPTags $ \ _ url _ -> PRRedirect $ httpToHttps $ addSlash url `T.append` "data/rss")
          addSlash "" = ""
          addSlash x
              | T.index x (T.length x - 1) == '/' = x
              | otherwise = T.append x "/"


ljGetRpcUrl user1 user postId page =
    T.concat ["https://", user1, ".livejournal.com/", user, "/__rpc_get_thread?journal=", user, "&itemid=", postId, "&flat=&skip=&page=", page, "&expand_all=1"]
     -- user1 может быть равен users.

transformLJPageUrl' u
    | [[_, user, postId, page]] <- regexGet "https?://([^\\.]+).livejournal\\.com/([0-9]+).html.*page=([0-9]+)" u =
        ljGetRpcUrl user user postId page
    | [[_, user, postId, page]] <- regexGet "https?://users\\.livejournal\\.com/([^/]+)/([0-9]+).html.*page=([0-9]+)" u =
        ljGetRpcUrl "users" user postId page
    | [[_, user, postId]] <- regexGet "https?://([^\\.]+).livejournal\\.com/([0-9]+).html" u =
        ljGetRpcUrl user user postId "1"
    | [[_, user, postId]] <- regexGet "https?://users\\.livejournal\\.com/([^/]+)/([0-9]+).html" u =
        ljGetRpcUrl "users" user postId "1"
    | otherwise = u
transformLJPageUrl u = do
    UrTime s us <- getUrTime
    return $ T.concat [transformLJPageUrl' u, "&_=", T.pack (show s), "000"]
transformLJURL u _ = do
    html <- transformLJHtmlURL u
    page <- transformLJPageUrl u
    return ( html, Just (page,
                         \ a b -> B.concat ["[", jsonString a, ",", b, "]"])
           , Nothing)
    where jsonString = BL.toStrict . JSON.encode . JSON.String . bst
transformLJHtmlURL (T.replace "&nojson" "" -> url) = do
    -- для livejournal необходимо добавлять nojs=1&format=light, в котором
    -- мы разгребаем, а также некоторый случайный ключ, чтобы не
    -- было лишнего кеширования
    n <- randomRIO (100000, 999999) :: IO Int
    return $ url' <> "nojs=1&format=light&updplz" <> T.pack (show n)
           --  ^ из-за random не работает кеширование,
           -- однако, с format=light (или с каким-нить format=light&asdf)
           -- ЖЖ слишком мощно кеширует и новые сообщения могут появиться
           -- только через сутки
           -- <> T.pack (show $ hashString $ T.unpack url)
    where url' = url <> (if isJust $ T.find (== '?') url then "&" else "?")


handlePageAndLJJSON t (httpToHttps -> u) (JSON.Array (V.toList -> [JSON.String html, json]))
    | pr@(PRParsedHtml _ NoSwitch _ jsonComments) <- handleLJJSON t u json =
        if length jsonComments == 0 && length _comments /= 0 then
            (preprocessLJ u p)
            -- может быть пустой json, а комментарии при этом есть
            { prSwitchUrl = Switch False (u <> "&nojson") }
        else
            pr
            { prMsgText = postMsgText
            , prThreadLinks =
                [(Nothing, pageUrl n, Nothing) | n <- [2..pagesCount]]
            }
    | pr@(PRParsedHtml _ (Switch _ u') _ _) <- handleLJJSON t u json =
        (preprocessLJ u p)
        { prSwitchUrl = Switch False u' }
        -- Чтобы сразу не перескачивать то, что уже было, делаем SwitchRescan
        --
        -- жж c нестандартным оформлением страницы выдает
        -- [{"thread":"…", "html":"<div ….>"}, …]
        -- вместо нормального json, причем некоторые пункты collapsed.
        -- для таких страниц используем старый парсер,
        -- а также переключаем страницу в nojson и остальные страницы
        -- добавляем с таким же режимом.
    where p@(pagesCount, postMsgText, _comments) =
              parseLJ u $ parseTagsT $ T.encodeUtf8 html
          postUrl = T.takeWhile (/= '?') u
          pageUrl n = postUrl <> "?page=" <> T.pack (show n)
handlePageAndLJJSON _ _ _ = PRError "Strange JSON?"

handleLJJSON _ (httpToHttps -> u) (JSON.Object (HM.lookup "comments" ->
                               Just (JSON.Array (V.toList -> cs)))) =
    PRParsedHtml Nothing NoSwitch (catMaybes $ map c cs) []
    where c x = do
            JSON.Object o <- return x
            dname <- str "dname" o
            subject <- str "subject" o <|> Just ""
            article <- str "article" o <|> Just ""
            ctime_ts <- int "ctime_ts" o
            guid <- sint "dtalkid" o
            userpic <- str "userpic" o <|> Just "https://l-stat.livejournal.com/img/userpics/userpic-user.png?v=15821"
            JSON.Array (V.toList -> n) <- HM.lookup "username" o
            journal_url <- listToMaybe $ catMaybes $ flip map n $ \ n -> do
                JSON.Object un <- return n
                str "journal_url" un
            return
                (sint "parent" o,
                 defaultFeedMsg
                 { fmGuid = guid
                 , fmAuthor = dname
                 , fmSubject = subject
                 , fmPublishedTime = Just $ UrTime (fromEnum ctime_ts) 0
                 , fmBody = article
                 , fmLinks =
                     [ (LKLink,
                        T.concat [postUrl, "?thread=", guid, "#t", guid])
                     , (LKAuthor, journal_url)
                     , (LKAuthorPic, userpic) ]
                 })
          postUrl = T.takeWhile (`notElem` ("?&" :: [Char])) $ u
          sint n x = fmap (T.pack . show) $ int n x
handleLJJSON _ u (JSON.Object (HM.lookup "error" ->
                               Just (JSON.Array (V.toList -> [])))) =
    -- бывает {"error":[]}, если нет комментариев
    PRParsedHtml Nothing NoSwitch [] []
handleLJJSON _ u (JSON.Array (V.toList -> [])) =
    -- бывает [], если нет комментариев?
    PRParsedHtml Nothing NoSwitch [] []
handleLJJSON _ u _ = -- PRError "Strange JSON?"
    PRParsedHtml Nothing (Switch True $ u <> "&nojson") [] []
-- "subject" "article" -- нет, если deleted
-- "dname" -- пользователь
-- "journal_url" -- link
-- "ctime_ts" -- int, время
-- "dtalkid" -- int, guid
-- "parent" -- int, parent guid, не у всех
-- "above" -- int, сосед сверху на том же уровне
-- "userpic" -- есть не у всех, вместо него https://l-stat.livejournal.com/img/userpics/userpic-user.png?v=15821

testLJ = mapM_ printLJ . (\ (_,_,c) -> c)
         -- mapM_ print . prThreadLinks . preprocessLJ (T.pack url)
         . parseLJ url
         . parseTagsT . ensureUtf8Xml
         =<< urlGetContents (T.unpack url)
    where url =
              "http://tema.livejournal.com/1115967.html?nojs=1&format=light&updplz=123123"

data LJMsgKind
    = LJNormal
    | LJShort
    | LJDeleted
    deriving (Show, Eq)
data LJMsg
    = LJMsg
      { ljmUserPic :: Maybe Text
      , ljmSubject :: Text
      , ljmAuthor :: Text
      , ljmAuthorUri :: Text
      , ljmTime :: Maybe UrTime
      , ljmText :: Text
      , ljmKind :: LJMsgKind
      }
    deriving Show

printLJ (w, n, LJMsg {..}) = do
    print (w, n, ljmKind)
    put (fromMaybe "" ljmUserPic)
    put ljmSubject
    put ljmAuthor
    put ljmAuthorUri
    print ljmTime
    put ljmText
    where put = B.putStrLn . T.encodeUtf8

preprocessLJ :: Text
             -> (Int, Maybe Text, [(Int, Guid, LJMsg)])
             -> ParseResult
preprocessLJ url (pagesCount, postMsgText, comments) = go [] [] [] comments
    where go a b c d | a `seq` b `seq` c `seq` False = undefined
          go msgs links _ [] =
              PRParsedHtml postMsgText NoSwitch
                  (reverse [(p,m) | (p,d,m) <- msgs])
                  (reverse links ++
                   if "page=" `T.isInfixOf` url then []
                   else [(Nothing, pageUrl n, Nothing) | n <- [2..pagesCount]])
          go msgs links depthes
                 (m@(depth, guid, LJMsg {ljmKind = LJShort, ..}) : xs)
              | depth > 0 = findCommonParent msgs
                --  ^ попался свернутый на нулевом уровне?
                -- добавим как есть
              where ((m:) -> collapsed, normal) = span isCollapsed xs
                    isCollapsed (_, _, LJMsg {ljmKind = LJNormal}) = False
                    isCollapsed (d, _, _) = d > 0
                    guidsAndTimes =
                        [ (guid, ljmAuthor, ljmTime)
                        | (_, guid, LJMsg {..}) <- collapsed ]
                    minCollapsedDepth = minimum [d | (d,_,_) <- collapsed]
                    findCommonParent ((p,d,m) : ms)
                        | d >= minCollapsedDepth = findCommonParent ms
                        | otherwise =
                            go msgs
                               -- предыдущие родительские сообщения оставляем,
                               -- чтобы сразу появились,
                               -- а линк на детей добавляем только самый общий
                               (( p
                                , threadUrl (fmGuid m) <> "&nojson"
                                , Just guidsAndTimes) : links)
                               (addDepth d (fmGuid m) depthes)
                               normal
                    findCommonParent [] = error "findCommonParent: []?"
          go msgs links depthes ((depth, guid, LJMsg {..}) : xs) =
              go ((mkParent depthes depth, depth,
                   defaultFeedMsg
                   { fmGuid = guid
                   , fmAuthor = ljmAuthor
                   , fmSubject = ljmSubject
                   , fmPublishedTime = ljmTime
                   , fmBody = ljmText
                   , fmLinks =
                       [ (LKLink, threadUrl guid <> "#t" <> guid)
                       , (LKAuthor, ljmAuthorUri)
                       ]
                       ++ maybe [] (\ p -> [(LKAuthorPic, p)]) ljmUserPic
                   }) : msgs)
                 links (addDepth depth guid depthes) xs
          addDepth d g ds = (d, g) : filter ((< d) . fst) ds
                            -- убираем все более вложенные глубины
          mkParent _ 0 = Nothing
          mkParent depthes depth
              | Just g <- lookup (depth-1) depthes = Just g
              | otherwise = mkParent depthes (depth-1)
                -- иногда попадаются комментарии с удвоенным отступом
                -- видимо, результат удаления предыдущего коммента,
                -- по-этому, линкуем их повыше
--               Guid $ fromMaybe
--               (error $ "depth " ++ show depth ++ " not found? " ++ show depthes)
--               (lookup depth depthes)
          postUrl = T.takeWhile (`notElem` ("?&" :: [Char])) url
          threadUrl guid = postUrl <> "?thread=" <> guid
          pageUrl n = postUrl <> "?page=" <> T.pack (show n) <> "&nojson"

getLJthread url = get $ T.breakOn -- B.breakSubstring
                  prefix url
    where get (_, "") = Nothing
          get (_, s) = Just $ T.takeWhile isDigit $ T.drop (T.length prefix) s
          prefix = "thread="

parseLJ url = getPages
    where skipTillThread c = case getLJthread url of
              -- новый жж выдает нитку от корня, а не от заданной в thread
              -- посему пропускаем родителей до нашего сообщения
              Just thread -> find c
                  where find [] = [] -- ???
                        find c'@((d, cmtid, msg) : cs)
                             -- нашли наше сообщение и берем его и соседей,
                             -- уменьшая глубину
                            | cmtid == thread = slice d c'
                            | otherwise       = find cs
                        slice d [] = []
                        slice d ((d', cmtid, msg) : cs)
                            | d' < d    = [] -- еще одна нить выше уровнем?
                            | otherwise = (d' - d, cmtid, msg) : slice d cs
              Nothing -> c
          getPages x = (pagesCount c, postMsgText x, skipTillThread $ go [] c)
              where c :: [Tag Text]
                    c = dropWhile (~/= TagOpen "div" [("id","comments")]) x
          postMsgText x
              | "thread=" `T.isInfixOf` url ||  -- отдельная нить
                "page=" `T.isInfixOf` url -- на страницах "Читать дальше"
                = Nothing
              | (_:xs) <- dropWhile (not . articleStart) x
                = goText [] xs
              | otherwise = Nothing
          articleStart (TagOpen "article" (lookup "class" -> Just c)) =
              "b-singlepost-body" `T.isInfixOf` c
          articleStart _ = False
          goText _ [] = Nothing
          goText !acc (TagClose "article" : _) = rmLastDiv 0 acc
          goText !acc (TagOpen "div" [("class", ("b-singlepost-tags" `T.isInfixOf`) -> True)] : _) = rmLastDiv 1 acc
          goText !acc (TagOpen "div" (lookup "id" -> Just "comments") : _)
              = rmLastDiv 2 acc
          goText !acc (t:ts) = goText (t:acc) ts
          pagesCount =
              countPageAhrefs 0 .
              takeWhile (/= TagClose "ul") .
              dropWhile (~/= TagOpen "ul" [("class","b-pager-pages")]) .
              takeWhile (~/= TagOpen "div" [("class","b-tree b-tree-root")])
          countPageAhrefs n (TagOpen "a" [("href", href)] : TagText t : xs)
                  = countPageAhrefs (n+1) xs
          countPageAhrefs n (_ : xs) = countPageAhrefs n xs
          countPageAhrefs n [] = n
          cmtClassInfix = "b-tree-twig-"
          ljUserSpan (TagOpen _ (lookup "class" -> Just "b-leaf-username")) = True
          ljUserSpan _ = False
          ljUserPicSpan (TagOpen _ (lookup "class" -> Just "b-leaf-userpic-inner")) = True
          ljUserPicSpan _ = False
          rmid (TagOpen t (("id",_):atts):ts) = TagOpen t atts : ts
          rmid x = x
          go !acc [] = reverse acc
          go !acc (TagOpen "div" [("class", cls),
                                 ("style", _),
                                 ("data-tid", tid)] : cmt)
              | cmtClassInfix `T.isInfixOf` cls
              , [(depth1, _)] <- reads $ T.unpack $
                                 T.dropWhile (not . isDigit) cls
              , depth <- depth1 - 1
              , "t" `T.isPrefixOf` tid
              , cmtId <- T.drop 1 tid
              = case rmid $ dropWhile (~/= TagOpen "div" []) cmt of
              TagOpen "div" (("class", (T.words -> ("b-leaf" : _)))
                             : atts) : xs
                  | ("data-full", "1") `elem` atts ->
                  -- полноценный коммент (возможно свернутый)
                  let narticleDiv = (~/= TagOpen "div" [("class","b-leaf-article")])
                      nfooterDiv = (~/= TagOpen "div" [("class","b-leaf-footer")])
                      (header, body) =
                          span (\ t -> narticleDiv t && nfooterDiv t)
                               xs
                      ((dropLastTwo . drop 1) -> text, next) =
                          --  ^ </div>\n
                          span nfooterDiv body
                      emptyText (TagText t) = T.all isSpace t
                      emptyText _ = False
                      (ljmSubject, ljmText) = case dropWhile emptyText text of
                          TagOpen "h4" [("class", "b-leaf-subject")]
                              : TagText s : TagClose "h4" : xs ->
                                  (s, renderTagsT xs)
                          xs -> ("", renderTagsT xs)
                      ljmUserPic = case dropWhile (not . ljUserPicSpan) xs of
                                     _ : _ : TagOpen "img" (("src", upic):_) : _ ->
                                         Just upic
--                                     xs -> Just (T.pack $ show $ take 10 xs)
                                     _ -> Nothing
                      userInfo = dropWhile (not . ljUserSpan) xs
                      (ljmAuthor, ljmAuthorUri) = getAuthor userInfo
                      ljmTime = getTime atts
                      ljmKind = LJNormal
                  in
                      go ((depth, cmtId, LJMsg {..}) : acc) next
              TagOpen "div" (("class",
                              (T.words -> ("b-leaf" : "b-leaf-collapsed" : _))
                              ):atts) : xs ->
                  -- сокращенный коммент с expand
                  let ljmSubject = ""
--                           | subj == "(no subject)" = ""
--                           | otherwise              = subj
                      userInfo = dropWhile (not . ljUserSpan) xs
                      (ljmAuthor', ljmAuthorUri) = getAuthor userInfo
                      ljmTime = getTime atts
                      (ljmKind, ljmAuthor) =
                          case userInfo of
                              TagOpen _ atts : _
                                  | Just s <- lookup "style" atts
                                  , "line-through" `T.isInfixOf` s
                                  ->
                                   (LJDeleted, "<s>" <> ljmAuthor' <> "</s>")
                              _ -> (LJShort, ljmAuthor')
                      ljmText = ""
                      ljmUserPic = Nothing
                  in
                  go ((depth, cmtId, LJMsg {..}) : acc) xs
              TagOpen "div" (("class",
                              (T.words -> ("b-leaf" : "b-leaf-seemore" : _))
                              ):atts) : xs
                  | Just ids <- lookup "data-dtalkids" atts ->
                  -- …and 2 more comments…
                  -- …go 9 levels up… -- у них пустые ids
                  go ([(depth, id, LJMsg Nothing "" "" "" Nothing "" LJShort)
                       | id <- T.splitOn ":" ids ] ++ acc) xs
              TagOpen "div" (("class",
                              (T.words -> ("b-leaf" : "b-leaf-clipped" : _))
                              ):atts) : rest -> do
                  -- t == "(Deleted post)"
                  let t = case rest of
                            _ : _ : _ : _ : _ : _ : TagText t : next -> t
                            _ -> "(Deleted post)"
                  go ((depth, cmtId,
                       LJMsg Nothing t "" "" Nothing "" LJDeleted) : acc) rest
                      -- TODO: вот тут бы время этого поста подкрутить,
                      -- чтобы порядок не менялся
              TagOpen "div" (("class",
                              (T.words -> ["b-leaf"])
                              ):_) : xs ->
                  -- странный, пустой комментарий (Anonymous, 42 years ago)
                  -- похоже, это из-за кривой ссылки
                  -- (если смотреть залогиненным, то просто ошибка выдается),
                  -- а ссылка из-за косяка в туннелировании -- как раз
                  -- comicstrip с virtul-ом перепутались.
                  go acc xs
              xs -> error $ "Non-exhaustive: " ++ show (renderTagsT $ take 10 xs)
          go !acc (x : xs) = go acc xs
          getAuthor header
              | TagOpen _ _ : TagOpen _ _ : profile <-
                  dropWhile (~/= TagOpen "a" []) header
              , TagOpen _ atts : TagOpen "b" _ : TagText a : _ <-
                  dropWhile (~/= TagOpen "a" []) profile
              , Just uri <- lookup "href" atts
              = (a, uri)
              | otherwise = ("", "")
              -- ["" | TagText "(Anonymous)" <- header]
          getTime atts
              | Just t <- lookup "data-updated-ts" atts =
                          fmap (\ s -> UrTime s 0) $ tryReadUnsignedInt t
              | otherwise = Nothing
--               case dropWhile (~/= TagOpen "span" [("class","b-leaf-createdtime")])
--                    header of
--                   _ : TagText time : _ ->
--                       readLJTime $ T.unpack time
--                   _ -> Nothing

rmLastDiv 0 acc = Just $ renderTagsT $ reverse acc
rmLastDiv n (TagOpen "div" _ : ts) = rmLastDiv n ts
rmLastDiv n (TagClose "div" : ts) = rmLastDiv (n-1) ts
rmLastDiv n (TagText (T.all isSpace -> True) : ts) = rmLastDiv n ts
rmLastDiv _ ts = rmLastDiv 0 ts

readLJTime = tryParseTime
    [ "%B %e %Y, %H:%M:%S %Z" -- February 22 2012, 07:19:12 UTC
    , "%Y-%m-%d %H:%M %P %Z" -- 2011-08-15 06:33 pm UTC
    ]

-- -- У ltu, как и у ЖЖ фида комментов нет, зато есть
-- -- <div style="margin-left:25px;">
-- -- <a id="comment-57779"></a>
-- -- <div class="comment">
