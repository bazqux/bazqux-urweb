{-# LANGUAGE ViewPatterns, RecordWildCards, OverloadedStrings, RankNTypes #-}
module Payments.FastSpring
    ( buyPage, invoiceLink, processPayment, parseOrderNotification, productInfo
    , parseOrderNotificationNew
    , FastSpringOrder(..), tryDownloadFastSpringOrder, downloadFastSpringOrder
    ) where

import Control.Monad
import Control.Applicative
import Control.Concurrent.Async
import qualified Control.Exception as E
import Data.List
import Data.Scientific (Scientific)
import qualified Network.HTTP.Client as C
import qualified Data.Default as C
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Generated.DataTypes
import Lib.UrTime
import Lib.Log
import Lib.StringConversion
import Data.Maybe
import URL
import Riak
import Generated.RiakIO
import qualified Data.HashMap.Strict as HM
import System.IO.Unsafe
import Control.Concurrent
import Network.HTTP.Conduit.Downloader
import Network.HTTP.Types (notFound404, unauthorized401)
import Lib.Hash
import Lib.ReadUtils
import qualified Lib.ElasticSearch as ES
import Text.HTML.TagSoup hiding (parseTags, renderTags)
import Text.HTML.TagSoup.Fast
import Data.Aeson
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import qualified Data.Vector as V
import Parser.Custom (jsonParser)
import qualified Data.ByteString.Base64 as Base64
import qualified Crypto.MAC.HMAC as HMAC
import Config

buyPage pid0 userId email = do
    u <- cachedReadUser' userId
    let pid =
            (if uPayments u /= [] then
                T.replace "reader-lifetime" "readerlifetime" .
                T.replace "readeryear" "reader"
                -- повторные покупки называем по-другому
             else
                id)
            $ T.replace "reader_lifetime" "reader-lifetime" pid0
    return $
        "<head>\n\
        \  <script\n\
        \    id='fsc-api'\n\
        \    src='https://d1f8f9xcsvx3ha.cloudfront.net/sbl/0.8.2/fastspring-builder.min.js'\n\
        \    type='text/javascript'\n\
        \    data-storefront='bazqux.onfastspring.com'>\n\
        \  </script>\n\
        \</head>\n\
        \<body style='background-color: #E7E7E7'>\n\
        \  <script>\n\
        \    document.addEventListener('DOMContentLoaded', function() {\n\
        \        fastspring.builder.push({\n\
        \            'reset':true,\n\
        \            'products' : [{\n\
        \                'path': '" <> pid <> "',\n\
        \                'quantity': 1\n\
        \            }],\n\
        \            'tags' : {\n\
        \                'referrer': '" <> userId <> "'\n\
        \            },\n\
        \            'checkout': true\n\
        \            " <> maybe "" (\e -> ",'paymentContact' : { 'email' : '" <> eaEmail e <> "' }") email <> "\n\
        \        })\n\
        \    })\n\
        \  </script>\n\
        \</body>\n"

invoiceLink oid
    | oid > "BAZ200306" =
        "https://bazqux.onfastspring.com/account/order/" <> oid <> "/invoice"
    | otherwise =
        "https://sites.fastspring.com/bazqux/order/invoice/" <> oid

productInfo :: T.Text -> T.Text -> Maybe (UrTime -> UrTime, T.Text, Scientific)
productInfo oid product = do
    case product of
        "readeryear9"  -> yearSubscription  9 False
        "readeryear19" -> yearSubscription 19 False
        "readeryear"   -> yearSubscription 29 False
        "readeryear30" -> yearSubscription 30 False
        "readeryear50" -> yearSubscription 50 False
        "reader9"  -> yearSubscription  9 True
        "reader19" -> yearSubscription 19 True
        "reader"   -> yearSubscription 29 True
        "reader30" -> yearSubscription 30 True
        "reader50" -> yearSubscription 50 True
        "reader-lifetime" -> lifetimeSubscription
        "reader_lifetime" -> lifetimeSubscription
        "readerlifetime"  -> lifetimeSubscription
        "reader-lifetime249" -> lifetimeSubscription' 249
        "reader_lifetime249" -> lifetimeSubscription' 249
        "readerlifetime249"  -> lifetimeSubscription' 249
        "reader2months" ->
            Just ((`plusUrTime` (60*24*3600)), "BazQux Reader 2 months subscription", 4.99)
        "reader2years" ->
            Just (urTimeAddYear . urTimeAddYear, "BazQux Reader 2 years subscription", 49)
        _  ->
            Nothing
    where yearSubscription price renewal = Just
              (urTimeAddYear
              ,"BazQux Reader year subscription ($" <> showT price
               <> if renewal then ", renewal)" else ")", fromIntegral price)
          lifetimeSubscription = lifetimeSubscription' ltPrice
          lifetimeSubscription' p = Just
              ((!! 100) . iterate urTimeAddYear
              ,"BazQux Reader Lifetime Subscription ($"<>showT p<>")", fromIntegral p)
          ltPrice
              | oid > "BAZ151218" = 149
              | otherwise = 99

-- У FastSpring в паре заказов был глюк,
-- когда вместо @gmail.com пришел %40gmail.com
-- Пользователей с '%' в userId нет, по-этому повторный url decode не страшен
processPayment checkOnly (decodeURIComponentT -> userId) orderId products = do
    let addTime product
            | Just (a,_,_) <- productInfo orderId product = return a
            | otherwise = fail $ "Invalid product name: " ++ T.unpack product
    add <- if userId == "demo" then return id else
        fmap (foldr (.) id) $ mapM addTime products
    t <- getUrTime
    when (userId == "") $
        fail "Empty user ID"
    u <- readUser userId
    when (isNothing u) $
        fail $ "User " ++ T.unpack userId ++ " does not exists (account deleted?)"
    modifyUser' userId $ \ u -> do
        let uvm = uViewMode u
            same (PFastSpring {..}) = pOrderId == orderId
            same _ = False
            order = PFastSpring
                    { pOrderId = orderId
                    , pOrderType = T.unwords products
                    , pOrderTime = t
                    }
        case find same (uPayments u) of
            Just p -> return (u, (userId, p))
            Nothing
                | checkOnly -> fail "Order is too old"
                | otherwise -> return
                    (u
                     { uPayments = order : uPayments u
                     , uViewMode = uvm { uvmPaidTill = PTPaid $ add $
                                            case uvmPaidTill uvm of
                                                PTPaid pt -> max t pt
                                                _ -> t
                                       }
                     }
                    ,(userId, order))

fastSpringOrderCache :: MVar (HM.HashMap T.Text (Maybe FastSpringOrder), UrTime)
fastSpringOrderCache = unsafePerformIO $ newMVar (HM.empty, UrTime 0 0)
{-# NOINLINE fastSpringOrderCache #-}

downloadFastSpringOrder oid = do
    r <- tryDownloadFastSpringOrder oid
    maybe (fail $ "downloadFastSpringOrder: order "
              <> T.unpack oid <> " not found") return r

tryDownloadFastSpringOrder oid =
    modifyMVar fastSpringOrderCache $ \ oc@(cache, updateTime) ->
        case HM.lookup oid cache of
            Just r -> return (oc, r)
            Nothing -> do
                t <- getUrTime
                let wt = truncate $ diffUrTime updateTime t * 1000000 + 500000
                when (wt > 0) $ do
                    threadDelay wt
                    -- rate limit, а то FastSpring может 404 вернуть
                r <- downloadFastSpringOrder' oid
                ut <- getUrTime
                return ((HM.insert oid r cache, ut), r)

withD = withDownloaderSettings $ C.def { dsUserAgent = "bazqux" }

test = do
--    fastSpringApiGet "/company/bazqux/order/BAZ200226-3459-20161"
    downloadFastSpringOrderNew "BAZ200226-3459-20161"
    -- а внутри результата по charges можно найти order id
--     fastSpringApiPost "/accounts" $
--         E.obj' [("contact", E.obj' [("first", ""), ("last", ""), ("email","")])]
--         --  ^ необходимо указывать и first, и last и email

setOrderReferrer internalOid uid =
    BL.putStrLn =<< fastSpringApiPost "/orders"
        (ES.obj "orders" $ ES.arr
            [ES.obj'
                [("order", internalOid)
                ,("tags", ES.obj "referrer" uid)]])
        (return . Right . (JSON.encode :: JSON.Value -> BL.ByteString))

obj n = (.: n) >=> withObject (T.unpack n) return
arr n = (.: n) >=> withArray (T.unpack n) (return . V.toList)
str n = (.: n) >=> withText (T.unpack n) return
num n = (.: n) >=> withScientific (T.unpack n) return
arr1 n o = do
    a <- arr n o
    case a of
        [] -> fail $ show n <> "is empty"
        (x:_) -> return x

downloadFastSpringOrderNew oid
    | oid < "BAZ2002" = return Nothing
    | otherwise = do
        fsOid <- fastSpringApiGet ("/accounts?orderReference=" <> oid) charges
        case fsOid of
            Nothing -> return Nothing
            Just fsOid -> fastSpringApiGet ("/orders/" <> fsOid) $ \ o ->
                ordersErr o <|> (Right . Just) <$> parseFastSpringOrderNew o
    where err e = do
              logT ("downloadFastSpringOrderNew: " <> e)
              fail $ "Can’t check order on FastSpring: " ++ T.unpack e
          ordersErr = withObject "root object" $
              arr1 "orders" >=> withObject "orders[0]" parseErr
          charges = withObject "root object" $ \ o ->
              parseErr o
              <|> do
                  a <- arr1 "accounts" o
                  c <- withObject "account" (arr "charges") a
                  findCharge c
          parseErr = (.: "error") >=> withObject "error"
              (const $ return $ Right Nothing)
              -- (fmap (Left . T.intercalate "; ") . mapM errEntry . HM.toList)
          errEntry (n, v) =
              withText "error type" (return . ((n <> ": ") <>)) v
          findCharge [] =
              return $ Right Nothing
          findCharge (x:xs) = do
              (r,o) <- withObject "charge"
                  (\ c -> liftA2 (,) (str "orderReference" c) (str "order" c)) x
              if r == oid then
                  return $ Right $ Just o
              else
                  findCharge xs

parseFastSpringOrderNew = withObject "order" $ \ o -> do
    fsoId <- str "reference" o
    fsoTime <- urTimeFromMsec . truncate <$> num "changed" o
    fsoReferrer <- (str "referrer" =<< obj "tags" o) <|> return ""
    fsoCountry <- str "country" =<< obj "address" o
    c <- obj "customer" o
    eaFirstName <- str "first" c <|> return ""
    eaLastName <- str "last" c <|> return ""
    eaEmail <- str "email" c
    let fsoEmail = EmailAddress {..}
    items <- arr "items" o
    fsoProducts <- fmap concat $ forM items $ withObject "item" $ \ i -> do
        p <- str "product" i
        q <- num "quantity" i
        return $ replicate (truncate q) p
    pType <- str "type" =<< obj "payment" o
    let fsoTest = pType == "test"
    return $ FastSpringOrder {..}

fastSpringApiPost path payload = fastSpringApiRequest path (Just payload)
fastSpringApiGet path = fastSpringApiRequest path Nothing

fastSpringApiRequest :: T.Text -> Maybe JSON.Value
    -> (JSON.Value -> JSON.Parser (Either T.Text a)) -> IO a
fastSpringApiRequest path payload parse = do
    (dr,rdr) <- withD $ \ d -> rawDownload
        (return
         . maybe id (postRequest . BL.toStrict . JSON.encode) payload
         . C.applyBasicAuth fastspringNewStoreUsername
            fastspringNewStorePassword)
        d
        (T.unpack $
         T.concat [ "https://api.fastspring.com", path ])
        Nothing []
    case rdr of
        Just (rdrBody -> d) -> do
--            B.putStrLn d
            case jsonParser parse d of
                Right (Right r) -> return r
                Right (Left e) ->
                    err e
                Left e ->
                    err $ T.pack $ "Can’t parse JSON: " ++ e
        _ ->
            err "Timeout, please try again later"
    where err e = do
              logT ("fastSpringApiRequest: " <> e)
              fail $ "Can’t check order on FastSpring: " ++ T.unpack e

downloadFastSpringOrder' oid = do
    (o,n) <- concurrently
        (try $ downloadFastSpringOrderOld oid)
        (try $ downloadFastSpringOrderNew oid)
    case (o,n) of
        (Right a, Right b) -> return $ a <|> b
        (Right a, _) -> return a
        (_, Right b) -> return b
        (_, Left e) -> E.throw e
    where try :: IO a -> IO (Either E.SomeException a)
          try = E.try

downloadFastSpringOrderOld oid = do
    (dr,rdr) <- withD $ \ d -> rawDownload
        return
        d
        (T.unpack $
         T.concat [ "https://api.fastspring.com/company/bazqux/order/"
                  , oid, "?user=api&pass=", fastspringPassword ])
        Nothing []

    case (dr, rdr) of
        (DROK d _, _) -> do
--            B.putStrLn d
            Just <$> parseFastSpringOrder d
        (DRError _, Just r)
            | rdrStatus r `elem` [notFound404, unauthorized401] ->
                return Nothing
        (DRError e, _) -> do
            logS $ "downloadFastSpringOrder: download failure: " ++ show e
            fail $ "Can’t check order on FastSpring: " ++ e
        e -> do
            logS $ "downloadFastSpringOrder: download failure: " ++ show e
            fail "Can’t check order on FastSpring, please try again later"

data FastSpringOrder
    = FastSpringOrder
      { fsoId :: Text
      , fsoTime :: UrTime
      , fsoReferrer :: Key User
      , fsoCountry :: Text
      , fsoEmail :: EmailAddress
      , fsoProducts :: [Text]
      , fsoTest :: Bool
      }
    deriving Show

parseFastSpringOrder :: B.ByteString -> IO FastSpringOrder
parseFastSpringOrder = go emptyFastSpringOrder . parseTagsT
    where emptyFastSpringOrder =
              FastSpringOrder
              { fsoId = ""
              , fsoTime = UrTime 0 0
              , fsoReferrer = ""
              , fsoCountry = ""
              , fsoEmail = EmailAddress "" "" ""
              , fsoProducts = []
              , fsoTest = False
              }
          err o m = fail $ "parseFastSpringOrder: " <> m
              <> " (" <> T.unpack (fsoId o) <> ")"
          go o []
              | fsoTime o == UrTime 0 0 = err o "can’t find order time"
              | null (fsoProducts o) = err o "productName not found"
              | fsoReferrer o == "" = case orderReferrer (fsoId o) of
                  Just r -> return $ o { fsoReferrer = r }
                  Nothing -> err o "user ID not found"
              | otherwise = return $ o { fsoProducts = sort $ fsoProducts o }
          go o (TagOpen "reference" _ : TagText r : ts) =
              go (o { fsoId = r }) ts
          go o (TagOpen "referrer" _ : TagText r : ts) =
              go (o { fsoReferrer = r }) ts
          go o (TagOpen "country" _ : TagText c : ts) =
              go (o { fsoCountry = c }) ts
          go o (TagOpen "productname" _ : TagText n : ts) =
              go (o { fsoProducts = n : fsoProducts o }) ts
          go o (TagOpen "quantity" _ :
                    TagText (tryReadUnsignedInt -> Just q) : ts)
              | (n:p) <- fsoProducts o =
              go (o { fsoProducts = replicate q n ++ p }) ts
          go o (TagOpen "firstname" _ : TagText f : ts) =
              go (o { fsoEmail = (fsoEmail o) { eaFirstName = f } }) ts
          go o (TagOpen "lastname" _  : TagText l : ts) =
              go (o { fsoEmail = (fsoEmail o) { eaLastName = l } }) ts
          go o (TagOpen "email" _     : TagText e : ts) =
              go (o { fsoEmail = (fsoEmail o) { eaEmail = fixEmail e } }) ts
          go o (TagOpen "statuschanged" _
               : TagText (readRfc3339 . T.unpack -> Just t) : ts)
              | fsoTime o == UrTime 0 0
                -- запоминаем только первый statuschanged, относящийся к заказу,
                -- а не к платежу (который округляется до 0 часов 0 минут)
              = go (o { fsoTime = t }) ts
          go o (TagOpen "test" _ : TagText "true" : ts) =
              go (o { fsoTest = True }) ts
          go o (_ : ts) = go o ts
          fixEmail e = fromMaybe e $ lookup e orderEmailRedirects


parseOrderNotification :: T.Text -> IO (Key User, T.Text, [T.Text])
parseOrderNotification qs = do
    let fields = parseQueryStringUtf8Only qs
    logT $ T.concat ["Order parameters: ", T.pack $ show $ sort fields]
    let (p,pc) = ppc fields
        test = elem ("OrderIsTest","true") fields
--    when test $ fail "test mode???"
    userId <- p "OrderReferrer"
    orderId <- p "OrderID"
    pid <- sort . T.words <$> p "OrderProductNames"
    secData <- p "security_data"
--     eaEmail <- p "CustomerEmail"
--     eaFirstName <- p "CustomerFirstName"
--     eaLastName <- p "CustomerLastName"
    -- даты среди полей нет
    pc "security_hash" $ md5 $ T.concat [ secData, fastspringPrivateKey ]
    return (userId, orderId, pid)

parseOrderNotificationNew :: T.Text -> T.Text -> IO [FastSpringOrder]
parseOrderNotificationNew payload sign
    | sign /= sign' = do
        fail "parseOrderNotificationNew: invalid signature"
    | otherwise =
        case jsonParser notification (tbs payload) of
            Right r -> do
                logTL ["Orders: ", showT r]
                return r
            Left e ->
                fail $ "parseOrderNotificationNew: can’t parse: " <> e
    where notification = withObject "root" (arr "events" >=> mapM event)
          event = withObject "event" $ \ e -> do
              t <- str "type" e
              when (t /= "order.completed") $
                  fail $ "Unsupported event type: " <> T.unpack t
              parseFastSpringOrderNew =<< (e .: "data")
          sign' =
              bst $ Base64.encode $ sha256_digest2bs $ HMAC.hmacGetDigest
              $ HMAC.hmac fastspringNewStoreHMAC_SHA256_Secret (tbs payload)

ppc fields = (p, pc)
    where p name =
            maybe (fail $ "No " ++ T.unpack name ++ " parameter found") return $
            lookup name fields
          pc name val = do
            v <- p name
            when (v /= val) $
                fail $ "Invalid " ++ T.unpack name ++ " value " ++ show v
