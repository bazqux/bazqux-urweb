{-# LANGUAGE ViewPatterns, RecordWildCards, OverloadedStrings, RankNTypes #-}
module Payments.Eofd
    ( createOfdReceipt
    ) where

import Control.Monad
import qualified Control.Exception as E
import Data.List
import Data.Bits (testBit)
import Data.Binary
import Data.Scientific (Scientific, formatScientific, FPFormat(Fixed))
import qualified Data.Aeson as JSON
import           Data.Aeson (FromJSON, (.:), (.:?), withObject)
import qualified Data.Default as C
import qualified Network.HTTP.Client as C
import qualified Network.HTTP.Types as N
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Short as SB
import qualified Data.ByteString.Base64 as Base64
import Generated.DataTypes
import Lib.UrTime
import Lib.Log
import Lib.StringConversion
import Data.Maybe
import Generated.RiakIO
import System.IO.Unsafe
import System.Directory
import qualified System.Posix.Files as Posix
import Control.Concurrent
import Network.HTTP.Conduit.Downloader
import Lib.Hash
import Lib.ReadUtils
import Lib.ElasticSearch (obj, obj', arr)
import Text.HTML.TagSoup hiding (parseTags, renderTags)
import Text.HTML.TagSoup.Fast
import Config
import PageInfo (runLimitedUser)
import Lib.FastChar (emptyText)
import qualified Lib.Cache as Cache
import Mailer
import Resolvables
import Payments.FastSpring

------------------------------------------------------------------------------
-- Работа с e-ofd.ru

data CBRRate
    = CBRRate
      { cbrrId :: Text
      , cbrrNumCode :: Int
      , cbrrCharCode :: Text
      , cbrrNominal :: Int
      , cbrrName :: Text
      , cbrrValue :: Scientific
      }
    deriving Show
-- <Valute ID="R01010">
-- <NumCode>036</NumCode>
-- <CharCode>AUD</CharCode>
-- <Nominal>1</Nominal>
-- <Name>Австралийский доллар</Name>
-- <Value>16,0102</Value>
-- </Valute>

parseCBR :: [Tag Text] -> Either Text [CBRRate]
parseCBR = start . filter notEmpty
    where notEmpty (TagText t) = not (emptyText t)
          notEmpty _ = True
          errTags e ts = Left $ e <> ": " <> renderTagsT (take 3 ts) <> "…"
          start (TagOpen "?xml" _ : TagClose "?xml" : ts) = start ts
          start (TagOpen "valcurs" _ : TagText e : _) =
              Left $ T.strip e
          start (TagOpen "valcurs" _ : ts) = go [] ts
          start ts = errTags "Invalid CBR response" ts
          go acc [TagClose "valcurs"] = return $ reverse acc
          go acc (TagOpen "valute" (lookup "id" -> Just cbrrId) : ts) = do
              (as, ts') <- valute [] ts
              let err m = Left $ m <> " in Valute ID=" <> cbrrId
                  l n =
                      maybe (err $ "Can’t find " <> n <> " parameter") return $
                      lookup (T.toLower n) as
                  i n = do
                      x <- l n
                      maybe (err $ "Bad number in " <> n <> " parameter")
                          return $ tryReadUnsignedInt x
              cbrrNumCode <- i "NumCode"
              cbrrCharCode <- l "CharCode"
              cbrrNominal <- i "Nominal"
              cbrrName <- l "Name"
              v <- l "Value"
              cbrrValue <- case reads $ T.unpack $ T.replace "," "." v of
                  [(r, "")] -> return r
                  _ -> err $ "Bad Value " <> v
              go (CBRRate {..} : acc) ts'
          go acc ts = errTags "Invalid entry" ts
          valute _ [] = Left "Unfinished Valute entity"
          valute acc (TagClose "valute" : ts) = return (acc, ts)
          valute acc (TagOpen n [] : TagText v : TagClose n2 : ts) =
              if n /= n2 then
                  Left $ "Invalid parameter names in Valute entity: "
                      <> n <> " /= " <> n2
              else
                  valute ((n,v) : acc) ts
          valute _ ts = errTags "Invalid Valute entity content" ts

withD = withDownloaderSettings $ C.def { dsUserAgent = "bazqux" }

cbrRates t = withLogger $ \ l -> withD $ \ d -> cbrRates' l d t
cbrRates' l d t = flip (Cache.cached cbrRatesCache) url $ \ url -> do
    logLS l $ "fetching " ++ url
    dr <- logTime l "CBR fetch" $ download d url Nothing []
    case dr of
        DROK dat _ -> case parseCBR $ parseTagsT $ ensureUtf8Xml dat of
            Right p -> return p
            Left e -> fail $ "Can’t parse CBR output: " <> T.unpack e
        DRError e ->
            fail $ "Can’t fetch CBR prices: " <> e
        e ->
            fail $ "Can’t fetch CBR prices: " <> show e
    where url = formatUrTime
              "https://www.cbr.ru/scripts/XML_daily.asp?date_req=%d/%m/%Y" $
              utcToMskTime t

utcToMskTime t = t `plusUrTime` (3*hour)
mskToUtcTime t = t `plusUrTime` (-3*hour)

cbrRatesCache :: Cache.Cache String [CBRRate]
cbrRatesCache = unsafePerformIO $ Cache.new 86400
{-# NOINLINE cbrRatesCache #-}

data EofdReply =
    EofdReply
    { eorKktNumber :: Text
    , eorError :: EofdError
    , eorDoc :: Maybe EofdDoc
    }

instance FromJSON EofdReply where
    parseJSON = withObject "EofdReply" $ \ v -> do
        kktNumber <- v .: "kktNumber"
        EofdReply kktNumber
         <$> v .: "Error"
         <*> (fmap (\ (EofdDoc' d) -> d kktNumber) <$> (v .:? "doc"))

data EofdError =
    EofdError
    { eoeErrorCode :: Int
    , eoeErrorMessage :: Text
    }
    deriving Show

instance FromJSON EofdError where
    parseJSON = withObject "EofdError" $ \ v ->
        EofdError
        <$> v .: "ErrorCode"
        <*> v .: "ErrorMessage"

data EofdDoc =
    EofdDoc
    { eodFiscalDocumentNumber :: Int
    , eodFiscalSignShort :: Int
    , eodDocType :: Text
    , eodJSON :: Maybe ((UrTime, Scientific) -> FtsReceipt)
    , eodEmail :: Maybe Text
    , eodQRdata :: Maybe EofdQRdata
    }

newtype EofdDoc' = EofdDoc' (Text -> EofdDoc)

instance FromJSON EofdDoc' where
    parseJSON = withObject "EofdDoc" $ \ v -> do
        eodFiscalSignShort <- v .: "fiscalSignShort"
        eodFiscalDocumentNumber <- v .: "fiscalDocumentNumber"
        eodDocType <- v .: "docType"
        json <- v .:? "JSON"
        eodEmail <- v .:? "Email"
        eodQRdata <- v .:? "QRdata"
        return $ EofdDoc' $ \ kktNumber ->
            let eodJSON = (\ (EofdJSON j) -> j kktNumber (T.pack $ show eodFiscalSignShort)) <$> json
            in
                EofdDoc {..}


data EofdQRdata =
    EofdQRdata
    { eoqrdString :: Text
    , eoqrdRows :: [[Bool]]
    }
    deriving Show

instance FromJSON EofdQRdata where
    parseJSON = withObject "EofdQRdata" $ \ v -> do
        s <- v .: "QRstring"
        m <- v .: "QRmatrix"
        fmap (EofdQRdata s) $ forM m $ withObject "QRmatrix Row" $ \ r -> do
            d <- r .: "Data"
            when (length d /= 10 || not (all hex d)) $
                fail $ "Invalid 40 bit hex string: " ++ d
            return [testBit (read ("0x" ++ d) :: Int) i | i <- [39,38..0]]
        where hex c = (c >= '0' && c <= '9')
                  ||  (c >= 'a' && c <= 'f')
                  ||  (c >= 'A' && c <= 'F')

newtype EofdJSON
    = EofdJSON
      { unEofdJSON :: Text -> Text -> (UrTime, Scientific) -> FtsReceipt }

instance FromJSON EofdJSON where
    parseJSON = withObject "EofdJSON" $ \ j -> do
        v <- j .: "receipt"
        frDocumentName     <- v .: "documentName"
        frNomerZaSmenu     <- v .: "requestNumber"
        frTime0            <- v .: "dateTime"
        frTime <- maybe
            (fail $ "Invalid dateTime format: " ++ frTime0) return
            (tryParseTime ["%Y-%m-%eT%k:%M:%S"] frTime0)
        frAddress          <- v .: "placeMarket"
        frRetailAddress    <- v .: "retailPlaceAddress"
        frOrganizationName <- v .: "user"
        frINN              <- v .: "userInn"
        frTaxSystem        <- parseEnum v "flagsSNO"
            [(1, FRTSObschaya)
            ,(2, FRTSUsnDohod)
            ,(4, FRTSUsnDohodMinusRashod)
            ,(8, FRTSEnvd)
            ,(16, FRTSEshn)
            ,(32, FRTSPatent)]
        frOperationType    <- parseEnum v "operationType"
            [(1, FROTPrihod)
            ,(2, FROTVozvratPrihoda)
            ,(3, FROTRashod)
            ,(4, FROTVozvratRashoda)]
        frItems            <- map unEofdItem <$>
            v .: "items" <> ((:[]) <$> v .: "items")
        frTotal            <- v .: "ecashTotalSum"
        frTotalVats        <- return []
        frKKTRegNumber     <- v .: "kktRegId"
        frFNNumber         <- v .: "fiscalDriveNumber"
        -- frFPD              <- v .: "fiscalSign"
        -- "это полный ФПД, состоящий из 6 байт. fiscalSignShort это
        -- часть от fiscalSign, байты со 2-го по 5-ый. В ФН хранится
        -- полный fiscalSign, но на печать нужно выводить
        -- fiscalSignShort (см. ФФД, таблица 5, тег 1077)"
        frReceiptSite        <- v .: "urlFNS"
        frBuyerEmail       <- v .: "buyerAddress"
        frSenderEmail      <- v .: "senderAddress"
        frFDNumber         <- v .: "fiscalDocumentNumber"
        frShiftNumber      <- v .: "shiftNumber"
        let frPdf          = Nothing
        let frHtmlRus      = Nothing
        let frHtmlEng      = Nothing
        let frReserved_1   = 0
        let frReserved_2   = 0
        let frReserved_3   = 0
        let frReserved_4   = 0
        let frReserved_5   = 0
        let frReserved_6   = 0
        let frReserved_7   = 0
        let frReserved_8   = 0
        let frReserved_9   = 0
        let frReserved_10  = 0
        return $ EofdJSON $ \ frKKTNumber frFPD frExchangeRate -> FtsReceipt {..}

newtype EofdItem = EofdItem { unEofdItem :: FtsReceiptItem }

instance FromJSON EofdItem where
    parseJSON = withObject "EofdItem" $ \ v -> do
        friTitle     <- v .: "name"
        friCount     <- v .: "quantity"
        friPrice     <- v .: "price"
        friTotal     <- v .: "sum"
        friVat       <- parseEnum v "VATrate"
            [(1, FRVT18)
            ,(2, FRVT10)
            ,(3, FRVT118)
            ,(4, FRVT110)
            ,(5, FRVT0)
            ,(6, FRVTNone)]
        let friReserved1 = 0
        let friReserved2 = 0
        let friReserved3 = 0
        let friReserved4 = 0
        return $ EofdItem $ FtsReceiptItem {..}

parseEnum v n values = do
    x <- v .: n
    maybe
        (fail $ "Unknown " ++ T.unpack n ++ " value (" ++ show x ++ ")")
        return (lookup (x :: Int) values)

createOfdReceiptLock = unsafePerformIO $ newMVar ()
{-# NOINLINE createOfdReceiptLock #-}

createOfdReceipt forceSend refund orderId =
    withMVar createOfdReceiptLock $ const $
    E.handle h $ withLogger $ \ l -> logTime l "total" $ do
    let getOrder =
--             (\ f -> f { fsoEmail = EmailAddress "" "" "" })
--             <$>
            (logTime l "fastspring dl" $ downloadFastSpringOrder orderId)
    or <- readOfdReceipt key
    case or of
        Just or
            | Just pfr <- orPrintableFtsReceipt or ->
                if forceSend then do
                    logLT l "Resending order receipt email"
                    fso <- getOrder
                    send l fso pfr
                else
                    logLT l "Receipt exists, email already sent"
            | otherwise -> do
                fso <- getOrder
                formatAndSend l fso (orFtsReceipt or)
        Nothing -> do
            logLT l "Registering receipt"
            fso@(FastSpringOrder {..}) <- getOrder
            if fsoTime < minTime then
                logLT l "Order is too old"
            else do
                (tid, fr) <- registerFtsReceipt l refund orderId
                    fsoTime fsoProducts
                -- сразу же сохраняем, чтобы не пробивать повторно в случае
                -- проблем во время генерации pdf
                modifyOfdReceipt_ key $ const $ return $
                    (defaultOfdReceipt key)
                    { orTransactionID = tid
                    , orFtsReceipt = fr
                    }
                formatAndSend l fso fr
    where key = (orderId, refund)
          Just minTime = readRfc3339 "2018-12-16"
          formatAndSend l fso fr = do
              pfr <- logTime l "format " $
                  formatReceipt l orderId (eaEmail $ fsoEmail fso) fr
              send l fso pfr
              modifyOfdReceipt'_ key $ \ or ->
                  return $ or { orPrintableFtsReceipt = Just pfr }
          send l fso pfr = do
              let rus = fsoCountry fso == "RU"
                  subject
                      | rus =
                          "Электронный чек по вашему заказу " <> fsoId fso
                      | otherwise =
                          "Your electronic order receipt [" <> fsoId fso <> "]"
                  body
                      | rus =
                          [T.intercalate "\n\n" russianPreface
                          ,T.intercalate "<br><br>\n" $
                           p russianPreface
                           <> [pfrHtmlRus pfr]
                           <> p ["Вы можете найти версию для печати в приложении."]]
                      | otherwise =
                          [T.intercalate "\n\n" englishPreface
                          ,T.intercalate "<br><br>\n" $
                           p englishPreface
                           <> [pfrHtmlRus pfr]
                           <> p ["Translated version:"]
                           <> [pfrHtmlEng pfr]
                           <> p ["You can find a printable version of the receipt in attachment."]]
                  p s = [renderTagsT [TagText l] | l <- s]
                  englishPreface =
                      ["Thank you for your purchase!"
                      ,"BazQux Reader is developed in Russia and in accordance with Russian Federal law №54-FZ we’re sending you an electronic receipt. Prices are in Russian roubles since it is required by the law. You must also receive an invoice in US dollars from FastSpring.com in separate e-mail."]
                  russianPreface =
                      ["Спасибо за вашу покупку!"
                      ,"BazQux Reader разрабатывается в России и, в соответствии с Федеральным законом №54-ФЗ, мы направляем вам электронный чек. Сумма указана в рублях, т.к. этого требует закон. Вы также должны получить счет в долларах США от FastSpring.com отдельным письмом."]
                  attachments =
                      [("application/pdf"
                       ,fsoId fso <> ".pdf"
--                         if rus then
--                             "Электронный_кассовый_чек.pdf"
-- --                             ^ mime-mail некорректно обрабатывает русские
-- --                               буквы в именах файлов
--                         else
--                             "Order_receipt.pdf"
                       ,BL.fromStrict $ SB.fromShort $ pfrPdfRus pfr)]
              mail subject body supportEmail (fsoEmail fso) attachments
              mail subject body supportEmail receiptDebugEmail attachments
          h :: E.SomeException -> IO a
          h e = fail $ "Can’t register receipt in Federal Tax Service of Russia.\nDon’t worry - your payment was processed, reader will work fine,\nyou’ll get invoice from FastSpring and you can request a refund if you need.\nIt’s additional receipt that is required by Russian Federal Law 54-FZ\nthat was failed. We will send it to you immediately as we resolve the issue.\n\nError details:\n" ++ show e

registerFtsReceipt l refund orderId orderTime products = withD $ \ d -> do
    rates <- cbrRates' l d orderTime
    usd <- maybe (fail "Can’t find USD exchange rate in CBR response") return $
        find ((== "usd") . T.toLower . cbrrCharCode) rates
    let exchangeRate = cbrrValue usd
        pd = postData exchangeRate
    (dr, mbRdr) <- logTime l "e-ofd  " $ pd `seq`
        rawDownload (req pd) d (eOfdUrl ++ "/Transaction") Nothing []
    case mbRdr of
        Just (RawDownloadResult {..}) -> do
            let r = JSON.eitherDecode (BL.fromStrict rdrBody)
            when (N.statusCode rdrStatus /= 200) $
                fail ("HTTP " ++ show (N.statusCode rdrStatus) ++ " "
                    ++ T.unpack (bst $ N.statusMessage rdrStatus) ++ "\n"
                    ++
                    case r of
                        Right (EofdReply {..}) ->
                            errorMessage eorError
                        _ ->
                            T.unpack (bst rdrBody)
                    )
            let rTid = lookup "TransactionID" rdrHeaders
            when (rTid /= Just tid) $ fail $ case rTid of
                Nothing -> "TransactionID header is missing in reply"
                Just t' -> "TransactionID recieved " ++ show t'
                    ++ " is not the same that was sent (" ++ show tid ++ ")"
            logLT l (bst rdrBody)
            case r of
                Right (EofdReply {..}) -> do
                    when (eoeErrorCode eorError /= 0) $
                        fail (errorMessage eorError)
                    doc <-
                        maybe (fail "'doc' field is missing in reply") return
                        eorDoc
                    receipt <-
                        maybe (fail "'JSON' field is missing in reply")
                            (return . ($ (orderTime, exchangeRate))) $
                        eodJSON doc
                    -- logLS l $ show receipt
                    -- maybe (return ()) (logLT l . T.replace "\r\n" "\n") (eodEmail doc)
--                     B.writeFile "QRCode.html" . renderTags =<< formatQRCode err (receiptQRcode receipt)
--                     logLS l $ show $ do
--                         q <- eodQRdata doc
--                         return (eoqrdString q == receiptQRcode receipt)
                    return (bst tid, receipt)
                Left e ->
                    fail $ "Can’t decode JSON\n" ++ e
        Nothing
            | DRError e <- dr ->
                fail $ "Can’t connect to e-ofd.ru: " <> e
            | otherwise ->
                fail "Timeout connecting to e-ofd.ru"
    where errorMessage e =
              T.unpack (eoeErrorMessage e)
              ++ " (error code: " ++ show (eoeErrorCode e) ++ ")"
          req pd = return .
              postRequest pd .
              C.applyBasicAuth eOfdUser eOfdPassword .
              addTransactionID
          tid = tbs $ -- T.toLower $
              T.take 16 $
              base58T sha3_512_bs $ bst $ BL.toStrict $
              encode (refund, orderId)
              -- transaction id в base58, чтобы можно было использовать её
              -- в e-mail без префикса (ну и чтобы текст читался однозначно,
              -- хотя это не так важно)
          addTransactionID c =
              c { C.requestHeaders =
                  ("TransactionID", tid) : C.requestHeaders c }
          postData exchangeRate = BL.toStrict $ JSON.encode $ obj "bill" $ obj'
              [("operationType", JSON.Number (if refund then 2 else 1))
               -- 1 Приход
               -- 2 Возврат прихода
              ,("AnswerType", JSON.Number (2^3 + 2^0))
               -- Бит
               -- 0 Вернуть JSON документ
               -- 1 Вернуть форму SMS для отправки покупателю (только для чеков)
               -- 2 Печатная форма документа
               -- 3 Вернуть форму Email для отправки покупателю (только для чеков)
               -- 4 Вернуть форму штрих-кода QR (только для чеков)
              ,("userPass", JSON.String "29") -- пароль администратора кассы
              ,("ecashTotalSum", JSON.Number (sum $ map fst items))
              ,("buyerAddress", JSON.String $ bst tid <> "@user.bazqux.com")
              ,("items", arr $ map snd items)]
              where items = map item products
                    item name
                        | Just (_, name, usd2rub -> price)
                            <- productInfo orderId name
                        = (price
                          ,obj'
                              [("name", JSON.String name)
                              ,("price", JSON.Number price)
                              ,("quantity", JSON.Number 1)
                              ,("VATrate", JSON.Number 6) -- НДС не облагается
                              ])
                        | otherwise =
                            error $ "Unknown product: " <> T.unpack name
                    usd2rub :: Scientific -> Scientific
                    usd2rub usd =
                        fromIntegral (roundRub (usd * exchangeRate * 100)) / 100

-- обычный round округляет 0.5 до ближайшего четного целого (как требует IEEE,
-- для повышения точности расчетов), а не до ближайшего большего, как требуется
-- при округлении цены в рублях.
roundRub :: (RealFrac a, Integral b) => a -> b
roundRub x
    | f >= 0.5 = n + 1
    | f <= -0.5 = n - 1
    | otherwise = n
    where (n, f) = properFraction x

pngSize d = do
    i <- runLimitedUser "identify"
        [ "-format", "%[width] %[height]\n", "png:-" ] d
    case i of
        Right (bst -> i)
            | [[Just w, Just h]]
                <- map (map tryReadUnsignedInt . T.words) $ T.lines i
            -> return (w, h)
            | otherwise -> fail $ "Can’t parse identify output:\n" <> T.unpack i
        Left e ->
            fail $ T.unpack e

formatReceipt :: Logger -> Text -> Text -> FtsReceipt -> IO PrintableFtsReceipt
formatReceipt l orderId email c@(FtsReceipt {..}) = withDir $ do
    eps <- formatQRCode "EPS" (receiptQRcode c)
    pngData <- formatQRCode "PNG" (receiptQRcode c)
    (pngW, pngH) <- pngSize pngData
    B.writeFile qrFile eps
    home <- getHomeDirectory
    copyFont home "PTSerif-Regular"
    copyFont home "PTSerif-Bold"
    B.writeFile texFile $ tbs texContents
    let png = (pngData, pngW, pngH)
        loop n = do
            r <- logTime l "xelatex" $ runLimitedUser "xelatex"
                ["-output-directory=" <> dir, texFile] ""
            case r of
                Left e ->
                    fail $ "Can’t produce receipt PDF file\n" ++ T.unpack e
                Right o
                    | "Rerun LaTeX" `B.isInfixOf` o ->
                        if n < 5 then do
                            -- rerunning latex to fix table size
                            -- logLT l "Rerunning LaTex"
                            loop (n+1)
                        else
                            fail "Too many attempts to rerun LaTeX"
                    | otherwise -> do
                        pdf <- B.readFile pdfFile
                        return $
                            PrintableFtsReceipt
                            { pfrPdfRus = SB.toShort pdf
                            , pfrHtmlRus = htmlContents rus png
                            , pfrHtmlEng = htmlContents eng png
                            , pfrReserved_1 = 0
                            , pfrReserved_2 = 0
                            , pfrReserved_3 = 0
                            , pfrReserved_4 = 0
                            }
    loop 0
    where dir = "/tmp/formatReceipt-" <> T.unpack orderId
          qrFile = dir <> "/qrcode.eps"
          texFile = dir <> "/receipt.tex"
          pdfFile = dir <> "/receipt.pdf"
          withDir act = do
              E.try (removeDirectoryRecursive dir)
                  :: IO (Either E.SomeException ())
              E.bracket
                  (createDirectoryIfMissing False dir >>
                   Posix.setFileMode dir Posix.accessModes)
                  -- (const $ return ())
                  (const $ removeDirectoryRecursive dir)
                  (\ () -> act)
          copyFont home f =
              copyFile (home <> "/bazqux/images/fonts/" <> f <> ".ttf")
                  (dir <> "/" <> f <> ".ttf")
          rus r _ = r
          eng _ e = e
          htmlContents l png =
              "<table cellspacing='0' cellpadding='0' border='0' style='margin: 8px; border: solid 1px #ddd; color: black; background-color: white; font-family: arial, helvetica, sans-serif; font-size: 16px; font-stretch: normal; font-style: normal; font-weight: normal; letter-spacing: normal; line-height: 1.5; max-width: 500px; overflow-wrap: break-word; border-spacing: 8px 0; border-collapse: separate; padding: 16px 16px 24px; box-shadow: 0px 4px 8px rgba(0, 0, 0, 0.25) '>\n\
              \<tbody style='vertical-align: top'>"
              <> format l (htmlFormatter png)
              <> "</tbody></table>"
          texContents =
              "\\nonstopmode\\input\n\
              \\\documentclass[a4paper,10pt]{article}\n\
              \\\usepackage{fontspec}\n\
              \\\usepackage{longtable,ragged2e,array}\n\
              \\\usepackage{graphicx}\n\
              \\\usepackage[unicode=true, pdfauthor={" <> frOrganizationName <>
              "},pdftitle={" <> frDocumentName <> ". Заказ " <> orderId <>
              "},pdfsubject={" <> frDocumentName <> "},\
              \pdfkeywords={" <> frDocumentName <> ", " <> orderId <> ", Электронный чек, №54-ФЗ},\
              \pdfcreator={XeTeX},pdfproducer={LaTeX with hyperref},\
              \colorlinks=true,urlcolor=blue,breaklinks,implicit=false]{hyperref}\n\
              \\\hypersetup{pdfinfo={CreationDate={"
              <> T.pack (formatUrTime "D:%Y%m%d%H%M%S" (mskToUtcTime frTime))
              <> "}}}\n\
              \\\newcommand{\\link}[2]{\\href{#1}{\\underline{#2}}}\n\
              \\\newcommand{\\rcolumn}[3]{\\multicolumn{#1}{r}{\\parbox[t][][t]{#2}{\\raggedleft\\arraybackslash#3\\par\\vspace{-\\prevdepth}\\vspace{1ex}}}}\n\
              \% с rcolumn чуть большая высота строк, чем без неё \n\
              \\\setmainfont{PTSerif}[Path = " <> T.pack dir <> "/ , Extension = .ttf, UprightFont = *-Regular, BoldFont = *-Bold]\n\
              \\\pagestyle{empty}\n\
              \\\begin{document}\\begin{center}\
              \\\newcolumntype{R}[1]{>{\\RaggedLeft\\arraybackslash}p{#1}}\n\
              \% \\renewcommand*{\\arraystretch}{1.4}\n\
              \\\begin{longtable}{l l l r r r}"
              <> format rus texFormatter <>
              "\\end{longtable}\\end{center}\\end{document}"
          texSingleColumn w x =
              "\\multicolumn{6}{" <> w <> "}{" <> x <> "} \\\\ \n"
          tex =
              T.replace "_" "\\_" .
              T.replace "#" "\\#" .
              T.replace "$" "\\$"
--               T.replace "\\\\link{" "\\link{" .
--               T.replace "\\" "\\\\"
          texNewline = "\\\\ \n"
          texFormatter :: Formatter Text
          texFormatter = Formatter
              { caption = \ t -> texSingleColumn "c" ("\\Large " <> tex t)
              , centered = \ t -> texSingleColumn "c" (tex t)
              , right = \ t -> texSingleColumn "r" (tex t)
              , field = \ a b ->
                  if T.length b > 20 then
                      "\\multicolumn{2}{l}{" <> tex a
                      <> "} & \\multicolumn{4}{r}{" <> tex b <> "} \\\\ \n"
                  else
                      "\\multicolumn{4}{l}{" <> tex a
                      <> "} & \\multicolumn{2}{r}{" <> tex b <> "} \\\\ \n"
              , vspace = texNewline
              , addRub = \ t -> t <> "~₽"
              , item = \ bold a b c d e ->
                  T.intercalate " & "
                  [if num == 2 then
                       "\\multicolumn{2}{p{4.6cm}}{" <> t <> "}"
                   else
                       t
                  |(num, tex -> col) <- zip [1..] [a,b,c,d,e],
                   let t = if bold then "\\bf " <> col else col
                  ]
                  <> " \\\\ \n"
              , total = \ a b ->
                  "\\multicolumn{3}{l}{\\Large\\bf " <> tex a
                  <> "} & \\multicolumn{3}{r}{\\Large\\bf " <> tex b <> "} \\\\ \n"
              , qrcode = \ _ ->
                  texNewline
                  <> texSingleColumn "c"
                      ("\\includegraphics[width=3cm,height=3cm]{"
                       -- 54-ФЗ требует > 2см
                       <> T.pack qrFile <> "}")
                  <> texNewline
              , details = \ t -> texSingleColumn "p{11cm}" $
                  "\\raggedright\\footnotesize " <> tex t
              , link = \ u t -> "\\link{" <> u <> "}{" <> tex t <> "}"
              }
          i = showT
          stripSec tz t = stripEnd ":00" (T.pack t) <> tz
          format :: (Text -> Text -> Text) -> Formatter Text -> Text
          format l (Formatter {..}) =
              caption (l frDocumentName "Receipt")
              <> centered
                 -- field (l "Признак расчета" "Operation type")
                  (case frOperationType of
                      FROTPrihod -> l "Приход" "Purchase"
                      FROTVozvratPrihoda -> l "Возврат прихода" "Return"
                      FROTRashod -> l "Расход" "Expenditure"
                      FROTVozvratRashoda -> l "Возврат разсхода" "Expenditure return")
              <> -- field (l "Адрес" "Address")
                 centered (link frAddress frAddress)
              <> vspace
              <> item True (l "№" "#") (l "Наименование товара" "Product name")
                  (l "Цена" "Price") (l "Кол-во" "Count") (l "Сумма" "Sum")
              <> foldr (<>) ""
                  [item False (i n) friTitle (rub friPrice)
                   (i friCount) (rub friTotal)
                  | (n, FtsReceiptItem {..}) <- zip [1..] frItems ]
              <> vspace
              <> total (l "Итого" "Total") (rub frTotal)
                 -- total vats
              <> details (l "Без НДС" "No Russian VAT")
              <> field (l "Безналичными" "Non-cash payment") (rub frTotal)
              -- по закону требуется сумма, озон ставит,
              <> qrcode (l "QR-код" "QR code")
              <> field (l "Дата выдачи" "Receipt date")
                  (l (stripSec " (МСК)" $
                      formatUrTimeRussian "%-d %B %Y, %H:%M:%S" frTime)
                     (stripSec " (MSK)" $
                      formatUrTime "%b %-d, %Y at %H:%M:%S" frTime))
              <> field (l "Организация" "Organization")
                  -- "ИП Шабанов Владимир Викторович"
                  frOrganizationName
              <> field (l "ИНН" "Taxpayer identification number") frINN
              <> field (l "Система налогообложения" "Taxation system")
                  (case frTaxSystem of
                      FRTSObschaya -> l "ОСН" "General"
                      FRTSUsnDohod -> l "УСН доход" "Simplified"
                      FRTSUsnDohodMinusRashod ->
                          l "УСН доход-расход" "Simplified"
                      FRTSEnvd -> l "ЕНВД" "ENVD"
                      FRTSEshn -> l "ЕСХН" "ESHN"
                      FRTSPatent -> l "Патент" "Patent")
              <> vspace
              <> field (l "Заводской номер ККТ" "Cash register factory no.") frKKTNumber
                 --  ^ необязательное поле
              <> field (l "Регистрационный номер ККТ" "Cash register reg. no.") (T.strip frKKTRegNumber)
              <> field (l "Номер смены" "Shift no.") (i frShiftNumber)
              <> field (l "Номер за смену" "Operation no. in shift")
                  (i frNomerZaSmenu)
              <> field (l "Номер фискального накопителя" "Fiscal drive no.") frFNNumber
              <> field (l "Номер фискального документа" "Fiscal document no.") (i frFDNumber)
              <> field (l "Фискальный признак документа" "Fiscal document signature") frFPD
              <> field (l "Сайт проверки чека" "Receipt validation site")
                  frReceiptSite
              <> field (l "Адрес покупателя" "Buyer e-mail")
                  -- "mmmmmmmmmmmmmmmm@user.bazqux.com"
                  frBuyerEmail
              <> right ("(" <> email <> ")")
              <> field (l "Адрес отправителя" "Sender e-mail") frSenderEmail
              <> vspace
              <> details
                  (l ("Стоимость рассчитана по курсу ЦБ на "
                      <> T.pack (formatUrTime "%d.%m.%Y" (utcToMskTime $ fst frExchangeRate))
                      <> ", равному " <> cb (snd frExchangeRate)
                      <> " за доллар США.")
                     ("Prices are calculated using Central Bank of Russian Federation exchange rate on "
                      <> T.pack (formatUrTime "%m/%d/%y" (utcToMskTime $ fst frExchangeRate))
                      <> " which is equal to " <> cb (snd frExchangeRate)
                      <> " for US dollar."))
              <> details
                  (l  "Право использования программы для ЭВМ НДС не облагается на основании подпункта 26 пункта 2 статьи 149 Налогового кодекса Российской Федерации."
                      "Right to use computer program is VAT free under article 149, item 2, sub-item 26 of Tax Code of Russian Federation.")
              <> details
                  (l ("Вы можете проверить корректность чека, просканировав QR-код в приложении Федеральной налоговой службы для " <> iOS link <> " или " <> android link <> ".")
                     ("You could validate receipt correctness by scanning QR code with Federal Tax Service of Russian Federation app for " <> iOS link <> " or " <> android link <> "."))
              <> vspace
              <> vspace
              <> caption (l "Спасибо за покупку!" "Thank you")
              where rub = rub' (Just 2)
                    cb x
                        | T.length cbu > T.length cb4 = cbu
                        | otherwise = cb4
                        -- добиваем до 4-х знаков, но позволяем более точные
                        -- значения
                        where cb4 = rub' (Just 4) x
                              cbu = rub' Nothing x
                    rub' f x = addRub $ l (T.replace "." "," s) s
                        where s = T.pack $ formatScientific Fixed f x
          iOS link = link "https://itunes.apple.com/app/id1169353005" "iOS"
          android link = link "https://play.google.com/store/apps/details?id=ru.fns.billchecker" "Android"

data Formatter a
    = Formatter
      { caption :: Text -> a
      , centered :: Text -> a
      , right :: Text -> a
      , field :: Text -> Text -> a
      , vspace :: a
      , addRub :: Text -> Text
      , item :: Bool -> Text -> Text -> Text -> Text -> Text -> a
      , total :: Text -> Text -> a
      , qrcode :: Text -> a
      , details :: Text -> a
      , link :: Text -> Text -> a
      }

htmlFormatter :: (B.ByteString, Int, Int) -> Formatter Text
htmlFormatter (pngData, pngW, pngH) = Formatter
    { caption = \ t -> singleColumn "center" (large $ esc t)
    , centered = \ t -> singleColumn "center" (esc t)
    , right = \ t -> singleColumn "right" (esc t)
    , field = \ a b ->
        tr $
        if sum [T.length t | TagText t <- parseTagsT (tbs b)] > 20 then
            td 2 "left; white-space: nowrap" (esc a) <> td 4 "right" (esc b)
        else
            td 4 "left; white-space: nowrap" (esc a) <> td 2 "right" (esc b)
    , vspace = singleColumn "center" "<br>"
    , addRub = \ t ->
        "<span style='white-space:nowrap'>" <> t <> "&thinsp;₽</span>"
    , item = \ boldRow a b c d e ->
        tr $ T.concat
        [if num == 2 then
             td 2 "left; width: 190px" t
         else
             td 1 (if num == 1 then "left" else "right") t
        |(num, esc -> col) <- zip [1..] [a,b,c,d,e],
         let t = if boldRow then bold col else col
        ]
    , total = \ a b ->
        tr $ td 3 "left" (large $ bold $ esc a)
            <> td 3 "right" (large $ bold $ esc b)
    , qrcode = \ t -> singleColumn "center" $ renderTagsT
        [TagOpen "img"
            [("src", "data:image/png;base64," <> bst (Base64.encode pngData))
            ,("width", showT pngW)
            ,("height", showT pngH)
            ,("alt", t)
            ,("border", "0")
            ,("style", "border: none")
            ]
        ]
    , details = \ t -> singleColumn "left; padding-bottom: 0.5em" $
        "<span style='font-size: 0.8em'>" <> t <> "</span>"
    , link = \ u t ->
        renderTagsT [TagOpen "a" [("href", u)], TagText t, TagClose "a"]
    }
    where esc x = x -- renderTagsT [TagText x]
          tr x = "<tr>\n" <> x <> "</tr>\n"
          td colspan align x =
              "<td colspan=" <> showT colspan <> " style='text-align: " <> align
              <> "'>" <> x <> "</td>\n"
          singleColumn a x =
              tr $ td 6 a x
          large x =
              "<span style='font-size: 1.4em'>" <> x <> "</span>"
          bold x =
              "<span style='font-weight: bold'>" <> x <> "</span>"

receiptQRcode (FtsReceipt {..}) =
    T.concat
    ["t=", stripEnd "00" $ T.pack $ formatUrTime "%Y%m%dT%H%M%S" frTime
    ,"&s=", T.pack (show frTotal), "&fn=", frFNNumber
    ,"&i=", T.pack (show frFDNumber), "&fp=", frFPD
    ,"&n=", case frOperationType of
        FROTPrihod -> "1"
        FROTVozvratPrihoda -> "2"
        FROTRashod -> "3" -- точно ли?
        FROTVozvratRashoda -> "4" -- точно ли?
    ]

stripEnd e t0
    | Just s <- T.stripSuffix e t0 = s
    | otherwise = t0

-- t=20181113T1610&s=123.45&fn=9999078900011707&i=190&fp=712769254&n=1
-- t=YYYYMMDDTHHMM&s=<Total>&fn=<№ ФН>&i=<№ ФД>&fp=<ФП>&n={1 приход|2 возврат}
formatQRCode :: String -> Text -> IO B.ByteString
formatQRCode fileExt string = do
    r <- runLimitedUser "qrencode"
        [ "--size=5", "--margin=" <> if fileExt == "PNG" then "5" else "0"
        , "--level=M", "--type=" <> fileExt
        , "--output=-", T.unpack string]
        ""
    case r of
        Left e ->
            fail $ "Can’t produce QR code\n" ++ T.unpack e
        Right r ->
            return r
--            return [TagOpen "img" [("src", "data:image/png;base64," <> Base64.encode r)]]
