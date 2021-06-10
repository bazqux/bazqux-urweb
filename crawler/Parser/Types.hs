{-# LANGUAGE CPP, BangPatterns, DeriveGeneric #-}
{-# OPTIONS_DERIVE --output=Generated/BinaryInstancesParser_nonstrict.h #-}
module Parser.Types
    ( ParseResult(..), CombineDownloads(..), SwitchUrl(..), JSComments(..)
    , FeedMsg(..), Media(..), LinkKind(..), Parent(..), Guid
    , ParseServerRequest(..), DownloadAndParseResult(..), ParseServerResult(..)
    , FeedItem(..)
    , ProcessUrlRequest(..), ProcessUrlResult(..)
    , PushOrder(..), AddUrl(..))
    where

import Data.Int
import GHC.Generics
import Data.Hashable
import Generated.DataTypes
import Data.Binary
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Short as SB
import URL
import Lib.UrTime
import Lib.BinaryInstances ()
import qualified Network.Socket as NS
import Data.HashMap.Strict (HashMap)

data JSComments
    = JSCSupported TURL
    | JSCUnsupported TURL
    deriving Show

data SwitchUrl
    = NoSwitch
    | Switch Bool -- сканировать новый url сразу или делать addRescan
             TURL
    deriving Show

-- TODO: по идее, тут должен быть Error/Redirect/Feed
-- Т.е. надо слить PRParsedHtml=>PRFeed. и Html=>Redirect (с отладочной инфой)
-- хотя с redirect не стоит (у нас есть отсекание Feed->Html->Feed->[Html],
-- но нет Feed->Redirect->Feed->Redirect->Feed)
-- c ParsedHtml есть вопрос при подписке на него -- это надо отдельно
-- обрабатывать, т.к. у нас посты плоские, вложенность не поддерживают
data ParseResult
    = PRError Text
    | PRHtml
      { -- | Все link rss
        prHtmlFeeds :: [TURL]
        -- | Предпочтительные фиды для комментов
      , prCommentFeeds :: [TURL]
        -- | Есть ли js комментарии
      , prJsComments :: [JSComments]
      }
    | PRFeed
      { prBaseUri :: TURL
      , prFeedMsg :: FeedMsg
      , prFeedMsgs :: [FeedMsg]
      }
    | PRParsedHtml
      { -- | Новый текст сообщения
        prMsgText :: Maybe Text
        -- | URL, на который надо переключиться с текущего.
        -- Используется для обработки курсоров Disqus -- текущий URL перестает
        -- сканироваться и вместо него обрабатывается следующий URL
      , prSwitchUrl :: SwitchUrl
        -- | Список сообщений и родителей, куда надо подлинковывать сообщения
      , prHtmlMsgs :: [(Parent, FeedMsg)]
        -- | Список страниц, с которых можно получить дополнительные
        -- сообщения с родителями, куда их надо будет подлинковывать.
        -- Также список guid-ов в нитках (если есть, в страницах нет),
        -- для определения необходимости их сканирования
      , prThreadLinks :: [(Parent, TURL, Maybe [(Guid, Text, Maybe UrTime)])]
      }
      -- редирект внутри html-я
      -- <meta http-equiv="refresh" content="0; url=http://example.com/">
    | PRRedirect TURL
    | PRAdditionalDownload Text [TURL] CombineDownloads
      -- ^ Дополнительные скачивания с ф-ей комбинирования результата.
      -- Не являются redirect-ами. Пока предназначены для выкачивания информации
      -- о видео youtube-а, а также о каналах подписки.
      -- Первый аргумент -- справка, что качаем
    | PRSetEnv [(Text, Text)] ParseResult
      -- ^ установка переменных окружения, которые могут использоваться
      -- в следующем разборе
    deriving Show

newtype CombineDownloads
    = CombineDownloads ([Either Text B.ByteString] -> ParseResult)

instance Show CombineDownloads where
    show _ = "<combine function>"
instance Binary CombineDownloads where
    put = error "put CombineDownloads?"
    get = error "get CombineDownloads?"

type Guid = Text
type Parent = Maybe Guid

-- | Сообщение фида в сыром виде. Промежуточный тип перед Msg
data FeedMsg
    = FeedMsg
      { fmGuid :: !Guid
      , fmAuthor :: !Text
      , fmAuthorEmail :: !Text
      , fmSubject :: !Text
      , fmPublishedTime :: !(Maybe UrTime)
      , fmUpdatedTime :: !(Maybe UrTime)
      , fmSummary :: !Text
      , fmBody :: !Text
      , fmTags :: ![Text] -- в обратном порядке
      , fmLinks :: ![(LinkKind, TURL)]
      , fmCommentsCount :: !(Maybe Int)
      , fmEnclosures :: [(TURL, [(Text,Text)])]
        -- их может быть несколько и в RSS.
        -- Яндекс такое допускает
        -- http://partner.news.yandex.ru/tech.pdf
      , fmEnclosureLinks :: [(TURL, TURL)]
      , fmImages :: [TURL]
      , fmMedia :: !Media
      , fmDuration :: !(Maybe Text)
      , fmSource :: Maybe (TURL, Text, TURL) -- id, title, link html
      , fmLinkNotUnique :: Bool
      }
    deriving (Show, Eq, Generic)

instance Hashable FeedMsg

data Media
    = Media
      { mThumbnails :: ![(TURL, [(Text,Text)])]
        -- <media:thumbnail url="http://a.com/b.jpg" width="75" height="50" />
      , mContents :: ![(TURL, [(Text,Text)], Media)]
        -- url необязательно, может быть media:player, только вот нужен ли он?
      , mGroups :: ![Media]
        --
--       , mPlayers :: ![(TURL, Maybe Int, Maybe Int)]
--         -- может быть на уровне item, а может быть и в content
      , mTitle :: !(Maybe Text)
      , mDescription :: !(Maybe Text)
      }
    deriving (Show, Eq, Generic)

instance Hashable Media

data LinkKind
    = LKFeed
    | LKApiFeed
    | LKOrigLink -- FeedBurner/pheedo
    | LKCommentsHtml
    | LKLink
    | LKNext
    | LKAuthor
    | LKAuthorPic
    | LKHub
    | LKGROriginalId
    | LKBaseUri
    deriving (Show, Eq, Generic)

instance Hashable LinkKind

-- TODO: надо это вынести в WorkerTypes
-- или Worker сделать, так будет правильно Queue+Worker

data ParseServerRequest
    = ParseServerRequest
      { psrqUrl :: TURL
      , psrqHostAddress :: NS.HostAddress
      , psrqRedownloadOptions :: [Text]
      , psrqDataHash :: Text
      , psrqQueueTime :: UrTime
      , psrqSubscribersCount :: Int
        -- | номер скачивания с домена/сервиса для равномерного распределения
        -- по прокси серверам
      , psrqProxyIndex :: Maybe Int
      }
    deriving Show

-- | Склеенный DownloadResult и ParseResult
data DownloadAndParseResult
    = DPRNotModified
    | DPRSameHash [TURL]
      -- ^ Все-таки парсим и возвращаем список хабов для повторной подписки
    | DPRError Text
    | DPRRedirect Text
    | DPROK
      { dprRedownloadOptions :: [Text]
      , dprDataHash :: Text
      , dprPr :: ParseResult
      }
    | DPRToRemove
      -- ^ это не от парсера, а от самой очереди на сканирование,
      -- но для удобства распределения по worker-ам добавлено сюда
    deriving Show

data ParseServerResult
    = ParseServerResult
      { psrDownloadAndParseResult :: DownloadAndParseResult
      , psrRateLimitDelay :: Maybe (Text, Double)
        -- ^ отладочная информация о задержке и задержка в секундах
      , psrLog :: Text
      }
    deriving Show

data ProcessUrlRequest
    = ProcessUrlRequest
      { purqDownloadAndParseResult :: DownloadAndParseResult
      , purqUrlToScan :: UrlToScan
      , purqQueueType :: QueueType
      , purqUtsUrl' :: TURL
      , purqQueueTime :: UrTime
      , purqSubscribersCount :: Int
      , purqFromHub :: Bool
      , purqProxyIndex :: Maybe Int
      }
data ProcessUrlResult
    = ProcessUrlResult
      { purNewUrls :: [AddUrl]
      , purLog :: Text
      , purException :: Maybe Text
      , purStatsMap :: HashMap Text Int64
      }
    deriving Show

data PushOrder
    = PushBack
    | PushFront
    | Delay Int -- если 0, то PushFront, иначе PushBack
    deriving (Eq, Ord, Show)

data AddUrl
    = AddUrl
      { auPushOrder :: PushOrder
      , auQueueType :: QueueType
      , auTime :: UrTime
      , auUrl :: TURL
      }
    deriving (Eq, Ord, Show)

data FeedItem
    = FeedItem
      { fiMsg :: Msg
      , fiHeader :: MsgHeader
      , fiParent :: Maybe SB.ShortByteString
      , fiCommentsPage :: Maybe TURL
      , fiFeed :: Maybe TURL
      , fiNext :: Maybe TURL
      , fiLinks :: [(TURL, Text)]
      , fiHasImgTag :: Bool
      , fiHasGif :: Bool
      , fiHasVideo :: Bool
      , fiSearchText :: Text
      , fiSubjectText :: Text
      , fiNoCommentsYet :: Bool
      , fiHubs :: [TURL]
      }
    deriving Show

{-!
deriving instance Binary JSComments
deriving instance Binary SwitchUrl
deriving instance Binary FeedMsg
deriving instance Binary Media
deriving instance Binary LinkKind
deriving instance Binary ParseResult
deriving instance Binary ParseServerRequest
deriving instance Binary DownloadAndParseResult
deriving instance Binary ParseServerResult
deriving instance Binary FeedItem
deriving instance Binary AddUrl
deriving instance Binary PushOrder
deriving instance Binary ProcessUrlRequest
deriving instance Binary ProcessUrlResult
!-}

#include "../Generated/BinaryInstancesParser.h"
