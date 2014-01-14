{-# LANGUAGE CPP, BangPatterns #-}
{-# OPTIONS_DERIVE --output=Generated/BinaryInstancesParser_nonstrict.h #-}
module ParserTypes
    ( ParseResult(..), SwitchUrl(..), JSComments(..)
    , FeedMsg(..), Media(..), LinkKind(..), Parent(..), Text, Guid
    , ParseServerRequest(..), ParseServerResult(..), FeedItem(..)
    , ProcessUrlRequest(..), ProcessUrlResult(..)
    , PushOrder(..), AddUrl(..))
    where

import Generated.DataTypes
import Data.Binary
import qualified Data.Text as T
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
    = PRError T.Text
    | PRHtml
      { -- | Все link rss
        prHtmlFeeds :: [TURL]
        -- | Предпочтительные фиды для комментов
      , prCommentFeeds :: [TURL]
        -- | Есть ли js комментарии
      , prJsComments :: [JSComments]
      }
    | PRFeed
      { prBaseUri :: URL
      , prFeedMsg :: FeedMsg
      , prFeedMsgs :: [FeedMsg]
      }
    | PRParsedHtml
      { -- | Новый текст сообщения
        prMsgText :: Maybe T.Text
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
    deriving Show

type Text = T.Text
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
      , fmTags :: ![Text]
      , fmDebug :: ![Text]
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
    deriving Show
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
      }
    deriving (Show, Eq)

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
    deriving (Show, Eq)

-- TODO: надо это вынести в WorkerTypes
-- или Worker сделать, так будет правильно Queue+Worker

data ParseServerRequest
    = ParseServerRequest
      { psrqUrl :: TURL
      , psrqHostAddress :: NS.HostAddress
      , psrqRedownloadOptions :: [Text]
      , psrqDataHash :: Text
      , psrqQueueTime :: UrTime
      }
    deriving Show

-- | Склеенный DownloadResult и ParseResult
data ParseServerResult
    = PSRNotModified
    | PSRSameHash
    | PSRError Text
    | PSRRedirect Text
    | PSROK
      { psrRedownloadOptions :: [Text]
      , psrDataHash :: Text
      , psrPr :: ParseResult
--      , psrLog :: Text
-- не нужно лог забивать
      }
    deriving Show

data ProcessUrlRequest
    = ProcessUrlRequest
      { purqParseServerResult :: ParseServerResult
      , purqUrlToScan :: UrlToScan
      , purqQueueType :: QueueType
      , purqUtsUrl' :: TURL
      , purqQueueTime :: UrTime
      }
data ProcessUrlResult
    = ProcessUrlResult
      { purNewUrls :: [AddUrl]
      , purLog :: T.Text
      , purException :: Maybe T.Text
      , purStatsMap :: HashMap T.Text Int
      }

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
      , fiParent :: Parent
      , fiCommentsPage :: Maybe TURL
      , fiFeed :: Maybe TURL
      , fiNext :: Maybe TURL
      , fiHasImgTag :: Bool
      , fiHasVideo :: Bool
      , fiSearchText :: T.Text
      , fiSubjectText :: T.Text
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
deriving instance Binary ParseServerResult
deriving instance Binary FeedItem
deriving instance Binary AddUrl
deriving instance Binary PushOrder
deriving instance Binary ProcessUrlRequest
deriving instance Binary ProcessUrlResult
!-}

#include "Generated/BinaryInstancesParser.h"
