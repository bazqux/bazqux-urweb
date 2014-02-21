-- ФАЙЛ СГЕНЕРИРОВАН АВТОМАТИЧЕСКИ (см. Gen.hs) !!!

{-# LANGUAGE CPP, BangPatterns #-}
{-# OPTIONS_DERIVE --output=BinaryInstances_nonstrict.h #-}

-- | Описание структур данных, сохраняемых в Riak 
-- и передаваемых между Ur/Web и Haskell
module Generated.DataTypes where

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Short as SB
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import Lib.UrTime
import qualified Lib.BArray as BA
import Lib.UnsafeRef
import Lib.ReadSet (ReadSet)
import URL
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import Data.Array
import Data.Binary
import Data.Binary.Get (getByteString)
import Data.List
import Data.Ord
import Data.Hashable
import Lib.BinaryInstances

instance Hashable ItemTag where
    hashWithSalt s ITStarred = s `hashWithSalt` (0 :: Int)
    hashWithSalt s (ITTag t) = s `hashWithSalt` t

 
data Stats
    = Stats
      { statsKey :: !T.Text
      , statsMap :: Map T.Text Int
      }
    deriving (Show, Eq, Ord)

data SubscriptionState
    = SSAdded
    | SSScanning
      { ssStartTime :: {-# UNPACK #-} !UrTime
      }
    | SSError
      { ssMessage   :: !T.Text
      }
    | SSFeed
      { ssUrl       :: !T.Text
      }
    deriving (Show, Eq, Ord)

data Subscription
    = Subscription
      { sUrl        :: !T.Text
      , sState      :: SubscriptionState
      , sEditsCount :: {-# UNPACK #-} !Int
      , sTitle      :: Maybe T.Text
      , sFolders    :: [T.Text]
      }
    deriving (Show, Eq, Ord)

data PostsViewMode
    = PVMShort
    | PVMFull
    | PVMMagazine
    | PVMMosaic
    deriving (Show, Eq, Ord)

data MsgTreeViewMode
    = MsgTreeViewMode
      { mtvmAscending        :: !Bool
      , mtvmUnreadOnly       :: !Bool
      , mtvmExpandedComments :: !Bool
      , mtvmPosts            :: PostsViewMode
      , mtvmFolderExpanded   :: !Bool
      , mtvmNoOverride       :: !Bool
      }
    deriving (Show, Eq, Ord)

data Payment
    = PReserved
    | PFastSpring
      { pOrderId   :: !T.Text
      , pOrderType :: !T.Text
      , pOrderTime :: {-# UNPACK #-} !UrTime
      }
    deriving (Show, Eq, Ord)

data PaidTill
    = PTUnknown
    | PTPaid
      { ptTill :: {-# UNPACK #-} !UrTime
      }
    | PTFreeTrial
      { ptTill :: {-# UNPACK #-} !UrTime
      }
    | PTFreeTrialFinished
      { ptTill :: {-# UNPACK #-} !UrTime
      }
    | PTPaidFinished
      { ptTill :: {-# UNPACK #-} !UrTime
      }
    deriving (Show, Eq, Ord)

data UserViewMode
    = UserViewMode
      { uvmPaidTill                 :: PaidTill
      , uvmOnlyUpdatedSubscriptions :: (Int, Bool)
      , uvmSubViewModes             :: HM.HashMap T.Text (Int, MsgTreeViewMode)
      , uvmFolderViewModes          :: HM.HashMap T.Text (Int, MsgTreeViewMode)
      , uvmSubUrlRenames            :: [(UrTime, T.Text, T.Text)]
      }
    deriving (Show, Eq, Ord)

data User
    = User
      { uId            :: !T.Text
      , uSubscriptions :: [Subscription]
      , uViewMode      :: UserViewMode
      , uPayments      :: [Payment]
      }
    deriving (Show, Eq, Ord)

data UserFilters
    = UserFilters
      { ufUser      :: !T.Text
      , ufFilters   :: [(UrTime, T.Text)]
      , ufReserved1 :: [T.Text]
      , ufReserved2 :: [T.Text]
      , ufReserved3 :: [T.Text]
      }
    deriving (Show, Eq, Ord)

data ScrollMode
    = SMNormal
    | SMQuick
    | SMImmediate
    deriving (Show, Eq, Ord)

data ListViewMode
    = LVMCompact
    | LVMTwoLines
    deriving (Show, Eq, Ord)

data MarkReadMode
    = MRMOnScroll
    | MRMManual
    | MRMOnScrollEverywhere
    deriving (Show, Eq, Ord)

data PublicFeedType
    = PFTAll
    | PFTFolder
      { pftFolder  :: !T.Text
      }
    | PFTTag
      { pftTagName :: !T.Text
      }
    | PFTStarred
    | PFTAllTags
    deriving (Show, Eq, Ord)

data ApiKeys
    = ApiKeys
      { akPocket        :: Maybe (T.Text, T.Text)
      , akPocketRequest :: Maybe (T.Text, T.Text, T.Text)
      , akReserved1     :: {-# UNPACK #-} !Int
      , akReserved2     :: {-# UNPACK #-} !Int
      , akReserved3     :: {-# UNPACK #-} !Int
      , akReserved4     :: {-# UNPACK #-} !Int
      }
    deriving (Show, Eq, Ord)

data UserSettings
    = UserSettings
      { ustUser              :: !T.Text
      , ustEditsCount        :: {-# UNPACK #-} !Int
      , ustScrollMode        :: ScrollMode
      , ustListViewMode      :: ListViewMode
      , ustShowFavicons      :: !Bool
      , ustMarkReadMode      :: MarkReadMode
      , ustUltraCompact      :: !Bool
      , ustMobileLogin       :: Maybe T.Text
      , ustExactUnreadCounts :: !Bool
      , ustPublicFeeds       :: Maybe (Map PublicFeedType [(T.Text, Bool, Maybe T.Text)])
      , ustCountry           :: Maybe T.Text
      , ustApiKeys           :: Maybe ApiKeys
      , ustReserved7         :: Maybe T.Text
      , ustReserved8         :: Maybe T.Text
      , ustReserved9         :: Maybe T.Text
      }
    deriving (Show, Eq, Ord)

data PublicFeed
    = PublicFeed
      { pfId        :: !T.Text
      , pfUser      :: Maybe T.Text
      , pfReserved1 :: Maybe T.Text
      , pfReserved2 :: Maybe T.Text
      , pfReserved3 :: Maybe T.Text
      , pfReserved4 :: Maybe T.Text
      }
    deriving (Show, Eq, Ord)

data UID
    = EMail
      { uidId :: !T.Text
      }
    | Url
      { uidId :: !T.Text
      }
    deriving (Show, Eq, Ord)

data MobileLogin
    = MobileLogin
      { mlLogin        :: !T.Text
      , mlEditsCount   :: {-# UNPACK #-} !Int
      , mlUID          :: Maybe UID
      , mlPasswordHash :: !T.Text
      , mlFeverApiKey  :: Maybe T.Text
      , mlReserved2    :: Maybe T.Text
      , mlReserved3    :: Maybe T.Text
      , mlReserved4    :: Maybe T.Text
      }
    deriving (Show, Eq, Ord)

data FeverApiKey
    = FeverApiKey
      { fakKey        :: !T.Text
      , fakEditsCount :: {-# UNPACK #-} !Int
      , fakUID        :: Maybe UID
      , fakReserved1  :: Maybe T.Text
      , fakReserved2  :: Maybe T.Text
      , fakReserved3  :: Maybe T.Text
      , fakReserved4  :: Maybe T.Text
      }
    deriving (Show, Eq, Ord)

data FeverIds
    = FeverIds
      { fiUser        :: !T.Text
      , fiMaxId       :: {-# UNPACK #-} !Int
      , fiGRIds       :: IntMap Int
      , fiFeverIds    :: IntMap Int
      , fiLastRefresh :: Maybe UrTime
      , fiReserved2   :: Maybe T.Text
      , fiReserved3   :: Maybe T.Text
      , fiReserved4   :: Maybe T.Text
      }
    deriving (Show, Eq, Ord)

data UserStats
    = UserStats
      { usId                   :: !T.Text
      , usUID                  :: UID
      , usFirstSignInTime      :: {-# UNPACK #-} !UrTime
      , usFirstSignInDetails   :: [(T.Text, T.Text)]
      , usCounters             :: Map T.Text Int
      , usSubscriptionCounters :: Map T.Text (Map T.Text Int)
      , usReserved1            :: Map T.Text Int
      , usReserved2            :: Map T.Text Int
      , usReserved3            :: Map T.Text Int
      , usReserved4            :: Map T.Text Int
      }
    deriving (Show, Eq, Ord)

data MailQueue
    = MailQueue
      { mqId       :: !T.Text
      , mqActive   :: Set T.Text
      , mqInactive :: Set T.Text
      }
    deriving (Show, Eq, Ord)

data Session
    = Session
      { sessionKey     :: !T.Text
      , sessionExpire  :: {-# UNPACK #-} !UrTime
      , sessionCleared :: !Bool
      , sessionUser    :: !T.Text
      }
    deriving (Show, Eq, Ord)

data SubscriptionUrlKind
    = SUKError
      { sukMessage :: !T.Text
      }
    | SUKFeed
      { sukUrl     :: !TURL
      }
    deriving (Show, Eq, Ord)

data SubscriptionUrlInfo
    = SubscriptionUrlInfo
      { suiUrl  :: !TURL
      , suiTime :: {-# UNPACK #-} !UrTime
      , suiKind :: SubscriptionUrlKind
      }
    deriving (Show, Eq, Ord)

data Attachment
    = AImage
      { aUrl         :: !TURL
      , aWidth       :: Maybe Int
      , aHeight      :: Maybe Int
      , aTitle       :: Maybe T.Text
      }
    | AAudio
      { aUrl         :: !TURL
      , aMime        :: !T.Text
      , aFileSize    :: Maybe Int
      , aDuration    :: Maybe Int
      , aTitle       :: Maybe T.Text
      }
    | AVideo
      { aUrl         :: !TURL
      , aMime        :: !T.Text
      , aFileSize    :: Maybe Int
      , aDuration    :: Maybe Int
      , aTitle       :: Maybe T.Text
      , aWidth       :: Maybe Int
      , aHeight      :: Maybe Int
      , aPoster      :: Maybe TURL
      }
    | AIframe
      { aUrl         :: !TURL
      , aXml         :: !T.Text
      , aDuration    :: Maybe Int
      , aTitle       :: Maybe T.Text
      }
    | AOther
      { aUrl         :: !TURL
      , aMime        :: !T.Text
      , aFileSize    :: Maybe Int
      }
    | AGrOrigin
      { aFeed        :: !TURL
      , aGuid        :: !T.Text
      , aStreamTitle :: !T.Text
      , aHtmlUrl     :: !T.Text
      }
    deriving (Show, Eq, Ord)

data MsgKey
    = MsgKey
      { msgKeyBlogFeedUrl :: !T.Text
      , msgKeyPostGuid    :: Maybe T.Text
      , msgKeyCommentGuid :: Maybe T.Text
      }
    deriving (Show, Eq, Ord)

data Msg
    = Msg
      { msgKey         :: MsgKey
      , msgAttachments :: [Attachment]
      , msgAuthor      :: !T.Text
      , msgAuthorUri   :: Maybe TURL
      , msgAuthorEmail :: !T.Text
      , msgAuthorPic   :: Maybe TURL
      , msgLink        :: Maybe TURL
      , msgSubject     :: !T.Text
      , msgTags        :: [T.Text]
      , msgTime        :: Maybe UrTime
      , msgDlTime      :: {-# UNPACK #-} !UrTime
      , msgText        :: !T.Text
      , msgShortText   :: !T.Text
      , msgDebug       :: !T.Text
      }
    deriving (Show, Eq, Ord)

data MsgHeader
    = MsgHeader
      { mhGuid        :: !SB.ShortByteString
      , mhContentHash :: !SB.ShortByteString
      , mhAuthor      :: !T.Text
      , mhAuthorPic   :: Maybe TURL
      , mhSubject     :: !T.Text
      , mhTime        :: Maybe UrTime
      , mhDlTime      :: {-# UNPACK #-} !UrTime
      , mhShortText   :: !T.Text
      }
    deriving (Show, Eq, Ord)

data MsgTree
    = MsgTree
      { mtHeaders  :: BA.Array Int MsgHeader
      , mtChildren :: IntMap (Set (UrTime, Int))
      }
    deriving (Show, Eq, Ord)

data CommentUrlState
    = CUSNew
    | CUSError
      { cusMessage :: !T.Text
      }
    | CUSRedirect
      { cusURL     :: !T.Text
      }
    | CUSNoComments
    | CUSOK
    deriving (Show, Eq, Ord)

data BlogPostsScanned
    = BlogPostsScanned
      { bpsBlogFeedUrl   :: !TURL
      , bpsSubscribeTime :: {-# UNPACK #-} !UrTime
      , bpsUrls          :: Map T.Text (Map TURL (UrTime, Maybe UrTime, CommentUrlState))
      }
    deriving (Show, Eq, Ord)

data Posts
    = Posts
      { pBlogFeedUrl     :: !TURL
      , pUpdatedComments :: !IntSet
      , pRootMessage     :: Msg
      , pMsgTree         :: MsgTree
      , pTotalComments   :: {-# UNPACK #-} !Int
      , pDeletedComments :: {-# UNPACK #-} !Int
      , pCommentCounts   :: IntMap (IntMap Int)
      , pCCVersions      :: Map UrTime Int
      }
    deriving (Show, Eq, Ord)

data DiscoveryFeed
    = DiscoveryFeed
      { dfUrl                   :: !TURL
      , dfTitle                 :: !T.Text
      , dfWebsite               :: Maybe T.Text
      , dfImage                 :: Maybe T.Text
      , dfCategory              :: !T.Text
      , dfTranslatedCategory    :: HM.HashMap T.Text T.Text
      , dfTags                  :: [T.Text]
      , dfTranslatedTags        :: HM.HashMap T.Text [T.Text]
      , dfSubscribers           :: {-# UNPACK #-} !Int
      , dfNormalizedSubscribers :: !Double
      , dfPostsPerDay           :: !Double
      , dfLastRefreshTime       :: {-# UNPACK #-} !UrTime
      , dfPaidCountries         :: HM.HashMap T.Text Int
      , dfCountries             :: HM.HashMap T.Text Int
      , dfReserved1             :: {-# UNPACK #-} !Int
      , dfReserved2             :: {-# UNPACK #-} !Int
      , dfReserved3             :: {-# UNPACK #-} !Int
      , dfReserved4             :: {-# UNPACK #-} !Int
      }
    deriving (Show, Eq, Ord)

data PostsClearTime
    = PostsClearTime
      { pctBlogFeedUrl :: !TURL
      , pctTime        :: {-# UNPACK #-} !UrTime
      , pctReserved1   :: {-# UNPACK #-} !Int
      , pctReserved2   :: {-# UNPACK #-} !Int
      , pctReserved3   :: {-# UNPACK #-} !Int
      , pctReserved4   :: {-# UNPACK #-} !Int
      }
    deriving (Show, Eq, Ord)

data PostsSubscribers
    = PostsSubscribers
      { psBlogFeedUrl :: !TURL
      , psActions     :: Set (UrTime, T.Text, Bool)
      , psSubscribers :: HS.HashSet T.Text
      , psReserved1   :: {-# UNPACK #-} !Int
      , psReserved2   :: {-# UNPACK #-} !Int
      , psReserved3   :: {-# UNPACK #-} !Int
      , psReserved4   :: {-# UNPACK #-} !Int
      }
    deriving (Show, Eq, Ord)

data ActiveCheckSubscriptions
    = ActiveCheckSubscriptions
      { acsKey   :: !()
      , acsUsers :: HM.HashMap T.Text UrTime
      }
    deriving (Show, Eq, Ord)

data CommentsKey
    = CommentsKey
      { ckBlogFeedUrl :: !T.Text
      , ckPostGuid    :: !T.Text
      }
    deriving (Show, Eq, Ord)

data Comments
    = Comments
      { cKey     :: CommentsKey
      , cMsgTree :: MsgTree
      }
    deriving (Show, Eq, Ord)

data SubscriptionParentUrl
    = SpuRedirect
      { spuUrl   :: !TURL
      }
    | SpuHtml
      { spuUrl   :: !TURL
      , spuDebug :: !T.Text
      }
    deriving (Show, Eq, Ord)

data ParentUrl
    = PuRedirect
      { puUrl   :: !TURL
      }
    | PuHtml
      { puUrl   :: !TURL
      , puDebug :: !T.Text
      }
    | PuFeed
      { puUrl   :: !TURL
      , puGuid  :: Maybe T.Text
      }
    | PuCommentsFeed
      { puUrl   :: !TURL
      }
    deriving (Show, Eq, Ord)

data SubscriptionParentPath
    = SubscriptionParentPath
      { sppSubscriptionUrl :: !TURL
      , sppParents         :: [SubscriptionParentUrl]
      }
    deriving (Show, Eq, Ord)

data ParentPath
    = ParentPath
      { ppBlogFeedUrl :: !TURL
      , ppParents     :: [ParentUrl]
      }
    deriving (Show, Eq, Ord)

data UrlToScan
    = UrlToScan
      { utsUrl                     :: !TURL
      , utsRedownloadOptions       :: [T.Text]
      , utsDataHash                :: !T.Text
      , utsModifyTime              :: {-# UNPACK #-} !UrTime
      , utsNextScanTime            :: {-# UNPACK #-} !UrTime
      , utsErrorStartTime          :: Maybe UrTime
      , utsParentPaths             :: [ParentPath]
      , utsSubscriptionParentPaths :: [SubscriptionParentPath]
      }
    deriving (Show, Eq, Ord)

data QueueType
    = QTSubscription
    | QTBlogFeed
    | QTTemporary1
    | QTNewComment1
    | QTRescan1
    | QTTemporary
    | QTNewComment
    | QTRescan
    deriving (Show, Eq, Ord)

data ScanList
    = ScanList
      { slTime :: {-# UNPACK #-} !UrTime
      , slUrls :: [(TURL, QueueType)]
      }
    deriving (Show, Eq, Ord)

data PostsRead
    = PostsRead
      { prKey               :: (T.Text, TURL)
      , prSet               :: !ReadSet
      , prTotalCommentsRead :: {-# UNPACK #-} !Int
      , prCommentsRead      :: IntMap ReadSet
      , prIgnoredPosts      :: !ReadSet
      , prIgnoredComments   :: IntMap IntSet
      }
    deriving (Show, Eq, Ord)

data PostsTagged
    = PostsTagged
      { ptBlogFeedUrl :: !TURL
      , ptSet         :: !ReadSet
      , ptReserved1   :: {-# UNPACK #-} !Int
      , ptReserved2   :: {-# UNPACK #-} !Int
      , ptReserved3   :: {-# UNPACK #-} !Int
      , ptReserved4   :: {-# UNPACK #-} !Int
      }
    deriving (Show, Eq, Ord)

data PostsTaggedGuids
    = PostsTaggedGuids
      { ptgBlogFeedUrl :: !TURL
      , ptgGuids       :: IntMap T.Text
      , ptgReserved1   :: {-# UNPACK #-} !Int
      , ptgReserved2   :: {-# UNPACK #-} !Int
      , ptgReserved3   :: {-# UNPACK #-} !Int
      , ptgReserved4   :: {-# UNPACK #-} !Int
      }
    deriving (Show, Eq, Ord)

data ItemTag
    = ITStarred
    | ITTag
      { itTagName :: !T.Text
      }
    deriving (Show, Eq, Ord)

data RemovedFeedInfo
    = RemovedFeedInfo
      { rfiStreamTitle :: !T.Text
      , rfiHtmlUrl     :: !T.Text
      , rfiTime        :: {-# UNPACK #-} !UrTime
      , rfiReserved    :: {-# UNPACK #-} !Int
      }
    deriving (Show, Eq, Ord)

data GRIds
    = GRIds
      { griUser           :: !T.Text
      , griLastId         :: {-# UNPACK #-} !Int
      , griFeedIds        :: HM.HashMap TURL Int
      , griFolderIds      :: HM.HashMap T.Text Int
      , griFeedUrls       :: IntMap TURL
      , griFolderNames    :: IntMap T.Text
      , griTaggedItems    :: Map ItemTag [(UrTime, Int, MsgKey)]
      , griItemTags       :: IntMap [ItemTag]
      , griOrdering       :: Map T.Text [Int]
      , griActiveFeeds    :: Maybe (HS.HashSet T.Text)
      , griRemovedFeeds   :: Maybe (HM.HashMap T.Text RemovedFeedInfo)
      , griGRTagsImported :: !Bool
      , griReserved4      :: !Bool
      , griReserved5      :: !Bool
      , griReserved6      :: !Bool
      , griReserved7      :: !Bool
      , griReserved8      :: !Bool
      }
    deriving (Show, Eq, Ord)

data UserBackup
    = UserBackup
      { ubKey          :: (T.Text, UrTime)
      , ubUser         :: User
      , ubUserStats    :: UserStats
      , ubUserFilters  :: UserFilters
      , ubUserSettings :: UserSettings
      , ubGRIds        :: GRIds
      , ubFeverIds     :: FeverIds
      , ubReserved1    :: !Bool
      , ubReserved2    :: !Bool
      , ubReserved3    :: !Bool
      , ubReserved4    :: !Bool
      }
    deriving (Show, Eq, Ord)

data DeletedUser
    = DeletedUser
      { duUser      :: !T.Text
      , duBackups   :: [UrTime]
      , duMailsSent :: Maybe [UrTime]
      , duReserved2 :: !Bool
      , duReserved3 :: !Bool
      , duReserved4 :: !Bool
      }
    deriving (Show, Eq, Ord)

data ApiMode
    = AMNormal
    | AMGRIdsOnly
      { amFetch        :: !Bool
      , amCount        :: {-# UNPACK #-} !Int
      , amContinuation :: UnsafeRef (Maybe MsgKey)
      , amMinDlTime    :: Maybe UrTime
      , amMaxDlTime    :: Maybe UrTime
      , amMaxTime      :: Maybe UrTime
      , amExcludeTags  :: HS.HashSet ItemTag
      , amIncludeTags  :: HS.HashSet ItemTag
      , amReadOnly     :: !Bool
      }
    | AMDiscovery
      { amUrl          :: !T.Text
      }
    deriving (Show, Eq, Ord)

data MsgTreePoint
    = MsgTreePoint
      { mtpParentId :: {-# UNPACK #-} !Int
      , mtpTime     :: {-# UNPACK #-} !UrTime
      , mtpId       :: {-# UNPACK #-} !Int
      }
    deriving (Show, Eq, Ord)

data PostsReq
    = PostsReq
      { prqBlogFeedUrl   :: !T.Text
      , prqMsgTreePoint  :: MsgTreePoint
      , prqTotalPosts    :: {-# UNPACK #-} !Int
      , prqTotalComments :: {-# UNPACK #-} !Int
      }
    deriving (Show, Eq, Ord)

data CommentsReq
    = CommentsReq
      { crqKey           :: CommentsKey
      , crqPostId        :: {-# UNPACK #-} !Int
      , crqMsgTreePoint  :: MsgTreePoint
      , crqTotalComments :: {-# UNPACK #-} !Int
      }
    deriving (Show, Eq, Ord)

data TreeReq
    = TRPosts
      { trReqs        :: [PostsReq]
      }
    | TRComments
      { trOnExpand    :: !Bool
      , trReq         :: CommentsReq
      }
    | TRSearch
      { trQuery       :: !T.Text
      , trFeeds       :: [(T.Text, Int, Int)]
      , trPostTime    :: {-# UNPACK #-} !UrTime
      , trBlogFeedUrl :: !T.Text
      , trPostGuid    :: !T.Text
      }
    | TRTags
      { trLastMsg     :: Maybe MsgKey
      , trTags        :: Maybe [ItemTag]
      }
    deriving (Show, Eq, Ord)

data MsgView
    = MVFull
      { msgViewMsg       :: Msg
      }
    | MVShort
      { msgViewHeader    :: MsgHeader
      , msgViewCachedMsg :: Maybe Msg
      }
    deriving (Show, Eq, Ord)

data MsgId
    = MsgId
      { midMsgKey    :: MsgKey
      , midFeedId    :: {-# UNPACK #-} !Int
      , midPostId    :: {-# UNPACK #-} !Int
      , midCommentId :: Maybe Int
      }
    deriving (Show, Eq, Ord)

data MsgItem
    = MsgItem
      { miMsgView    :: MsgView
      , miMsgId      :: MsgId
      , miRead       :: !Bool
      , miStarred    :: !Bool
      , miTags       :: [T.Text]
      , miReadLocked :: !Bool
      }
    deriving (Show, Eq, Ord)

data MsgForest
    = MsgForest
      { mfResultsCount :: {-# UNPACK #-} !Int
      , mfUnreadCount  :: {-# UNPACK #-} !Int
      , mfList         :: [(MsgItem, MsgForest)]
      , mfNextReq      :: Maybe TreeReq
      }
    deriving (Show, Eq, Ord)

data LoginType
    = Google
    | Facebook
    | Twitter
    | OpenId
      { ltURL :: !T.Text
      }
    deriving (Show, Eq, Ord)

data Counters
    = Counters
      { cReadPosts        :: {-# UNPACK #-} !Int
      , cReadComments     :: {-# UNPACK #-} !Int
      , cTotalPosts       :: {-# UNPACK #-} !Int
      , cTotalComments    :: {-# UNPACK #-} !Int
      , cScanning         :: {-# UNPACK #-} !Int
      , cScanningComments :: {-# UNPACK #-} !Int
      , cError            :: {-# UNPACK #-} !Int
      , cFeed             :: {-# UNPACK #-} !Int
      , cScannedPercent   :: {-# UNPACK #-} !Int
      }
    deriving (Show, Eq, Ord)

data SITFeedDetails
    = SITFeedDetails
      { sitPointAllAsc              :: Maybe MsgTreePoint
      , sitPointUnreadAsc           :: Maybe MsgTreePoint
      , sitPointUnreadDesc          :: Maybe MsgTreePoint
      , sitPointUnreadPostsOnlyAsc  :: Maybe MsgTreePoint
      , sitPointUnreadPostsOnlyDesc :: Maybe MsgTreePoint
      }
    deriving (Show, Eq, Ord)

data SubItemType
    = SITAll
    | SITSearch
      { sitQuery        :: !T.Text
      }
    | SITFolder
      { sitFolder       :: !T.Text
      }
    | SITFeed
      { sitSubscription :: Subscription
      , sitFeedLink     :: Maybe T.Text
      , sitPointAllDesc :: Maybe MsgTreePoint
      }
    | SITTag
      { sitTagName      :: !T.Text
      }
    | SITStarred
    | SITAllTags
    deriving (Show, Eq, Ord)

data SubItemRpc
    = SubItemRpc
      { sirHash          :: !T.Text
      , sirIndex         :: {-# UNPACK #-} !Int
      , sirTitle         :: !T.Text
      , sirSIType        :: SubItemType
      , sirCounters      :: Counters
      , sirViewMode      :: MsgTreeViewMode
      , sirParentFolders :: [Int]
      , sirDomIds        :: [Int]
      , sirFaviconStyle  :: Maybe T.Text
      , sirGRId          :: {-# UNPACK #-} !Int
      }
    deriving (Show, Eq, Ord)

data WelcomeState
    = WelcomeState
      { wsHasPrevAccount  :: !Bool
      , wsHasPrevSubs     :: !Bool
      , wsStarredRestored :: !Bool
      , wsTaggedRestored  :: !Bool
      }
    deriving (Show, Eq, Ord)

data ShareAction
    = SAEMail
    | SATwitter
    | SAFacebook
    | SAGooglePlus
    | SATumblr
    | SAEvernote
    | SADelicious
    | SAPinboard
    | SAPocket
    | SAReadability
    | SAInstapaper
    | SATranslate
    deriving (Show, Eq, Ord)

data BrowserType
    = BTUnknown
    | BTAndroid
    | BTIPhone
    | BTIPad
    | BTIPod
    | BTChrome
    | BTIE
    | BTIEMobile
    | BTSafari
    | BTOpera
    | BTOperaMini
    | BTFirefox
    deriving (Show, Eq, Ord)

data AppType
    = ATUnknown
    | ATFeeddler
    | ATMrReader
    | ATReeder
    | ATSlowFeeds
    | ATJustReader
    | ATNewsPlus
    | ATPress
    | ATVienna
    | ATReadKit
    | ATNewsJet
    | ATAmber
    | ATgzip
    deriving (Show, Eq, Ord)

data OperatingSystem
    = OSUnknown
    | OSWindows
    | OSMac
    | OSLinux
    | OSAndroid
    | OSIOS
    deriving (Show, Eq, Ord)

data UsageFlag
    = UFWeb
      { ufBrowserType     :: BrowserType
      , ufOperatingSystem :: OperatingSystem
      }
    | UFApp
      { ufAppType         :: AppType
      , ufOperatingSystem :: OperatingSystem
      }
    | UFShareAction
      { ufShareAction     :: ShareAction
      }
    | UFOPML
    | UFAddSubscription
    | UFSearchSubscriptions
    | UFDiscoverySubscription
    | UFAddDiscoverySubscription
    | UFUnsubscribe
    | UFRetrySubscription
    | UFRenameSubscription
    | UFRenameFolder
    | UFEditSubscriptionFolders
    | UFDragAndDrop
    | UFSearch
    | UFSearchTags
    | UFSkip
    | UFIgnore
    | UFKeepUnread
    | UFMarkAllAsRead
    | UFStar
    | UFTag
    | UFReadability
    | UFSetMobileLogin
    | UFEnablePublicFeed
    | UFDisablePublicFeed
    | UFGenerateNewPublicFeed
    | UFDeleteAccount
    | UFExportOPML
    | UFMarkAllAsReadD
      { ufOlderThan       :: {-# UNPACK #-} !Int
      }
    deriving (Show, Eq, Ord)

data UserUsageFlags
    = UserUsageFlags
      { uufPaidTill   :: PaidTill
      , uufCountry    :: !T.Text
      , uufUsageFlags :: Set UsageFlag
      , uufReserved1  :: Set ()
      , uufReserved2  :: Set ()
      , uufReserved3  :: Set ()
      , uufReserved4  :: Set ()
      }
    deriving (Show, Eq, Ord)

data UsageFlags
    = UsageFlags
      { uflTime      :: {-# UNPACK #-} !UrTime
      , uflFlags     :: HM.HashMap T.Text UserUsageFlags
      , uflReserved1 :: Set ()
      , uflReserved2 :: Set ()
      , uflReserved3 :: Set ()
      , uflReserved4 :: Set ()
      }
    deriving (Show, Eq, Ord)

data BgAction
    = BGMarkMsgRead
      { baMsgId         :: MsgId
      , baRead          :: !Bool
      , baTotalComments :: {-# UNPACK #-} !Int
      }
    | BGAddTag
      { baMsgId         :: MsgId
      , baTag           :: ItemTag
      }
    | BGRemoveTag
      { baMsgId         :: MsgId
      , baTag           :: ItemTag
      }
    | BGSkipComments
      { baMsgId         :: MsgId
      , baTotalComments :: {-# UNPACK #-} !Int
      }
    | BGIgnorePost
      { baMsgId         :: MsgId
      , baTotalComments :: {-# UNPACK #-} !Int
      }
    | BGMarkBlogRead
      { baBlogFeedUrl   :: !T.Text
      , baTotalPosts    :: {-# UNPACK #-} !Int
      , baTotalComments :: {-# UNPACK #-} !Int
      }
    | BGMarkBlogReadD
      { baBlogFeedUrl   :: !T.Text
      , baTotalPosts    :: {-# UNPACK #-} !Int
      , baTotalComments :: {-# UNPACK #-} !Int
      , baOlderThan     :: {-# UNPACK #-} !Int
      }
    | BGSetOnlyUpdatedSubscriptions
      { baValue         :: !Bool
      }
    | BGSetFolderViewMode
      { baFolder        :: !T.Text
      , baViewMode      :: MsgTreeViewMode
      }
    | BGSetSubscriptionViewMode
      { baUrl           :: !T.Text
      , baViewMode      :: MsgTreeViewMode
      }
    | BGClearAllSubscriptions
    | BGSaveFilterQuery
      { baQuery         :: !T.Text
      }
    | BGSetScrollMode
      { baScrollMode    :: ScrollMode
      }
    | BGSetListViewMode
      { baListViewMode  :: ListViewMode
      }
    | BGSetMarkReadMode
      { baMarkReadMode  :: MarkReadMode
      }
    | BGSetUltraCompact
      { baUltraCompact  :: !Bool
      }
    | BGDragAndDrop
      { baWhat          :: SubItemType
      , baInsertAfter   :: Maybe SubItemType
      , baSourceFolder  :: Maybe T.Text
      , baTargetFolder  :: Maybe T.Text
      }
    | BGSetExactUnreadCounts
      { baValue         :: !Bool
      }
    | BGSortAllFeedsAndFolders
    | BGSortFolder
      { baFolder        :: !T.Text
      }
    | BGSortTags
    | BGShareAction
      { baShareAction   :: ShareAction
      }
    | BGSetCountry
      { baCountry       :: !T.Text
      }
    deriving (Show, Eq, Ord)

data SearchResults
    = SearchResults
      { srTotal     :: {-# UNPACK #-} !Int
      , srTook      :: {-# UNPACK #-} !Int
      , srTookReal  :: {-# UNPACK #-} !Int
      , srMsgForest :: MsgForest
      }
    deriving (Show, Eq, Ord)

data FullTextCache
    = FullTextCache
      { ftcUrl       :: !TURL
      , ftcText      :: Either T.Text T.Text
      , ftcTime      :: {-# UNPACK #-} !UrTime
      , ftcReserved1 :: !Bool
      , ftcReserved2 :: !Bool
      }
    deriving (Show, Eq, Ord)

data OkErrorRedirect
    = OEROK
    | OERError
      { oerError :: !T.Text
      }
    | OERRedirect
      { oerUrl   :: !T.Text
      }
    deriving (Show, Eq, Ord)


{-!
deriving instance Binary Stats
deriving instance Binary SubscriptionState
deriving instance Binary Subscription
deriving instance Binary PostsViewMode
deriving instance Binary MsgTreeViewMode
deriving instance Binary Payment
deriving instance Binary PaidTill
deriving instance Binary UserViewMode
deriving instance Binary User
deriving instance Binary UserFilters
deriving instance Binary ScrollMode
deriving instance Binary ListViewMode
deriving instance Binary MarkReadMode
deriving instance Binary PublicFeedType
deriving instance Binary ApiKeys
deriving instance Binary UserSettings
deriving instance Binary PublicFeed
deriving instance Binary UID
deriving instance Binary MobileLogin
deriving instance Binary FeverApiKey
deriving instance Binary FeverIds
deriving instance Binary UserStats
deriving instance Binary MailQueue
deriving instance Binary Session
deriving instance Binary SubscriptionUrlKind
deriving instance Binary SubscriptionUrlInfo
deriving instance Binary Attachment
deriving instance Binary MsgKey
deriving instance Binary Msg
deriving instance Binary MsgHeader
deriving instance Binary MsgTree
deriving instance Binary CommentUrlState
deriving instance Binary BlogPostsScanned
deriving instance Binary Posts
deriving instance Binary DiscoveryFeed
deriving instance Binary PostsClearTime
deriving instance Binary PostsSubscribers
deriving instance Binary ActiveCheckSubscriptions
deriving instance Binary CommentsKey
deriving instance Binary Comments
deriving instance Binary SubscriptionParentUrl
deriving instance Binary ParentUrl
deriving instance Binary SubscriptionParentPath
deriving instance Binary ParentPath
deriving instance Binary UrlToScan
deriving instance Binary QueueType
deriving instance Binary ScanList
deriving instance Binary PostsRead
deriving instance Binary PostsTagged
deriving instance Binary PostsTaggedGuids
deriving instance Binary ItemTag
deriving instance Binary RemovedFeedInfo
deriving instance Binary GRIds
deriving instance Binary UserBackup
deriving instance Binary DeletedUser
deriving instance Binary ApiMode
deriving instance Binary MsgTreePoint
deriving instance Binary PostsReq
deriving instance Binary CommentsReq
deriving instance Binary TreeReq
deriving instance Binary MsgView
deriving instance Binary MsgId
deriving instance Binary MsgItem
deriving instance Binary MsgForest
deriving instance Binary LoginType
deriving instance Binary Counters
deriving instance Binary SITFeedDetails
deriving instance Binary SubItemType
deriving instance Binary SubItemRpc
deriving instance Binary WelcomeState
deriving instance Binary ShareAction
deriving instance Binary BrowserType
deriving instance Binary AppType
deriving instance Binary OperatingSystem
deriving instance Binary UsageFlag
deriving instance Binary UserUsageFlags
deriving instance Binary UsageFlags
deriving instance Binary BgAction
deriving instance Binary SearchResults
deriving instance Binary FullTextCache
deriving instance Binary OkErrorRedirect
!-}
#include "BinaryInstances.h"
