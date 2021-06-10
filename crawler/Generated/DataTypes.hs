-- ФАЙЛ СГЕНЕРИРОВАН АВТОМАТИЧЕСКИ (см. Gen.hs) !!!

{-# LANGUAGE CPP, BangPatterns, StandaloneDeriving, DeriveGeneric #-}
{-# OPTIONS_DERIVE --output=Generated/BinaryInstances_nonstrict.h #-}

-- | Описание структур данных, сохраняемых в Riak 
-- и передаваемых между Ur/Web и Haskell
module Generated.DataTypes where

import qualified Data.ByteString.Short as SB
import qualified Data.Text as T
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import Lib.UrTime
import qualified Lib.BArray as BA
import Lib.UnsafeRef
import Lib.ReadSet (ReadSet)
import URL
import Data.Scientific (Scientific)
import Data.Set (Set)
import Data.Map (Map)
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import Data.Binary
import Data.Hashable
-- import Control.DeepSeq
import Lib.BinaryInstances()
-- import GHC.Generics

instance Hashable ItemTag where
    hashWithSalt s ITStarred = s `hashWithSalt` (0 :: Int)
    hashWithSalt s (ITTag t) = s `hashWithSalt` t

 
data Stats
    = Stats
      { statsKey :: !T.Text
      , statsMap :: Map T.Text Int
      }
    deriving (Show, Eq{-, Generic-})

data SubscriptionParentUrl
    = SpuRedirect
      { spuUrl   :: !T.Text
      }
    | SpuHtml
      { spuUrl   :: !T.Text
      , spuDebug :: !T.Text
      }
    deriving (Show, Eq, Ord{-, Generic-})

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
    | SSErrorPath
      { ssMessage   :: !T.Text
      , ssPath      :: [SubscriptionParentUrl]
      }
    deriving (Show, Eq, Ord{-, Generic-})

data Subscription
    = Subscription
      { sUrl        :: !T.Text
      , sState      :: SubscriptionState
      , sEditsCount :: {-# UNPACK #-} !Int
      , sTitle      :: Maybe T.Text
      , sFolders    :: [T.Text]
      }
    deriving (Show, Eq, Ord{-, Generic-})

data PostsViewMode
    = PVMShort
    | PVMFull
    | PVMMagazine
    | PVMMosaic
    deriving (Show, Eq, Ord{-, Generic-})

data MTVMEx
    = MTVMFolderCollapsed
    | MTVMFolderExpanded
    | MTVMEx
      { mtvmexFolderExpanded :: !Bool
      , mtvmexGroupByFeed    :: !Bool
      , mtvmexReserved1      :: !Bool
      , mtvmexReserved2      :: {-# UNPACK #-} !Int
      }
    deriving (Show, Eq, Ord{-, Generic-})

data MsgTreeViewMode
    = MsgTreeViewMode
      { mtvmAscending        :: !Bool
      , mtvmUnreadOnly       :: !Bool
      , mtvmExpandedComments :: !Bool
      , mtvmPosts            :: PostsViewMode
      , mtvmEx               :: MTVMEx
      , mtvmNoOverride       :: !Bool
      }
    deriving (Show, Eq, Ord{-, Generic-})

data Payment
    = PReserved
    | PFastSpring
      { pOrderId   :: !T.Text
      , pOrderType :: !T.Text
      , pOrderTime :: {-# UNPACK #-} !UrTime
      }
    deriving (Show, Eq, Ord{-, Generic-})

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
    deriving (Show, Eq, Ord{-, Generic-})

data UserViewMode
    = UserViewMode
      { uvmPaidTill                 :: PaidTill
      , uvmOnlyUpdatedSubscriptions :: (Int, Bool)
      , uvmSubViewModes             :: HM.HashMap T.Text (Int, MsgTreeViewMode)
      , uvmFolderViewModes          :: HM.HashMap T.Text (Int, MsgTreeViewMode)
      , uvmSubUrlRenames            :: [(UrTime, T.Text, T.Text)]
      }
    deriving (Show, Eq{-, Generic-})

data User
    = User
      { uId            :: !T.Text
      , uSubscriptions :: [Subscription]
      , uViewMode      :: UserViewMode
      , uPayments      :: [Payment]
      }
    deriving (Show, Eq{-, Generic-})

data UserFilters
    = UserFilters
      { ufUser      :: !T.Text
      , ufFilters   :: [(UrTime, T.Text)]
      , ufReserved1 :: [T.Text]
      , ufReserved2 :: [T.Text]
      , ufReserved3 :: [T.Text]
      }
    deriving (Show, Eq, Ord{-, Generic-})

data ScrollMode
    = SMNormal
    | SMQuick
    | SMImmediate
    deriving (Show, Eq, Ord{-, Generic-})

data ListViewMode
    = LVMCompact
    | LVMTwoLines
    deriving (Show, Eq, Ord{-, Generic-})

data MarkReadMode
    = MRMOnScroll
    | MRMManual
    | MRMOnScrollEverywhere
    deriving (Show, Eq, Ord{-, Generic-})

data PublicFeedType
    = PFTAll
    | PFTFolder
      { pftFolder     :: !T.Text
      }
    | PFTTag
      { pftTagName    :: !T.Text
      }
    | PFTStarred
    | PFTAllTags
    | PFTSmartStream
      { pftStreamName :: !T.Text
      }
    deriving (Show, Eq, Ord{-, Generic-})

data LoginAccessToken
    = LATNone
    | LATFacebook
      { latAccessToken :: !T.Text
      }
    | LATTwitter
      { latCredentials :: [(T.Text, T.Text)]
      }
    deriving (Show, Eq, Ord{-, Generic-})

data ApiKeys
    = ApiKeys
      { akPocket              :: Maybe (T.Text, T.Text)
      , akPocketRequest       :: Maybe (T.Text, T.Text, T.Text)
      , akReserved10          :: Maybe Int
      , akFacebookAccessToken :: Maybe (UrTime, T.Text)
      , akTwitterAccessToken  :: Maybe (UrTime, [(T.Text, T.Text)])
      , akReserved13          :: !Bool
      , akReserved14          :: !Bool
      , akReserved15          :: !Bool
      , akReserved16          :: !Bool
      , akReserved17          :: !Bool
      , akReserved2           :: {-# UNPACK #-} !Int
      , akReserved3           :: {-# UNPACK #-} !Int
      , akReserved4           :: {-# UNPACK #-} !Int
      }
    deriving (Show, Eq, Ord{-, Generic-})

data UserExperiment
    = UENo9
    deriving (Show, Eq, Ord{-, Generic-})

data CustomShareAction
    = CustomShareAction
      { csaId        :: {-# UNPACK #-} !Int
      , csaTitle     :: !T.Text
      , csaUrlFormat :: !T.Text
      , csaShorten   :: !Bool
      }
    deriving (Show, Eq, Ord{-, Generic-})

data ShareAction
    = SAEMail
    | SATwitter
    | SAFacebook
    | SAGooglePlus
    | SATumblr
    | SAEvernote
    | SADelicious_discontinued
    | SAPinboard
    | SAPocket
    | SAReadability_discontinued
    | SAInstapaper
    | SATranslate
    | SABlogger
    | SAWordpress
    | SALinkedIn
    | SAPinterest
    | SAVK
    | SASkype
    | SAReddit
    | SAStumbleUpon
    | SADigg
    | SAScoopIt
    | SAFlipboard
    | SABuffer
    | SANewsVine
    | SADiigo
    | SARememberTheMilk
    | SAGoogleBookmarks
    | SAWallabag
    | SAWakelet
    | SACustom
      { saCustomShareAction :: CustomShareAction
      }
    | SASystem
    deriving (Show, Eq, Ord{-, Generic-})

data MsgButton
    = MBKeepUnread
    | MBStar
    | MBTag
    | MBShare
    | MBShareAction
      { mbShareAction :: ShareAction
      }
    deriving (Show, Eq, Ord{-, Generic-})

data EmailContact
    = EMailContact
      { ctEMail    :: !T.Text
      , ctFullName :: !T.Text
      , ctGroups   :: [T.Text]
      , ctAvatar   :: Maybe T.Text
      , ctStats    :: Maybe (UrTime, Int)
      }
    deriving (Show, Eq, Ord{-, Generic-})

data SharingSettings
    = SharingSettings
      { shsShareMenuButtons   :: Maybe [MsgButton]
      , shsMsgButtons         :: Maybe [MsgButton]
      , shsListViewButtons    :: Maybe [MsgButton]
      , shsCustomShareActions :: [CustomShareAction]
      , shsEMailUsingMailto   :: !Bool
      , shsReplyToEMail       :: Maybe (T.Text, T.Text)
      , shsContacts           :: [EmailContact]
      , shsReserved1          :: {-# UNPACK #-} !Int
      }
    deriving (Show, Eq, Ord{-, Generic-})

data LoginType
    = LTGoogle
      { ltEmail    :: !T.Text
      }
    | LTFacebook
      { ltEmail    :: !T.Text
      }
    | LTTwitter
      { ltId       :: !T.Text
      }
    | LTOpenId
      { ltURL      :: !T.Text
      }
    | LTEmail
      { ltEmail    :: !T.Text
      }
    | LTUsername
      { ltUsername :: !T.Text
      }
    | LTFeverApiKey
      { ltApiKey   :: !T.Text
      }
    deriving (Show, Eq, Ord{-, Generic-})

data Login
    = Login
      { lLoginType :: LoginType
      , lUserID    :: !T.Text
      , lReserved1 :: {-# UNPACK #-} !Int
      , lReserved2 :: {-# UNPACK #-} !Int
      , lReserved3 :: {-# UNPACK #-} !Int
      , lReserved4 :: {-# UNPACK #-} !Int
      }
    deriving (Show, Eq, Ord{-, Generic-})

data UserSettingsEx
    = UserSettingsEx
      { usteLastWhatsNewTime       :: {-# UNPACK #-} !UrTime
      , ustePasswordHash           :: Maybe T.Text
      , usteReserved1_1            :: Maybe Int
      , usteReserved1_2            :: Maybe Int
      , usteReserved1_3            :: Maybe Int
      , usteReserved1_4            :: Maybe Int
      , usteReserved1_5            :: Maybe Int
      , usteReserved1_6            :: Maybe Int
      , usteReserved1_7            :: Maybe Int
      , usteAssociatedAccounts     :: [LoginType]
      , usteAssociatedAccountNames :: Map LoginType T.Text
      , usteReserved4              :: {-# UNPACK #-} !Int
      , usteReserved5              :: {-# UNPACK #-} !Int
      , usteReserved6              :: {-# UNPACK #-} !Int
      , usteReserved7              :: {-# UNPACK #-} !Int
      , usteReserved8              :: {-# UNPACK #-} !Int
      , usteReserved9              :: {-# UNPACK #-} !Int
      , usteReserved10             :: {-# UNPACK #-} !Int
      , usteReserved11             :: {-# UNPACK #-} !Int
      , usteReserved12             :: {-# UNPACK #-} !Int
      , usteReserved13             :: {-# UNPACK #-} !Int
      , usteReserved14             :: {-# UNPACK #-} !Int
      , usteReserved15             :: {-# UNPACK #-} !Int
      }
    deriving (Show, Eq{-, Generic-})

data UserSettings
    = UserSettings
      { ustUser              :: !T.Text
      , ustEditsCount        :: {-# UNPACK #-} !Int
      , ustScrollMode        :: ScrollMode
      , ustListViewMode      :: ListViewMode
      , ustShowFavicons      :: !Bool
      , ustMarkReadMode      :: MarkReadMode
      , ustUltraCompact      :: !Bool
      , ustReserved          :: Maybe T.Text
      , ustExactUnreadCounts :: !Bool
      , ustPublicFeeds       :: Maybe (Map PublicFeedType [(T.Text, Bool, Maybe T.Text)])
      , ustCountry           :: Maybe T.Text
      , ustApiKeys           :: Maybe ApiKeys
      , ustExperiments       :: Maybe [UserExperiment]
      , ustSharingSettings_  :: Maybe SharingSettings
      , ustEx                :: Maybe UserSettingsEx
      }
    deriving (Show, Eq{-, Generic-})

data PublicFeed
    = PublicFeed
      { pfId        :: !T.Text
      , pfUser      :: Maybe T.Text
      , pfReserved1 :: Maybe T.Text
      , pfReserved2 :: Maybe T.Text
      , pfReserved3 :: Maybe T.Text
      , pfReserved4 :: Maybe T.Text
      }
    deriving (Show, Eq, Ord{-, Generic-})

data UID
    = EMail
      { uidId :: !T.Text
      }
    | Url
      { uidId :: !T.Text
      }
    deriving (Show, Eq, Ord{-, Generic-})

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
    deriving (Show, Eq{-, Generic-})

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
    deriving (Show, Eq{-, Generic-})

data MailQueue
    = MailQueue
      { mqId       :: !T.Text
      , mqActive   :: Set T.Text
      , mqInactive :: Set T.Text
      }
    deriving (Show, Eq{-, Generic-})

data Session
    = Session
      { sessionKey     :: !T.Text
      , sessionExpire  :: {-# UNPACK #-} !UrTime
      , sessionCleared :: !Bool
      , sessionUser    :: !T.Text
      }
    deriving (Show, Eq, Ord{-, Generic-})

data EmailVerificationType
    = EVTSignUp
      { evtPasswordHash :: !T.Text
      , evtFeverApiKey  :: !T.Text
      }
    | EVTChangeEmail
      { evtUser         :: !T.Text
      }
    | EVTResetPassword
      { evtUser         :: !T.Text
      }
    | EVTRestoreAccess
      { evtUser         :: !T.Text
      }
    deriving (Show, Eq, Ord{-, Generic-})

data EmailVerificationToken
    = EmailVerificationToken
      { evtkToken            :: !T.Text
      , evtkExpire           :: {-# UNPACK #-} !UrTime
      , evtkVerified         :: !Bool
      , evtkEmail            :: !T.Text
      , evtkVerificationType :: EmailVerificationType
      , evtkReserved1        :: {-# UNPACK #-} !Int
      , evtkReserved2        :: {-# UNPACK #-} !Int
      , evtkReserved3        :: {-# UNPACK #-} !Int
      , evtkReserved4        :: {-# UNPACK #-} !Int
      }
    deriving (Show, Eq, Ord{-, Generic-})

data EmailVerification
    = EmailVerification
      { evEmail             :: !T.Text
      , evVerified          :: [T.Text]
      , evSignUpTokens      :: [(UrTime, T.Text)]
      , evChangeEmailTokens :: HM.HashMap T.Text [(UrTime, T.Text)]
      , evResetTokens       :: [(UrTime, T.Text)]
      , evReserved1         :: {-# UNPACK #-} !Int
      , evReserved2         :: {-# UNPACK #-} !Int
      , evReserved3         :: {-# UNPACK #-} !Int
      , evReserved4         :: {-# UNPACK #-} !Int
      }
    deriving (Show, Eq{-, Generic-})

data UserEmailVerificationTokens
    = UserEmailVerificationTokens
      { uevtUser      :: !T.Text
      , uevtTokens    :: HM.HashMap T.Text (UrTime, T.Text, EmailVerificationType)
      , uevtReserved1 :: {-# UNPACK #-} !Int
      , uevtReserved2 :: {-# UNPACK #-} !Int
      , uevtReserved3 :: {-# UNPACK #-} !Int
      , uevtReserved4 :: {-# UNPACK #-} !Int
      }
    deriving (Show, Eq{-, Generic-})

data SubscriptionUrlKind
    = SUKError
      { sukMessage :: !T.Text
      }
    | SUKFeed
      { sukUrl     :: !TURL
      }
    | SUKErrorPath
      { sukMessage :: !T.Text
      , sukPath    :: [SubscriptionParentUrl]
      }
    deriving (Show, Eq, Ord{-, Generic-})

data SubscriptionUrlInfo
    = SubscriptionUrlInfo
      { suiUrl  :: !TURL
      , suiTime :: {-# UNPACK #-} !UrTime
      , suiKind :: SubscriptionUrlKind
      }
    deriving (Show, Eq, Ord{-, Generic-})

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
      , aGuid        :: !SB.ShortByteString
      , aStreamTitle :: !T.Text
      , aHtmlUrl     :: !T.Text
      }
    | AVideo2
      { aUrl         :: !TURL
      , aMime        :: !T.Text
      , aFileSize    :: Maybe Int
      , aDuration    :: Maybe Int
      , aTitle       :: Maybe T.Text
      , aWidth       :: Maybe Int
      , aHeight      :: Maybe Int
      , aPoster      :: Maybe TURL
      , aLoop        :: !Bool
      }
    | AThumbnail
      { aUrl         :: !TURL
      }
    deriving (Show, Eq, Ord{-, Generic-})

data MsgKey
    = MsgKey
      { msgKeyBlogFeedUrl :: !T.Text
      , msgKeyPostGuid    :: Maybe SB.ShortByteString
      , msgKeyCommentGuid :: Maybe SB.ShortByteString
      }
    deriving (Show, Eq, Ord{-, Generic-})

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
      , msgShorterText :: !T.Text
      }
    deriving (Show, Eq, Ord{-, Generic-})

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
    deriving (Show, Eq, Ord{-, Generic-})

data TimeId
    = TimeId
      { tiTime :: {-# UNPACK #-} !UrTime
      , tiId   :: {-# UNPACK #-} !Int
      }
    deriving (Show, Eq, Ord{-, Generic-})

data MsgTree
    = MsgTree
      { mtHeaders  :: BA.Array Int MsgHeader
      , mtChildren :: IntMap (Set TimeId)
      }
    deriving (Show, Eq{-, Generic-})

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
    deriving (Show, Eq, Ord{-, Generic-})

data BlogPostsScanned
    = BlogPostsScanned
      { bpsBlogFeedUrl   :: !TURL
      , bpsSubscribeTime :: {-# UNPACK #-} !UrTime
      , bpsUrls          :: Map SB.ShortByteString (Map TURL (UrTime, Maybe UrTime, CommentUrlState))
      }
    deriving (Show, Eq{-, Generic-})

data Posts
    = Posts
      { pBlogFeedUrl     :: !TURL
      , pUpdatedComments :: IntSet 
      , pRootMessage     :: Msg
      , pMsgTree         :: MsgTree
      , pTotalComments   :: {-# UNPACK #-} !Int
      , pDeletedComments :: {-# UNPACK #-} !Int
      , pCommentCounts   :: IntMap (IntMap Int)
      , pCCVersions      :: Map UrTime Int
      }
    deriving (Show, Eq{-, Generic-})

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
      , dfAveragePostLength     :: !Double
      , dfPaidCountries         :: HM.HashMap T.Text Int
      , dfCountries             :: HM.HashMap T.Text Int
      }
    deriving (Show, Eq{-, Generic-})

data PostsClearTime
    = PostsClearTime
      { pctBlogFeedUrl :: !TURL
      , pctTime        :: {-# UNPACK #-} !UrTime
      , pctReserved1   :: {-# UNPACK #-} !Int
      , pctReserved2   :: {-# UNPACK #-} !Int
      , pctReserved3   :: {-# UNPACK #-} !Int
      , pctReserved4   :: {-# UNPACK #-} !Int
      }
    deriving (Show, Eq, Ord{-, Generic-})

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
    deriving (Show, Eq{-, Generic-})

data ActiveCheckSubscriptions
    = ActiveCheckSubscriptions
      { acsKey   :: !()
      , acsUsers :: HM.HashMap T.Text UrTime
      }
    deriving (Show, Eq{-, Generic-})

data CommentsKey
    = CommentsKey
      { ckBlogFeedUrl :: !T.Text
      , ckPostGuid    :: !SB.ShortByteString
      }
    deriving (Show, Eq, Ord{-, Generic-})

data Comments
    = Comments
      { cKey     :: CommentsKey
      , cMsgTree :: MsgTree
      }
    deriving (Show, Eq{-, Generic-})

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
      , puGuid  :: Maybe SB.ShortByteString
      }
    | PuCommentsFeed
      { puUrl   :: !TURL
      }
    deriving (Show, Eq, Ord{-, Generic-})

data SubscriptionParentPath
    = SubscriptionParentPath
      { sppSubscriptionUrl :: !TURL
      , sppParents         :: [SubscriptionParentUrl]
      }
    deriving (Show, Eq, Ord{-, Generic-})

data ParentPath
    = ParentPath
      { ppBlogFeedUrl :: !TURL
      , ppParents     :: [ParentUrl]
      }
    deriving (Show, Eq, Ord{-, Generic-})

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
    deriving (Show, Eq, Ord{-, Generic-})

data QueueType
    = QTSubscription
    | QTBlogFeed
    | QTTemporary1
    | QTNewComment1
    | QTRescan1
    | QTTemporary
    | QTNewComment
    | QTRescan
    | QTSubscriptionOPML
    deriving (Show, Eq, Ord{-, Generic-})

data ScanList
    = ScanList
      { slTime :: {-# UNPACK #-} !UrTime
      , slUrls :: [(TURL, QueueType)]
      }
    deriving (Show, Eq, Ord{-, Generic-})

data OldFeedMask
    = OldFeedMask
      { ofmPosts    :: !ReadSet
      , ofmComments :: Maybe (IntMap ReadSet)
      }
    deriving (Show, Eq{-, Generic-})

data FeedMask
    = FMFeedMask
      { fmPostsMask    :: !ReadSet
      , fmCommentsMask :: Maybe (IntMap ReadSet)
      }
    | FMError
    deriving (Show, Eq{-, Generic-})

data PostsRead
    = PostsRead
      { prKey               :: (T.Text, TURL)
      , prSet               :: !ReadSet
      , prTotalCommentsRead :: {-# UNPACK #-} !Int
      , prCommentsRead      :: IntMap ReadSet
      , prIgnoredPosts      :: !ReadSet
      , prIgnoredComments   :: IntMap (IntSet )
      }
    deriving (Show, Eq{-, Generic-})

data PostsTagged
    = PostsTagged
      { ptBlogFeedUrl :: !TURL
      , ptSet         :: !ReadSet
      , ptReserved1   :: {-# UNPACK #-} !Int
      , ptReserved2   :: {-# UNPACK #-} !Int
      , ptReserved3   :: {-# UNPACK #-} !Int
      , ptReserved4   :: {-# UNPACK #-} !Int
      }
    deriving (Show, Eq, Ord{-, Generic-})

data PostsTaggedGuids
    = PostsTaggedGuids
      { ptgBlogFeedUrl :: !TURL
      , ptgGuids       :: IntMap SB.ShortByteString
      , ptgReserved1   :: {-# UNPACK #-} !Int
      , ptgReserved2   :: {-# UNPACK #-} !Int
      , ptgReserved3   :: {-# UNPACK #-} !Int
      , ptgReserved4   :: {-# UNPACK #-} !Int
      }
    deriving (Show, Eq{-, Generic-})

data ItemTag
    = ITStarred
    | ITTag
      { itTagName :: !T.Text
      }
    deriving (Show, Eq, Ord{-, Generic-})

data RemovedFeedInfo
    = RemovedFeedInfo
      { rfiStreamTitle :: !T.Text
      , rfiHtmlUrl     :: !T.Text
      , rfiTime        :: {-# UNPACK #-} !UrTime
      , rfiReserved    :: {-# UNPACK #-} !Int
      }
    deriving (Show, Eq, Ord{-, Generic-})

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
    deriving (Show, Eq{-, Generic-})

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
    deriving (Show, Eq{-, Generic-})

data DeletedUser
    = DeletedUser
      { duUser      :: !T.Text
      , duBackups   :: [UrTime]
      , duMailsSent :: Maybe [UrTime]
      , duReserved2 :: !Bool
      , duReserved3 :: !Bool
      , duReserved4 :: !Bool
      }
    deriving (Show, Eq, Ord{-, Generic-})

data MailType
    = MTRenewInTwoWeeksReminder
      { mtPaidTill :: {-# UNPACK #-} !UrTime
      }
    | MTInactivityReasonRequest
    deriving (Show, Eq, Ord{-, Generic-})

data MailsSent
    = MailsSent
      { msUser      :: !T.Text
      , msMailsSent :: Set MailType
      , msReserved2 :: {-# UNPACK #-} !Int
      , msReserved3 :: {-# UNPACK #-} !Int
      , msReserved4 :: {-# UNPACK #-} !Int
      }
    deriving (Show, Eq{-, Generic-})

data FilterQuery
    = FilterQuery
      { fqQuery     :: !T.Text
      , fqNegate    :: !Bool
      , fqFeeds     :: HM.HashMap T.Text Bool
      , fqReserved1 :: {-# UNPACK #-} !Int
      , fqReserved2 :: {-# UNPACK #-} !Int
      }
    deriving (Show, Eq{-, Generic-})

data FilterQueryRpc
    = FilterQueryRpc
      { fqrQuery     :: !T.Text
      , fqrNegate    :: !Bool
      , fqrFeedGRIds :: [Int]
      }
    deriving (Show, Eq, Ord{-, Generic-})

data SearchError
    = SESyntaxError
      { seErrorMessage :: !T.Text
      }
    | SESystemError
      { seErrorMessage :: !T.Text
      }
    deriving (Show, Eq, Ord{-, Generic-})

data FilterUpdateTime
    = FUTNever
    | FUTUpdatedAt
      { futUpdateTime   :: {-# UNPACK #-} !UrTime
      }
    | FUTError
      { futErrorTime    :: {-# UNPACK #-} !UrTime
      , futSearchError  :: SearchError
      }
    | FUTEdited
      { futUpdateTime   :: {-# UNPACK #-} !UrTime
      , futChangedFeeds :: IntSet 
      }
    deriving (Show, Eq, Ord{-, Generic-})

data FilterFeedMasks
    = FilterFeedMasks
      { ffmLastUpdated  :: FilterUpdateTime
      , ffmFeedMasks    :: IntMap FeedMask
      , ffmOldFeedMasks :: IntMap OldFeedMask
      , ffmReserved2    :: {-# UNPACK #-} !Int
      }
    deriving (Show, Eq{-, Generic-})

data OldSmartStream
    = OldSmartStream
      { ossName      :: !T.Text
      , ossQueries   :: [FilterQuery]
      , ossFeedMasks :: FilterFeedMasks
      , ossReserved1 :: {-# UNPACK #-} !Int
      , ossReserved2 :: {-# UNPACK #-} !Int
      }
    deriving (Show, Eq{-, Generic-})

data Filter
    = Filter
      { filterId        :: {-# UNPACK #-} !Int
      , filterQuery     :: FilterQuery
      , filterFeedMasks :: FilterFeedMasks
      , filterReserved1 :: {-# UNPACK #-} !Int
      , filterReserved2 :: {-# UNPACK #-} !Int
      }
    deriving (Show, Eq{-, Generic-})

data SmartStream
    = SmartStream
      { ssName                :: !T.Text
      , ssQuery               :: FilterQuery
      , ssFeedMasks           :: FilterFeedMasks
      , ssUnfilteredFeedMasks :: FilterFeedMasks
      , ssReserved1           :: {-# UNPACK #-} !Int
      , ssReserved2           :: {-# UNPACK #-} !Int
      }
    deriving (Show, Eq{-, Generic-})

data Filters
    = Filters
      { fUser            :: !T.Text
      , fVersion         :: {-# UNPACK #-} !Int
      , fOldFilters      :: [FilterQuery]
      , fFeedMasks       :: FilterFeedMasks
      , fOldSmartStreams :: [OldSmartStream]
      , fOverloadDelay   :: {-# UNPACK #-} !Int
      , fNewFilters      :: [Filter]
      , fNewSmartStreams :: [SmartStream]
      , fReserved4       :: {-# UNPACK #-} !Int
      }
    deriving (Show, Eq{-, Generic-})

data ApiMode
    = AMNormal
      { amHostName         :: !T.Text
      , amAcceptLanguage   :: !T.Text
      }
    | AMGRIdsOnly
      { amFetch            :: !Bool
      , amCount            :: {-# UNPACK #-} !Int
      , amContinuation     :: UnsafeRef (Maybe MsgKey)
      , amMinDlTime        :: Maybe UrTime
      , amMaxDlTime        :: Maybe UrTime
      , amMaxTime          :: Maybe UrTime
      , amExcludeTags      :: HS.HashSet ItemTag
      , amIncludeTags      :: HS.HashSet ItemTag
      , amReadOnly         :: !Bool
      , amMsgLinkParams    :: [(T.Text, T.Text)]
      , amFromUI           :: !Bool
      , amMaxMsgTextLength :: Maybe Int
      }
    | AMDiscovery
      { amHostName         :: !T.Text
      , amAcceptLanguage   :: !T.Text
      , amUrl              :: !T.Text
      }
    deriving (Show, Eq{-, Generic-})

data MsgTreePoint
    = MsgTreePoint
      { mtpParentId :: {-# UNPACK #-} !Int
      , mtpTime     :: {-# UNPACK #-} !UrTime
      , mtpId       :: {-# UNPACK #-} !Int
      }
    deriving (Show, Eq, Ord{-, Generic-})

data PostsReq
    = PostsReq
      { prqFeedId        :: {-# UNPACK #-} !Int
      , prqMsgTreePoint  :: MsgTreePoint
      , prqTotalPosts    :: {-# UNPACK #-} !Int
      , prqTotalComments :: {-# UNPACK #-} !Int
      }
    deriving (Show, Eq, Ord{-, Generic-})

data CommentsReq
    = CommentsReq
      { crqKey           :: CommentsKey
      , crqPostId        :: {-# UNPACK #-} !Int
      , crqMsgTreePoint  :: MsgTreePoint
      , crqTotalComments :: {-# UNPACK #-} !Int
      }
    deriving (Show, Eq, Ord{-, Generic-})

data MsgId
    = MsgId
      { midFeedId    :: {-# UNPACK #-} !Int
      , midPostId    :: {-# UNPACK #-} !Int
      , midCommentId :: Maybe Int
      }
    deriving (Show, Eq, Ord{-, Generic-})

data LongMsgId
    = LongMsgId
      { lmidMsgKey :: MsgKey
      , lmidMsgId  :: MsgId
      }
    deriving (Show, Eq, Ord{-, Generic-})

data TreeReq
    = TRPosts
      { trReqs         :: [PostsReq]
      }
    | TRTags
      { trTags         :: Maybe [ItemTag]
      , trMaxTag       :: Maybe (UrTime, MsgId)
      , trLastMsg      :: Maybe (UrTime, MsgId)
      }
    | TRComments
      { trOnExpand     :: !Bool
      , trReq          :: CommentsReq
      }
    | TRCommentsS
      { trOnExpand     :: !Bool
      , trStreamName   :: !T.Text
      , trReq          :: CommentsReq
      }
    | TRSmartStream
      { trStreamName   :: !T.Text
      , trReqs         :: [PostsReq]
      }
    | TRSearchPosts
      { trQuery        :: !T.Text
      , trFeedMasksKey :: !T.Text
      , trReqs         :: [PostsReq]
      }
    | TRSearchSmartStream
      { trStreamName   :: !T.Text
      , trQuery        :: !T.Text
      , trFeedMasksKey :: !T.Text
      , trReqs         :: [PostsReq]
      }
    | TRSearchTags
      { trQuery        :: !T.Text
      , trIdsKey       :: !T.Text
      , trTags         :: Maybe [ItemTag]
      , trMaxTag       :: Maybe (UrTime, MsgId)
      , trLastMsg      :: Maybe (UrTime, MsgId)
      }
    deriving (Show, Eq, Ord{-, Generic-})

data MsgView
    = MVFull
      { msgViewMsg       :: Msg
      }
    | MVShort
      { msgViewHeader    :: MsgHeader
      , msgViewCachedMsg :: Maybe Msg
      }
    deriving (Show, Eq, Ord{-, Generic-})

data MsgItem
    = MsgItem
      { miMsgView      :: MsgView
      , miMsgKey       :: MsgKey
      , miMsgId        :: MsgId
      , miRead         :: !Bool
      , miTags         :: [ItemTag]
      , miSmartStreams :: IntSet 
      , miReadLocked   :: !Bool
      , miFull         :: !Bool
      , miSearchResult :: !Bool
      }
    deriving (Show, Eq, Ord{-, Generic-})

data MsgForest
    = MsgForest
      { mfTotalCount                    :: {-# UNPACK #-} !Int
      , mfUnreadCount                   :: {-# UNPACK #-} !Int
      , mfTotalResultsCount             :: {-# UNPACK #-} !Int
      , mfUnreadResultsCount            :: {-# UNPACK #-} !Int
      , mfSmartStreamUnreadCounts       :: IntMap Int
      , mfSmartStreamUnreadResultCounts :: IntMap Int
      , mfTagTotalCounts                :: Map (Maybe ItemTag) Int
      , mfTagUnreadCounts               :: Map (Maybe ItemTag) Int
      , mfTagUnreadResultCounts         :: Map (Maybe ItemTag) Int
      , mfList                          :: [(MsgItem, MsgForest)]
      , mfNextReq                       :: Maybe TreeReq
      }
    deriving (Show, Eq{-, Generic-})

data ExternalLoginType
    = Google
    | Facebook
    | Twitter
    | OpenId
      { eltURL :: !T.Text
      }
    deriving (Show, Eq, Ord{-, Generic-})

data ExternalLoginAction
    = ELALogin
    | ELAAddUrl
      { elaURL :: !T.Text
      }
    | ELAAddAssociatedAccount
    deriving (Show, Eq, Ord{-, Generic-})

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
    deriving (Show, Eq, Ord{-, Generic-})

data SITFeedDetails
    = SITFeedDetails
      { sitPointAllAsc              :: Maybe MsgTreePoint
      , sitPointUnreadAsc           :: Maybe MsgTreePoint
      , sitPointUnreadDesc          :: Maybe MsgTreePoint
      , sitPointUnreadPostsOnlyAsc  :: Maybe MsgTreePoint
      , sitPointUnreadPostsOnlyDesc :: Maybe MsgTreePoint
      }
    deriving (Show, Eq, Ord{-, Generic-})

data SubItemType
    = SITAll
    | SITSearch
      { sitQuery          :: !T.Text
      }
    | SITFolder
      { sitFolder         :: !T.Text
      }
    | SITFeed
      { sitSubscription   :: Subscription
      , sitFeedLink       :: Maybe T.Text
      , sitPointAllDesc   :: Maybe MsgTreePoint
      }
    | SITTag
      { sitTagName        :: !T.Text
      }
    | SITSmartStream
      { sitStreamName     :: !T.Text
      , sitStreamFeedSirs :: [Int]
      }
    | SITStarred
    | SITAllTags
    deriving (Show, Eq, Ord{-, Generic-})

data SubItemRpc
    = SubItemRpc
      { sirPath          :: !T.Text
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
    deriving (Show, Eq, Ord{-, Generic-})

data WelcomeState
    = WelcomeState
      { wsHasPrevAccount  :: !Bool
      , wsHasPrevSubs     :: !Bool
      , wsStarredRestored :: !Bool
      , wsTaggedRestored  :: !Bool
      }
    deriving (Show, Eq, Ord{-, Generic-})

data UpdateFilters
    = UFNone
    | UFChanged
    | UFAll
    deriving (Show, Eq, Ord{-, Generic-})

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
    | BTVivaldi
    | BTEdge
    deriving (Show, Eq, Ord{-, Generic-})

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
    | ATUnread
    | ATFeedMe
    | ATFieryFeeds
    | ATLire
    | ATWebSubscriber
    | ATReadably
    | ATokhttp
    | ATFluentReader
    | ATRavenReader
    | ATFocusReader
    | ATNetNewsWire
    deriving (Show, Eq, Ord{-, Generic-})

data OperatingSystem
    = OSUnknown
    | OSWindows
    | OSMac
    | OSLinux
    | OSAndroid
    | OSIOS
    | OSChromeOS
    deriving (Show, Eq, Ord{-, Generic-})

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
    | UFSetUsername
    | UFEnablePublicFeed
    | UFDisablePublicFeed
    | UFGenerateNewPublicFeed
    | UFDeleteAccount
    | UFExportOPML
    | UFMarkAllAsReadD
      { ufOlderThan       :: {-# UNPACK #-} !Int
      }
    | UFMarkSearchAsReadD
      { ufOlderThan       :: {-# UNPACK #-} !Int
      }
    | UFFilterApply
    | UFFilterHide
    | UFNewSmartStream
    | UFEditFilter
    | UFEditSmartStream
    | UFDeleteFilter
    | UFDeleteSmartStream
    | UFWhatsNewClick
      { ufTime            :: {-# UNPACK #-} !UrTime
      }
    | UFWhatsNewClose
      { ufTime            :: {-# UNPACK #-} !UrTime
      }
    | UFThemeChange
      { ufThemeName       :: !T.Text
      }
    | UFFontChange
      { ufFontName        :: !T.Text
      }
    | UFFontSizeChange
      { ufSize            :: {-# UNPACK #-} !Int
      }
    | UFLineHeightChange
      { ufPixels          :: {-# UNPACK #-} !Int
      , ufFontSize        :: {-# UNPACK #-} !Int
      }
    | UFSetPassword
    | UFSetEmail
    | UFMarkReadAbove
    | UFMarkReadBelow
    | UFUnstarAbove
    | UFUnstarBelow
    | UFUntagAbove
    | UFUntagBelow
    deriving (Show, Eq, Ord{-, Generic-})

data UserUsageFlags
    = UserUsageFlags
      { uufPaidTill   :: PaidTill
      , uufCountry    :: !T.Text
      , uufUsageFlags :: Set UsageFlag
      , uufReserved1  :: {-# UNPACK #-} !Int
      , uufReserved2  :: {-# UNPACK #-} !Int
      , uufReserved3  :: {-# UNPACK #-} !Int
      , uufReserved4  :: {-# UNPACK #-} !Int
      }
    deriving (Show, Eq{-, Generic-})

data UsageFlags
    = UsageFlags
      { uflTime      :: {-# UNPACK #-} !UrTime
      , uflFlags     :: HM.HashMap T.Text UserUsageFlags
      , uflReserved1 :: {-# UNPACK #-} !Int
      , uflReserved2 :: {-# UNPACK #-} !Int
      , uflReserved3 :: {-# UNPACK #-} !Int
      , uflReserved4 :: {-# UNPACK #-} !Int
      }
    deriving (Show, Eq{-, Generic-})

data UserSessions
    = UserSessions
      { uSessionsUser      :: !T.Text
      , uSessionsSessions  :: HS.HashSet T.Text
      , uSessionsReserved1 :: {-# UNPACK #-} !Int
      , uSessionsReserved2 :: {-# UNPACK #-} !Int
      , uSessionsReserved3 :: {-# UNPACK #-} !Int
      , uSessionsReserved4 :: {-# UNPACK #-} !Int
      }
    deriving (Show, Eq{-, Generic-})

data MarkReq
    = MRPosts
      { mrFeedTcs      :: [(Int, (Int, Int))]
      }
    | MRTags
      { mrTags         :: Maybe [ItemTag]
      , mrMaxTag       :: Maybe (UrTime, MsgId)
      }
    | MRSmartStream
      { mrStreamName   :: !T.Text
      , mrFeedTcs      :: [(Int, (Int, Int))]
      }
    | MRSearchPosts
      { mrQuery        :: !T.Text
      , mrFeedMasksKey :: !T.Text
      , mrFeedTcs      :: [(Int, (Int, Int))]
      }
    | MRSearchTags
      { mrQuery        :: !T.Text
      , mrIdsKey       :: !T.Text
      , mrTags         :: Maybe [ItemTag]
      , mrMaxTag       :: Maybe (UrTime, MsgId)
      }
    | MRSearchSmartStream
      { mrStreamName   :: !T.Text
      , mrQuery        :: !T.Text
      , mrFeedMasksKey :: !T.Text
      , mrFeedTcs      :: [(Int, (Int, Int))]
      }
    deriving (Show, Eq, Ord{-, Generic-})

data MarkReadDirection
    = MRDAll
    | MRDAbove
      { mrdPoint :: (UrTime, (Bool, MsgId), (Bool, MsgId))
      }
    | MRDBelow
      { mrdPoint :: (UrTime, (Bool, MsgId), (Bool, MsgId))
      }
    deriving (Show, Eq, Ord{-, Generic-})

data BgAction
    = BGMarkMsgRead
      { baMsgId         :: MsgId
      , baRead          :: !Bool
      , baTotalComments :: {-# UNPACK #-} !Int
      }
    | BGAddTag
      { baLongMsgId     :: LongMsgId
      , baTag           :: ItemTag
      }
    | BGRemoveTag
      { baLongMsgId     :: LongMsgId
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
    | BGMarkRead
      { baDirection     :: MarkReadDirection
      , baOlderThan     :: {-# UNPACK #-} !Int
      , baViewMode      :: MsgTreeViewMode
      , baPosts         :: [(UrTime, MsgId)]
      , baMarkReq       :: MarkReq
      }
    | BGRemoveTagFromTree
      { baAbove         :: !Bool
      , baTags          :: Maybe [ItemTag]
      , baViewMode      :: MsgTreeViewMode
      , baTreeReqs      :: [TreeReq]
      }
    | BGRemoveTagD
      { baTags          :: Maybe [ItemTag]
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
    | BGWhatsNewClick
      { baTime          :: {-# UNPACK #-} !UrTime
      }
    | BGWhatsNewClose
      { baTime          :: {-# UNPACK #-} !UrTime
      }
    deriving (Show, Eq, Ord{-, Generic-})

data FeedsOrDiscovery
    = FODFeeds
      { fodReadCounters :: [(Int, Int, Int, Int, Int)]
      }
    | FODFeedsApi
      { fodAPIMode      :: ApiMode
      , fodFeeds        :: [T.Text]
      }
    | FODDiscovery
      { fodUrl          :: !T.Text
      }
    deriving (Show, Eq{-, Generic-})

data FilterResults
    = FilterResults
      { frTotalPosts     :: {-# UNPACK #-} !Int
      , frTotalComments  :: {-# UNPACK #-} !Int
      , frUnreadPosts    :: {-# UNPACK #-} !Int
      , frUnreadComments :: {-# UNPACK #-} !Int
      , frTook           :: {-# UNPACK #-} !Int
      , frTookReal       :: {-# UNPACK #-} !Int
      , frMsgForest      :: MsgForest
      }
    deriving (Show, Eq{-, Generic-})

data EmailAddress
    = EmailAddress
      { eaEmail     :: !T.Text
      , eaFirstName :: !T.Text
      , eaLastName  :: !T.Text
      }
    deriving (Show, Eq, Ord{-, Generic-})

data FullText
    = FTError
      { ftMessage :: !T.Text
      }
    | FTTextV0
      { ftText    :: !T.Text
      }
    | FTTitleAndText
      { ftTitle   :: !T.Text
      , ftText    :: !T.Text
      }
    deriving (Show, Eq, Ord{-, Generic-})

data FullTextCache
    = FullTextCache
      { ftcUrl       :: !TURL
      , ftcText      :: FullText
      , ftcTime      :: {-# UNPACK #-} !UrTime
      , ftcReserved1 :: !Bool
      , ftcReserved2 :: !Bool
      }
    deriving (Show, Eq, Ord{-, Generic-})

data OkErrorRedirect
    = OEROK
    | OERError
      { oerError :: !T.Text
      }
    | OERRedirect
      { oerUrl   :: !T.Text
      }
    deriving (Show, Eq, Ord{-, Generic-})

data PageIconSize
    = PISAny
    | PIS
      { pisWidth  :: {-# UNPACK #-} !Int
      , pisHeight :: {-# UNPACK #-} !Int
      }
    deriving (Show, Eq, Ord{-, Generic-})

data PageInfoTitleSource
    = PITSTag
    | PITSItempropName
    | PITSTwitter
    | PITSOpenGraph
    deriving (Show, Eq, Ord{-, Generic-})

data PageInfoDescriptionSource
    = PIDSItemprop
    | PIDSName
    | PIDSTwitter
    | PIDSOpenGraph
    deriving (Show, Eq, Ord{-, Generic-})

data PageInfoImageSource
    = PIISLinkRelImageSrc
    | PIISItemprop
    | PIISTwitter
    | PIISOpenGraph
    | PIISUserPic
    deriving (Show, Eq, Ord{-, Generic-})

data PageInfoIconSource
    = PIICSIcon
    | PIICSShortcutIcon
    | PIICSShortcut
    | PIICSAppleTouchIcon
    | PIICSAppleTouchIconPrecomposed
    | PIICSIconMask
    deriving (Show, Eq, Ord{-, Generic-})

data PageInfo
    = PageInfo
      { piUrl               :: !TURL
      , piFetchTime         :: {-# UNPACK #-} !UrTime
      , piRedownloadOptions :: [T.Text]
      , piError             :: Maybe (UrTime, T.Text)
      , piContentType       :: Maybe T.Text
      , piContentLength     :: Maybe Int
      , piTitle             :: [(PageInfoTitleSource, T.Text)]
      , piDescription       :: [(PageInfoDescriptionSource, T.Text)]
      , piImage             :: [(PageInfoImageSource, T.Text)]
      , piIcon              :: [(PageInfoIconSource, ([PageIconSize], Maybe T.Text, T.Text))]
      , piRedirectUrl       :: Maybe T.Text
      , piReserved11        :: !Bool
      , piReserved12        :: !Bool
      , piReserved13        :: !Bool
      , piReserved14        :: !Bool
      , piReserved15        :: !Bool
      , piReserved16        :: !Bool
      , piReserved17        :: !Bool
      , piErrorsCount       :: {-# UNPACK #-} !Int
      , piReserved3         :: {-# UNPACK #-} !Int
      , piReserved4         :: {-# UNPACK #-} !Int
      }
    deriving (Show, Eq, Ord{-, Generic-})

data Favicon
    = Favicon
      { faviconSourceUrl         :: !TURL
      , faviconFetchTime         :: {-# UNPACK #-} !UrTime
      , faviconRedownloadOptions :: [T.Text]
      , faviconRedirectUrl       :: Maybe T.Text
      , faviconFile              :: Either (UrTime, T.Text) (SB.ShortByteString, SB.ShortByteString)
      , faviconErrorsCount       :: {-# UNPACK #-} !Int
      , faviconReserved2         :: {-# UNPACK #-} !Int
      , faviconReserved3         :: {-# UNPACK #-} !Int
      , faviconReserved4         :: {-# UNPACK #-} !Int
      }
    deriving (Show, Eq, Ord{-, Generic-})

data LinkInfo
    = LinkInfo
      { liUrl         :: !TURL
      , liTitle       :: !T.Text
      , liDescription :: !T.Text
      , liImage       :: Maybe TURL
      , liAvatar      :: Maybe TURL
      }
    deriving (Show, Eq, Ord{-, Generic-})

data HotLink
    = HotLink
      { hlChecksum :: {-# UNPACK #-} !Int
      , hlLinkInfo :: LinkInfo
      , hlUniqMsgs :: [LongMsgId]
      , hlMoreMsgs :: [LongMsgId]
      , hlDupMsgs  :: [LongMsgId]
      , hlTime     :: {-# UNPACK #-} !UrTime
      }
    deriving (Show, Eq, Ord{-, Generic-})

data HotLinkState
    = HotLinkState
      { hlsRead            :: !Bool
      , hlsFirstAppearedAt :: {-# UNPACK #-} !UrTime
      , hlsReserved_1      :: {-# UNPACK #-} !Int
      , hlsReserved_2      :: {-# UNPACK #-} !Int
      }
    deriving (Show, Eq, Ord{-, Generic-})

data HotLinks
    = HotLinks
      { hlsUser          :: !T.Text
      , hlsHotLinks      :: [HotLink]
      , hlsVersion       :: {-# UNPACK #-} !UrTime
      , hlsLastVisit     :: {-# UNPACK #-} !UrTime
      , hlsHiddenLinks   :: IntSet 
      , hlsHotLinksState :: IntMap HotLinkState
      , hlsExcludedFeeds :: HS.HashSet TURL
      , hlsBlacklist     :: [T.Text]
      , hlsTimeRange     :: {-# UNPACK #-} !Int
      , hlsTimeOffset    :: {-# UNPACK #-} !Int
      , hlsMaxHotLinks   :: {-# UNPACK #-} !Int
      , hlsMinLinks      :: {-# UNPACK #-} !Int
      , hlsUnreadOnly    :: !Bool
      , hlsSortByTime    :: !Bool
      , hlsNewLinksFirst :: !Bool
      , hlsReserved1     :: {-# UNPACK #-} !Int
      , hlsReserved2     :: {-# UNPACK #-} !Int
      , hlsReserved3     :: {-# UNPACK #-} !Int
      , hlsReserved4     :: {-# UNPACK #-} !Int
      }
    deriving (Show, Eq{-, Generic-})

data FeedbackEmail
    = FeedbackEmail
      { feedbackEmailAddress   :: EmailAddress
      , feedbackEmailTime      :: {-# UNPACK #-} !UrTime
      , feedbackEmailSubject   :: !T.Text
      , feedbackEmailText      :: !T.Text
      , feedbackEmailReserved1 :: {-# UNPACK #-} !Int
      , feedbackEmailReserved2 :: {-# UNPACK #-} !Int
      }
    deriving (Show, Eq, Ord{-, Generic-})

data FeedbackUserInfo
    = FeedbackUserInfo
      { fuiId              :: !T.Text
      , fuiWho             :: Maybe T.Text
      , fuiPaidTill        :: PaidTill
      , fuiCountry         :: !T.Text
      , fuiUsageFlags      :: [UsageFlag]
      , fuiLastUsedTime    :: {-# UNPACK #-} !UrTime
      , fuiDeleted         :: !Bool
      , fuiPayments        :: [(UrTime, T.Text, T.Text, EmailAddress)]
      , fuiFeedsCount      :: {-# UNPACK #-} !Int
      , fuiErrorFeedsCount :: {-# UNPACK #-} !Int
      , fuiProcessedAt     :: Maybe UrTime
      , fuiMailSent        :: Maybe FeedbackEmail
      , fuiRepliedAt       :: Maybe UrTime
      , fuiTags            :: [T.Text]
      , fuiNotes           :: !T.Text
      , fuiReserved1       :: {-# UNPACK #-} !Int
      , fuiReserved2       :: {-# UNPACK #-} !Int
      , fuiReserved3       :: {-# UNPACK #-} !Int
      , fuiReserved4       :: {-# UNPACK #-} !Int
      }
    deriving (Show, Eq, Ord{-, Generic-})

data FeedbackUserInfosList
    = FeedbackUserInfoList
      { fuilId               :: !T.Text
      , fuilProcessed        :: [FeedbackUserInfo]
      , fuilOrderEmailsCache :: HM.HashMap T.Text EmailAddress
      , fuilReserved2        :: {-# UNPACK #-} !Int
      , fuilReserved3        :: {-# UNPACK #-} !Int
      , fuilReserved4        :: {-# UNPACK #-} !Int
      }
    deriving (Show, Eq{-, Generic-})

data FtsReceiptTaxSystem
    = FRTSObschaya
    | FRTSUsnDohod
    | FRTSUsnDohodMinusRashod
    | FRTSEnvd
    | FRTSEshn
    | FRTSPatent
    deriving (Show, Eq, Ord{-, Generic-})

data FtsReceiptOperationType
    = FROTPrihod
    | FROTVozvratPrihoda
    | FROTRashod
    | FROTVozvratRashoda
    deriving (Show, Eq, Ord{-, Generic-})

data FtsReceiptVatType
    = FRVT18
    | FRVT10
    | FRVT118
    | FRVT110
    | FRVT0
    | FRVTNone
    deriving (Show, Eq, Ord{-, Generic-})

data FtsReceiptItem
    = FtsReceiptItem
      { friTitle     :: !T.Text
      , friCount     :: {-# UNPACK #-} !Int
      , friPrice     :: !Scientific
      , friTotal     :: !Scientific
      , friVat       :: FtsReceiptVatType
      , friReserved1 :: {-# UNPACK #-} !Int
      , friReserved2 :: {-# UNPACK #-} !Int
      , friReserved3 :: {-# UNPACK #-} !Int
      , friReserved4 :: {-# UNPACK #-} !Int
      }
    deriving (Show, Eq, Ord{-, Generic-})

data FtsReceipt
    = FtsReceipt
      { frDocumentName     :: !T.Text
      , frNomerZaSmenu     :: {-# UNPACK #-} !Int
      , frTime             :: {-# UNPACK #-} !UrTime
      , frAddress          :: !T.Text
      , frOrganizationName :: !T.Text
      , frINN              :: !T.Text
      , frTaxSystem        :: FtsReceiptTaxSystem
      , frOperationType    :: FtsReceiptOperationType
      , frItems            :: [FtsReceiptItem]
      , frTotal            :: !Scientific
      , frTotalVats        :: [(FtsReceiptVatType, Scientific)]
      , frKKTNumber        :: !T.Text
      , frKKTRegNumber     :: !T.Text
      , frFNNumber         :: !T.Text
      , frFPD              :: !T.Text
      , frReceiptSite      :: !T.Text
      , frBuyerEmail       :: !T.Text
      , frSenderEmail      :: !T.Text
      , frFDNumber         :: {-# UNPACK #-} !Int
      , frShiftNumber      :: {-# UNPACK #-} !Int
      , frExchangeRate     :: (UrTime, Scientific)
      , frRetailAddress    :: !T.Text
      , frReserved_2       :: {-# UNPACK #-} !Int
      , frReserved_3       :: {-# UNPACK #-} !Int
      , frReserved_4       :: {-# UNPACK #-} !Int
      , frReserved_5       :: {-# UNPACK #-} !Int
      , frReserved_6       :: {-# UNPACK #-} !Int
      , frReserved_7       :: {-# UNPACK #-} !Int
      , frReserved_8       :: {-# UNPACK #-} !Int
      , frReserved_9       :: {-# UNPACK #-} !Int
      , frReserved_10      :: {-# UNPACK #-} !Int
      }
    deriving (Show, Eq, Ord{-, Generic-})

data PrintableFtsReceipt
    = PrintableFtsReceipt
      { pfrPdfRus     :: !SB.ShortByteString
      , pfrHtmlRus    :: !T.Text
      , pfrHtmlEng    :: !T.Text
      , pfrReserved_1 :: {-# UNPACK #-} !Int
      , pfrReserved_2 :: {-# UNPACK #-} !Int
      , pfrReserved_3 :: {-# UNPACK #-} !Int
      , pfrReserved_4 :: {-# UNPACK #-} !Int
      }
    deriving (Show, Eq, Ord{-, Generic-})

data OfdReceipt
    = OfdReceipt
      { orOrderIdRefund       :: (T.Text, Bool)
      , orTransactionID       :: !T.Text
      , orFtsReceipt          :: FtsReceipt
      , orPrintableFtsReceipt :: Maybe PrintableFtsReceipt
      , orReserved_1          :: {-# UNPACK #-} !Int
      , orReserved_2          :: {-# UNPACK #-} !Int
      , orReserved_3          :: {-# UNPACK #-} !Int
      , orReserved_4          :: {-# UNPACK #-} !Int
      }
    deriving (Show, Eq, Ord{-, Generic-})

data ParserEnvironment
    = ParserEnvironment
      { peKey   :: !T.Text
      , peValue :: Maybe T.Text
      }
    deriving (Show, Eq, Ord{-, Generic-})


{-!
deriving instance Binary Stats
deriving instance Binary SubscriptionParentUrl
deriving instance Binary SubscriptionState
deriving instance Binary Subscription
deriving instance Binary PostsViewMode
deriving instance Binary MTVMEx
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
deriving instance Binary LoginAccessToken
deriving instance Binary ApiKeys
deriving instance Binary UserExperiment
deriving instance Binary CustomShareAction
deriving instance Binary ShareAction
deriving instance Binary MsgButton
deriving instance Binary EmailContact
deriving instance Binary SharingSettings
deriving instance Binary LoginType
deriving instance Binary Login
deriving instance Binary UserSettingsEx
deriving instance Binary UserSettings
deriving instance Binary PublicFeed
deriving instance Binary UID
deriving instance Binary FeverIds
deriving instance Binary UserStats
deriving instance Binary MailQueue
deriving instance Binary Session
deriving instance Binary EmailVerificationType
deriving instance Binary EmailVerificationToken
deriving instance Binary EmailVerification
deriving instance Binary UserEmailVerificationTokens
deriving instance Binary SubscriptionUrlKind
deriving instance Binary SubscriptionUrlInfo
deriving instance Binary Attachment
deriving instance Binary MsgKey
deriving instance Binary Msg
deriving instance Binary MsgHeader
deriving instance Binary TimeId
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
deriving instance Binary ParentUrl
deriving instance Binary SubscriptionParentPath
deriving instance Binary ParentPath
deriving instance Binary UrlToScan
deriving instance Binary QueueType
deriving instance Binary ScanList
deriving instance Binary OldFeedMask
deriving instance Binary FeedMask
deriving instance Binary PostsRead
deriving instance Binary PostsTagged
deriving instance Binary PostsTaggedGuids
deriving instance Binary ItemTag
deriving instance Binary RemovedFeedInfo
deriving instance Binary GRIds
deriving instance Binary UserBackup
deriving instance Binary DeletedUser
deriving instance Binary MailType
deriving instance Binary MailsSent
deriving instance Binary FilterQuery
deriving instance Binary FilterQueryRpc
deriving instance Binary SearchError
deriving instance Binary FilterUpdateTime
deriving instance Binary FilterFeedMasks
deriving instance Binary OldSmartStream
deriving instance Binary Filter
deriving instance Binary SmartStream
deriving instance Binary Filters
deriving instance Binary ApiMode
deriving instance Binary MsgTreePoint
deriving instance Binary PostsReq
deriving instance Binary CommentsReq
deriving instance Binary MsgId
deriving instance Binary LongMsgId
deriving instance Binary TreeReq
deriving instance Binary MsgView
deriving instance Binary MsgItem
deriving instance Binary MsgForest
deriving instance Binary ExternalLoginType
deriving instance Binary ExternalLoginAction
deriving instance Binary Counters
deriving instance Binary SITFeedDetails
deriving instance Binary SubItemType
deriving instance Binary SubItemRpc
deriving instance Binary WelcomeState
deriving instance Binary UpdateFilters
deriving instance Binary BrowserType
deriving instance Binary AppType
deriving instance Binary OperatingSystem
deriving instance Binary UsageFlag
deriving instance Binary UserUsageFlags
deriving instance Binary UsageFlags
deriving instance Binary UserSessions
deriving instance Binary MarkReq
deriving instance Binary MarkReadDirection
deriving instance Binary BgAction
deriving instance Binary FeedsOrDiscovery
deriving instance Binary FilterResults
deriving instance Binary EmailAddress
deriving instance Binary FullText
deriving instance Binary FullTextCache
deriving instance Binary OkErrorRedirect
deriving instance Binary PageIconSize
deriving instance Binary PageInfoTitleSource
deriving instance Binary PageInfoDescriptionSource
deriving instance Binary PageInfoImageSource
deriving instance Binary PageInfoIconSource
deriving instance Binary PageInfo
deriving instance Binary Favicon
deriving instance Binary LinkInfo
deriving instance Binary HotLink
deriving instance Binary HotLinkState
deriving instance Binary HotLinks
deriving instance Binary FeedbackEmail
deriving instance Binary FeedbackUserInfo
deriving instance Binary FeedbackUserInfosList
deriving instance Binary FtsReceiptTaxSystem
deriving instance Binary FtsReceiptOperationType
deriving instance Binary FtsReceiptVatType
deriving instance Binary FtsReceiptItem
deriving instance Binary FtsReceipt
deriving instance Binary PrintableFtsReceipt
deriving instance Binary OfdReceipt
deriving instance Binary ParserEnvironment
!-}
#include "BinaryInstances.h"
