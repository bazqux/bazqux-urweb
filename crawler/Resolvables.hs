{-# LANGUAGE RecordWildCards, OverloadedStrings, BangPatterns #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
-- | default-значения и Resolvable instance-ы для значений, хранящихся в Riak.
module Resolvables where

import Generated.DataTypes
import Riak
import Data.List
import Data.Maybe
import Control.Monad
import Control.Arrow
import Lib.ReadSet (ReadSet)
import qualified Lib.ReadSet as ReadSet
import qualified Data.IntSet as IntSet
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Array
import Lib.UrTime
import qualified Lib.BArray as BA
import Lib.Merge
import Lib.StringConversion

instance Resolvable User where
    resolve a b
        | uId a /= uId b = error "Resolvable User: inequal ids"
        | otherwise =
            a { uSubscriptions = unionByWith sUrl merge
                                 (uSubscriptions a) (uSubscriptions b)
              , uViewMode = resolve (uViewMode a) (uViewMode b)
              , uPayments = union (uPayments a) (uPayments b)
              }
        where merge a b
                  | sState a > sState b = a
                  | sState a < sState b = b
                  | sEditsCount a < sEditsCount b = b
                  | otherwise = a

instance Resolvable PaidTill where
    resolve = maxC pte
        where pte PTUnknown = (0, UrTime 0 0)
              pte (PTFreeTrial t) = (1,t)
              pte (PTFreeTrialFinished t) = (2,t)
              pte (PTPaidFinished t) = (3,t)
              pte (PTPaid t) = (4,t)
              maxC c a b
                  | c a > c b = a
                  | otherwise = b

instance Resolvable UserViewMode where
    resolve a b =
        UserViewMode
        { uvmPaidTill                 = resolve (uvmPaidTill a) (uvmPaidTill b)
        , uvmOnlyUpdatedSubscriptions = max (uvmOnlyUpdatedSubscriptions a)
                                        (uvmOnlyUpdatedSubscriptions b)
        , uvmSubViewModes             = mergeVM (uvmSubViewModes a)
                                        (uvmSubViewModes b)
        , uvmFolderViewModes          = mergeVM (uvmFolderViewModes a)
                                        (uvmFolderViewModes b)
        , uvmSubUrlRenames            = reverse $ sort $
                                        union (uvmSubUrlRenames a)
                                        (uvmSubUrlRenames b)
        }
        where mergeVM = HM.unionWith max

-- instance Resolvable MsgTreeViewMode where
--     resolve a b =
--         MsgTreeViewMode
--         { mtvmAscending     = mtvmAscending a && mtvmAscending b
--         , mtvmUnreadOnly    = mtvmUnreadOnly a || mtvmUnreadOnly b
--         , mtvmShowComments  = mtvmShowComments a || mtvmShowComments b
--         , mtvmFetchComments = mtvmFetchComments a || mtvmFetchComments b
--         , mtvmFullPosts     = mtvmFullPosts a || mtvmFullPosts b
--         , mtvmFullComments  = mtvmFullComments a || mtvmFullComments b
--         }


defaultUserViewMode =
    UserViewMode
    { uvmPaidTill                 = PTUnknown
    , uvmOnlyUpdatedSubscriptions = (0,False)
    , uvmSubViewModes             = HM.empty
    , uvmFolderViewModes          = HM.empty
    , uvmSubUrlRenames            = []
    }
defaultUser id = User id [] defaultUserViewMode []
defaultUrTime = UrTime 0 0

defaultMsgTreeViewMode =
    MsgTreeViewMode
    { mtvmAscending = False
    , mtvmUnreadOnly = True
    , mtvmExpandedComments = False
    , mtvmPosts = PVMFull
    , mtvmFolderExpanded = True
    , mtvmNoOverride = True
    }

maxBy f a b
    | f a >= f b = a
    | otherwise  = b
selMax f a b
    | f a >= f b = f a
    | otherwise  = f b

defaultUserStats i =
    UserStats
    { usId = i
    , usUID = Url i
    , usFirstSignInTime = UrTime 0 0
    , usFirstSignInDetails = []
    , usCounters  = Map.empty
    , usSubscriptionCounters = Map.empty
    , usReserved1 = Map.empty
    , usReserved2 = Map.empty
    , usReserved3 = Map.empty
    , usReserved4 = Map.empty
    }

instance Resolvable UserStats where
    resolve a b =
        UserStats
        { usId = usId a
        , usUID = selMax usUID a b
        , usFirstSignInTime = selMax usFirstSignInTime a b
        , usFirstSignInDetails = selMax usFirstSignInDetails a b
        , usCounters  = um usCounters
        , usSubscriptionCounters = um2 usSubscriptionCounters
        , usReserved1 = um usReserved1
        , usReserved2 = um usReserved2
        , usReserved3 = um usReserved3
        , usReserved4 = um usReserved4
        }
        where um f = Map.unionWith max (f a) (f b)
              um2 f = Map.unionWith (Map.unionWith max) (f a) (f b)

instance Resolvable SubscriptionUrlInfo where
    resolve = maxBy suiTime
defaultSubscriptionUrlInfo url =
    SubscriptionUrlInfo
    { suiUrl = url
    , suiTime = UrTime 0 0
    , suiKind = SUKError "Uninitialized SubscriptionUrlInfo???"
    }

instance Resolvable Msg where
    resolve = maxBy $ \ Msg {..} -> (msgTime, T.length msgText)

defaultMsg key =
    Msg
    { msgKey = key
    , msgAttachments = []
    , msgAuthor      = ""
    , msgAuthorUri   = Nothing
    , msgAuthorEmail = ""
    , msgAuthorPic   = Nothing
    , msgLink        = Nothing
    , msgSubject     = ""
    , msgTags        = []
    , msgTime        = Nothing
    , msgDlTime      = UrTime 0 0
    , msgText        = ""
    , msgShortText   = ""
    , msgDebug       = "defaultMsg"
    }

instance Resolvable Posts where
    resolve = maxBy $ \ Posts {..} -> (snd $ BA.bounds $ mtHeaders pMsgTree,
                                       pTotalComments)
--         trace ("RESOLVING Posts " ++ show (s a) ++ " and " ++ show (s b)
--               ++ " -> " ++ show (mtSize mt')
--               ++ "\nA " ++ show (pUpdatedComments a)
--               ++ "\nB " ++ show (pUpdatedComments b)
--               ++ "\nR " ++ show uc'
-- --                ++ "\nppppppppppppp AAAAAA ppppppppppppp" ++ ppShow (pMsgTree a)
-- --                ++ "\nppppppppppppp BBBBBB ppppppppppppp" ++ ppShow (pMsgTree b)
-- --                ++ "\nppppppppppppp RESULT ppppppppppppp" ++ ppShow mt'
--               ) $

--         a { pRootMessage = resolve (pRootMessage a) (pRootMessage b)
--           , pUpdatedComments = uc'
--           , pHeaders = h'
--           , pMsgTree = mt'
--           }
--         where s = mtSize . pMsgTree
--               (h', mt', uc') =
--                   resolveMsgTree (pHeaders a) (pMsgTree a) (pUpdatedComments a)
--                                  (pHeaders b) (pMsgTree b) (pUpdatedComments b)

-- TODO: по хорошему, тут надо сливать деревья, т.к. для агрегаторов
-- возможно одновременное скачивание и обработка нескольких постов.
-- С другой стороны они перевыкачиваются, а кол-во детей не плюсуется,
-- а устанавливается напрямую, так что все может оказаться eventually consistent

-- resolveMsgTree h1 mt1 uc1 h2 mt2 uc2
--     | hSize h1 < hSize h2 = resolveMsgTree h2 mt2 uc2 h1 mt1 uc1
--     | otherwise = -- предпочитаем работать с большим деревом,
--                   -- чтобы уменьшить вероятность сдвигания id-шек
--         ( listArray (0, IntMap.size hm - 1) (IntMap.elems hm)
--         , fromIMsgsMap $ unionIMsgMaps
--             (toIMsgsMap id mt1) (toIMsgsMap i2to1 mt2)
--         , IntSet.union uc1 (IntSet.map i2to1 uc2) )
--     where hSize h = snd (bounds h) + 1
--           hm1 = IntMap.fromDistinctAscList $ assocs h1
--           guids1 = Map.fromList [(mhGuid h,i) | (i,h) <- assocs h1]
--           (hm, i2to1Map) = foldl mergeMH (hm1, IntMap.empty) (assocs h2)
--           i2to1 i = fromJust $ IntMap.lookup i i2to1Map
--           mergeMH (hm, i2to1Map) (i2, mh2)
--               | Just i1 <- Map.lookup (mhGuid mh2) guids1
--                   = (hm, IntMap.insert i2 i1 i2to1Map)
--               | otherwise
--                   = (IntMap.insert (IntMap.size hm) mh2 hm,
--                      IntMap.insert i2 (IntMap.size hm) i2to1Map)
--           toIMsgsMap i2i mt =
--               IntMap.fromList $ map toIMsg $ Map.toList (mtChildren mt)
--               where toIMsg ((time, i), subTree) =
--                         (i2i i,
--                          IMsg
--                          { imsgTime = time
--                          , imsgCommentsCount =
--                              if Map.null (mtChildren subTree) then
--                                  mtSize subTree
--                              else
--                                  0
--                          , imsgChildren = toIMsgsMap i2i subTree
--                          })
--           fromIMsgsMap m =
--               MsgTree
--               { mtSize = sum $ map (succ . mtSize . snd) children
--               , mtChildren = Map.fromList children
--               }
--               where children = map toSubTree $ IntMap.toList m
--                     toSubTree (i, IMsg {..})
--                         | IntMap.null imsgChildren =
--                             ((imsgTime, i), MsgTree imsgCommentsCount Map.empty)
--                         | otherwise =
--                             ((imsgTime, i), fromIMsgsMap imsgChildren)
--           unionIMsgMaps = IntMap.unionWith unionIMsgs
--           unionIMsgs m1 m2 =
--               m1 { imsgCommentsCount = max (imsgCommentsCount m1)
--                                            (imsgCommentsCount m2)
--                  , imsgChildren = unionIMsgMaps (imsgChildren m1)
--                                                 (imsgChildren m2)
--                  }

-- data IMsg
--     = IMsg
--       { imsgTime :: UrTime
--       , imsgCommentsCount :: Int  -- mtSize для mtChildren = Map.empty
--       , imsgChildren :: IntMap IMsg
--       }

emptyHeaders = BA.listArray (0,-1) []
emptyMsgTree = MsgTree emptyHeaders IntMap.empty

defaultPosts key =
    Posts
    { pBlogFeedUrl = key
    , pUpdatedComments = IntSet.empty
    , pRootMessage = defaultMsg $ MsgKey key Nothing Nothing
    , pMsgTree     = emptyMsgTree
    , pTotalComments = 0
    , pDeletedComments = 0
    , pCommentCounts = IntMap.empty
    , pCCVersions    = Map.empty
    }

mtSize = rangeSize . BA.bounds . mtHeaders

instance Resolvable Comments where
    resolve = maxBy (mtSize . cMsgTree)
--         trace ("RESOLVING Comments " ++ show (s a) ++ " and " ++ show (s b)
--               ++ " -> " ++ show (mtSize mt')
-- --                ++ "\nccccccccccccc AAAAAA ccccccccccccc" ++ ppShow (cMsgTree a)
-- --                ++ "\nccccccccccccc BBBBBB ccccccccccccc" ++ ppShow (cMsgTree b)
-- --                ++ "\nccccccccccccc RESULT ccccccccccccc" ++ ppShow mt'
--               ) $

--         a { cHeaders = h'
--           , cMsgTree = mt'
--           }
--         where s = mtSize . cMsgTree
--               (h', mt', _) =
--                   resolveMsgTree (cHeaders a) (cMsgTree a) IntSet.empty
--                                  (cHeaders b) (cMsgTree b) IntSet.empty

defaultComments key =
    Comments
    { cKey     = key
    , cMsgTree = emptyMsgTree
    }

instance Resolvable ReadSet where
    resolve = maxBy ReadSet.size

instance Resolvable PostsRead where
    resolve = maxBy (\ p -> (-- ReadSet.size
                             (prSet p), prTotalCommentsRead p))

defaultPostsRead key =
    PostsRead
    { prKey               = key
    , prSet               = ReadSet.empty
    , prTotalCommentsRead = 0
    , prCommentsRead      = IntMap.empty
    , prIgnoredPosts      = ReadSet.empty
    , prIgnoredComments   = IntMap.empty
    }


instance Resolvable UrlToScan where
    resolve a b =
        UrlToScan
        { utsUrl                = utsUrl a
        , utsRedownloadOptions  = []
        , utsDataHash           = ""
        , utsModifyTime         = max (utsModifyTime a) (utsModifyTime b)
        , utsNextScanTime       = max (utsNextScanTime a) (utsNextScanTime b)
        , utsErrorStartTime     = min (utsErrorStartTime a) (utsErrorStartTime b)
        , utsParentPaths        =
            unionByWith ppBlogFeedUrl max (utsParentPaths a) (utsParentPaths b)
        , utsSubscriptionParentPaths =
            unionByWith sppSubscriptionUrl max (utsSubscriptionParentPaths a)
                        (utsSubscriptionParentPaths b)
        }

defaultUrlToScan url =
    UrlToScan
    { utsUrl                     = url
    , utsRedownloadOptions       = []
    , utsDataHash                = ""
    , utsModifyTime              = UrTime 0 0
    , utsNextScanTime            = UrTime 0 0
    , utsErrorStartTime          = Nothing
    , utsParentPaths             = []
    , utsSubscriptionParentPaths = []
    }

instance Resolvable ScanList where
    resolve a b = a { slUrls = unionByWith id min (slUrls a) (slUrls b) }

defaultScanList time =
    ScanList
    { slTime = time
    , slUrls = []
    }

instance Resolvable Stats where
    resolve = maxBy statsMap

defaultStats k =
    Stats
    { statsKey = k
    , statsMap = Map.empty
    }

instance Resolvable Session where
    resolve = max

defaultSession k = error "defaultSession"

defaultBlogPostsScanned k =
    BlogPostsScanned
    { bpsBlogFeedUrl   = k
    , bpsSubscribeTime = UrTime 0 0
    , bpsUrls = Map.empty
    }


instance Resolvable BlogPostsScanned where
    resolve a b =
        BlogPostsScanned
        { bpsBlogFeedUrl   = bpsBlogFeedUrl a
        , bpsSubscribeTime = min (bpsSubscribeTime a) (bpsSubscribeTime b)
        , bpsUrls = Map.unionWith (Map.unionWith unionState)
                    (bpsUrls a) (bpsUrls b)
        }
        where unionState (st1, sct1, cus1) (st2, sct2, cus2) =
                  ( min st1 st2
                  , min sct1 sct2 `mplus` sct1 `mplus` sct2
                  , max cus1 cus2 )

instance TextKey MsgKey where
    textKey (MsgKey a b c) = textKeyList [Just a, fmap sbt b, fmap sbt c]
instance TextKey CommentsKey where
    textKey (CommentsKey bf g) = textKeyList [bf, sbt g]
instance TextKey UrTime where
    textKey (UrTime a 0) = T.pack $ show a
    textKey (UrTime a b) = T.pack $ show a ++ "." ++ show b
instance TextKey () where
    textKey () = "()"

defaultMailQueue k =
    MailQueue
    { mqId       = k
    , mqActive   = Set.empty
    , mqInactive = Set.empty
    }

instance Resolvable MailQueue where
    resolve a b =
        MailQueue
        { mqId = mqId a
        , mqActive = Set.difference active inactive
        , mqInactive = inactive
        }
        where active = Set.union (mqActive a) (mqActive b)
              inactive = Set.union (mqInactive a) (mqInactive b)

defaultFullTextCache k = error "defaultFullTextCache"

instance Resolvable FullTextCache where
    resolve = maxBy (ftcTime &&& ftcText)

defaultUserFilters k =
    UserFilters
    { ufUser      = k
    , ufFilters   = []
    , ufReserved1 = []
    , ufReserved2 = []
    , ufReserved3 = []
    }

instance Resolvable UserFilters where
    resolve = maxBy ufFilters

defaultUserSettings u =
    UserSettings
    { ustUser         = u
    , ustEditsCount   = 0
    , ustScrollMode   = SMNormal
    , ustListViewMode = LVMCompact
    , ustShowFavicons = True
    , ustMarkReadMode = MRMOnScroll
    , ustUltraCompact = False
    , ustMobileLogin  = Nothing
    , ustExactUnreadCounts    = False
    , ustPublicFeeds  = Nothing
    , ustCountry      = Nothing
    , ustApiKeys      = Nothing
    , ustReserved7    = Nothing
    , ustReserved8    = Nothing
    , ustReserved9    = Nothing
    }

instance Resolvable UserSettings where
    resolve = maxBy ustEditsCount

defaultPublicFeed i =
    PublicFeed
    { pfId        = i
    , pfUser      = Nothing
    , pfReserved1 = Nothing
    , pfReserved2 = Nothing
    , pfReserved3 = Nothing
    , pfReserved4 = Nothing
    }

instance Resolvable PublicFeed where
    resolve = maxBy pfUser

defaultGRIds user =
    GRIds
    { griUser      = user
    , griLastId    = 1
    , griFeedIds   = HM.empty
    , griFolderIds = HM.empty
    , griFeedUrls  = IntMap.empty
    , griFolderNames = IntMap.empty
    , griTaggedItems = Map.empty
    , griItemTags    = IntMap.empty
    , griOrdering    = Map.empty
    , griActiveFeeds  = Nothing
    , griRemovedFeeds = Nothing
    , griGRTagsImported = False
    , griReserved4    = False
    , griReserved5    = False
    , griReserved6    = False
    , griReserved7    = False
    , griReserved8    = False
    }

instance Resolvable GRIds where
    resolve = maxBy griLastId

defaultMobileLogin login =
    MobileLogin
    { mlLogin        = login
    , mlEditsCount   = 0
    , mlUID          = Nothing
    , mlPasswordHash = ""
    , mlFeverApiKey  = Nothing
    , mlReserved2    = Nothing
    , mlReserved3    = Nothing
    , mlReserved4    = Nothing
    }

instance Resolvable MobileLogin where
    resolve = maxBy mlEditsCount

defaultFeverApiKey key =
    FeverApiKey
    { fakKey          = key
    , fakEditsCount   = 0
    , fakUID          = Nothing
    , fakReserved1    = Nothing
    , fakReserved2    = Nothing
    , fakReserved3    = Nothing
    , fakReserved4    = Nothing
    }

instance Resolvable FeverApiKey where
    resolve = maxBy fakEditsCount

defaultFeverIds user =
    FeverIds
    { fiUser      = user
    , fiMaxId     = 0
    , fiGRIds     = IntMap.empty
    , fiFeverIds  = IntMap.empty
    , fiLastRefresh = Nothing
    , fiReserved2 = Nothing
    , fiReserved3 = Nothing
    , fiReserved4 = Nothing
    }

instance Resolvable FeverIds where
    resolve = maxBy fiMaxId

instance Resolvable PostsTagged where
    resolve a b = a { ptSet = ReadSet.union (ptSet a) (ptSet b) }

defaultPostsTagged k =
    PostsTagged
    { ptBlogFeedUrl = k
    , ptSet         = ReadSet.empty
    , ptReserved1   = 0
    , ptReserved2   = 0
    , ptReserved3   = 0
    , ptReserved4   = 0
    }

instance Resolvable PostsTaggedGuids where
    resolve a b = a { ptgGuids = IntMap.unionWith max (ptgGuids a) (ptgGuids b) }

defaultPostsTaggedGuids k =
    PostsTaggedGuids
    { ptgBlogFeedUrl = k
    , ptgGuids       = IntMap.empty
    , ptgReserved1   = 0
    , ptgReserved2   = 0
    , ptgReserved3   = 0
    , ptgReserved4   = 0
    }

instance Resolvable PostsClearTime where
    resolve = max

defaultPostsClearTime k =
    PostsClearTime
    { pctBlogFeedUrl = k
    , pctTime        = UrTime 0 0
    , pctReserved1   = 0
    , pctReserved2   = 0
    , pctReserved3   = 0
    , pctReserved4   = 0
    }

instance Resolvable PostsSubscribers where
    resolve a b =
        a { psActions = filterAs
          , psSubscribers = ss
          }
        where as = Set.union (psActions a) (psActions b)
              ss = go HS.empty (Set.toAscList as)
              filterAs
--                   | Just ((maxT,maxU,_),x) <- Set.maxView as =
--                       Set.filter (\ (t,u,_) -> not $
--                                       diffUrTime maxT t > 86400 &&
--                                       -- ^ неправильно, надо именно пары
--                                       -- чистить
--                                       not (HS.member u ss) && u /= maxU) as
                  | otherwise = as
              go s [] = s
              go !s ((_,u,True):ss) = go (HS.insert u s) ss
              go !s ((_,u,False):ss) = go (HS.delete u s) ss

defaultPostsSubscribers k =
    PostsSubscribers
    { psBlogFeedUrl = k
    , psActions     = Set.empty
    , psSubscribers = HS.empty
    , psReserved1   = 0
    , psReserved2   = 0
    , psReserved3   = 0
    , psReserved4   = 0
    }

defaultDiscoveryFeed f =
    DiscoveryFeed
    { dfUrl                   = f
    , dfTitle                 = ""
    , dfWebsite               = Nothing
    , dfImage                 = Nothing
    , dfCategory              = ""
    , dfTranslatedCategory    = HM.empty
    , dfTags                  = []
    , dfTranslatedTags        = HM.empty
    , dfSubscribers           = 0
    , dfNormalizedSubscribers = 0.0
    , dfPostsPerDay           = 0.0
    , dfLastRefreshTime       = UrTime 0 0
    , dfPaidCountries         = HM.empty
    , dfCountries             = HM.empty
    , dfReserved1             = 0
    , dfReserved2             = 0
    , dfReserved3             = 0
    , dfReserved4             = 0
    }

instance Resolvable DiscoveryFeed where
    resolve = maxBy dfLastRefreshTime

defaultUserBackup key@(u,_) =
    UserBackup
    { ubKey          = key
    , ubUser         = defaultUser u
    , ubUserStats    = defaultUserStats u
    , ubUserFilters  = defaultUserFilters u
    , ubUserSettings = defaultUserSettings u
    , ubGRIds        = defaultGRIds u
    , ubFeverIds     = defaultFeverIds u
    , ubReserved1    = False
    , ubReserved2    = False
    , ubReserved3    = False
    , ubReserved4    = False
    }

instance Resolvable UserBackup where
    resolve a b =
        a
        { ubUser         = resolve (ubUser a) (ubUser b)
        , ubUserStats    = resolve (ubUserStats a) (ubUserStats b)
        , ubUserFilters  = resolve (ubUserFilters a) (ubUserFilters b)
        , ubUserSettings = resolve (ubUserSettings a) (ubUserSettings b)
        , ubGRIds        = resolve (ubGRIds a) (ubGRIds b)
        , ubFeverIds     = resolve (ubFeverIds a) (ubFeverIds b)
        }

defaultDeletedUser key =
    DeletedUser
    { duUser      = key
    , duBackups   = []
    , duMailsSent = Nothing
    , duReserved2 = False
    , duReserved3 = False
    , duReserved4 = False
    }

instance Resolvable DeletedUser where
    resolve a b = a { duBackups = ut duBackups
                    , duMailsSent = if null ms then Nothing else Just ms
                    }
        where ut f = reverse $ sort $ union (f a) (f b)
              ms = ut (fromMaybe [] . duMailsSent)

defaultActiveCheckSubscriptions k =
    ActiveCheckSubscriptions
    { acsKey   = k
    , acsUsers = HM.empty
    }

instance Resolvable ActiveCheckSubscriptions where
    resolve a b = a { acsUsers = HM.unionWith max (acsUsers a) (acsUsers b) }

defaultUserUsageFlags =
    UserUsageFlags
    { uufPaidTill   = PTUnknown
    , uufCountry    = "-"
    , uufUsageFlags = Set.empty
    , uufReserved1  = Set.empty
    , uufReserved2  = Set.empty
    , uufReserved3  = Set.empty
    , uufReserved4  = Set.empty
    }

instance Resolvable UserUsageFlags where
    resolve a b =
        a
        { uufPaidTill = resolve (uufPaidTill a) (uufPaidTill b)
        , uufCountry = max (uufCountry a) (uufCountry b)
        , uufUsageFlags = Set.union (uufUsageFlags a) (uufUsageFlags b)
        }

defaultUsageFlags t =
    UsageFlags
    { uflTime      = t
    , uflFlags     = HM.empty
    , uflReserved1 = Set.empty
    , uflReserved2 = Set.empty
    , uflReserved3 = Set.empty
    , uflReserved4 = Set.empty
    }

instance Resolvable UsageFlags where
    resolve a b =
        a { uflFlags = HM.unionWith resolve (uflFlags a) (uflFlags b) }

defaultApiKeys =
    ApiKeys
    { akPocket    = Nothing
    , akPocketRequest = Nothing
    , akReserved1 = 0
    , akReserved2 = 0
    , akReserved3 = 0
    , akReserved4 = 0
    }

emptyFilterFeedMasks =
    FilterFeedMasks
    { ffmLastUpdated = Nothing
    , ffmFeedMasks   = HM.empty
    , ffmReserved1   = 0
    , ffmReserved2   = 0
    }

instance Resolvable Filters where
    resolve = maxBy vt
        where vt f = (fVersion f, ffmLastUpdated $ fFeedMasks f)

defaultFilters u =
    Filters
    { fUser         = u
    , fVersion      = 0
    , fFilters      = []
    , fFeedMasks    = emptyFilterFeedMasks
    , fSmartStreams = []
    , fReserved1    = 0
    , fReserved2    = 0
    , fReserved3    = 0
    , fReserved4    = 0
    }
