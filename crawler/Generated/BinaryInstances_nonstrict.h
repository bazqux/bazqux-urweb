
 
instance Binary Stats where
        put (Stats x1 x2)
          = do put x1
               put x2
        get
          = do x1 <- get
               x2 <- get
               return (Stats x1 x2)

 
instance Binary SubscriptionState where
        put x
          = case x of
                SSAdded -> putWord8 0
                SSScanning x1 -> do putWord8 1
                                    put x1
                SSError x1 -> do putWord8 2
                                 put x1
                SSFeed x1 -> do putWord8 3
                                put x1
        get
          = do i <- getWord8
               case i of
                   0 -> return SSAdded
                   1 -> do x1 <- get
                           return (SSScanning x1)
                   2 -> do x1 <- get
                           return (SSError x1)
                   3 -> do x1 <- get
                           return (SSFeed x1)
                   _ -> error "Corrupted binary data for SubscriptionState"

 
instance Binary Subscription where
        put (Subscription x1 x2 x3 x4 x5)
          = do put x1
               put x2
               put x3
               put x4
               put x5
        get
          = do x1 <- get
               x2 <- get
               x3 <- get
               x4 <- get
               x5 <- get
               return (Subscription x1 x2 x3 x4 x5)

 
instance Binary PostsViewMode where
        put x
          = case x of
                PVMShort -> putWord8 0
                PVMFull -> putWord8 1
                PVMMagazine -> putWord8 2
                PVMMosaic -> putWord8 3
        get
          = do i <- getWord8
               case i of
                   0 -> return PVMShort
                   1 -> return PVMFull
                   2 -> return PVMMagazine
                   3 -> return PVMMosaic
                   _ -> error "Corrupted binary data for PostsViewMode"

 
instance Binary MsgTreeViewMode where
        put (MsgTreeViewMode x1 x2 x3 x4 x5 x6)
          = do put x1
               put x2
               put x3
               put x4
               put x5
               put x6
        get
          = do x1 <- get
               x2 <- get
               x3 <- get
               x4 <- get
               x5 <- get
               x6 <- get
               return (MsgTreeViewMode x1 x2 x3 x4 x5 x6)

 
instance Binary Payment where
        put x
          = case x of
                PReserved -> putWord8 0
                PFastSpring x1 x2 x3 -> do putWord8 1
                                           put x1
                                           put x2
                                           put x3
        get
          = do i <- getWord8
               case i of
                   0 -> return PReserved
                   1 -> do x1 <- get
                           x2 <- get
                           x3 <- get
                           return (PFastSpring x1 x2 x3)
                   _ -> error "Corrupted binary data for Payment"

 
instance Binary PaidTill where
        put x
          = case x of
                PTUnknown -> putWord8 0
                PTPaid x1 -> do putWord8 1
                                put x1
                PTFreeTrial x1 -> do putWord8 2
                                     put x1
                PTFreeTrialFinished x1 -> do putWord8 3
                                             put x1
                PTPaidFinished x1 -> do putWord8 4
                                        put x1
        get
          = do i <- getWord8
               case i of
                   0 -> return PTUnknown
                   1 -> do x1 <- get
                           return (PTPaid x1)
                   2 -> do x1 <- get
                           return (PTFreeTrial x1)
                   3 -> do x1 <- get
                           return (PTFreeTrialFinished x1)
                   4 -> do x1 <- get
                           return (PTPaidFinished x1)
                   _ -> error "Corrupted binary data for PaidTill"

 
instance Binary UserViewMode where
        put (UserViewMode x1 x2 x3 x4 x5)
          = do put x1
               put x2
               put x3
               put x4
               put x5
        get
          = do x1 <- get
               x2 <- get
               x3 <- get
               x4 <- get
               x5 <- get
               return (UserViewMode x1 x2 x3 x4 x5)

 
instance Binary User where
        put (User x1 x2 x3 x4)
          = do put x1
               put x2
               put x3
               put x4
        get
          = do x1 <- get
               x2 <- get
               x3 <- get
               x4 <- get
               return (User x1 x2 x3 x4)

 
instance Binary UserFilters where
        put (UserFilters x1 x2 x3 x4 x5)
          = do put x1
               put x2
               put x3
               put x4
               put x5
        get
          = do x1 <- get
               x2 <- get
               x3 <- get
               x4 <- get
               x5 <- get
               return (UserFilters x1 x2 x3 x4 x5)

 
instance Binary ScrollMode where
        put x
          = case x of
                SMNormal -> putWord8 0
                SMQuick -> putWord8 1
                SMImmediate -> putWord8 2
        get
          = do i <- getWord8
               case i of
                   0 -> return SMNormal
                   1 -> return SMQuick
                   2 -> return SMImmediate
                   _ -> error "Corrupted binary data for ScrollMode"

 
instance Binary ListViewMode where
        put x
          = case x of
                LVMCompact -> putWord8 0
                LVMTwoLines -> putWord8 1
        get
          = do i <- getWord8
               case i of
                   0 -> return LVMCompact
                   1 -> return LVMTwoLines
                   _ -> error "Corrupted binary data for ListViewMode"

 
instance Binary MarkReadMode where
        put x
          = case x of
                MRMOnScroll -> putWord8 0
                MRMManual -> putWord8 1
                MRMOnScrollEverywhere -> putWord8 2
        get
          = do i <- getWord8
               case i of
                   0 -> return MRMOnScroll
                   1 -> return MRMManual
                   2 -> return MRMOnScrollEverywhere
                   _ -> error "Corrupted binary data for MarkReadMode"

 
instance Binary PublicFeedType where
        put x
          = case x of
                PFTAll -> putWord8 0
                PFTFolder x1 -> do putWord8 1
                                   put x1
                PFTTag x1 -> do putWord8 2
                                put x1
                PFTStarred -> putWord8 3
                PFTAllTags -> putWord8 4
        get
          = do i <- getWord8
               case i of
                   0 -> return PFTAll
                   1 -> do x1 <- get
                           return (PFTFolder x1)
                   2 -> do x1 <- get
                           return (PFTTag x1)
                   3 -> return PFTStarred
                   4 -> return PFTAllTags
                   _ -> error "Corrupted binary data for PublicFeedType"

 
instance Binary ApiKeys where
        put (ApiKeys x1 x2 x3 x4 x5 x6)
          = do put x1
               put x2
               put x3
               put x4
               put x5
               put x6
        get
          = do x1 <- get
               x2 <- get
               x3 <- get
               x4 <- get
               x5 <- get
               x6 <- get
               return (ApiKeys x1 x2 x3 x4 x5 x6)

 
instance Binary UserSettings where
        put
          (UserSettings x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15)
          = do put x1
               put x2
               put x3
               put x4
               put x5
               put x6
               put x7
               put x8
               put x9
               put x10
               put x11
               put x12
               put x13
               put x14
               put x15
        get
          = do x1 <- get
               x2 <- get
               x3 <- get
               x4 <- get
               x5 <- get
               x6 <- get
               x7 <- get
               x8 <- get
               x9 <- get
               x10 <- get
               x11 <- get
               x12 <- get
               x13 <- get
               x14 <- get
               x15 <- get
               return
                 (UserSettings x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15)

 
instance Binary PublicFeed where
        put (PublicFeed x1 x2 x3 x4 x5 x6)
          = do put x1
               put x2
               put x3
               put x4
               put x5
               put x6
        get
          = do x1 <- get
               x2 <- get
               x3 <- get
               x4 <- get
               x5 <- get
               x6 <- get
               return (PublicFeed x1 x2 x3 x4 x5 x6)

 
instance Binary UID where
        put x
          = case x of
                EMail x1 -> do putWord8 0
                               put x1
                Url x1 -> do putWord8 1
                             put x1
        get
          = do i <- getWord8
               case i of
                   0 -> do x1 <- get
                           return (EMail x1)
                   1 -> do x1 <- get
                           return (Url x1)
                   _ -> error "Corrupted binary data for UID"

 
instance Binary MobileLogin where
        put (MobileLogin x1 x2 x3 x4 x5 x6 x7 x8)
          = do put x1
               put x2
               put x3
               put x4
               put x5
               put x6
               put x7
               put x8
        get
          = do x1 <- get
               x2 <- get
               x3 <- get
               x4 <- get
               x5 <- get
               x6 <- get
               x7 <- get
               x8 <- get
               return (MobileLogin x1 x2 x3 x4 x5 x6 x7 x8)

 
instance Binary FeverApiKey where
        put (FeverApiKey x1 x2 x3 x4 x5 x6 x7)
          = do put x1
               put x2
               put x3
               put x4
               put x5
               put x6
               put x7
        get
          = do x1 <- get
               x2 <- get
               x3 <- get
               x4 <- get
               x5 <- get
               x6 <- get
               x7 <- get
               return (FeverApiKey x1 x2 x3 x4 x5 x6 x7)

 
instance Binary FeverIds where
        put (FeverIds x1 x2 x3 x4 x5 x6 x7 x8)
          = do put x1
               put x2
               put x3
               put x4
               put x5
               put x6
               put x7
               put x8
        get
          = do x1 <- get
               x2 <- get
               x3 <- get
               x4 <- get
               x5 <- get
               x6 <- get
               x7 <- get
               x8 <- get
               return (FeverIds x1 x2 x3 x4 x5 x6 x7 x8)

 
instance Binary UserStats where
        put (UserStats x1 x2 x3 x4 x5 x6 x7 x8 x9 x10)
          = do put x1
               put x2
               put x3
               put x4
               put x5
               put x6
               put x7
               put x8
               put x9
               put x10
        get
          = do x1 <- get
               x2 <- get
               x3 <- get
               x4 <- get
               x5 <- get
               x6 <- get
               x7 <- get
               x8 <- get
               x9 <- get
               x10 <- get
               return (UserStats x1 x2 x3 x4 x5 x6 x7 x8 x9 x10)

 
instance Binary MailQueue where
        put (MailQueue x1 x2 x3)
          = do put x1
               put x2
               put x3
        get
          = do x1 <- get
               x2 <- get
               x3 <- get
               return (MailQueue x1 x2 x3)

 
instance Binary Session where
        put (Session x1 x2 x3 x4)
          = do put x1
               put x2
               put x3
               put x4
        get
          = do x1 <- get
               x2 <- get
               x3 <- get
               x4 <- get
               return (Session x1 x2 x3 x4)

 
instance Binary SubscriptionUrlKind where
        put x
          = case x of
                SUKError x1 -> do putWord8 0
                                  put x1
                SUKFeed x1 -> do putWord8 1
                                 put x1
        get
          = do i <- getWord8
               case i of
                   0 -> do x1 <- get
                           return (SUKError x1)
                   1 -> do x1 <- get
                           return (SUKFeed x1)
                   _ -> error "Corrupted binary data for SubscriptionUrlKind"

 
instance Binary SubscriptionUrlInfo where
        put (SubscriptionUrlInfo x1 x2 x3)
          = do put x1
               put x2
               put x3
        get
          = do x1 <- get
               x2 <- get
               x3 <- get
               return (SubscriptionUrlInfo x1 x2 x3)

 
instance Binary Attachment where
        put x
          = case x of
                AImage x1 x2 x3 x4 -> do putWord8 0
                                         put x1
                                         put x2
                                         put x3
                                         put x4
                AAudio x1 x2 x3 x4 x5 -> do putWord8 1
                                            put x1
                                            put x2
                                            put x3
                                            put x4
                                            put x5
                AVideo x1 x2 x3 x4 x5 x6 x7 x8 -> do putWord8 2
                                                     put x1
                                                     put x2
                                                     put x3
                                                     put x4
                                                     put x5
                                                     put x6
                                                     put x7
                                                     put x8
                AIframe x1 x2 x3 x4 -> do putWord8 3
                                          put x1
                                          put x2
                                          put x3
                                          put x4
                AOther x1 x2 x3 -> do putWord8 4
                                      put x1
                                      put x2
                                      put x3
                AGrOrigin x1 x2 x3 x4 -> do putWord8 5
                                            put x1
                                            put x2
                                            put x3
                                            put x4
        get
          = do i <- getWord8
               case i of
                   0 -> do x1 <- get
                           x2 <- get
                           x3 <- get
                           x4 <- get
                           return (AImage x1 x2 x3 x4)
                   1 -> do x1 <- get
                           x2 <- get
                           x3 <- get
                           x4 <- get
                           x5 <- get
                           return (AAudio x1 x2 x3 x4 x5)
                   2 -> do x1 <- get
                           x2 <- get
                           x3 <- get
                           x4 <- get
                           x5 <- get
                           x6 <- get
                           x7 <- get
                           x8 <- get
                           return (AVideo x1 x2 x3 x4 x5 x6 x7 x8)
                   3 -> do x1 <- get
                           x2 <- get
                           x3 <- get
                           x4 <- get
                           return (AIframe x1 x2 x3 x4)
                   4 -> do x1 <- get
                           x2 <- get
                           x3 <- get
                           return (AOther x1 x2 x3)
                   5 -> do x1 <- get
                           x2 <- get
                           x3 <- get
                           x4 <- get
                           return (AGrOrigin x1 x2 x3 x4)
                   _ -> error "Corrupted binary data for Attachment"

 
instance Binary MsgKey where
        put (MsgKey x1 x2 x3)
          = do put x1
               put x2
               put x3
        get
          = do x1 <- get
               x2 <- get
               x3 <- get
               return (MsgKey x1 x2 x3)

 
instance Binary Msg where
        put (Msg x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14)
          = do put x1
               put x2
               put x3
               put x4
               put x5
               put x6
               put x7
               put x8
               put x9
               put x10
               put x11
               put x12
               put x13
               put x14
        get
          = do x1 <- get
               x2 <- get
               x3 <- get
               x4 <- get
               x5 <- get
               x6 <- get
               x7 <- get
               x8 <- get
               x9 <- get
               x10 <- get
               x11 <- get
               x12 <- get
               x13 <- get
               x14 <- get
               return (Msg x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14)

 
instance Binary MsgHeader where
        put (MsgHeader x1 x2 x3 x4 x5 x6 x7 x8)
          = do put x1
               put x2
               put x3
               put x4
               put x5
               put x6
               put x7
               put x8
        get
          = do x1 <- get
               x2 <- get
               x3 <- get
               x4 <- get
               x5 <- get
               x6 <- get
               x7 <- get
               x8 <- get
               return (MsgHeader x1 x2 x3 x4 x5 x6 x7 x8)

 
instance Binary MsgTree where
        put (MsgTree x1 x2)
          = do put x1
               put x2
        get
          = do x1 <- get
               x2 <- get
               return (MsgTree x1 x2)

 
instance Binary CommentUrlState where
        put x
          = case x of
                CUSNew -> putWord8 0
                CUSError x1 -> do putWord8 1
                                  put x1
                CUSRedirect x1 -> do putWord8 2
                                     put x1
                CUSNoComments -> putWord8 3
                CUSOK -> putWord8 4
        get
          = do i <- getWord8
               case i of
                   0 -> return CUSNew
                   1 -> do x1 <- get
                           return (CUSError x1)
                   2 -> do x1 <- get
                           return (CUSRedirect x1)
                   3 -> return CUSNoComments
                   4 -> return CUSOK
                   _ -> error "Corrupted binary data for CommentUrlState"

 
instance Binary BlogPostsScanned where
        put (BlogPostsScanned x1 x2 x3)
          = do put x1
               put x2
               put x3
        get
          = do x1 <- get
               x2 <- get
               x3 <- get
               return (BlogPostsScanned x1 x2 x3)

 
instance Binary Posts where
        put (Posts x1 x2 x3 x4 x5 x6 x7 x8)
          = do put x1
               put x2
               put x3
               put x4
               put x5
               put x6
               put x7
               put x8
        get
          = do x1 <- get
               x2 <- get
               x3 <- get
               x4 <- get
               x5 <- get
               x6 <- get
               x7 <- get
               x8 <- get
               return (Posts x1 x2 x3 x4 x5 x6 x7 x8)

 
instance Binary DiscoveryFeed where
        put
          (DiscoveryFeed x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15
             x16 x17 x18)
          = do put x1
               put x2
               put x3
               put x4
               put x5
               put x6
               put x7
               put x8
               put x9
               put x10
               put x11
               put x12
               put x13
               put x14
               put x15
               put x16
               put x17
               put x18
        get
          = do x1 <- get
               x2 <- get
               x3 <- get
               x4 <- get
               x5 <- get
               x6 <- get
               x7 <- get
               x8 <- get
               x9 <- get
               x10 <- get
               x11 <- get
               x12 <- get
               x13 <- get
               x14 <- get
               x15 <- get
               x16 <- get
               x17 <- get
               x18 <- get
               return
                 (DiscoveryFeed x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15
                    x16
                    x17
                    x18)

 
instance Binary PostsClearTime where
        put (PostsClearTime x1 x2 x3 x4 x5 x6)
          = do put x1
               put x2
               put x3
               put x4
               put x5
               put x6
        get
          = do x1 <- get
               x2 <- get
               x3 <- get
               x4 <- get
               x5 <- get
               x6 <- get
               return (PostsClearTime x1 x2 x3 x4 x5 x6)

 
instance Binary PostsSubscribers where
        put (PostsSubscribers x1 x2 x3 x4 x5 x6 x7)
          = do put x1
               put x2
               put x3
               put x4
               put x5
               put x6
               put x7
        get
          = do x1 <- get
               x2 <- get
               x3 <- get
               x4 <- get
               x5 <- get
               x6 <- get
               x7 <- get
               return (PostsSubscribers x1 x2 x3 x4 x5 x6 x7)

 
instance Binary ActiveCheckSubscriptions where
        put (ActiveCheckSubscriptions x1 x2)
          = do put x1
               put x2
        get
          = do x1 <- get
               x2 <- get
               return (ActiveCheckSubscriptions x1 x2)

 
instance Binary CommentsKey where
        put (CommentsKey x1 x2)
          = do put x1
               put x2
        get
          = do x1 <- get
               x2 <- get
               return (CommentsKey x1 x2)

 
instance Binary Comments where
        put (Comments x1 x2)
          = do put x1
               put x2
        get
          = do x1 <- get
               x2 <- get
               return (Comments x1 x2)

 
instance Binary SubscriptionParentUrl where
        put x
          = case x of
                SpuRedirect x1 -> do putWord8 0
                                     put x1
                SpuHtml x1 x2 -> do putWord8 1
                                    put x1
                                    put x2
        get
          = do i <- getWord8
               case i of
                   0 -> do x1 <- get
                           return (SpuRedirect x1)
                   1 -> do x1 <- get
                           x2 <- get
                           return (SpuHtml x1 x2)
                   _ -> error "Corrupted binary data for SubscriptionParentUrl"

 
instance Binary ParentUrl where
        put x
          = case x of
                PuRedirect x1 -> do putWord8 0
                                    put x1
                PuHtml x1 x2 -> do putWord8 1
                                   put x1
                                   put x2
                PuFeed x1 x2 -> do putWord8 2
                                   put x1
                                   put x2
                PuCommentsFeed x1 -> do putWord8 3
                                        put x1
        get
          = do i <- getWord8
               case i of
                   0 -> do x1 <- get
                           return (PuRedirect x1)
                   1 -> do x1 <- get
                           x2 <- get
                           return (PuHtml x1 x2)
                   2 -> do x1 <- get
                           x2 <- get
                           return (PuFeed x1 x2)
                   3 -> do x1 <- get
                           return (PuCommentsFeed x1)
                   _ -> error "Corrupted binary data for ParentUrl"

 
instance Binary SubscriptionParentPath where
        put (SubscriptionParentPath x1 x2)
          = do put x1
               put x2
        get
          = do x1 <- get
               x2 <- get
               return (SubscriptionParentPath x1 x2)

 
instance Binary ParentPath where
        put (ParentPath x1 x2)
          = do put x1
               put x2
        get
          = do x1 <- get
               x2 <- get
               return (ParentPath x1 x2)

 
instance Binary UrlToScan where
        put (UrlToScan x1 x2 x3 x4 x5 x6 x7 x8)
          = do put x1
               put x2
               put x3
               put x4
               put x5
               put x6
               put x7
               put x8
        get
          = do x1 <- get
               x2 <- get
               x3 <- get
               x4 <- get
               x5 <- get
               x6 <- get
               x7 <- get
               x8 <- get
               return (UrlToScan x1 x2 x3 x4 x5 x6 x7 x8)

 
instance Binary QueueType where
        put x
          = case x of
                QTSubscription -> putWord8 0
                QTBlogFeed -> putWord8 1
                QTTemporary1 -> putWord8 2
                QTNewComment1 -> putWord8 3
                QTRescan1 -> putWord8 4
                QTTemporary -> putWord8 5
                QTNewComment -> putWord8 6
                QTRescan -> putWord8 7
        get
          = do i <- getWord8
               case i of
                   0 -> return QTSubscription
                   1 -> return QTBlogFeed
                   2 -> return QTTemporary1
                   3 -> return QTNewComment1
                   4 -> return QTRescan1
                   5 -> return QTTemporary
                   6 -> return QTNewComment
                   7 -> return QTRescan
                   _ -> error "Corrupted binary data for QueueType"

 
instance Binary ScanList where
        put (ScanList x1 x2)
          = do put x1
               put x2
        get
          = do x1 <- get
               x2 <- get
               return (ScanList x1 x2)

 
instance Binary PostsRead where
        put (PostsRead x1 x2 x3 x4 x5 x6)
          = do put x1
               put x2
               put x3
               put x4
               put x5
               put x6
        get
          = do x1 <- get
               x2 <- get
               x3 <- get
               x4 <- get
               x5 <- get
               x6 <- get
               return (PostsRead x1 x2 x3 x4 x5 x6)

 
instance Binary PostsTagged where
        put (PostsTagged x1 x2 x3 x4 x5 x6)
          = do put x1
               put x2
               put x3
               put x4
               put x5
               put x6
        get
          = do x1 <- get
               x2 <- get
               x3 <- get
               x4 <- get
               x5 <- get
               x6 <- get
               return (PostsTagged x1 x2 x3 x4 x5 x6)

 
instance Binary PostsTaggedGuids where
        put (PostsTaggedGuids x1 x2 x3 x4 x5 x6)
          = do put x1
               put x2
               put x3
               put x4
               put x5
               put x6
        get
          = do x1 <- get
               x2 <- get
               x3 <- get
               x4 <- get
               x5 <- get
               x6 <- get
               return (PostsTaggedGuids x1 x2 x3 x4 x5 x6)

 
instance Binary ItemTag where
        put x
          = case x of
                ITStarred -> putWord8 0
                ITTag x1 -> do putWord8 1
                               put x1
        get
          = do i <- getWord8
               case i of
                   0 -> return ITStarred
                   1 -> do x1 <- get
                           return (ITTag x1)
                   _ -> error "Corrupted binary data for ItemTag"

 
instance Binary RemovedFeedInfo where
        put (RemovedFeedInfo x1 x2 x3 x4)
          = do put x1
               put x2
               put x3
               put x4
        get
          = do x1 <- get
               x2 <- get
               x3 <- get
               x4 <- get
               return (RemovedFeedInfo x1 x2 x3 x4)

 
instance Binary GRIds where
        put
          (GRIds x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17)
          = do put x1
               put x2
               put x3
               put x4
               put x5
               put x6
               put x7
               put x8
               put x9
               put x10
               put x11
               put x12
               put x13
               put x14
               put x15
               put x16
               put x17
        get
          = do x1 <- get
               x2 <- get
               x3 <- get
               x4 <- get
               x5 <- get
               x6 <- get
               x7 <- get
               x8 <- get
               x9 <- get
               x10 <- get
               x11 <- get
               x12 <- get
               x13 <- get
               x14 <- get
               x15 <- get
               x16 <- get
               x17 <- get
               return
                 (GRIds x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17)

 
instance Binary UserBackup where
        put (UserBackup x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11)
          = do put x1
               put x2
               put x3
               put x4
               put x5
               put x6
               put x7
               put x8
               put x9
               put x10
               put x11
        get
          = do x1 <- get
               x2 <- get
               x3 <- get
               x4 <- get
               x5 <- get
               x6 <- get
               x7 <- get
               x8 <- get
               x9 <- get
               x10 <- get
               x11 <- get
               return (UserBackup x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11)

 
instance Binary DeletedUser where
        put (DeletedUser x1 x2 x3 x4 x5 x6)
          = do put x1
               put x2
               put x3
               put x4
               put x5
               put x6
        get
          = do x1 <- get
               x2 <- get
               x3 <- get
               x4 <- get
               x5 <- get
               x6 <- get
               return (DeletedUser x1 x2 x3 x4 x5 x6)

 
instance Binary ApiMode where
        put x
          = case x of
                AMNormal -> putWord8 0
                AMGRIdsOnly x1 x2 x3 x4 x5 x6 x7 x8 x9 -> do putWord8 1
                                                             put x1
                                                             put x2
                                                             put x3
                                                             put x4
                                                             put x5
                                                             put x6
                                                             put x7
                                                             put x8
                                                             put x9
                AMDiscovery x1 -> do putWord8 2
                                     put x1
        get
          = do i <- getWord8
               case i of
                   0 -> return AMNormal
                   1 -> do x1 <- get
                           x2 <- get
                           x3 <- get
                           x4 <- get
                           x5 <- get
                           x6 <- get
                           x7 <- get
                           x8 <- get
                           x9 <- get
                           return (AMGRIdsOnly x1 x2 x3 x4 x5 x6 x7 x8 x9)
                   2 -> do x1 <- get
                           return (AMDiscovery x1)
                   _ -> error "Corrupted binary data for ApiMode"

 
instance Binary MsgTreePoint where
        put (MsgTreePoint x1 x2 x3)
          = do put x1
               put x2
               put x3
        get
          = do x1 <- get
               x2 <- get
               x3 <- get
               return (MsgTreePoint x1 x2 x3)

 
instance Binary PostsReq where
        put (PostsReq x1 x2 x3 x4)
          = do put x1
               put x2
               put x3
               put x4
        get
          = do x1 <- get
               x2 <- get
               x3 <- get
               x4 <- get
               return (PostsReq x1 x2 x3 x4)

 
instance Binary CommentsReq where
        put (CommentsReq x1 x2 x3 x4)
          = do put x1
               put x2
               put x3
               put x4
        get
          = do x1 <- get
               x2 <- get
               x3 <- get
               x4 <- get
               return (CommentsReq x1 x2 x3 x4)

 
instance Binary TreeReq where
        put x
          = case x of
                TRPosts x1 -> do putWord8 0
                                 put x1
                TRComments x1 x2 -> do putWord8 1
                                       put x1
                                       put x2
                TRSearch x1 x2 x3 x4 x5 -> do putWord8 2
                                              put x1
                                              put x2
                                              put x3
                                              put x4
                                              put x5
                TRTags x1 x2 -> do putWord8 3
                                   put x1
                                   put x2
        get
          = do i <- getWord8
               case i of
                   0 -> do x1 <- get
                           return (TRPosts x1)
                   1 -> do x1 <- get
                           x2 <- get
                           return (TRComments x1 x2)
                   2 -> do x1 <- get
                           x2 <- get
                           x3 <- get
                           x4 <- get
                           x5 <- get
                           return (TRSearch x1 x2 x3 x4 x5)
                   3 -> do x1 <- get
                           x2 <- get
                           return (TRTags x1 x2)
                   _ -> error "Corrupted binary data for TreeReq"

 
instance Binary MsgView where
        put x
          = case x of
                MVFull x1 -> do putWord8 0
                                put x1
                MVShort x1 x2 -> do putWord8 1
                                    put x1
                                    put x2
        get
          = do i <- getWord8
               case i of
                   0 -> do x1 <- get
                           return (MVFull x1)
                   1 -> do x1 <- get
                           x2 <- get
                           return (MVShort x1 x2)
                   _ -> error "Corrupted binary data for MsgView"

 
instance Binary MsgId where
        put (MsgId x1 x2 x3 x4)
          = do put x1
               put x2
               put x3
               put x4
        get
          = do x1 <- get
               x2 <- get
               x3 <- get
               x4 <- get
               return (MsgId x1 x2 x3 x4)

 
instance Binary MsgItem where
        put (MsgItem x1 x2 x3 x4 x5 x6)
          = do put x1
               put x2
               put x3
               put x4
               put x5
               put x6
        get
          = do x1 <- get
               x2 <- get
               x3 <- get
               x4 <- get
               x5 <- get
               x6 <- get
               return (MsgItem x1 x2 x3 x4 x5 x6)

 
instance Binary MsgForest where
        put (MsgForest x1 x2 x3 x4)
          = do put x1
               put x2
               put x3
               put x4
        get
          = do x1 <- get
               x2 <- get
               x3 <- get
               x4 <- get
               return (MsgForest x1 x2 x3 x4)

 
instance Binary LoginType where
        put x
          = case x of
                Google -> putWord8 0
                Facebook -> putWord8 1
                Twitter -> putWord8 2
                OpenId x1 -> do putWord8 3
                                put x1
        get
          = do i <- getWord8
               case i of
                   0 -> return Google
                   1 -> return Facebook
                   2 -> return Twitter
                   3 -> do x1 <- get
                           return (OpenId x1)
                   _ -> error "Corrupted binary data for LoginType"

 
instance Binary Counters where
        put (Counters x1 x2 x3 x4 x5 x6 x7 x8 x9)
          = do put x1
               put x2
               put x3
               put x4
               put x5
               put x6
               put x7
               put x8
               put x9
        get
          = do x1 <- get
               x2 <- get
               x3 <- get
               x4 <- get
               x5 <- get
               x6 <- get
               x7 <- get
               x8 <- get
               x9 <- get
               return (Counters x1 x2 x3 x4 x5 x6 x7 x8 x9)

 
instance Binary SITFeedDetails where
        put (SITFeedDetails x1 x2 x3 x4 x5)
          = do put x1
               put x2
               put x3
               put x4
               put x5
        get
          = do x1 <- get
               x2 <- get
               x3 <- get
               x4 <- get
               x5 <- get
               return (SITFeedDetails x1 x2 x3 x4 x5)

 
instance Binary SubItemType where
        put x
          = case x of
                SITAll -> putWord8 0
                SITSearch x1 -> do putWord8 1
                                   put x1
                SITFolder x1 -> do putWord8 2
                                   put x1
                SITFeed x1 x2 x3 -> do putWord8 3
                                       put x1
                                       put x2
                                       put x3
                SITTag x1 -> do putWord8 4
                                put x1
                SITStarred -> putWord8 5
                SITAllTags -> putWord8 6
        get
          = do i <- getWord8
               case i of
                   0 -> return SITAll
                   1 -> do x1 <- get
                           return (SITSearch x1)
                   2 -> do x1 <- get
                           return (SITFolder x1)
                   3 -> do x1 <- get
                           x2 <- get
                           x3 <- get
                           return (SITFeed x1 x2 x3)
                   4 -> do x1 <- get
                           return (SITTag x1)
                   5 -> return SITStarred
                   6 -> return SITAllTags
                   _ -> error "Corrupted binary data for SubItemType"

 
instance Binary SubItemRpc where
        put (SubItemRpc x1 x2 x3 x4 x5 x6 x7 x8 x9 x10)
          = do put x1
               put x2
               put x3
               put x4
               put x5
               put x6
               put x7
               put x8
               put x9
               put x10
        get
          = do x1 <- get
               x2 <- get
               x3 <- get
               x4 <- get
               x5 <- get
               x6 <- get
               x7 <- get
               x8 <- get
               x9 <- get
               x10 <- get
               return (SubItemRpc x1 x2 x3 x4 x5 x6 x7 x8 x9 x10)

 
instance Binary WelcomeState where
        put (WelcomeState x1 x2 x3 x4)
          = do put x1
               put x2
               put x3
               put x4
        get
          = do x1 <- get
               x2 <- get
               x3 <- get
               x4 <- get
               return (WelcomeState x1 x2 x3 x4)

 
instance Binary ShareAction where
        put x
          = case x of
                SAEMail -> putWord8 0
                SATwitter -> putWord8 1
                SAFacebook -> putWord8 2
                SAGooglePlus -> putWord8 3
                SATumblr -> putWord8 4
                SAEvernote -> putWord8 5
                SADelicious -> putWord8 6
                SAPinboard -> putWord8 7
                SAPocket -> putWord8 8
                SAReadability -> putWord8 9
                SAInstapaper -> putWord8 10
                SATranslate -> putWord8 11
        get
          = do i <- getWord8
               case i of
                   0 -> return SAEMail
                   1 -> return SATwitter
                   2 -> return SAFacebook
                   3 -> return SAGooglePlus
                   4 -> return SATumblr
                   5 -> return SAEvernote
                   6 -> return SADelicious
                   7 -> return SAPinboard
                   8 -> return SAPocket
                   9 -> return SAReadability
                   10 -> return SAInstapaper
                   11 -> return SATranslate
                   _ -> error "Corrupted binary data for ShareAction"

 
instance Binary BrowserType where
        put x
          = case x of
                BTUnknown -> putWord8 0
                BTAndroid -> putWord8 1
                BTIPhone -> putWord8 2
                BTIPad -> putWord8 3
                BTIPod -> putWord8 4
                BTChrome -> putWord8 5
                BTIE -> putWord8 6
                BTIEMobile -> putWord8 7
                BTSafari -> putWord8 8
                BTOpera -> putWord8 9
                BTOperaMini -> putWord8 10
                BTFirefox -> putWord8 11
        get
          = do i <- getWord8
               case i of
                   0 -> return BTUnknown
                   1 -> return BTAndroid
                   2 -> return BTIPhone
                   3 -> return BTIPad
                   4 -> return BTIPod
                   5 -> return BTChrome
                   6 -> return BTIE
                   7 -> return BTIEMobile
                   8 -> return BTSafari
                   9 -> return BTOpera
                   10 -> return BTOperaMini
                   11 -> return BTFirefox
                   _ -> error "Corrupted binary data for BrowserType"

 
instance Binary AppType where
        put x
          = case x of
                ATUnknown -> putWord8 0
                ATFeeddler -> putWord8 1
                ATMrReader -> putWord8 2
                ATReeder -> putWord8 3
                ATSlowFeeds -> putWord8 4
                ATJustReader -> putWord8 5
                ATNewsPlus -> putWord8 6
                ATPress -> putWord8 7
                ATVienna -> putWord8 8
                ATReadKit -> putWord8 9
                ATNewsJet -> putWord8 10
                ATAmber -> putWord8 11
                ATgzip -> putWord8 12
        get
          = do i <- getWord8
               case i of
                   0 -> return ATUnknown
                   1 -> return ATFeeddler
                   2 -> return ATMrReader
                   3 -> return ATReeder
                   4 -> return ATSlowFeeds
                   5 -> return ATJustReader
                   6 -> return ATNewsPlus
                   7 -> return ATPress
                   8 -> return ATVienna
                   9 -> return ATReadKit
                   10 -> return ATNewsJet
                   11 -> return ATAmber
                   12 -> return ATgzip
                   _ -> error "Corrupted binary data for AppType"

 
instance Binary OperatingSystem where
        put x
          = case x of
                OSUnknown -> putWord8 0
                OSWindows -> putWord8 1
                OSMac -> putWord8 2
                OSLinux -> putWord8 3
                OSAndroid -> putWord8 4
                OSIOS -> putWord8 5
        get
          = do i <- getWord8
               case i of
                   0 -> return OSUnknown
                   1 -> return OSWindows
                   2 -> return OSMac
                   3 -> return OSLinux
                   4 -> return OSAndroid
                   5 -> return OSIOS
                   _ -> error "Corrupted binary data for OperatingSystem"

 
instance Binary UsageFlag where
        put x
          = case x of
                UFWeb x1 x2 -> do putWord8 0
                                  put x1
                                  put x2
                UFApp x1 x2 -> do putWord8 1
                                  put x1
                                  put x2
                UFShareAction x1 -> do putWord8 2
                                       put x1
                UFOPML -> putWord8 3
                UFAddSubscription -> putWord8 4
                UFSearchSubscriptions -> putWord8 5
                UFDiscoverySubscription -> putWord8 6
                UFAddDiscoverySubscription -> putWord8 7
                UFUnsubscribe -> putWord8 8
                UFRetrySubscription -> putWord8 9
                UFRenameSubscription -> putWord8 10
                UFRenameFolder -> putWord8 11
                UFEditSubscriptionFolders -> putWord8 12
                UFDragAndDrop -> putWord8 13
                UFSearch -> putWord8 14
                UFSearchTags -> putWord8 15
                UFSkip -> putWord8 16
                UFIgnore -> putWord8 17
                UFKeepUnread -> putWord8 18
                UFMarkAllAsRead -> putWord8 19
                UFStar -> putWord8 20
                UFTag -> putWord8 21
                UFReadability -> putWord8 22
                UFSetMobileLogin -> putWord8 23
                UFEnablePublicFeed -> putWord8 24
                UFDisablePublicFeed -> putWord8 25
                UFGenerateNewPublicFeed -> putWord8 26
                UFDeleteAccount -> putWord8 27
                UFExportOPML -> putWord8 28
                UFMarkAllAsReadD x1 -> do putWord8 29
                                          put x1
        get
          = do i <- getWord8
               case i of
                   0 -> do x1 <- get
                           x2 <- get
                           return (UFWeb x1 x2)
                   1 -> do x1 <- get
                           x2 <- get
                           return (UFApp x1 x2)
                   2 -> do x1 <- get
                           return (UFShareAction x1)
                   3 -> return UFOPML
                   4 -> return UFAddSubscription
                   5 -> return UFSearchSubscriptions
                   6 -> return UFDiscoverySubscription
                   7 -> return UFAddDiscoverySubscription
                   8 -> return UFUnsubscribe
                   9 -> return UFRetrySubscription
                   10 -> return UFRenameSubscription
                   11 -> return UFRenameFolder
                   12 -> return UFEditSubscriptionFolders
                   13 -> return UFDragAndDrop
                   14 -> return UFSearch
                   15 -> return UFSearchTags
                   16 -> return UFSkip
                   17 -> return UFIgnore
                   18 -> return UFKeepUnread
                   19 -> return UFMarkAllAsRead
                   20 -> return UFStar
                   21 -> return UFTag
                   22 -> return UFReadability
                   23 -> return UFSetMobileLogin
                   24 -> return UFEnablePublicFeed
                   25 -> return UFDisablePublicFeed
                   26 -> return UFGenerateNewPublicFeed
                   27 -> return UFDeleteAccount
                   28 -> return UFExportOPML
                   29 -> do x1 <- get
                            return (UFMarkAllAsReadD x1)
                   _ -> error "Corrupted binary data for UsageFlag"

 
instance Binary UserUsageFlags where
        put (UserUsageFlags x1 x2 x3 x4 x5 x6 x7)
          = do put x1
               put x2
               put x3
               put x4
               put x5
               put x6
               put x7
        get
          = do x1 <- get
               x2 <- get
               x3 <- get
               x4 <- get
               x5 <- get
               x6 <- get
               x7 <- get
               return (UserUsageFlags x1 x2 x3 x4 x5 x6 x7)

 
instance Binary UsageFlags where
        put (UsageFlags x1 x2 x3 x4 x5 x6)
          = do put x1
               put x2
               put x3
               put x4
               put x5
               put x6
        get
          = do x1 <- get
               x2 <- get
               x3 <- get
               x4 <- get
               x5 <- get
               x6 <- get
               return (UsageFlags x1 x2 x3 x4 x5 x6)

 
instance Binary BgAction where
        put x
          = case x of
                BGMarkMsgRead x1 x2 x3 -> do putWord8 0
                                             put x1
                                             put x2
                                             put x3
                BGAddTag x1 x2 -> do putWord8 1
                                     put x1
                                     put x2
                BGRemoveTag x1 x2 -> do putWord8 2
                                        put x1
                                        put x2
                BGSkipComments x1 x2 -> do putWord8 3
                                           put x1
                                           put x2
                BGIgnorePost x1 x2 -> do putWord8 4
                                         put x1
                                         put x2
                BGMarkBlogRead x1 x2 x3 -> do putWord8 5
                                              put x1
                                              put x2
                                              put x3
                BGMarkBlogReadD x1 x2 x3 x4 -> do putWord8 6
                                                  put x1
                                                  put x2
                                                  put x3
                                                  put x4
                BGSetOnlyUpdatedSubscriptions x1 -> do putWord8 7
                                                       put x1
                BGSetFolderViewMode x1 x2 -> do putWord8 8
                                                put x1
                                                put x2
                BGSetSubscriptionViewMode x1 x2 -> do putWord8 9
                                                      put x1
                                                      put x2
                BGClearAllSubscriptions -> putWord8 10
                BGSaveFilterQuery x1 -> do putWord8 11
                                           put x1
                BGSetScrollMode x1 -> do putWord8 12
                                         put x1
                BGSetListViewMode x1 -> do putWord8 13
                                           put x1
                BGSetMarkReadMode x1 -> do putWord8 14
                                           put x1
                BGSetUltraCompact x1 -> do putWord8 15
                                           put x1
                BGDragAndDrop x1 x2 x3 x4 -> do putWord8 16
                                                put x1
                                                put x2
                                                put x3
                                                put x4
                BGSetExactUnreadCounts x1 -> do putWord8 17
                                                put x1
                BGSortAllFeedsAndFolders -> putWord8 18
                BGSortFolder x1 -> do putWord8 19
                                      put x1
                BGSortTags -> putWord8 20
                BGShareAction x1 -> do putWord8 21
                                       put x1
                BGSetCountry x1 -> do putWord8 22
                                      put x1
        get
          = do i <- getWord8
               case i of
                   0 -> do x1 <- get
                           x2 <- get
                           x3 <- get
                           return (BGMarkMsgRead x1 x2 x3)
                   1 -> do x1 <- get
                           x2 <- get
                           return (BGAddTag x1 x2)
                   2 -> do x1 <- get
                           x2 <- get
                           return (BGRemoveTag x1 x2)
                   3 -> do x1 <- get
                           x2 <- get
                           return (BGSkipComments x1 x2)
                   4 -> do x1 <- get
                           x2 <- get
                           return (BGIgnorePost x1 x2)
                   5 -> do x1 <- get
                           x2 <- get
                           x3 <- get
                           return (BGMarkBlogRead x1 x2 x3)
                   6 -> do x1 <- get
                           x2 <- get
                           x3 <- get
                           x4 <- get
                           return (BGMarkBlogReadD x1 x2 x3 x4)
                   7 -> do x1 <- get
                           return (BGSetOnlyUpdatedSubscriptions x1)
                   8 -> do x1 <- get
                           x2 <- get
                           return (BGSetFolderViewMode x1 x2)
                   9 -> do x1 <- get
                           x2 <- get
                           return (BGSetSubscriptionViewMode x1 x2)
                   10 -> return BGClearAllSubscriptions
                   11 -> do x1 <- get
                            return (BGSaveFilterQuery x1)
                   12 -> do x1 <- get
                            return (BGSetScrollMode x1)
                   13 -> do x1 <- get
                            return (BGSetListViewMode x1)
                   14 -> do x1 <- get
                            return (BGSetMarkReadMode x1)
                   15 -> do x1 <- get
                            return (BGSetUltraCompact x1)
                   16 -> do x1 <- get
                            x2 <- get
                            x3 <- get
                            x4 <- get
                            return (BGDragAndDrop x1 x2 x3 x4)
                   17 -> do x1 <- get
                            return (BGSetExactUnreadCounts x1)
                   18 -> return BGSortAllFeedsAndFolders
                   19 -> do x1 <- get
                            return (BGSortFolder x1)
                   20 -> return BGSortTags
                   21 -> do x1 <- get
                            return (BGShareAction x1)
                   22 -> do x1 <- get
                            return (BGSetCountry x1)
                   _ -> error "Corrupted binary data for BgAction"

 
instance Binary SearchResults where
        put (SearchResults x1 x2 x3 x4)
          = do put x1
               put x2
               put x3
               put x4
        get
          = do x1 <- get
               x2 <- get
               x3 <- get
               x4 <- get
               return (SearchResults x1 x2 x3 x4)

 
instance Binary FullTextCache where
        put (FullTextCache x1 x2 x3 x4 x5)
          = do put x1
               put x2
               put x3
               put x4
               put x5
        get
          = do x1 <- get
               x2 <- get
               x3 <- get
               x4 <- get
               x5 <- get
               return (FullTextCache x1 x2 x3 x4 x5)

 
instance Binary OkErrorRedirect where
        put x
          = case x of
                OEROK -> putWord8 0
                OERError x1 -> do putWord8 1
                                  put x1
                OERRedirect x1 -> do putWord8 2
                                     put x1
        get
          = do i <- getWord8
               case i of
                   0 -> return OEROK
                   1 -> do x1 <- get
                           return (OERError x1)
                   2 -> do x1 <- get
                           return (OERRedirect x1)
                   _ -> error "Corrupted binary data for OkErrorRedirect"
