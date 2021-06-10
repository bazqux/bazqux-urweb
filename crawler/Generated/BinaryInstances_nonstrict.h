
instance () => Binary Stats where
        put (Stats x1 x2)
          = do put x1
               put x2
        get
          = do x1 <- get
               x2 <- get
               return (Stats x1 x2)

instance () => Binary SubscriptionParentUrl where
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

instance () => Binary SubscriptionState where
        put x
          = case x of
                SSAdded -> putWord8 0
                SSScanning x1 -> do putWord8 1
                                    put x1
                SSError x1 -> do putWord8 2
                                 put x1
                SSFeed x1 -> do putWord8 3
                                put x1
                SSErrorPath x1 x2 -> do putWord8 4
                                        put x1
                                        put x2
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
                   4 -> do x1 <- get
                           x2 <- get
                           return (SSErrorPath x1 x2)
                   _ -> error "Corrupted binary data for SubscriptionState"

instance () => Binary Subscription where
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

instance () => Binary PostsViewMode where
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

instance () => Binary MTVMEx where
        put x
          = case x of
                MTVMFolderCollapsed -> putWord8 0
                MTVMFolderExpanded -> putWord8 1
                MTVMEx x1 x2 x3 x4 -> do putWord8 2
                                         put x1
                                         put x2
                                         put x3
                                         put x4
        get
          = do i <- getWord8
               case i of
                   0 -> return MTVMFolderCollapsed
                   1 -> return MTVMFolderExpanded
                   2 -> do x1 <- get
                           x2 <- get
                           x3 <- get
                           x4 <- get
                           return (MTVMEx x1 x2 x3 x4)
                   _ -> error "Corrupted binary data for MTVMEx"

instance () => Binary MsgTreeViewMode where
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

instance () => Binary Payment where
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

instance () => Binary PaidTill where
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

instance () => Binary UserViewMode where
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

instance () => Binary User where
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

instance () => Binary UserFilters where
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

instance () => Binary ScrollMode where
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

instance () => Binary ListViewMode where
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

instance () => Binary MarkReadMode where
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

instance () => Binary PublicFeedType where
        put x
          = case x of
                PFTAll -> putWord8 0
                PFTFolder x1 -> do putWord8 1
                                   put x1
                PFTTag x1 -> do putWord8 2
                                put x1
                PFTStarred -> putWord8 3
                PFTAllTags -> putWord8 4
                PFTSmartStream x1 -> do putWord8 5
                                        put x1
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
                   5 -> do x1 <- get
                           return (PFTSmartStream x1)
                   _ -> error "Corrupted binary data for PublicFeedType"

instance () => Binary LoginAccessToken where
        put x
          = case x of
                LATNone -> putWord8 0
                LATFacebook x1 -> do putWord8 1
                                     put x1
                LATTwitter x1 -> do putWord8 2
                                    put x1
        get
          = do i <- getWord8
               case i of
                   0 -> return LATNone
                   1 -> do x1 <- get
                           return (LATFacebook x1)
                   2 -> do x1 <- get
                           return (LATTwitter x1)
                   _ -> error "Corrupted binary data for LoginAccessToken"

instance () => Binary ApiKeys where
        put (ApiKeys x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13)
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
               return (ApiKeys x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13)

instance () => Binary UserExperiment where
        put UENo9 = return ()
        get = return UENo9

instance () => Binary CustomShareAction where
        put (CustomShareAction x1 x2 x3 x4)
          = do put x1
               put x2
               put x3
               put x4
        get
          = do x1 <- get
               x2 <- get
               x3 <- get
               x4 <- get
               return (CustomShareAction x1 x2 x3 x4)

instance () => Binary ShareAction where
        put x
          = case x of
                SAEMail -> putWord8 0
                SATwitter -> putWord8 1
                SAFacebook -> putWord8 2
                SAGooglePlus -> putWord8 3
                SATumblr -> putWord8 4
                SAEvernote -> putWord8 5
                SADelicious_discontinued -> putWord8 6
                SAPinboard -> putWord8 7
                SAPocket -> putWord8 8
                SAReadability_discontinued -> putWord8 9
                SAInstapaper -> putWord8 10
                SATranslate -> putWord8 11
                SABlogger -> putWord8 12
                SAWordpress -> putWord8 13
                SALinkedIn -> putWord8 14
                SAPinterest -> putWord8 15
                SAVK -> putWord8 16
                SASkype -> putWord8 17
                SAReddit -> putWord8 18
                SAStumbleUpon -> putWord8 19
                SADigg -> putWord8 20
                SAScoopIt -> putWord8 21
                SAFlipboard -> putWord8 22
                SABuffer -> putWord8 23
                SANewsVine -> putWord8 24
                SADiigo -> putWord8 25
                SARememberTheMilk -> putWord8 26
                SAGoogleBookmarks -> putWord8 27
                SAWallabag -> putWord8 28
                SAWakelet -> putWord8 29
                SACustom x1 -> do putWord8 30
                                  put x1
                SASystem -> putWord8 31
        get
          = do i <- getWord8
               case i of
                   0 -> return SAEMail
                   1 -> return SATwitter
                   2 -> return SAFacebook
                   3 -> return SAGooglePlus
                   4 -> return SATumblr
                   5 -> return SAEvernote
                   6 -> return SADelicious_discontinued
                   7 -> return SAPinboard
                   8 -> return SAPocket
                   9 -> return SAReadability_discontinued
                   10 -> return SAInstapaper
                   11 -> return SATranslate
                   12 -> return SABlogger
                   13 -> return SAWordpress
                   14 -> return SALinkedIn
                   15 -> return SAPinterest
                   16 -> return SAVK
                   17 -> return SASkype
                   18 -> return SAReddit
                   19 -> return SAStumbleUpon
                   20 -> return SADigg
                   21 -> return SAScoopIt
                   22 -> return SAFlipboard
                   23 -> return SABuffer
                   24 -> return SANewsVine
                   25 -> return SADiigo
                   26 -> return SARememberTheMilk
                   27 -> return SAGoogleBookmarks
                   28 -> return SAWallabag
                   29 -> return SAWakelet
                   30 -> do x1 <- get
                            return (SACustom x1)
                   31 -> return SASystem
                   _ -> error "Corrupted binary data for ShareAction"

instance () => Binary MsgButton where
        put x
          = case x of
                MBKeepUnread -> putWord8 0
                MBStar -> putWord8 1
                MBTag -> putWord8 2
                MBShare -> putWord8 3
                MBShareAction x1 -> do putWord8 4
                                       put x1
        get
          = do i <- getWord8
               case i of
                   0 -> return MBKeepUnread
                   1 -> return MBStar
                   2 -> return MBTag
                   3 -> return MBShare
                   4 -> do x1 <- get
                           return (MBShareAction x1)
                   _ -> error "Corrupted binary data for MsgButton"

instance () => Binary EmailContact where
        put (EMailContact x1 x2 x3 x4 x5)
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
               return (EMailContact x1 x2 x3 x4 x5)

instance () => Binary SharingSettings where
        put (SharingSettings x1 x2 x3 x4 x5 x6 x7 x8)
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
               return (SharingSettings x1 x2 x3 x4 x5 x6 x7 x8)

instance () => Binary LoginType where
        put x
          = case x of
                LTGoogle x1 -> do putWord8 0
                                  put x1
                LTFacebook x1 -> do putWord8 1
                                    put x1
                LTTwitter x1 -> do putWord8 2
                                   put x1
                LTOpenId x1 -> do putWord8 3
                                  put x1
                LTEmail x1 -> do putWord8 4
                                 put x1
                LTUsername x1 -> do putWord8 5
                                    put x1
                LTFeverApiKey x1 -> do putWord8 6
                                       put x1
        get
          = do i <- getWord8
               case i of
                   0 -> do x1 <- get
                           return (LTGoogle x1)
                   1 -> do x1 <- get
                           return (LTFacebook x1)
                   2 -> do x1 <- get
                           return (LTTwitter x1)
                   3 -> do x1 <- get
                           return (LTOpenId x1)
                   4 -> do x1 <- get
                           return (LTEmail x1)
                   5 -> do x1 <- get
                           return (LTUsername x1)
                   6 -> do x1 <- get
                           return (LTFeverApiKey x1)
                   _ -> error "Corrupted binary data for LoginType"

instance () => Binary Login where
        put (Login x1 x2 x3 x4 x5 x6)
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
               return (Login x1 x2 x3 x4 x5 x6)

instance () => Binary UserSettingsEx where
        put
          (UserSettingsEx x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15
             x16 x17 x18 x19 x20 x21 x22 x23)
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
               put x19
               put x20
               put x21
               put x22
               put x23
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
               x19 <- get
               x20 <- get
               x21 <- get
               x22 <- get
               x23 <- get
               return
                 (UserSettingsEx x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15
                    x16
                    x17
                    x18
                    x19
                    x20
                    x21
                    x22
                    x23)

instance () => Binary UserSettings where
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

instance () => Binary PublicFeed where
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

instance () => Binary UID where
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

instance () => Binary FeverIds where
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

instance () => Binary UserStats where
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

instance () => Binary MailQueue where
        put (MailQueue x1 x2 x3)
          = do put x1
               put x2
               put x3
        get
          = do x1 <- get
               x2 <- get
               x3 <- get
               return (MailQueue x1 x2 x3)

instance () => Binary Session where
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

instance () => Binary EmailVerificationType where
        put x
          = case x of
                EVTSignUp x1 x2 -> do putWord8 0
                                      put x1
                                      put x2
                EVTChangeEmail x1 -> do putWord8 1
                                        put x1
                EVTResetPassword x1 -> do putWord8 2
                                          put x1
                EVTRestoreAccess x1 -> do putWord8 3
                                          put x1
        get
          = do i <- getWord8
               case i of
                   0 -> do x1 <- get
                           x2 <- get
                           return (EVTSignUp x1 x2)
                   1 -> do x1 <- get
                           return (EVTChangeEmail x1)
                   2 -> do x1 <- get
                           return (EVTResetPassword x1)
                   3 -> do x1 <- get
                           return (EVTRestoreAccess x1)
                   _ -> error "Corrupted binary data for EmailVerificationType"

instance () => Binary EmailVerificationToken where
        put (EmailVerificationToken x1 x2 x3 x4 x5 x6 x7 x8 x9)
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
               return (EmailVerificationToken x1 x2 x3 x4 x5 x6 x7 x8 x9)

instance () => Binary EmailVerification where
        put (EmailVerification x1 x2 x3 x4 x5 x6 x7 x8 x9)
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
               return (EmailVerification x1 x2 x3 x4 x5 x6 x7 x8 x9)

instance () => Binary UserEmailVerificationTokens where
        put (UserEmailVerificationTokens x1 x2 x3 x4 x5 x6)
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
               return (UserEmailVerificationTokens x1 x2 x3 x4 x5 x6)

instance () => Binary SubscriptionUrlKind where
        put x
          = case x of
                SUKError x1 -> do putWord8 0
                                  put x1
                SUKFeed x1 -> do putWord8 1
                                 put x1
                SUKErrorPath x1 x2 -> do putWord8 2
                                         put x1
                                         put x2
        get
          = do i <- getWord8
               case i of
                   0 -> do x1 <- get
                           return (SUKError x1)
                   1 -> do x1 <- get
                           return (SUKFeed x1)
                   2 -> do x1 <- get
                           x2 <- get
                           return (SUKErrorPath x1 x2)
                   _ -> error "Corrupted binary data for SubscriptionUrlKind"

instance () => Binary SubscriptionUrlInfo where
        put (SubscriptionUrlInfo x1 x2 x3)
          = do put x1
               put x2
               put x3
        get
          = do x1 <- get
               x2 <- get
               x3 <- get
               return (SubscriptionUrlInfo x1 x2 x3)

instance () => Binary Attachment where
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
                AVideo2 x1 x2 x3 x4 x5 x6 x7 x8 x9 -> do putWord8 6
                                                         put x1
                                                         put x2
                                                         put x3
                                                         put x4
                                                         put x5
                                                         put x6
                                                         put x7
                                                         put x8
                                                         put x9
                AThumbnail x1 -> do putWord8 7
                                    put x1
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
                   6 -> do x1 <- get
                           x2 <- get
                           x3 <- get
                           x4 <- get
                           x5 <- get
                           x6 <- get
                           x7 <- get
                           x8 <- get
                           x9 <- get
                           return (AVideo2 x1 x2 x3 x4 x5 x6 x7 x8 x9)
                   7 -> do x1 <- get
                           return (AThumbnail x1)
                   _ -> error "Corrupted binary data for Attachment"

instance () => Binary MsgKey where
        put (MsgKey x1 x2 x3)
          = do put x1
               put x2
               put x3
        get
          = do x1 <- get
               x2 <- get
               x3 <- get
               return (MsgKey x1 x2 x3)

instance () => Binary Msg where
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

instance () => Binary MsgHeader where
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

instance () => Binary TimeId where
        put (TimeId x1 x2)
          = do put x1
               put x2
        get
          = do x1 <- get
               x2 <- get
               return (TimeId x1 x2)

instance () => Binary MsgTree where
        put (MsgTree x1 x2)
          = do put x1
               put x2
        get
          = do x1 <- get
               x2 <- get
               return (MsgTree x1 x2)

instance () => Binary CommentUrlState where
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

instance () => Binary BlogPostsScanned where
        put (BlogPostsScanned x1 x2 x3)
          = do put x1
               put x2
               put x3
        get
          = do x1 <- get
               x2 <- get
               x3 <- get
               return (BlogPostsScanned x1 x2 x3)

instance () => Binary Posts where
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

instance () => Binary DiscoveryFeed where
        put
          (DiscoveryFeed x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15)
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
                 (DiscoveryFeed x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15)

instance () => Binary PostsClearTime where
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

instance () => Binary PostsSubscribers where
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

instance () => Binary ActiveCheckSubscriptions where
        put (ActiveCheckSubscriptions x1 x2)
          = do put x1
               put x2
        get
          = do x1 <- get
               x2 <- get
               return (ActiveCheckSubscriptions x1 x2)

instance () => Binary CommentsKey where
        put (CommentsKey x1 x2)
          = do put x1
               put x2
        get
          = do x1 <- get
               x2 <- get
               return (CommentsKey x1 x2)

instance () => Binary Comments where
        put (Comments x1 x2)
          = do put x1
               put x2
        get
          = do x1 <- get
               x2 <- get
               return (Comments x1 x2)

instance () => Binary ParentUrl where
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

instance () => Binary SubscriptionParentPath where
        put (SubscriptionParentPath x1 x2)
          = do put x1
               put x2
        get
          = do x1 <- get
               x2 <- get
               return (SubscriptionParentPath x1 x2)

instance () => Binary ParentPath where
        put (ParentPath x1 x2)
          = do put x1
               put x2
        get
          = do x1 <- get
               x2 <- get
               return (ParentPath x1 x2)

instance () => Binary UrlToScan where
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

instance () => Binary QueueType where
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
                QTSubscriptionOPML -> putWord8 8
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
                   8 -> return QTSubscriptionOPML
                   _ -> error "Corrupted binary data for QueueType"

instance () => Binary ScanList where
        put (ScanList x1 x2)
          = do put x1
               put x2
        get
          = do x1 <- get
               x2 <- get
               return (ScanList x1 x2)

instance () => Binary OldFeedMask where
        put (OldFeedMask x1 x2)
          = do put x1
               put x2
        get
          = do x1 <- get
               x2 <- get
               return (OldFeedMask x1 x2)

instance () => Binary FeedMask where
        put x
          = case x of
                FMFeedMask x1 x2 -> do putWord8 0
                                       put x1
                                       put x2
                FMError -> putWord8 1
        get
          = do i <- getWord8
               case i of
                   0 -> do x1 <- get
                           x2 <- get
                           return (FMFeedMask x1 x2)
                   1 -> return FMError
                   _ -> error "Corrupted binary data for FeedMask"

instance () => Binary PostsRead where
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

instance () => Binary PostsTagged where
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

instance () => Binary PostsTaggedGuids where
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

instance () => Binary ItemTag where
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

instance () => Binary RemovedFeedInfo where
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

instance () => Binary GRIds where
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

instance () => Binary UserBackup where
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

instance () => Binary DeletedUser where
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

instance () => Binary MailType where
        put x
          = case x of
                MTRenewInTwoWeeksReminder x1 -> do putWord8 0
                                                   put x1
                MTInactivityReasonRequest -> putWord8 1
        get
          = do i <- getWord8
               case i of
                   0 -> do x1 <- get
                           return (MTRenewInTwoWeeksReminder x1)
                   1 -> return MTInactivityReasonRequest
                   _ -> error "Corrupted binary data for MailType"

instance () => Binary MailsSent where
        put (MailsSent x1 x2 x3 x4 x5)
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
               return (MailsSent x1 x2 x3 x4 x5)

instance () => Binary FilterQuery where
        put (FilterQuery x1 x2 x3 x4 x5)
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
               return (FilterQuery x1 x2 x3 x4 x5)

instance () => Binary FilterQueryRpc where
        put (FilterQueryRpc x1 x2 x3)
          = do put x1
               put x2
               put x3
        get
          = do x1 <- get
               x2 <- get
               x3 <- get
               return (FilterQueryRpc x1 x2 x3)

instance () => Binary SearchError where
        put x
          = case x of
                SESyntaxError x1 -> do putWord8 0
                                       put x1
                SESystemError x1 -> do putWord8 1
                                       put x1
        get
          = do i <- getWord8
               case i of
                   0 -> do x1 <- get
                           return (SESyntaxError x1)
                   1 -> do x1 <- get
                           return (SESystemError x1)
                   _ -> error "Corrupted binary data for SearchError"

instance () => Binary FilterUpdateTime where
        put x
          = case x of
                FUTNever -> putWord8 0
                FUTUpdatedAt x1 -> do putWord8 1
                                      put x1
                FUTError x1 x2 -> do putWord8 2
                                     put x1
                                     put x2
                FUTEdited x1 x2 -> do putWord8 3
                                      put x1
                                      put x2
        get
          = do i <- getWord8
               case i of
                   0 -> return FUTNever
                   1 -> do x1 <- get
                           return (FUTUpdatedAt x1)
                   2 -> do x1 <- get
                           x2 <- get
                           return (FUTError x1 x2)
                   3 -> do x1 <- get
                           x2 <- get
                           return (FUTEdited x1 x2)
                   _ -> error "Corrupted binary data for FilterUpdateTime"

instance () => Binary FilterFeedMasks where
        put (FilterFeedMasks x1 x2 x3 x4)
          = do put x1
               put x2
               put x3
               put x4
        get
          = do x1 <- get
               x2 <- get
               x3 <- get
               x4 <- get
               return (FilterFeedMasks x1 x2 x3 x4)

instance () => Binary OldSmartStream where
        put (OldSmartStream x1 x2 x3 x4 x5)
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
               return (OldSmartStream x1 x2 x3 x4 x5)

instance () => Binary Filter where
        put (Filter x1 x2 x3 x4 x5)
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
               return (Filter x1 x2 x3 x4 x5)

instance () => Binary SmartStream where
        put (SmartStream x1 x2 x3 x4 x5 x6)
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
               return (SmartStream x1 x2 x3 x4 x5 x6)

instance () => Binary Filters where
        put (Filters x1 x2 x3 x4 x5 x6 x7 x8 x9)
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
               return (Filters x1 x2 x3 x4 x5 x6 x7 x8 x9)

instance () => Binary ApiMode where
        put x
          = case x of
                AMNormal x1 x2 -> do putWord8 0
                                     put x1
                                     put x2
                AMGRIdsOnly x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 -> do putWord8 1
                                                                         put x1
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
                AMDiscovery x1 x2 x3 -> do putWord8 2
                                           put x1
                                           put x2
                                           put x3
        get
          = do i <- getWord8
               case i of
                   0 -> do x1 <- get
                           x2 <- get
                           return (AMNormal x1 x2)
                   1 -> do x1 <- get
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
                           return (AMGRIdsOnly x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12)
                   2 -> do x1 <- get
                           x2 <- get
                           x3 <- get
                           return (AMDiscovery x1 x2 x3)
                   _ -> error "Corrupted binary data for ApiMode"

instance () => Binary MsgTreePoint where
        put (MsgTreePoint x1 x2 x3)
          = do put x1
               put x2
               put x3
        get
          = do x1 <- get
               x2 <- get
               x3 <- get
               return (MsgTreePoint x1 x2 x3)

instance () => Binary PostsReq where
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

instance () => Binary CommentsReq where
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

instance () => Binary MsgId where
        put (MsgId x1 x2 x3)
          = do put x1
               put x2
               put x3
        get
          = do x1 <- get
               x2 <- get
               x3 <- get
               return (MsgId x1 x2 x3)

instance () => Binary LongMsgId where
        put (LongMsgId x1 x2)
          = do put x1
               put x2
        get
          = do x1 <- get
               x2 <- get
               return (LongMsgId x1 x2)

instance () => Binary TreeReq where
        put x
          = case x of
                TRPosts x1 -> do putWord8 0
                                 put x1
                TRTags x1 x2 x3 -> do putWord8 1
                                      put x1
                                      put x2
                                      put x3
                TRComments x1 x2 -> do putWord8 2
                                       put x1
                                       put x2
                TRCommentsS x1 x2 x3 -> do putWord8 3
                                           put x1
                                           put x2
                                           put x3
                TRSmartStream x1 x2 -> do putWord8 4
                                          put x1
                                          put x2
                TRSearchPosts x1 x2 x3 -> do putWord8 5
                                             put x1
                                             put x2
                                             put x3
                TRSearchSmartStream x1 x2 x3 x4 -> do putWord8 6
                                                      put x1
                                                      put x2
                                                      put x3
                                                      put x4
                TRSearchTags x1 x2 x3 x4 x5 -> do putWord8 7
                                                  put x1
                                                  put x2
                                                  put x3
                                                  put x4
                                                  put x5
        get
          = do i <- getWord8
               case i of
                   0 -> do x1 <- get
                           return (TRPosts x1)
                   1 -> do x1 <- get
                           x2 <- get
                           x3 <- get
                           return (TRTags x1 x2 x3)
                   2 -> do x1 <- get
                           x2 <- get
                           return (TRComments x1 x2)
                   3 -> do x1 <- get
                           x2 <- get
                           x3 <- get
                           return (TRCommentsS x1 x2 x3)
                   4 -> do x1 <- get
                           x2 <- get
                           return (TRSmartStream x1 x2)
                   5 -> do x1 <- get
                           x2 <- get
                           x3 <- get
                           return (TRSearchPosts x1 x2 x3)
                   6 -> do x1 <- get
                           x2 <- get
                           x3 <- get
                           x4 <- get
                           return (TRSearchSmartStream x1 x2 x3 x4)
                   7 -> do x1 <- get
                           x2 <- get
                           x3 <- get
                           x4 <- get
                           x5 <- get
                           return (TRSearchTags x1 x2 x3 x4 x5)
                   _ -> error "Corrupted binary data for TreeReq"

instance () => Binary MsgView where
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

instance () => Binary MsgItem where
        put (MsgItem x1 x2 x3 x4 x5 x6 x7 x8 x9)
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
               return (MsgItem x1 x2 x3 x4 x5 x6 x7 x8 x9)

instance () => Binary MsgForest where
        put (MsgForest x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11)
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
               return (MsgForest x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11)

instance () => Binary ExternalLoginType where
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
                   _ -> error "Corrupted binary data for ExternalLoginType"

instance () => Binary ExternalLoginAction where
        put x
          = case x of
                ELALogin -> putWord8 0
                ELAAddUrl x1 -> do putWord8 1
                                   put x1
                ELAAddAssociatedAccount -> putWord8 2
        get
          = do i <- getWord8
               case i of
                   0 -> return ELALogin
                   1 -> do x1 <- get
                           return (ELAAddUrl x1)
                   2 -> return ELAAddAssociatedAccount
                   _ -> error "Corrupted binary data for ExternalLoginAction"

instance () => Binary Counters where
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

instance () => Binary SITFeedDetails where
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

instance () => Binary SubItemType where
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
                SITSmartStream x1 x2 -> do putWord8 5
                                           put x1
                                           put x2
                SITStarred -> putWord8 6
                SITAllTags -> putWord8 7
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
                   5 -> do x1 <- get
                           x2 <- get
                           return (SITSmartStream x1 x2)
                   6 -> return SITStarred
                   7 -> return SITAllTags
                   _ -> error "Corrupted binary data for SubItemType"

instance () => Binary SubItemRpc where
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

instance () => Binary WelcomeState where
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

instance () => Binary UpdateFilters where
        put x
          = case x of
                UFNone -> putWord8 0
                UFChanged -> putWord8 1
                UFAll -> putWord8 2
        get
          = do i <- getWord8
               case i of
                   0 -> return UFNone
                   1 -> return UFChanged
                   2 -> return UFAll
                   _ -> error "Corrupted binary data for UpdateFilters"

instance () => Binary BrowserType where
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
                BTVivaldi -> putWord8 12
                BTEdge -> putWord8 13
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
                   12 -> return BTVivaldi
                   13 -> return BTEdge
                   _ -> error "Corrupted binary data for BrowserType"

instance () => Binary AppType where
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
                ATUnread -> putWord8 13
                ATFeedMe -> putWord8 14
                ATFieryFeeds -> putWord8 15
                ATLire -> putWord8 16
                ATWebSubscriber -> putWord8 17
                ATReadably -> putWord8 18
                ATokhttp -> putWord8 19
                ATFluentReader -> putWord8 20
                ATRavenReader -> putWord8 21
                ATFocusReader -> putWord8 22
                ATNetNewsWire -> putWord8 23
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
                   13 -> return ATUnread
                   14 -> return ATFeedMe
                   15 -> return ATFieryFeeds
                   16 -> return ATLire
                   17 -> return ATWebSubscriber
                   18 -> return ATReadably
                   19 -> return ATokhttp
                   20 -> return ATFluentReader
                   21 -> return ATRavenReader
                   22 -> return ATFocusReader
                   23 -> return ATNetNewsWire
                   _ -> error "Corrupted binary data for AppType"

instance () => Binary OperatingSystem where
        put x
          = case x of
                OSUnknown -> putWord8 0
                OSWindows -> putWord8 1
                OSMac -> putWord8 2
                OSLinux -> putWord8 3
                OSAndroid -> putWord8 4
                OSIOS -> putWord8 5
                OSChromeOS -> putWord8 6
        get
          = do i <- getWord8
               case i of
                   0 -> return OSUnknown
                   1 -> return OSWindows
                   2 -> return OSMac
                   3 -> return OSLinux
                   4 -> return OSAndroid
                   5 -> return OSIOS
                   6 -> return OSChromeOS
                   _ -> error "Corrupted binary data for OperatingSystem"

instance () => Binary UsageFlag where
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
                UFSetUsername -> putWord8 23
                UFEnablePublicFeed -> putWord8 24
                UFDisablePublicFeed -> putWord8 25
                UFGenerateNewPublicFeed -> putWord8 26
                UFDeleteAccount -> putWord8 27
                UFExportOPML -> putWord8 28
                UFMarkAllAsReadD x1 -> do putWord8 29
                                          put x1
                UFMarkSearchAsReadD x1 -> do putWord8 30
                                             put x1
                UFFilterApply -> putWord8 31
                UFFilterHide -> putWord8 32
                UFNewSmartStream -> putWord8 33
                UFEditFilter -> putWord8 34
                UFEditSmartStream -> putWord8 35
                UFDeleteFilter -> putWord8 36
                UFDeleteSmartStream -> putWord8 37
                UFWhatsNewClick x1 -> do putWord8 38
                                         put x1
                UFWhatsNewClose x1 -> do putWord8 39
                                         put x1
                UFThemeChange x1 -> do putWord8 40
                                       put x1
                UFFontChange x1 -> do putWord8 41
                                      put x1
                UFFontSizeChange x1 -> do putWord8 42
                                          put x1
                UFLineHeightChange x1 x2 -> do putWord8 43
                                               put x1
                                               put x2
                UFSetPassword -> putWord8 44
                UFSetEmail -> putWord8 45
                UFMarkReadAbove -> putWord8 46
                UFMarkReadBelow -> putWord8 47
                UFUnstarAbove -> putWord8 48
                UFUnstarBelow -> putWord8 49
                UFUntagAbove -> putWord8 50
                UFUntagBelow -> putWord8 51
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
                   23 -> return UFSetUsername
                   24 -> return UFEnablePublicFeed
                   25 -> return UFDisablePublicFeed
                   26 -> return UFGenerateNewPublicFeed
                   27 -> return UFDeleteAccount
                   28 -> return UFExportOPML
                   29 -> do x1 <- get
                            return (UFMarkAllAsReadD x1)
                   30 -> do x1 <- get
                            return (UFMarkSearchAsReadD x1)
                   31 -> return UFFilterApply
                   32 -> return UFFilterHide
                   33 -> return UFNewSmartStream
                   34 -> return UFEditFilter
                   35 -> return UFEditSmartStream
                   36 -> return UFDeleteFilter
                   37 -> return UFDeleteSmartStream
                   38 -> do x1 <- get
                            return (UFWhatsNewClick x1)
                   39 -> do x1 <- get
                            return (UFWhatsNewClose x1)
                   40 -> do x1 <- get
                            return (UFThemeChange x1)
                   41 -> do x1 <- get
                            return (UFFontChange x1)
                   42 -> do x1 <- get
                            return (UFFontSizeChange x1)
                   43 -> do x1 <- get
                            x2 <- get
                            return (UFLineHeightChange x1 x2)
                   44 -> return UFSetPassword
                   45 -> return UFSetEmail
                   46 -> return UFMarkReadAbove
                   47 -> return UFMarkReadBelow
                   48 -> return UFUnstarAbove
                   49 -> return UFUnstarBelow
                   50 -> return UFUntagAbove
                   51 -> return UFUntagBelow
                   _ -> error "Corrupted binary data for UsageFlag"

instance () => Binary UserUsageFlags where
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

instance () => Binary UsageFlags where
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

instance () => Binary UserSessions where
        put (UserSessions x1 x2 x3 x4 x5 x6)
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
               return (UserSessions x1 x2 x3 x4 x5 x6)

instance () => Binary MarkReq where
        put x
          = case x of
                MRPosts x1 -> do putWord8 0
                                 put x1
                MRTags x1 x2 -> do putWord8 1
                                   put x1
                                   put x2
                MRSmartStream x1 x2 -> do putWord8 2
                                          put x1
                                          put x2
                MRSearchPosts x1 x2 x3 -> do putWord8 3
                                             put x1
                                             put x2
                                             put x3
                MRSearchTags x1 x2 x3 x4 -> do putWord8 4
                                               put x1
                                               put x2
                                               put x3
                                               put x4
                MRSearchSmartStream x1 x2 x3 x4 -> do putWord8 5
                                                      put x1
                                                      put x2
                                                      put x3
                                                      put x4
        get
          = do i <- getWord8
               case i of
                   0 -> do x1 <- get
                           return (MRPosts x1)
                   1 -> do x1 <- get
                           x2 <- get
                           return (MRTags x1 x2)
                   2 -> do x1 <- get
                           x2 <- get
                           return (MRSmartStream x1 x2)
                   3 -> do x1 <- get
                           x2 <- get
                           x3 <- get
                           return (MRSearchPosts x1 x2 x3)
                   4 -> do x1 <- get
                           x2 <- get
                           x3 <- get
                           x4 <- get
                           return (MRSearchTags x1 x2 x3 x4)
                   5 -> do x1 <- get
                           x2 <- get
                           x3 <- get
                           x4 <- get
                           return (MRSearchSmartStream x1 x2 x3 x4)
                   _ -> error "Corrupted binary data for MarkReq"

instance () => Binary MarkReadDirection where
        put x
          = case x of
                MRDAll -> putWord8 0
                MRDAbove x1 -> do putWord8 1
                                  put x1
                MRDBelow x1 -> do putWord8 2
                                  put x1
        get
          = do i <- getWord8
               case i of
                   0 -> return MRDAll
                   1 -> do x1 <- get
                           return (MRDAbove x1)
                   2 -> do x1 <- get
                           return (MRDBelow x1)
                   _ -> error "Corrupted binary data for MarkReadDirection"

instance () => Binary BgAction where
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
                BGMarkRead x1 x2 x3 x4 x5 -> do putWord8 5
                                                put x1
                                                put x2
                                                put x3
                                                put x4
                                                put x5
                BGRemoveTagFromTree x1 x2 x3 x4 -> do putWord8 6
                                                      put x1
                                                      put x2
                                                      put x3
                                                      put x4
                BGRemoveTagD x1 x2 -> do putWord8 7
                                         put x1
                                         put x2
                BGSetOnlyUpdatedSubscriptions x1 -> do putWord8 8
                                                       put x1
                BGSetFolderViewMode x1 x2 -> do putWord8 9
                                                put x1
                                                put x2
                BGSetSubscriptionViewMode x1 x2 -> do putWord8 10
                                                      put x1
                                                      put x2
                BGClearAllSubscriptions -> putWord8 11
                BGSaveFilterQuery x1 -> do putWord8 12
                                           put x1
                BGSetScrollMode x1 -> do putWord8 13
                                         put x1
                BGSetListViewMode x1 -> do putWord8 14
                                           put x1
                BGSetMarkReadMode x1 -> do putWord8 15
                                           put x1
                BGSetUltraCompact x1 -> do putWord8 16
                                           put x1
                BGDragAndDrop x1 x2 x3 x4 -> do putWord8 17
                                                put x1
                                                put x2
                                                put x3
                                                put x4
                BGSetExactUnreadCounts x1 -> do putWord8 18
                                                put x1
                BGSortAllFeedsAndFolders -> putWord8 19
                BGSortFolder x1 -> do putWord8 20
                                      put x1
                BGSortTags -> putWord8 21
                BGShareAction x1 -> do putWord8 22
                                       put x1
                BGSetCountry x1 -> do putWord8 23
                                      put x1
                BGWhatsNewClick x1 -> do putWord8 24
                                         put x1
                BGWhatsNewClose x1 -> do putWord8 25
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
                           x4 <- get
                           x5 <- get
                           return (BGMarkRead x1 x2 x3 x4 x5)
                   6 -> do x1 <- get
                           x2 <- get
                           x3 <- get
                           x4 <- get
                           return (BGRemoveTagFromTree x1 x2 x3 x4)
                   7 -> do x1 <- get
                           x2 <- get
                           return (BGRemoveTagD x1 x2)
                   8 -> do x1 <- get
                           return (BGSetOnlyUpdatedSubscriptions x1)
                   9 -> do x1 <- get
                           x2 <- get
                           return (BGSetFolderViewMode x1 x2)
                   10 -> do x1 <- get
                            x2 <- get
                            return (BGSetSubscriptionViewMode x1 x2)
                   11 -> return BGClearAllSubscriptions
                   12 -> do x1 <- get
                            return (BGSaveFilterQuery x1)
                   13 -> do x1 <- get
                            return (BGSetScrollMode x1)
                   14 -> do x1 <- get
                            return (BGSetListViewMode x1)
                   15 -> do x1 <- get
                            return (BGSetMarkReadMode x1)
                   16 -> do x1 <- get
                            return (BGSetUltraCompact x1)
                   17 -> do x1 <- get
                            x2 <- get
                            x3 <- get
                            x4 <- get
                            return (BGDragAndDrop x1 x2 x3 x4)
                   18 -> do x1 <- get
                            return (BGSetExactUnreadCounts x1)
                   19 -> return BGSortAllFeedsAndFolders
                   20 -> do x1 <- get
                            return (BGSortFolder x1)
                   21 -> return BGSortTags
                   22 -> do x1 <- get
                            return (BGShareAction x1)
                   23 -> do x1 <- get
                            return (BGSetCountry x1)
                   24 -> do x1 <- get
                            return (BGWhatsNewClick x1)
                   25 -> do x1 <- get
                            return (BGWhatsNewClose x1)
                   _ -> error "Corrupted binary data for BgAction"

instance () => Binary FeedsOrDiscovery where
        put x
          = case x of
                FODFeeds x1 -> do putWord8 0
                                  put x1
                FODFeedsApi x1 x2 -> do putWord8 1
                                        put x1
                                        put x2
                FODDiscovery x1 -> do putWord8 2
                                      put x1
        get
          = do i <- getWord8
               case i of
                   0 -> do x1 <- get
                           return (FODFeeds x1)
                   1 -> do x1 <- get
                           x2 <- get
                           return (FODFeedsApi x1 x2)
                   2 -> do x1 <- get
                           return (FODDiscovery x1)
                   _ -> error "Corrupted binary data for FeedsOrDiscovery"

instance () => Binary FilterResults where
        put (FilterResults x1 x2 x3 x4 x5 x6 x7)
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
               return (FilterResults x1 x2 x3 x4 x5 x6 x7)

instance () => Binary EmailAddress where
        put (EmailAddress x1 x2 x3)
          = do put x1
               put x2
               put x3
        get
          = do x1 <- get
               x2 <- get
               x3 <- get
               return (EmailAddress x1 x2 x3)

instance () => Binary FullText where
        put x
          = case x of
                FTError x1 -> do putWord8 0
                                 put x1
                FTTextV0 x1 -> do putWord8 1
                                  put x1
                FTTitleAndText x1 x2 -> do putWord8 2
                                           put x1
                                           put x2
        get
          = do i <- getWord8
               case i of
                   0 -> do x1 <- get
                           return (FTError x1)
                   1 -> do x1 <- get
                           return (FTTextV0 x1)
                   2 -> do x1 <- get
                           x2 <- get
                           return (FTTitleAndText x1 x2)
                   _ -> error "Corrupted binary data for FullText"

instance () => Binary FullTextCache where
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

instance () => Binary OkErrorRedirect where
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

instance () => Binary PageIconSize where
        put x
          = case x of
                PISAny -> putWord8 0
                PIS x1 x2 -> do putWord8 1
                                put x1
                                put x2
        get
          = do i <- getWord8
               case i of
                   0 -> return PISAny
                   1 -> do x1 <- get
                           x2 <- get
                           return (PIS x1 x2)
                   _ -> error "Corrupted binary data for PageIconSize"

instance () => Binary PageInfoTitleSource where
        put x
          = case x of
                PITSTag -> putWord8 0
                PITSItempropName -> putWord8 1
                PITSTwitter -> putWord8 2
                PITSOpenGraph -> putWord8 3
        get
          = do i <- getWord8
               case i of
                   0 -> return PITSTag
                   1 -> return PITSItempropName
                   2 -> return PITSTwitter
                   3 -> return PITSOpenGraph
                   _ -> error "Corrupted binary data for PageInfoTitleSource"

instance () => Binary PageInfoDescriptionSource where
        put x
          = case x of
                PIDSItemprop -> putWord8 0
                PIDSName -> putWord8 1
                PIDSTwitter -> putWord8 2
                PIDSOpenGraph -> putWord8 3
        get
          = do i <- getWord8
               case i of
                   0 -> return PIDSItemprop
                   1 -> return PIDSName
                   2 -> return PIDSTwitter
                   3 -> return PIDSOpenGraph
                   _ -> error "Corrupted binary data for PageInfoDescriptionSource"

instance () => Binary PageInfoImageSource where
        put x
          = case x of
                PIISLinkRelImageSrc -> putWord8 0
                PIISItemprop -> putWord8 1
                PIISTwitter -> putWord8 2
                PIISOpenGraph -> putWord8 3
                PIISUserPic -> putWord8 4
        get
          = do i <- getWord8
               case i of
                   0 -> return PIISLinkRelImageSrc
                   1 -> return PIISItemprop
                   2 -> return PIISTwitter
                   3 -> return PIISOpenGraph
                   4 -> return PIISUserPic
                   _ -> error "Corrupted binary data for PageInfoImageSource"

instance () => Binary PageInfoIconSource where
        put x
          = case x of
                PIICSIcon -> putWord8 0
                PIICSShortcutIcon -> putWord8 1
                PIICSShortcut -> putWord8 2
                PIICSAppleTouchIcon -> putWord8 3
                PIICSAppleTouchIconPrecomposed -> putWord8 4
                PIICSIconMask -> putWord8 5
        get
          = do i <- getWord8
               case i of
                   0 -> return PIICSIcon
                   1 -> return PIICSShortcutIcon
                   2 -> return PIICSShortcut
                   3 -> return PIICSAppleTouchIcon
                   4 -> return PIICSAppleTouchIconPrecomposed
                   5 -> return PIICSIconMask
                   _ -> error "Corrupted binary data for PageInfoIconSource"

instance () => Binary PageInfo where
        put
          (PageInfo x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16
             x17 x18 x19 x20 x21)
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
               put x19
               put x20
               put x21
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
               x19 <- get
               x20 <- get
               x21 <- get
               return
                 (PageInfo x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16
                    x17
                    x18
                    x19
                    x20
                    x21)

instance () => Binary Favicon where
        put (Favicon x1 x2 x3 x4 x5 x6 x7 x8 x9)
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
               return (Favicon x1 x2 x3 x4 x5 x6 x7 x8 x9)

instance () => Binary LinkInfo where
        put (LinkInfo x1 x2 x3 x4 x5)
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
               return (LinkInfo x1 x2 x3 x4 x5)

instance () => Binary HotLink where
        put (HotLink x1 x2 x3 x4 x5 x6)
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
               return (HotLink x1 x2 x3 x4 x5 x6)

instance () => Binary HotLinkState where
        put (HotLinkState x1 x2 x3 x4)
          = do put x1
               put x2
               put x3
               put x4
        get
          = do x1 <- get
               x2 <- get
               x3 <- get
               x4 <- get
               return (HotLinkState x1 x2 x3 x4)

instance () => Binary HotLinks where
        put
          (HotLinks x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16
             x17 x18 x19)
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
               put x19
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
               x19 <- get
               return
                 (HotLinks x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16
                    x17
                    x18
                    x19)

instance () => Binary FeedbackEmail where
        put (FeedbackEmail x1 x2 x3 x4 x5 x6)
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
               return (FeedbackEmail x1 x2 x3 x4 x5 x6)

instance () => Binary FeedbackUserInfo where
        put
          (FeedbackUserInfo x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14
             x15 x16 x17 x18 x19)
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
               put x19
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
               x19 <- get
               return
                 (FeedbackUserInfo x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14
                    x15
                    x16
                    x17
                    x18
                    x19)

instance () => Binary FeedbackUserInfosList where
        put (FeedbackUserInfoList x1 x2 x3 x4 x5 x6)
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
               return (FeedbackUserInfoList x1 x2 x3 x4 x5 x6)

instance () => Binary FtsReceiptTaxSystem where
        put x
          = case x of
                FRTSObschaya -> putWord8 0
                FRTSUsnDohod -> putWord8 1
                FRTSUsnDohodMinusRashod -> putWord8 2
                FRTSEnvd -> putWord8 3
                FRTSEshn -> putWord8 4
                FRTSPatent -> putWord8 5
        get
          = do i <- getWord8
               case i of
                   0 -> return FRTSObschaya
                   1 -> return FRTSUsnDohod
                   2 -> return FRTSUsnDohodMinusRashod
                   3 -> return FRTSEnvd
                   4 -> return FRTSEshn
                   5 -> return FRTSPatent
                   _ -> error "Corrupted binary data for FtsReceiptTaxSystem"

instance () => Binary FtsReceiptOperationType where
        put x
          = case x of
                FROTPrihod -> putWord8 0
                FROTVozvratPrihoda -> putWord8 1
                FROTRashod -> putWord8 2
                FROTVozvratRashoda -> putWord8 3
        get
          = do i <- getWord8
               case i of
                   0 -> return FROTPrihod
                   1 -> return FROTVozvratPrihoda
                   2 -> return FROTRashod
                   3 -> return FROTVozvratRashoda
                   _ -> error "Corrupted binary data for FtsReceiptOperationType"

instance () => Binary FtsReceiptVatType where
        put x
          = case x of
                FRVT18 -> putWord8 0
                FRVT10 -> putWord8 1
                FRVT118 -> putWord8 2
                FRVT110 -> putWord8 3
                FRVT0 -> putWord8 4
                FRVTNone -> putWord8 5
        get
          = do i <- getWord8
               case i of
                   0 -> return FRVT18
                   1 -> return FRVT10
                   2 -> return FRVT118
                   3 -> return FRVT110
                   4 -> return FRVT0
                   5 -> return FRVTNone
                   _ -> error "Corrupted binary data for FtsReceiptVatType"

instance () => Binary FtsReceiptItem where
        put (FtsReceiptItem x1 x2 x3 x4 x5 x6 x7 x8 x9)
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
               return (FtsReceiptItem x1 x2 x3 x4 x5 x6 x7 x8 x9)

instance () => Binary FtsReceipt where
        put
          (FtsReceipt x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16
             x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31)
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
               put x19
               put x20
               put x21
               put x22
               put x23
               put x24
               put x25
               put x26
               put x27
               put x28
               put x29
               put x30
               put x31
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
               x19 <- get
               x20 <- get
               x21 <- get
               x22 <- get
               x23 <- get
               x24 <- get
               x25 <- get
               x26 <- get
               x27 <- get
               x28 <- get
               x29 <- get
               x30 <- get
               x31 <- get
               return
                 (FtsReceipt x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16
                    x17
                    x18
                    x19
                    x20
                    x21
                    x22
                    x23
                    x24
                    x25
                    x26
                    x27
                    x28
                    x29
                    x30
                    x31)

instance () => Binary PrintableFtsReceipt where
        put (PrintableFtsReceipt x1 x2 x3 x4 x5 x6 x7)
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
               return (PrintableFtsReceipt x1 x2 x3 x4 x5 x6 x7)

instance () => Binary OfdReceipt where
        put (OfdReceipt x1 x2 x3 x4 x5 x6 x7 x8)
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
               return (OfdReceipt x1 x2 x3 x4 x5 x6 x7 x8)

instance () => Binary ParserEnvironment where
        put (ParserEnvironment x1 x2)
          = do put x1
               put x2
        get
          = do x1 <- get
               x2 <- get
               return (ParserEnvironment x1 x2)
