
 
instance Binary JSComments where
        put x
          = case x of
                JSCSupported x1 -> do putWord8 0
                                      put x1
                JSCUnsupported x1 -> do putWord8 1
                                        put x1
        get
          = do !i <- getWord8
               case i of
                   0 -> do !x1 <- get
                           return (JSCSupported x1)
                   1 -> do !x1 <- get
                           return (JSCUnsupported x1)
                   _ -> error "Corrupted binary data for JSComments"

 
instance Binary SwitchUrl where
        put x
          = case x of
                NoSwitch -> putWord8 0
                Switch x1 x2 -> do putWord8 1
                                   put x1
                                   put x2
        get
          = do !i <- getWord8
               case i of
                   0 -> return NoSwitch
                   1 -> do !x1 <- get
                           !x2 <- get
                           return (Switch x1 x2)
                   _ -> error "Corrupted binary data for SwitchUrl"

 
instance Binary FeedMsg where
        put
          (FeedMsg x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17
             x18 x19)
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
          = do !x1 <- get
               !x2 <- get
               !x3 <- get
               !x4 <- get
               !x5 <- get
               !x6 <- get
               !x7 <- get
               !x8 <- get
               !x9 <- get
               !x10 <- get
               !x11 <- get
               !x12 <- get
               !x13 <- get
               !x14 <- get
               !x15 <- get
               !x16 <- get
               !x17 <- get
               !x18 <- get
               !x19 <- get
               return
                 (FeedMsg x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17
                    x18
                    x19)

 
instance Binary Media where
        put (Media x1 x2 x3 x4)
          = do put x1
               put x2
               put x3
               put x4
        get
          = do !x1 <- get
               !x2 <- get
               !x3 <- get
               !x4 <- get
               return (Media x1 x2 x3 x4)

 
instance Binary LinkKind where
        put x
          = case x of
                LKFeed -> putWord8 0
                LKApiFeed -> putWord8 1
                LKOrigLink -> putWord8 2
                LKCommentsHtml -> putWord8 3
                LKLink -> putWord8 4
                LKNext -> putWord8 5
                LKAuthor -> putWord8 6
                LKAuthorPic -> putWord8 7
                LKHub -> putWord8 8
                LKGROriginalId -> putWord8 9
                LKBaseUri -> putWord8 10
        get
          = do !i <- getWord8
               case i of
                   0 -> return LKFeed
                   1 -> return LKApiFeed
                   2 -> return LKOrigLink
                   3 -> return LKCommentsHtml
                   4 -> return LKLink
                   5 -> return LKNext
                   6 -> return LKAuthor
                   7 -> return LKAuthorPic
                   8 -> return LKHub
                   9 -> return LKGROriginalId
                   10 -> return LKBaseUri
                   _ -> error "Corrupted binary data for LinkKind"

 
instance Binary ParseResult where
        put x
          = case x of
                PRError x1 -> do putWord8 0
                                 put x1
                PRHtml x1 x2 x3 -> do putWord8 1
                                      put x1
                                      put x2
                                      put x3
                PRFeed x1 x2 x3 -> do putWord8 2
                                      put x1
                                      put x2
                                      put x3
                PRParsedHtml x1 x2 x3 x4 -> do putWord8 3
                                               put x1
                                               put x2
                                               put x3
                                               put x4
                PRRedirect x1 -> do putWord8 4
                                    put x1
        get
          = do !i <- getWord8
               case i of
                   0 -> do !x1 <- get
                           return (PRError x1)
                   1 -> do !x1 <- get
                           !x2 <- get
                           !x3 <- get
                           return (PRHtml x1 x2 x3)
                   2 -> do !x1 <- get
                           !x2 <- get
                           !x3 <- get
                           return (PRFeed x1 x2 x3)
                   3 -> do !x1 <- get
                           !x2 <- get
                           !x3 <- get
                           !x4 <- get
                           return (PRParsedHtml x1 x2 x3 x4)
                   4 -> do !x1 <- get
                           return (PRRedirect x1)
                   _ -> error "Corrupted binary data for ParseResult"

 
instance Binary ParseServerRequest where
        put (ParseServerRequest x1 x2 x3 x4 x5)
          = do put x1
               put x2
               put x3
               put x4
               put x5
        get
          = do !x1 <- get
               !x2 <- get
               !x3 <- get
               !x4 <- get
               !x5 <- get
               return (ParseServerRequest x1 x2 x3 x4 x5)

 
instance Binary ParseServerResult where
        put x
          = case x of
                PSRNotModified -> putWord8 0
                PSRSameHash -> putWord8 1
                PSRError x1 -> do putWord8 2
                                  put x1
                PSRRedirect x1 -> do putWord8 3
                                     put x1
                PSROK x1 x2 x3 -> do putWord8 4
                                     put x1
                                     put x2
                                     put x3
        get
          = do !i <- getWord8
               case i of
                   0 -> return PSRNotModified
                   1 -> return PSRSameHash
                   2 -> do !x1 <- get
                           return (PSRError x1)
                   3 -> do !x1 <- get
                           return (PSRRedirect x1)
                   4 -> do !x1 <- get
                           !x2 <- get
                           !x3 <- get
                           return (PSROK x1 x2 x3)
                   _ -> error "Corrupted binary data for ParseServerResult"

 
instance Binary FeedItem where
        put (FeedItem x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12)
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
        get
          = do !x1 <- get
               !x2 <- get
               !x3 <- get
               !x4 <- get
               !x5 <- get
               !x6 <- get
               !x7 <- get
               !x8 <- get
               !x9 <- get
               !x10 <- get
               !x11 <- get
               !x12 <- get
               return (FeedItem x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12)

 
instance Binary AddUrl where
        put (AddUrl x1 x2 x3 x4)
          = do put x1
               put x2
               put x3
               put x4
        get
          = do !x1 <- get
               !x2 <- get
               !x3 <- get
               !x4 <- get
               return (AddUrl x1 x2 x3 x4)

 
instance Binary PushOrder where
        put x
          = case x of
                PushBack -> putWord8 0
                PushFront -> putWord8 1
                Delay x1 -> do putWord8 2
                               put x1
        get
          = do !i <- getWord8
               case i of
                   0 -> return PushBack
                   1 -> return PushFront
                   2 -> do !x1 <- get
                           return (Delay x1)
                   _ -> error "Corrupted binary data for PushOrder"

 
instance Binary ProcessUrlRequest where
        put (ProcessUrlRequest x1 x2 x3 x4 x5)
          = do put x1
               put x2
               put x3
               put x4
               put x5
        get
          = do !x1 <- get
               !x2 <- get
               !x3 <- get
               !x4 <- get
               !x5 <- get
               return (ProcessUrlRequest x1 x2 x3 x4 x5)

 
instance Binary ProcessUrlResult where
        put (ProcessUrlResult x1 x2 x3 x4)
          = do put x1
               put x2
               put x3
               put x4
        get
          = do !x1 <- get
               !x2 <- get
               !x3 <- get
               !x4 <- get
               return (ProcessUrlResult x1 x2 x3 x4)
