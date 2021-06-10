{-# LANGUAGE ViewPatterns, RecordWildCards, OverloadedStrings, RankNTypes #-}
module Payments
    ( getPaidTill
    , buyPage
    , invoiceLink
    , productInfo
    , clearPaidTill
    , checkOrder
    , orderNotification
    , orderNotificationNew
    , FastSpringOrder(..), downloadFastSpringOrder, tryDownloadFastSpringOrder
    , looksLikeOrderId
    ) where

import Control.Monad
import Data.List
import Data.Ord
import Data.Function
import qualified Data.Text as T
import Generated.DataTypes
import Lib.UrTime
import Lib.Log
import Lib.Regex
import Riak
import Generated.RiakIO
import Control.Concurrent
import Payments.FastSpring
import Payments.Eofd

getPaidTill :: Key User -> IO PaidTill
getPaidTill user = modifyUser' user $ \ u -> do
    -- не cached, т.к. мог пройти платеж с другой ноды
    -- и не read->modify т.к. riak может выдать на read пустышку, а на
    -- modify уже нормальные данные (после read repair)
    t <- getUrTime
    let uvm = uViewMode u
        pt' = case uvmPaidTill uvm of
            PTUnknown ->
                PTFreeTrial $ t `plusUrTime` (30*day-1)
            PTFreeTrial till
                | till <= t -> PTFreeTrialFinished till
            PTPaid till
                | till <= t -> PTPaidFinished till
            pt -> pt
    return (u { uViewMode = uvm { uvmPaidTill = pt' } }, pt')

clearPayments user = modifyUser'_ user $ \ u ->
    return $ u { uPayments = [] }
clearPayment user oid = modifyUser'_ user $ \ u ->
    return $ u { uPayments = filter ((/= oid) . pOrderId) $ uPayments u }

setPaidTill pt user = do
    modifyUser'_ user $ \ u ->
        return $ u { uViewMode = (uViewMode u) { uvmPaidTill = pt } }
    return pt
addPaidTill days user = do
    modifyUser' user $ \ u -> do
        let pt' = add (uvmPaidTill $ uViewMode u)
        return (u { uViewMode = (uViewMode u) { uvmPaidTill = pt' } }, pt')
    where add (PTPaid t) = PTPaid $ t `plusUrTime` (day*days)
          add x = x
clearPaidTill = setPaidTill PTUnknown

fixPaidTill user = withLogger $ \ l -> modifyUser'_ user $ \ u -> do
    let go t [] = t
        go Nothing (o : os) =
            go (Just $ add o $ pOrderTime o) os
        go (Just t) (o : os) =
            go (Just $ add o $ max (pOrderTime o) t) os
        add o = foldr (.) id $ map (addTime $ pOrderId o) $ T.words $ pOrderType o
        addTime oid p
            | Just (a,_,_) <- productInfo oid p = a
            | otherwise = error $ "invalid product? " ++ show p
        payments =
            nubBy ((==) `on` pOrderId) $
            sortBy (comparing $ Down . pOrderTime) $ uPayments u
        till = go Nothing $ reverse payments
    u <- if payments /= uPayments u then do
        logLS l $ show (user, "superfluous/out of order? payments")
        mapM_ (logLS l . show) payments
        logLS l "---"
        mapM_ (logLS l . show) $ uPayments u
        logLS l "---"
        return $ u { uPayments = payments }
    else
        return u
    case till of
        Just t -> do
            -- print (user, t)
            let uvm = uViewMode u
                till0 = case uvmPaidTill uvm of
                    PTUnknown -> Nothing
                    o -> Just $ ptTill o
            if Just t > till0 then do
                logLS l $ show (user, t, uvmPaidTill uvm)
                return $ u { uViewMode = uvm { uvmPaidTill = PTPaid t } }
            else do
                -- putStrLn "Already OK"
                return u
        Nothing -> do
            -- putStrLn "No payments"
            return u

set100YearsPaid user = do
    t <- getUrTime
    setPaidTill (PTPaid $ t `plusUrTime` (100*year)) user
initFreeTrial' days user = do
    t <- getUrTime
    setPaidTill (PTFreeTrial (t `plusUrTime` (days*day-1))) user
initFreeTrial = initFreeTrial' 30

checkOrder oid = do
    fso <- downloadFastSpringOrder oid
    when (fsoTest fso) $ fail $ T.unpack oid <> " is a test order."
    t <- getUrTime
    let checkOnly = diffUrTime t (fsoTime fso) > day
    r <- processPayment checkOnly (fsoReferrer fso) oid (fsoProducts fso)
    when (not checkOnly) $ void $ forkIO $ createOfdReceipt False False oid
    --  ^ не задерживаемся на отправку чека
    return r

orderNotification :: T.Text -> IO (Key User, Payment)
orderNotification qs = do
    (userId, orderId, pid) <- parseOrderNotification qs
    r <- processPayment False userId orderId pid
    createOfdReceipt False False orderId
    return r

orderNotificationNew :: T.Text -> T.Text -> IO ()
orderNotificationNew payload sign = do
    orders <- parseOrderNotificationNew payload sign
    forM_ orders $ \ FastSpringOrder {..} -> when (not fsoTest) $ do
        processPayment False fsoReferrer fsoId fsoProducts
        createOfdReceipt False False fsoId

looksLikeOrderId = regexTest "^BAZ[0-9]{6}-[0-9]{4}-[0-9]{5}$"
