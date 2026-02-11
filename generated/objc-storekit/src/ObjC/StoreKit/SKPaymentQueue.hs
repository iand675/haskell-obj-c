{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SKPaymentQueue@.
module ObjC.StoreKit.SKPaymentQueue
  ( SKPaymentQueue
  , IsSKPaymentQueue(..)
  , defaultQueue
  , canMakePayments
  , addPayment
  , restoreCompletedTransactions
  , restoreCompletedTransactionsWithApplicationUsername
  , finishTransaction
  , startDownloads
  , pauseDownloads
  , resumeDownloads
  , cancelDownloads
  , addTransactionObserver
  , removeTransactionObserver
  , showPriceConsentIfNeeded
  , presentCodeRedemptionSheet
  , storefront
  , transactions
  , defaultQueueSelector
  , canMakePaymentsSelector
  , addPaymentSelector
  , restoreCompletedTransactionsSelector
  , restoreCompletedTransactionsWithApplicationUsernameSelector
  , finishTransactionSelector
  , startDownloadsSelector
  , pauseDownloadsSelector
  , resumeDownloadsSelector
  , cancelDownloadsSelector
  , addTransactionObserverSelector
  , removeTransactionObserverSelector
  , showPriceConsentIfNeededSelector
  , presentCodeRedemptionSheetSelector
  , storefrontSelector
  , transactionsSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.StoreKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ defaultQueue@
defaultQueue :: IO (Id SKPaymentQueue)
defaultQueue  =
  do
    cls' <- getRequiredClass "SKPaymentQueue"
    sendClassMsg cls' (mkSelector "defaultQueue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ canMakePayments@
canMakePayments :: IO Bool
canMakePayments  =
  do
    cls' <- getRequiredClass "SKPaymentQueue"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "canMakePayments") retCULong []

-- | @- addPayment:@
addPayment :: (IsSKPaymentQueue skPaymentQueue, IsSKPayment payment) => skPaymentQueue -> payment -> IO ()
addPayment skPaymentQueue  payment =
withObjCPtr payment $ \raw_payment ->
    sendMsg skPaymentQueue (mkSelector "addPayment:") retVoid [argPtr (castPtr raw_payment :: Ptr ())]

-- | @- restoreCompletedTransactions@
restoreCompletedTransactions :: IsSKPaymentQueue skPaymentQueue => skPaymentQueue -> IO ()
restoreCompletedTransactions skPaymentQueue  =
  sendMsg skPaymentQueue (mkSelector "restoreCompletedTransactions") retVoid []

-- | @- restoreCompletedTransactionsWithApplicationUsername:@
restoreCompletedTransactionsWithApplicationUsername :: (IsSKPaymentQueue skPaymentQueue, IsNSString username) => skPaymentQueue -> username -> IO ()
restoreCompletedTransactionsWithApplicationUsername skPaymentQueue  username =
withObjCPtr username $ \raw_username ->
    sendMsg skPaymentQueue (mkSelector "restoreCompletedTransactionsWithApplicationUsername:") retVoid [argPtr (castPtr raw_username :: Ptr ())]

-- | @- finishTransaction:@
finishTransaction :: (IsSKPaymentQueue skPaymentQueue, IsSKPaymentTransaction transaction) => skPaymentQueue -> transaction -> IO ()
finishTransaction skPaymentQueue  transaction =
withObjCPtr transaction $ \raw_transaction ->
    sendMsg skPaymentQueue (mkSelector "finishTransaction:") retVoid [argPtr (castPtr raw_transaction :: Ptr ())]

-- | @- startDownloads:@
startDownloads :: (IsSKPaymentQueue skPaymentQueue, IsNSArray downloads) => skPaymentQueue -> downloads -> IO ()
startDownloads skPaymentQueue  downloads =
withObjCPtr downloads $ \raw_downloads ->
    sendMsg skPaymentQueue (mkSelector "startDownloads:") retVoid [argPtr (castPtr raw_downloads :: Ptr ())]

-- | @- pauseDownloads:@
pauseDownloads :: (IsSKPaymentQueue skPaymentQueue, IsNSArray downloads) => skPaymentQueue -> downloads -> IO ()
pauseDownloads skPaymentQueue  downloads =
withObjCPtr downloads $ \raw_downloads ->
    sendMsg skPaymentQueue (mkSelector "pauseDownloads:") retVoid [argPtr (castPtr raw_downloads :: Ptr ())]

-- | @- resumeDownloads:@
resumeDownloads :: (IsSKPaymentQueue skPaymentQueue, IsNSArray downloads) => skPaymentQueue -> downloads -> IO ()
resumeDownloads skPaymentQueue  downloads =
withObjCPtr downloads $ \raw_downloads ->
    sendMsg skPaymentQueue (mkSelector "resumeDownloads:") retVoid [argPtr (castPtr raw_downloads :: Ptr ())]

-- | @- cancelDownloads:@
cancelDownloads :: (IsSKPaymentQueue skPaymentQueue, IsNSArray downloads) => skPaymentQueue -> downloads -> IO ()
cancelDownloads skPaymentQueue  downloads =
withObjCPtr downloads $ \raw_downloads ->
    sendMsg skPaymentQueue (mkSelector "cancelDownloads:") retVoid [argPtr (castPtr raw_downloads :: Ptr ())]

-- | @- addTransactionObserver:@
addTransactionObserver :: IsSKPaymentQueue skPaymentQueue => skPaymentQueue -> RawId -> IO ()
addTransactionObserver skPaymentQueue  observer =
  sendMsg skPaymentQueue (mkSelector "addTransactionObserver:") retVoid [argPtr (castPtr (unRawId observer) :: Ptr ())]

-- | @- removeTransactionObserver:@
removeTransactionObserver :: IsSKPaymentQueue skPaymentQueue => skPaymentQueue -> RawId -> IO ()
removeTransactionObserver skPaymentQueue  observer =
  sendMsg skPaymentQueue (mkSelector "removeTransactionObserver:") retVoid [argPtr (castPtr (unRawId observer) :: Ptr ())]

-- | @- showPriceConsentIfNeeded@
showPriceConsentIfNeeded :: IsSKPaymentQueue skPaymentQueue => skPaymentQueue -> IO ()
showPriceConsentIfNeeded skPaymentQueue  =
  sendMsg skPaymentQueue (mkSelector "showPriceConsentIfNeeded") retVoid []

-- | @- presentCodeRedemptionSheet@
presentCodeRedemptionSheet :: IsSKPaymentQueue skPaymentQueue => skPaymentQueue -> IO ()
presentCodeRedemptionSheet skPaymentQueue  =
  sendMsg skPaymentQueue (mkSelector "presentCodeRedemptionSheet") retVoid []

-- | @- storefront@
storefront :: IsSKPaymentQueue skPaymentQueue => skPaymentQueue -> IO (Id SKStorefront)
storefront skPaymentQueue  =
  sendMsg skPaymentQueue (mkSelector "storefront") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- transactions@
transactions :: IsSKPaymentQueue skPaymentQueue => skPaymentQueue -> IO (Id NSArray)
transactions skPaymentQueue  =
  sendMsg skPaymentQueue (mkSelector "transactions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @defaultQueue@
defaultQueueSelector :: Selector
defaultQueueSelector = mkSelector "defaultQueue"

-- | @Selector@ for @canMakePayments@
canMakePaymentsSelector :: Selector
canMakePaymentsSelector = mkSelector "canMakePayments"

-- | @Selector@ for @addPayment:@
addPaymentSelector :: Selector
addPaymentSelector = mkSelector "addPayment:"

-- | @Selector@ for @restoreCompletedTransactions@
restoreCompletedTransactionsSelector :: Selector
restoreCompletedTransactionsSelector = mkSelector "restoreCompletedTransactions"

-- | @Selector@ for @restoreCompletedTransactionsWithApplicationUsername:@
restoreCompletedTransactionsWithApplicationUsernameSelector :: Selector
restoreCompletedTransactionsWithApplicationUsernameSelector = mkSelector "restoreCompletedTransactionsWithApplicationUsername:"

-- | @Selector@ for @finishTransaction:@
finishTransactionSelector :: Selector
finishTransactionSelector = mkSelector "finishTransaction:"

-- | @Selector@ for @startDownloads:@
startDownloadsSelector :: Selector
startDownloadsSelector = mkSelector "startDownloads:"

-- | @Selector@ for @pauseDownloads:@
pauseDownloadsSelector :: Selector
pauseDownloadsSelector = mkSelector "pauseDownloads:"

-- | @Selector@ for @resumeDownloads:@
resumeDownloadsSelector :: Selector
resumeDownloadsSelector = mkSelector "resumeDownloads:"

-- | @Selector@ for @cancelDownloads:@
cancelDownloadsSelector :: Selector
cancelDownloadsSelector = mkSelector "cancelDownloads:"

-- | @Selector@ for @addTransactionObserver:@
addTransactionObserverSelector :: Selector
addTransactionObserverSelector = mkSelector "addTransactionObserver:"

-- | @Selector@ for @removeTransactionObserver:@
removeTransactionObserverSelector :: Selector
removeTransactionObserverSelector = mkSelector "removeTransactionObserver:"

-- | @Selector@ for @showPriceConsentIfNeeded@
showPriceConsentIfNeededSelector :: Selector
showPriceConsentIfNeededSelector = mkSelector "showPriceConsentIfNeeded"

-- | @Selector@ for @presentCodeRedemptionSheet@
presentCodeRedemptionSheetSelector :: Selector
presentCodeRedemptionSheetSelector = mkSelector "presentCodeRedemptionSheet"

-- | @Selector@ for @storefront@
storefrontSelector :: Selector
storefrontSelector = mkSelector "storefront"

-- | @Selector@ for @transactions@
transactionsSelector :: Selector
transactionsSelector = mkSelector "transactions"

