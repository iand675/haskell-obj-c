{-# LANGUAGE DataKinds #-}
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
  , delegate
  , setDelegate
  , storefront
  , transactionObservers
  , transactions
  , addPaymentSelector
  , addTransactionObserverSelector
  , canMakePaymentsSelector
  , cancelDownloadsSelector
  , defaultQueueSelector
  , delegateSelector
  , finishTransactionSelector
  , pauseDownloadsSelector
  , presentCodeRedemptionSheetSelector
  , removeTransactionObserverSelector
  , restoreCompletedTransactionsSelector
  , restoreCompletedTransactionsWithApplicationUsernameSelector
  , resumeDownloadsSelector
  , setDelegateSelector
  , showPriceConsentIfNeededSelector
  , startDownloadsSelector
  , storefrontSelector
  , transactionObserversSelector
  , transactionsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.StoreKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ defaultQueue@
defaultQueue :: IO (Id SKPaymentQueue)
defaultQueue  =
  do
    cls' <- getRequiredClass "SKPaymentQueue"
    sendClassMessage cls' defaultQueueSelector

-- | @+ canMakePayments@
canMakePayments :: IO Bool
canMakePayments  =
  do
    cls' <- getRequiredClass "SKPaymentQueue"
    sendClassMessage cls' canMakePaymentsSelector

-- | @- addPayment:@
addPayment :: (IsSKPaymentQueue skPaymentQueue, IsSKPayment payment) => skPaymentQueue -> payment -> IO ()
addPayment skPaymentQueue payment =
  sendMessage skPaymentQueue addPaymentSelector (toSKPayment payment)

-- | @- restoreCompletedTransactions@
restoreCompletedTransactions :: IsSKPaymentQueue skPaymentQueue => skPaymentQueue -> IO ()
restoreCompletedTransactions skPaymentQueue =
  sendMessage skPaymentQueue restoreCompletedTransactionsSelector

-- | @- restoreCompletedTransactionsWithApplicationUsername:@
restoreCompletedTransactionsWithApplicationUsername :: (IsSKPaymentQueue skPaymentQueue, IsNSString username) => skPaymentQueue -> username -> IO ()
restoreCompletedTransactionsWithApplicationUsername skPaymentQueue username =
  sendMessage skPaymentQueue restoreCompletedTransactionsWithApplicationUsernameSelector (toNSString username)

-- | @- finishTransaction:@
finishTransaction :: (IsSKPaymentQueue skPaymentQueue, IsSKPaymentTransaction transaction) => skPaymentQueue -> transaction -> IO ()
finishTransaction skPaymentQueue transaction =
  sendMessage skPaymentQueue finishTransactionSelector (toSKPaymentTransaction transaction)

-- | @- startDownloads:@
startDownloads :: (IsSKPaymentQueue skPaymentQueue, IsNSArray downloads) => skPaymentQueue -> downloads -> IO ()
startDownloads skPaymentQueue downloads =
  sendMessage skPaymentQueue startDownloadsSelector (toNSArray downloads)

-- | @- pauseDownloads:@
pauseDownloads :: (IsSKPaymentQueue skPaymentQueue, IsNSArray downloads) => skPaymentQueue -> downloads -> IO ()
pauseDownloads skPaymentQueue downloads =
  sendMessage skPaymentQueue pauseDownloadsSelector (toNSArray downloads)

-- | @- resumeDownloads:@
resumeDownloads :: (IsSKPaymentQueue skPaymentQueue, IsNSArray downloads) => skPaymentQueue -> downloads -> IO ()
resumeDownloads skPaymentQueue downloads =
  sendMessage skPaymentQueue resumeDownloadsSelector (toNSArray downloads)

-- | @- cancelDownloads:@
cancelDownloads :: (IsSKPaymentQueue skPaymentQueue, IsNSArray downloads) => skPaymentQueue -> downloads -> IO ()
cancelDownloads skPaymentQueue downloads =
  sendMessage skPaymentQueue cancelDownloadsSelector (toNSArray downloads)

-- | @- addTransactionObserver:@
addTransactionObserver :: IsSKPaymentQueue skPaymentQueue => skPaymentQueue -> RawId -> IO ()
addTransactionObserver skPaymentQueue observer =
  sendMessage skPaymentQueue addTransactionObserverSelector observer

-- | @- removeTransactionObserver:@
removeTransactionObserver :: IsSKPaymentQueue skPaymentQueue => skPaymentQueue -> RawId -> IO ()
removeTransactionObserver skPaymentQueue observer =
  sendMessage skPaymentQueue removeTransactionObserverSelector observer

-- | @- showPriceConsentIfNeeded@
showPriceConsentIfNeeded :: IsSKPaymentQueue skPaymentQueue => skPaymentQueue -> IO ()
showPriceConsentIfNeeded skPaymentQueue =
  sendMessage skPaymentQueue showPriceConsentIfNeededSelector

-- | @- presentCodeRedemptionSheet@
presentCodeRedemptionSheet :: IsSKPaymentQueue skPaymentQueue => skPaymentQueue -> IO ()
presentCodeRedemptionSheet skPaymentQueue =
  sendMessage skPaymentQueue presentCodeRedemptionSheetSelector

-- | @- delegate@
delegate :: IsSKPaymentQueue skPaymentQueue => skPaymentQueue -> IO RawId
delegate skPaymentQueue =
  sendMessage skPaymentQueue delegateSelector

-- | @- setDelegate:@
setDelegate :: IsSKPaymentQueue skPaymentQueue => skPaymentQueue -> RawId -> IO ()
setDelegate skPaymentQueue value =
  sendMessage skPaymentQueue setDelegateSelector value

-- | @- storefront@
storefront :: IsSKPaymentQueue skPaymentQueue => skPaymentQueue -> IO (Id SKStorefront)
storefront skPaymentQueue =
  sendMessage skPaymentQueue storefrontSelector

-- | @- transactionObservers@
transactionObservers :: IsSKPaymentQueue skPaymentQueue => skPaymentQueue -> IO (Id NSArray)
transactionObservers skPaymentQueue =
  sendMessage skPaymentQueue transactionObserversSelector

-- | @- transactions@
transactions :: IsSKPaymentQueue skPaymentQueue => skPaymentQueue -> IO (Id NSArray)
transactions skPaymentQueue =
  sendMessage skPaymentQueue transactionsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @defaultQueue@
defaultQueueSelector :: Selector '[] (Id SKPaymentQueue)
defaultQueueSelector = mkSelector "defaultQueue"

-- | @Selector@ for @canMakePayments@
canMakePaymentsSelector :: Selector '[] Bool
canMakePaymentsSelector = mkSelector "canMakePayments"

-- | @Selector@ for @addPayment:@
addPaymentSelector :: Selector '[Id SKPayment] ()
addPaymentSelector = mkSelector "addPayment:"

-- | @Selector@ for @restoreCompletedTransactions@
restoreCompletedTransactionsSelector :: Selector '[] ()
restoreCompletedTransactionsSelector = mkSelector "restoreCompletedTransactions"

-- | @Selector@ for @restoreCompletedTransactionsWithApplicationUsername:@
restoreCompletedTransactionsWithApplicationUsernameSelector :: Selector '[Id NSString] ()
restoreCompletedTransactionsWithApplicationUsernameSelector = mkSelector "restoreCompletedTransactionsWithApplicationUsername:"

-- | @Selector@ for @finishTransaction:@
finishTransactionSelector :: Selector '[Id SKPaymentTransaction] ()
finishTransactionSelector = mkSelector "finishTransaction:"

-- | @Selector@ for @startDownloads:@
startDownloadsSelector :: Selector '[Id NSArray] ()
startDownloadsSelector = mkSelector "startDownloads:"

-- | @Selector@ for @pauseDownloads:@
pauseDownloadsSelector :: Selector '[Id NSArray] ()
pauseDownloadsSelector = mkSelector "pauseDownloads:"

-- | @Selector@ for @resumeDownloads:@
resumeDownloadsSelector :: Selector '[Id NSArray] ()
resumeDownloadsSelector = mkSelector "resumeDownloads:"

-- | @Selector@ for @cancelDownloads:@
cancelDownloadsSelector :: Selector '[Id NSArray] ()
cancelDownloadsSelector = mkSelector "cancelDownloads:"

-- | @Selector@ for @addTransactionObserver:@
addTransactionObserverSelector :: Selector '[RawId] ()
addTransactionObserverSelector = mkSelector "addTransactionObserver:"

-- | @Selector@ for @removeTransactionObserver:@
removeTransactionObserverSelector :: Selector '[RawId] ()
removeTransactionObserverSelector = mkSelector "removeTransactionObserver:"

-- | @Selector@ for @showPriceConsentIfNeeded@
showPriceConsentIfNeededSelector :: Selector '[] ()
showPriceConsentIfNeededSelector = mkSelector "showPriceConsentIfNeeded"

-- | @Selector@ for @presentCodeRedemptionSheet@
presentCodeRedemptionSheetSelector :: Selector '[] ()
presentCodeRedemptionSheetSelector = mkSelector "presentCodeRedemptionSheet"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @storefront@
storefrontSelector :: Selector '[] (Id SKStorefront)
storefrontSelector = mkSelector "storefront"

-- | @Selector@ for @transactionObservers@
transactionObserversSelector :: Selector '[] (Id NSArray)
transactionObserversSelector = mkSelector "transactionObservers"

-- | @Selector@ for @transactions@
transactionsSelector :: Selector '[] (Id NSArray)
transactionsSelector = mkSelector "transactions"

