{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SKPaymentTransaction@.
module ObjC.StoreKit.SKPaymentTransaction
  ( SKPaymentTransaction
  , IsSKPaymentTransaction(..)
  , error_
  , originalTransaction
  , payment
  , downloads
  , transactionDate
  , transactionIdentifier
  , transactionReceipt
  , transactionState
  , downloadsSelector
  , errorSelector
  , originalTransactionSelector
  , paymentSelector
  , transactionDateSelector
  , transactionIdentifierSelector
  , transactionReceiptSelector
  , transactionStateSelector

  -- * Enum types
  , SKPaymentTransactionState(SKPaymentTransactionState)
  , pattern SKPaymentTransactionStatePurchasing
  , pattern SKPaymentTransactionStatePurchased
  , pattern SKPaymentTransactionStateFailed
  , pattern SKPaymentTransactionStateRestored
  , pattern SKPaymentTransactionStateDeferred

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.StoreKit.Internal.Classes
import ObjC.StoreKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- error@
error_ :: IsSKPaymentTransaction skPaymentTransaction => skPaymentTransaction -> IO (Id NSError)
error_ skPaymentTransaction =
  sendMessage skPaymentTransaction errorSelector

-- | @- originalTransaction@
originalTransaction :: IsSKPaymentTransaction skPaymentTransaction => skPaymentTransaction -> IO (Id SKPaymentTransaction)
originalTransaction skPaymentTransaction =
  sendMessage skPaymentTransaction originalTransactionSelector

-- | @- payment@
payment :: IsSKPaymentTransaction skPaymentTransaction => skPaymentTransaction -> IO (Id SKPayment)
payment skPaymentTransaction =
  sendMessage skPaymentTransaction paymentSelector

-- | @- downloads@
downloads :: IsSKPaymentTransaction skPaymentTransaction => skPaymentTransaction -> IO (Id NSArray)
downloads skPaymentTransaction =
  sendMessage skPaymentTransaction downloadsSelector

-- | @- transactionDate@
transactionDate :: IsSKPaymentTransaction skPaymentTransaction => skPaymentTransaction -> IO (Id NSDate)
transactionDate skPaymentTransaction =
  sendMessage skPaymentTransaction transactionDateSelector

-- | @- transactionIdentifier@
transactionIdentifier :: IsSKPaymentTransaction skPaymentTransaction => skPaymentTransaction -> IO (Id NSString)
transactionIdentifier skPaymentTransaction =
  sendMessage skPaymentTransaction transactionIdentifierSelector

-- | @- transactionReceipt@
transactionReceipt :: IsSKPaymentTransaction skPaymentTransaction => skPaymentTransaction -> IO (Id NSData)
transactionReceipt skPaymentTransaction =
  sendMessage skPaymentTransaction transactionReceiptSelector

-- | @- transactionState@
transactionState :: IsSKPaymentTransaction skPaymentTransaction => skPaymentTransaction -> IO SKPaymentTransactionState
transactionState skPaymentTransaction =
  sendMessage skPaymentTransaction transactionStateSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @error@
errorSelector :: Selector '[] (Id NSError)
errorSelector = mkSelector "error"

-- | @Selector@ for @originalTransaction@
originalTransactionSelector :: Selector '[] (Id SKPaymentTransaction)
originalTransactionSelector = mkSelector "originalTransaction"

-- | @Selector@ for @payment@
paymentSelector :: Selector '[] (Id SKPayment)
paymentSelector = mkSelector "payment"

-- | @Selector@ for @downloads@
downloadsSelector :: Selector '[] (Id NSArray)
downloadsSelector = mkSelector "downloads"

-- | @Selector@ for @transactionDate@
transactionDateSelector :: Selector '[] (Id NSDate)
transactionDateSelector = mkSelector "transactionDate"

-- | @Selector@ for @transactionIdentifier@
transactionIdentifierSelector :: Selector '[] (Id NSString)
transactionIdentifierSelector = mkSelector "transactionIdentifier"

-- | @Selector@ for @transactionReceipt@
transactionReceiptSelector :: Selector '[] (Id NSData)
transactionReceiptSelector = mkSelector "transactionReceipt"

-- | @Selector@ for @transactionState@
transactionStateSelector :: Selector '[] SKPaymentTransactionState
transactionStateSelector = mkSelector "transactionState"

