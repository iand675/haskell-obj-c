{-# LANGUAGE PatternSynonyms #-}
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
  , errorSelector
  , originalTransactionSelector
  , paymentSelector
  , downloadsSelector
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
import ObjC.StoreKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- error@
error_ :: IsSKPaymentTransaction skPaymentTransaction => skPaymentTransaction -> IO (Id NSError)
error_ skPaymentTransaction  =
    sendMsg skPaymentTransaction (mkSelector "error") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- originalTransaction@
originalTransaction :: IsSKPaymentTransaction skPaymentTransaction => skPaymentTransaction -> IO (Id SKPaymentTransaction)
originalTransaction skPaymentTransaction  =
    sendMsg skPaymentTransaction (mkSelector "originalTransaction") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- payment@
payment :: IsSKPaymentTransaction skPaymentTransaction => skPaymentTransaction -> IO (Id SKPayment)
payment skPaymentTransaction  =
    sendMsg skPaymentTransaction (mkSelector "payment") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- downloads@
downloads :: IsSKPaymentTransaction skPaymentTransaction => skPaymentTransaction -> IO (Id NSArray)
downloads skPaymentTransaction  =
    sendMsg skPaymentTransaction (mkSelector "downloads") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- transactionDate@
transactionDate :: IsSKPaymentTransaction skPaymentTransaction => skPaymentTransaction -> IO (Id NSDate)
transactionDate skPaymentTransaction  =
    sendMsg skPaymentTransaction (mkSelector "transactionDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- transactionIdentifier@
transactionIdentifier :: IsSKPaymentTransaction skPaymentTransaction => skPaymentTransaction -> IO (Id NSString)
transactionIdentifier skPaymentTransaction  =
    sendMsg skPaymentTransaction (mkSelector "transactionIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- transactionReceipt@
transactionReceipt :: IsSKPaymentTransaction skPaymentTransaction => skPaymentTransaction -> IO (Id NSData)
transactionReceipt skPaymentTransaction  =
    sendMsg skPaymentTransaction (mkSelector "transactionReceipt") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- transactionState@
transactionState :: IsSKPaymentTransaction skPaymentTransaction => skPaymentTransaction -> IO SKPaymentTransactionState
transactionState skPaymentTransaction  =
    fmap (coerce :: CLong -> SKPaymentTransactionState) $ sendMsg skPaymentTransaction (mkSelector "transactionState") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @error@
errorSelector :: Selector
errorSelector = mkSelector "error"

-- | @Selector@ for @originalTransaction@
originalTransactionSelector :: Selector
originalTransactionSelector = mkSelector "originalTransaction"

-- | @Selector@ for @payment@
paymentSelector :: Selector
paymentSelector = mkSelector "payment"

-- | @Selector@ for @downloads@
downloadsSelector :: Selector
downloadsSelector = mkSelector "downloads"

-- | @Selector@ for @transactionDate@
transactionDateSelector :: Selector
transactionDateSelector = mkSelector "transactionDate"

-- | @Selector@ for @transactionIdentifier@
transactionIdentifierSelector :: Selector
transactionIdentifierSelector = mkSelector "transactionIdentifier"

-- | @Selector@ for @transactionReceipt@
transactionReceiptSelector :: Selector
transactionReceiptSelector = mkSelector "transactionReceipt"

-- | @Selector@ for @transactionState@
transactionStateSelector :: Selector
transactionStateSelector = mkSelector "transactionState"

