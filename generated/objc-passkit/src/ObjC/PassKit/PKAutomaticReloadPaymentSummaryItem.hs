{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PKAutomaticReloadPaymentSummaryItem@.
module ObjC.PassKit.PKAutomaticReloadPaymentSummaryItem
  ( PKAutomaticReloadPaymentSummaryItem
  , IsPKAutomaticReloadPaymentSummaryItem(..)
  , thresholdAmount
  , setThresholdAmount
  , thresholdAmountSelector
  , setThresholdAmountSelector


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

import ObjC.PassKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- thresholdAmount@
thresholdAmount :: IsPKAutomaticReloadPaymentSummaryItem pkAutomaticReloadPaymentSummaryItem => pkAutomaticReloadPaymentSummaryItem -> IO (Id NSDecimalNumber)
thresholdAmount pkAutomaticReloadPaymentSummaryItem  =
  sendMsg pkAutomaticReloadPaymentSummaryItem (mkSelector "thresholdAmount") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setThresholdAmount:@
setThresholdAmount :: (IsPKAutomaticReloadPaymentSummaryItem pkAutomaticReloadPaymentSummaryItem, IsNSDecimalNumber value) => pkAutomaticReloadPaymentSummaryItem -> value -> IO ()
setThresholdAmount pkAutomaticReloadPaymentSummaryItem  value =
withObjCPtr value $ \raw_value ->
    sendMsg pkAutomaticReloadPaymentSummaryItem (mkSelector "setThresholdAmount:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @thresholdAmount@
thresholdAmountSelector :: Selector
thresholdAmountSelector = mkSelector "thresholdAmount"

-- | @Selector@ for @setThresholdAmount:@
setThresholdAmountSelector :: Selector
setThresholdAmountSelector = mkSelector "setThresholdAmount:"

