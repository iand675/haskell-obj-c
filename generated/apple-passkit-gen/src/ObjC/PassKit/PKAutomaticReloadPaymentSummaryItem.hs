{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PKAutomaticReloadPaymentSummaryItem@.
module ObjC.PassKit.PKAutomaticReloadPaymentSummaryItem
  ( PKAutomaticReloadPaymentSummaryItem
  , IsPKAutomaticReloadPaymentSummaryItem(..)
  , thresholdAmount
  , setThresholdAmount
  , setThresholdAmountSelector
  , thresholdAmountSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PassKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- thresholdAmount@
thresholdAmount :: IsPKAutomaticReloadPaymentSummaryItem pkAutomaticReloadPaymentSummaryItem => pkAutomaticReloadPaymentSummaryItem -> IO (Id NSDecimalNumber)
thresholdAmount pkAutomaticReloadPaymentSummaryItem =
  sendMessage pkAutomaticReloadPaymentSummaryItem thresholdAmountSelector

-- | @- setThresholdAmount:@
setThresholdAmount :: (IsPKAutomaticReloadPaymentSummaryItem pkAutomaticReloadPaymentSummaryItem, IsNSDecimalNumber value) => pkAutomaticReloadPaymentSummaryItem -> value -> IO ()
setThresholdAmount pkAutomaticReloadPaymentSummaryItem value =
  sendMessage pkAutomaticReloadPaymentSummaryItem setThresholdAmountSelector (toNSDecimalNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @thresholdAmount@
thresholdAmountSelector :: Selector '[] (Id NSDecimalNumber)
thresholdAmountSelector = mkSelector "thresholdAmount"

-- | @Selector@ for @setThresholdAmount:@
setThresholdAmountSelector :: Selector '[Id NSDecimalNumber] ()
setThresholdAmountSelector = mkSelector "setThresholdAmount:"

