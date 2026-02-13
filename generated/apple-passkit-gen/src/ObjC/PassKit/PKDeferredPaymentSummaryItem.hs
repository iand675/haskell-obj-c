{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PKDeferredPaymentSummaryItem@.
module ObjC.PassKit.PKDeferredPaymentSummaryItem
  ( PKDeferredPaymentSummaryItem
  , IsPKDeferredPaymentSummaryItem(..)
  , deferredDate
  , setDeferredDate
  , deferredDateSelector
  , setDeferredDateSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PassKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- deferredDate@
deferredDate :: IsPKDeferredPaymentSummaryItem pkDeferredPaymentSummaryItem => pkDeferredPaymentSummaryItem -> IO (Id NSDate)
deferredDate pkDeferredPaymentSummaryItem =
  sendMessage pkDeferredPaymentSummaryItem deferredDateSelector

-- | @- setDeferredDate:@
setDeferredDate :: (IsPKDeferredPaymentSummaryItem pkDeferredPaymentSummaryItem, IsNSDate value) => pkDeferredPaymentSummaryItem -> value -> IO ()
setDeferredDate pkDeferredPaymentSummaryItem value =
  sendMessage pkDeferredPaymentSummaryItem setDeferredDateSelector (toNSDate value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @deferredDate@
deferredDateSelector :: Selector '[] (Id NSDate)
deferredDateSelector = mkSelector "deferredDate"

-- | @Selector@ for @setDeferredDate:@
setDeferredDateSelector :: Selector '[Id NSDate] ()
setDeferredDateSelector = mkSelector "setDeferredDate:"

