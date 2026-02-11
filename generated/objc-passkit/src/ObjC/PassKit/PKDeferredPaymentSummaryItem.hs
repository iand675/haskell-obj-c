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

-- | @- deferredDate@
deferredDate :: IsPKDeferredPaymentSummaryItem pkDeferredPaymentSummaryItem => pkDeferredPaymentSummaryItem -> IO (Id NSDate)
deferredDate pkDeferredPaymentSummaryItem  =
  sendMsg pkDeferredPaymentSummaryItem (mkSelector "deferredDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDeferredDate:@
setDeferredDate :: (IsPKDeferredPaymentSummaryItem pkDeferredPaymentSummaryItem, IsNSDate value) => pkDeferredPaymentSummaryItem -> value -> IO ()
setDeferredDate pkDeferredPaymentSummaryItem  value =
withObjCPtr value $ \raw_value ->
    sendMsg pkDeferredPaymentSummaryItem (mkSelector "setDeferredDate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @deferredDate@
deferredDateSelector :: Selector
deferredDateSelector = mkSelector "deferredDate"

-- | @Selector@ for @setDeferredDate:@
setDeferredDateSelector :: Selector
setDeferredDateSelector = mkSelector "setDeferredDate:"

