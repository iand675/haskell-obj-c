{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SKReceiptRefreshRequest@.
module ObjC.StoreKit.SKReceiptRefreshRequest
  ( SKReceiptRefreshRequest
  , IsSKReceiptRefreshRequest(..)
  , initWithReceiptProperties
  , receiptProperties
  , initWithReceiptPropertiesSelector
  , receiptPropertiesSelector


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

-- | @- initWithReceiptProperties:@
initWithReceiptProperties :: (IsSKReceiptRefreshRequest skReceiptRefreshRequest, IsNSDictionary properties) => skReceiptRefreshRequest -> properties -> IO (Id SKReceiptRefreshRequest)
initWithReceiptProperties skReceiptRefreshRequest  properties =
withObjCPtr properties $ \raw_properties ->
    sendMsg skReceiptRefreshRequest (mkSelector "initWithReceiptProperties:") (retPtr retVoid) [argPtr (castPtr raw_properties :: Ptr ())] >>= ownedObject . castPtr

-- | @- receiptProperties@
receiptProperties :: IsSKReceiptRefreshRequest skReceiptRefreshRequest => skReceiptRefreshRequest -> IO (Id NSDictionary)
receiptProperties skReceiptRefreshRequest  =
  sendMsg skReceiptRefreshRequest (mkSelector "receiptProperties") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithReceiptProperties:@
initWithReceiptPropertiesSelector :: Selector
initWithReceiptPropertiesSelector = mkSelector "initWithReceiptProperties:"

-- | @Selector@ for @receiptProperties@
receiptPropertiesSelector :: Selector
receiptPropertiesSelector = mkSelector "receiptProperties"

