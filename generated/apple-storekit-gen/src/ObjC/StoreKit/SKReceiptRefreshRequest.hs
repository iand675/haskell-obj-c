{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.StoreKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithReceiptProperties:@
initWithReceiptProperties :: (IsSKReceiptRefreshRequest skReceiptRefreshRequest, IsNSDictionary properties) => skReceiptRefreshRequest -> properties -> IO (Id SKReceiptRefreshRequest)
initWithReceiptProperties skReceiptRefreshRequest properties =
  sendOwnedMessage skReceiptRefreshRequest initWithReceiptPropertiesSelector (toNSDictionary properties)

-- | @- receiptProperties@
receiptProperties :: IsSKReceiptRefreshRequest skReceiptRefreshRequest => skReceiptRefreshRequest -> IO (Id NSDictionary)
receiptProperties skReceiptRefreshRequest =
  sendMessage skReceiptRefreshRequest receiptPropertiesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithReceiptProperties:@
initWithReceiptPropertiesSelector :: Selector '[Id NSDictionary] (Id SKReceiptRefreshRequest)
initWithReceiptPropertiesSelector = mkSelector "initWithReceiptProperties:"

-- | @Selector@ for @receiptProperties@
receiptPropertiesSelector :: Selector '[] (Id NSDictionary)
receiptPropertiesSelector = mkSelector "receiptProperties"

