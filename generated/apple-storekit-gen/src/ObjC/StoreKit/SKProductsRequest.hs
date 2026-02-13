{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SKProductsRequest@.
module ObjC.StoreKit.SKProductsRequest
  ( SKProductsRequest
  , IsSKProductsRequest(..)
  , initWithProductIdentifiers
  , delegate
  , setDelegate
  , delegateSelector
  , initWithProductIdentifiersSelector
  , setDelegateSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.StoreKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithProductIdentifiers:@
initWithProductIdentifiers :: (IsSKProductsRequest skProductsRequest, IsNSSet productIdentifiers) => skProductsRequest -> productIdentifiers -> IO (Id SKProductsRequest)
initWithProductIdentifiers skProductsRequest productIdentifiers =
  sendOwnedMessage skProductsRequest initWithProductIdentifiersSelector (toNSSet productIdentifiers)

-- | @- delegate@
delegate :: IsSKProductsRequest skProductsRequest => skProductsRequest -> IO RawId
delegate skProductsRequest =
  sendMessage skProductsRequest delegateSelector

-- | @- setDelegate:@
setDelegate :: IsSKProductsRequest skProductsRequest => skProductsRequest -> RawId -> IO ()
setDelegate skProductsRequest value =
  sendMessage skProductsRequest setDelegateSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithProductIdentifiers:@
initWithProductIdentifiersSelector :: Selector '[Id NSSet] (Id SKProductsRequest)
initWithProductIdentifiersSelector = mkSelector "initWithProductIdentifiers:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

