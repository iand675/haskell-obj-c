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
  , initWithProductIdentifiersSelector
  , delegateSelector
  , setDelegateSelector


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

-- | @- initWithProductIdentifiers:@
initWithProductIdentifiers :: (IsSKProductsRequest skProductsRequest, IsNSSet productIdentifiers) => skProductsRequest -> productIdentifiers -> IO (Id SKProductsRequest)
initWithProductIdentifiers skProductsRequest  productIdentifiers =
  withObjCPtr productIdentifiers $ \raw_productIdentifiers ->
      sendMsg skProductsRequest (mkSelector "initWithProductIdentifiers:") (retPtr retVoid) [argPtr (castPtr raw_productIdentifiers :: Ptr ())] >>= ownedObject . castPtr

-- | @- delegate@
delegate :: IsSKProductsRequest skProductsRequest => skProductsRequest -> IO RawId
delegate skProductsRequest  =
    fmap (RawId . castPtr) $ sendMsg skProductsRequest (mkSelector "delegate") (retPtr retVoid) []

-- | @- setDelegate:@
setDelegate :: IsSKProductsRequest skProductsRequest => skProductsRequest -> RawId -> IO ()
setDelegate skProductsRequest  value =
    sendMsg skProductsRequest (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithProductIdentifiers:@
initWithProductIdentifiersSelector :: Selector
initWithProductIdentifiersSelector = mkSelector "initWithProductIdentifiers:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

