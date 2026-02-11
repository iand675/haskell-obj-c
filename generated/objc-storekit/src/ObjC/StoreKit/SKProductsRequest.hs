{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SKProductsRequest@.
module ObjC.StoreKit.SKProductsRequest
  ( SKProductsRequest
  , IsSKProductsRequest(..)
  , initWithProductIdentifiers
  , initWithProductIdentifiersSelector


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

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithProductIdentifiers:@
initWithProductIdentifiersSelector :: Selector
initWithProductIdentifiersSelector = mkSelector "initWithProductIdentifiers:"

