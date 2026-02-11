{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SKProductsResponse@.
module ObjC.StoreKit.SKProductsResponse
  ( SKProductsResponse
  , IsSKProductsResponse(..)
  , products
  , invalidProductIdentifiers
  , productsSelector
  , invalidProductIdentifiersSelector


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

-- | @- products@
products :: IsSKProductsResponse skProductsResponse => skProductsResponse -> IO (Id NSArray)
products skProductsResponse  =
  sendMsg skProductsResponse (mkSelector "products") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- invalidProductIdentifiers@
invalidProductIdentifiers :: IsSKProductsResponse skProductsResponse => skProductsResponse -> IO (Id NSArray)
invalidProductIdentifiers skProductsResponse  =
  sendMsg skProductsResponse (mkSelector "invalidProductIdentifiers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @products@
productsSelector :: Selector
productsSelector = mkSelector "products"

-- | @Selector@ for @invalidProductIdentifiers@
invalidProductIdentifiersSelector :: Selector
invalidProductIdentifiersSelector = mkSelector "invalidProductIdentifiers"

