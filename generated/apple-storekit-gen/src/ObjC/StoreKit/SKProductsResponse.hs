{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SKProductsResponse@.
module ObjC.StoreKit.SKProductsResponse
  ( SKProductsResponse
  , IsSKProductsResponse(..)
  , products
  , invalidProductIdentifiers
  , invalidProductIdentifiersSelector
  , productsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.StoreKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- products@
products :: IsSKProductsResponse skProductsResponse => skProductsResponse -> IO (Id NSArray)
products skProductsResponse =
  sendMessage skProductsResponse productsSelector

-- | @- invalidProductIdentifiers@
invalidProductIdentifiers :: IsSKProductsResponse skProductsResponse => skProductsResponse -> IO (Id NSArray)
invalidProductIdentifiers skProductsResponse =
  sendMessage skProductsResponse invalidProductIdentifiersSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @products@
productsSelector :: Selector '[] (Id NSArray)
productsSelector = mkSelector "products"

-- | @Selector@ for @invalidProductIdentifiers@
invalidProductIdentifiersSelector :: Selector '[] (Id NSArray)
invalidProductIdentifiersSelector = mkSelector "invalidProductIdentifiers"

