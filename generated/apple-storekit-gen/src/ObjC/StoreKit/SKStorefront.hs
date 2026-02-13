{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SKStorefront@.
module ObjC.StoreKit.SKStorefront
  ( SKStorefront
  , IsSKStorefront(..)
  , countryCode
  , identifier
  , countryCodeSelector
  , identifierSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.StoreKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- countryCode@
countryCode :: IsSKStorefront skStorefront => skStorefront -> IO (Id NSString)
countryCode skStorefront =
  sendMessage skStorefront countryCodeSelector

-- | @- identifier@
identifier :: IsSKStorefront skStorefront => skStorefront -> IO (Id NSString)
identifier skStorefront =
  sendMessage skStorefront identifierSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @countryCode@
countryCodeSelector :: Selector '[] (Id NSString)
countryCodeSelector = mkSelector "countryCode"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

