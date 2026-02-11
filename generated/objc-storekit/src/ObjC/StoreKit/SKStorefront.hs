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

-- | @- countryCode@
countryCode :: IsSKStorefront skStorefront => skStorefront -> IO (Id NSString)
countryCode skStorefront  =
  sendMsg skStorefront (mkSelector "countryCode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- identifier@
identifier :: IsSKStorefront skStorefront => skStorefront -> IO (Id NSString)
identifier skStorefront  =
  sendMsg skStorefront (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @countryCode@
countryCodeSelector :: Selector
countryCodeSelector = mkSelector "countryCode"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

