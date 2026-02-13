{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Used to request information from a user's national id card (or equivalent document).
--
-- Generated bindings for @PKIdentityNationalIDCardDescriptor@.
module ObjC.PassKit.PKIdentityNationalIDCardDescriptor
  ( PKIdentityNationalIDCardDescriptor
  , IsPKIdentityNationalIDCardDescriptor(..)
  , regionCode
  , setRegionCode
  , regionCodeSelector
  , setRegionCodeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PassKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Alpha-2 country code, as defined in ISO 3166-1, of the issuing authority’s country or territory
--
-- ObjC selector: @- regionCode@
regionCode :: IsPKIdentityNationalIDCardDescriptor pkIdentityNationalIDCardDescriptor => pkIdentityNationalIDCardDescriptor -> IO (Id NSString)
regionCode pkIdentityNationalIDCardDescriptor =
  sendMessage pkIdentityNationalIDCardDescriptor regionCodeSelector

-- | Alpha-2 country code, as defined in ISO 3166-1, of the issuing authority’s country or territory
--
-- ObjC selector: @- setRegionCode:@
setRegionCode :: (IsPKIdentityNationalIDCardDescriptor pkIdentityNationalIDCardDescriptor, IsNSString value) => pkIdentityNationalIDCardDescriptor -> value -> IO ()
setRegionCode pkIdentityNationalIDCardDescriptor value =
  sendMessage pkIdentityNationalIDCardDescriptor setRegionCodeSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @regionCode@
regionCodeSelector :: Selector '[] (Id NSString)
regionCodeSelector = mkSelector "regionCode"

-- | @Selector@ for @setRegionCode:@
setRegionCodeSelector :: Selector '[Id NSString] ()
setRegionCodeSelector = mkSelector "setRegionCode:"

