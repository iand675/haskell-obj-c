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

import ObjC.PassKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Alpha-2 country code, as defined in ISO 3166-1, of the issuing authority’s country or territory
--
-- ObjC selector: @- regionCode@
regionCode :: IsPKIdentityNationalIDCardDescriptor pkIdentityNationalIDCardDescriptor => pkIdentityNationalIDCardDescriptor -> IO (Id NSString)
regionCode pkIdentityNationalIDCardDescriptor  =
    sendMsg pkIdentityNationalIDCardDescriptor (mkSelector "regionCode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Alpha-2 country code, as defined in ISO 3166-1, of the issuing authority’s country or territory
--
-- ObjC selector: @- setRegionCode:@
setRegionCode :: (IsPKIdentityNationalIDCardDescriptor pkIdentityNationalIDCardDescriptor, IsNSString value) => pkIdentityNationalIDCardDescriptor -> value -> IO ()
setRegionCode pkIdentityNationalIDCardDescriptor  value =
  withObjCPtr value $ \raw_value ->
      sendMsg pkIdentityNationalIDCardDescriptor (mkSelector "setRegionCode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @regionCode@
regionCodeSelector :: Selector
regionCodeSelector = mkSelector "regionCode"

-- | @Selector@ for @setRegionCode:@
setRegionCodeSelector :: Selector
setRegionCodeSelector = mkSelector "setRegionCode:"

