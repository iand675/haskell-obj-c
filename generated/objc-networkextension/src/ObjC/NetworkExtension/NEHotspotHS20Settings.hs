{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NEHotspotHS20Settings
--
-- NEHotspotHS20Settings class provides a set of properties that are required   to discover and negotiate Hotspot 2.0 Wi-Fi networks.
--
-- Generated bindings for @NEHotspotHS20Settings@.
module ObjC.NetworkExtension.NEHotspotHS20Settings
  ( NEHotspotHS20Settings
  , IsNEHotspotHS20Settings(..)
  , initWithDomainName_roamingEnabled
  , roamingEnabled
  , setRoamingEnabled
  , initWithDomainName_roamingEnabledSelector
  , roamingEnabledSelector
  , setRoamingEnabledSelector


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

import ObjC.NetworkExtension.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | initWithDomainName:roamingEnabled
--
-- A designated initializer to instantiate a new NEHotspotHSSettings object.   This initializer is used to configure Legacy Hotspot or HS2.0 Wi-Fi Networks.
--
-- @domainName@ — The domain name of HS2.0 Wi-Fi Network
--
-- @roamingEnabled@ — If YES, allows connections to networks of roaming service providers.
--
-- ObjC selector: @- initWithDomainName:roamingEnabled:@
initWithDomainName_roamingEnabled :: (IsNEHotspotHS20Settings neHotspotHS20Settings, IsNSString domainName) => neHotspotHS20Settings -> domainName -> Bool -> IO (Id NEHotspotHS20Settings)
initWithDomainName_roamingEnabled neHotspotHS20Settings  domainName roamingEnabled =
withObjCPtr domainName $ \raw_domainName ->
    sendMsg neHotspotHS20Settings (mkSelector "initWithDomainName:roamingEnabled:") (retPtr retVoid) [argPtr (castPtr raw_domainName :: Ptr ()), argCULong (if roamingEnabled then 1 else 0)] >>= ownedObject . castPtr

-- | roamingEnabled
--
-- If set to YES, allows connection to networks of roaming service   providers. Defaults to NO.
--
-- ObjC selector: @- roamingEnabled@
roamingEnabled :: IsNEHotspotHS20Settings neHotspotHS20Settings => neHotspotHS20Settings -> IO Bool
roamingEnabled neHotspotHS20Settings  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg neHotspotHS20Settings (mkSelector "roamingEnabled") retCULong []

-- | roamingEnabled
--
-- If set to YES, allows connection to networks of roaming service   providers. Defaults to NO.
--
-- ObjC selector: @- setRoamingEnabled:@
setRoamingEnabled :: IsNEHotspotHS20Settings neHotspotHS20Settings => neHotspotHS20Settings -> Bool -> IO ()
setRoamingEnabled neHotspotHS20Settings  value =
  sendMsg neHotspotHS20Settings (mkSelector "setRoamingEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDomainName:roamingEnabled:@
initWithDomainName_roamingEnabledSelector :: Selector
initWithDomainName_roamingEnabledSelector = mkSelector "initWithDomainName:roamingEnabled:"

-- | @Selector@ for @roamingEnabled@
roamingEnabledSelector :: Selector
roamingEnabledSelector = mkSelector "roamingEnabled"

-- | @Selector@ for @setRoamingEnabled:@
setRoamingEnabledSelector :: Selector
setRoamingEnabledSelector = mkSelector "setRoamingEnabled:"

