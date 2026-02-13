{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Mutable subclass of CWNetworkProfile.  Use this class for changing profile properties.
--
-- To commit Wi-Fi network profile changes, use -[CWMutableConfiguration setNetworkProfiles:] and  -[CWInterface commitConfiguration:authorization:error:].
--
-- Generated bindings for @CWMutableNetworkProfile@.
module ObjC.CoreWLAN.CWMutableNetworkProfile
  ( CWMutableNetworkProfile
  , IsCWMutableNetworkProfile(..)
  , ssidData
  , setSsidData
  , security
  , setSecurity
  , securitySelector
  , setSecuritySelector
  , setSsidDataSelector
  , ssidDataSelector

  -- * Enum types
  , CWSecurity(CWSecurity)
  , pattern KCWSecurityNone
  , pattern KCWSecurityWEP
  , pattern KCWSecurityWPAPersonal
  , pattern KCWSecurityWPAPersonalMixed
  , pattern KCWSecurityWPA2Personal
  , pattern KCWSecurityPersonal
  , pattern KCWSecurityDynamicWEP
  , pattern KCWSecurityWPAEnterprise
  , pattern KCWSecurityWPAEnterpriseMixed
  , pattern KCWSecurityWPA2Enterprise
  , pattern KCWSecurityEnterprise
  , pattern KCWSecurityWPA3Personal
  , pattern KCWSecurityWPA3Enterprise
  , pattern KCWSecurityWPA3Transition
  , pattern KCWSecurityOWE
  , pattern KCWSecurityOWETransition
  , pattern KCWSecurityUnknown

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreWLAN.Internal.Classes
import ObjC.CoreWLAN.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Set the service set identifier (SSID).
--
-- ObjC selector: @- ssidData@
ssidData :: IsCWMutableNetworkProfile cwMutableNetworkProfile => cwMutableNetworkProfile -> IO RawId
ssidData cwMutableNetworkProfile =
  sendMessage cwMutableNetworkProfile ssidDataSelector

-- | Set the service set identifier (SSID).
--
-- ObjC selector: @- setSsidData:@
setSsidData :: IsCWMutableNetworkProfile cwMutableNetworkProfile => cwMutableNetworkProfile -> RawId -> IO ()
setSsidData cwMutableNetworkProfile value =
  sendMessage cwMutableNetworkProfile setSsidDataSelector value

-- | Set the security type.
--
-- ObjC selector: @- security@
security :: IsCWMutableNetworkProfile cwMutableNetworkProfile => cwMutableNetworkProfile -> IO CWSecurity
security cwMutableNetworkProfile =
  sendMessage cwMutableNetworkProfile securitySelector

-- | Set the security type.
--
-- ObjC selector: @- setSecurity:@
setSecurity :: IsCWMutableNetworkProfile cwMutableNetworkProfile => cwMutableNetworkProfile -> CWSecurity -> IO ()
setSecurity cwMutableNetworkProfile value =
  sendMessage cwMutableNetworkProfile setSecuritySelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @ssidData@
ssidDataSelector :: Selector '[] RawId
ssidDataSelector = mkSelector "ssidData"

-- | @Selector@ for @setSsidData:@
setSsidDataSelector :: Selector '[RawId] ()
setSsidDataSelector = mkSelector "setSsidData:"

-- | @Selector@ for @security@
securitySelector :: Selector '[] CWSecurity
securitySelector = mkSelector "security"

-- | @Selector@ for @setSecurity:@
setSecuritySelector :: Selector '[CWSecurity] ()
setSecuritySelector = mkSelector "setSecurity:"

