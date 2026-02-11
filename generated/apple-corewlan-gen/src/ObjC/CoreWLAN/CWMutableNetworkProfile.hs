{-# LANGUAGE PatternSynonyms #-}
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
  , ssidDataSelector
  , setSsidDataSelector
  , securitySelector
  , setSecuritySelector

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

import ObjC.CoreWLAN.Internal.Classes
import ObjC.CoreWLAN.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Set the service set identifier (SSID).
--
-- ObjC selector: @- ssidData@
ssidData :: IsCWMutableNetworkProfile cwMutableNetworkProfile => cwMutableNetworkProfile -> IO RawId
ssidData cwMutableNetworkProfile  =
    fmap (RawId . castPtr) $ sendMsg cwMutableNetworkProfile (mkSelector "ssidData") (retPtr retVoid) []

-- | Set the service set identifier (SSID).
--
-- ObjC selector: @- setSsidData:@
setSsidData :: IsCWMutableNetworkProfile cwMutableNetworkProfile => cwMutableNetworkProfile -> RawId -> IO ()
setSsidData cwMutableNetworkProfile  value =
    sendMsg cwMutableNetworkProfile (mkSelector "setSsidData:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | Set the security type.
--
-- ObjC selector: @- security@
security :: IsCWMutableNetworkProfile cwMutableNetworkProfile => cwMutableNetworkProfile -> IO CWSecurity
security cwMutableNetworkProfile  =
    fmap (coerce :: CLong -> CWSecurity) $ sendMsg cwMutableNetworkProfile (mkSelector "security") retCLong []

-- | Set the security type.
--
-- ObjC selector: @- setSecurity:@
setSecurity :: IsCWMutableNetworkProfile cwMutableNetworkProfile => cwMutableNetworkProfile -> CWSecurity -> IO ()
setSecurity cwMutableNetworkProfile  value =
    sendMsg cwMutableNetworkProfile (mkSelector "setSecurity:") retVoid [argCLong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @ssidData@
ssidDataSelector :: Selector
ssidDataSelector = mkSelector "ssidData"

-- | @Selector@ for @setSsidData:@
setSsidDataSelector :: Selector
setSsidDataSelector = mkSelector "setSsidData:"

-- | @Selector@ for @security@
securitySelector :: Selector
securitySelector = mkSelector "security"

-- | @Selector@ for @setSecurity:@
setSecuritySelector :: Selector
setSecuritySelector = mkSelector "setSecurity:"

