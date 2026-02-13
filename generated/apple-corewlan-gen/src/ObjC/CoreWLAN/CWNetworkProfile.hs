{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Encapsulates a preferred network entry.
--
-- Generated bindings for @CWNetworkProfile@.
module ObjC.CoreWLAN.CWNetworkProfile
  ( CWNetworkProfile
  , IsCWNetworkProfile(..)
  , networkProfile
  , init_
  , initWithNetworkProfile
  , networkProfileWithNetworkProfile
  , isEqualToNetworkProfile
  , ssid
  , ssidData
  , security
  , initSelector
  , initWithNetworkProfileSelector
  , isEqualToNetworkProfileSelector
  , networkProfileSelector
  , networkProfileWithNetworkProfileSelector
  , securitySelector
  , ssidDataSelector
  , ssidSelector

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

-- | Convenience method for getting a CWNetworkProfile object.
--
-- ObjC selector: @+ networkProfile@
networkProfile :: IO (Id CWNetworkProfile)
networkProfile  =
  do
    cls' <- getRequiredClass "CWNetworkProfile"
    sendClassMessage cls' networkProfileSelector

-- | Initializes a CWNetworkProfile object.
--
-- ObjC selector: @- init@
init_ :: IsCWNetworkProfile cwNetworkProfile => cwNetworkProfile -> IO (Id CWNetworkProfile)
init_ cwNetworkProfile =
  sendOwnedMessage cwNetworkProfile initSelector

-- | @networkProfile@ — A CWNetworkProfile object.
--
-- Returns: A CWNetworkProfile object.
--
-- Initializes a CWNetworkProfile object with the properties of an existing CWNetworkProfile object.
--
-- ObjC selector: @- initWithNetworkProfile:@
initWithNetworkProfile :: (IsCWNetworkProfile cwNetworkProfile, IsCWNetworkProfile networkProfile) => cwNetworkProfile -> networkProfile -> IO (Id CWNetworkProfile)
initWithNetworkProfile cwNetworkProfile networkProfile =
  sendOwnedMessage cwNetworkProfile initWithNetworkProfileSelector (toCWNetworkProfile networkProfile)

-- | @networkProfile@ — A CWNetworkProfile object.
--
-- Returns: A CWNetworkProfile object.
--
-- Convenience method for getting a CWNetworkProfile object initialized with the properties of an existing CWNetworkProfile object.
--
-- ObjC selector: @+ networkProfileWithNetworkProfile:@
networkProfileWithNetworkProfile :: IsCWNetworkProfile networkProfile => networkProfile -> IO (Id CWNetworkProfile)
networkProfileWithNetworkProfile networkProfile =
  do
    cls' <- getRequiredClass "CWNetworkProfile"
    sendClassMessage cls' networkProfileWithNetworkProfileSelector (toCWNetworkProfile networkProfile)

-- | @network@ — A CWNetworkProfile object.
--
-- Returns: YES if the objects are equal, NO otherwise.
--
-- Determine CWNetworkProfile equality.
--
-- CWNetworkProfile objects are considered equal if their corresponding ssidData and security properties are equal.
--
-- ObjC selector: @- isEqualToNetworkProfile:@
isEqualToNetworkProfile :: (IsCWNetworkProfile cwNetworkProfile, IsCWNetworkProfile networkProfile) => cwNetworkProfile -> networkProfile -> IO Bool
isEqualToNetworkProfile cwNetworkProfile networkProfile =
  sendMessage cwNetworkProfile isEqualToNetworkProfileSelector (toCWNetworkProfile networkProfile)

-- | Returns the service set identifier (SSID) for the Wi-Fi network profile, encoded as a string.
--
-- Returns nil if the SSID can not be encoded as a valid UTF-8 or WinLatin1 string.
--
-- ObjC selector: @- ssid@
ssid :: IsCWNetworkProfile cwNetworkProfile => cwNetworkProfile -> IO RawId
ssid cwNetworkProfile =
  sendMessage cwNetworkProfile ssidSelector

-- | Returns the service set identifier (SSID) for the Wi-Fi network profile, encapsulated in an NSData object.
--
-- The SSID is 1-32 octets.
--
-- ObjC selector: @- ssidData@
ssidData :: IsCWNetworkProfile cwNetworkProfile => cwNetworkProfile -> IO RawId
ssidData cwNetworkProfile =
  sendMessage cwNetworkProfile ssidDataSelector

-- | Returns the security type of the Wi-Fi network profile.
--
-- ObjC selector: @- security@
security :: IsCWNetworkProfile cwNetworkProfile => cwNetworkProfile -> IO CWSecurity
security cwNetworkProfile =
  sendMessage cwNetworkProfile securitySelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @networkProfile@
networkProfileSelector :: Selector '[] (Id CWNetworkProfile)
networkProfileSelector = mkSelector "networkProfile"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CWNetworkProfile)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithNetworkProfile:@
initWithNetworkProfileSelector :: Selector '[Id CWNetworkProfile] (Id CWNetworkProfile)
initWithNetworkProfileSelector = mkSelector "initWithNetworkProfile:"

-- | @Selector@ for @networkProfileWithNetworkProfile:@
networkProfileWithNetworkProfileSelector :: Selector '[Id CWNetworkProfile] (Id CWNetworkProfile)
networkProfileWithNetworkProfileSelector = mkSelector "networkProfileWithNetworkProfile:"

-- | @Selector@ for @isEqualToNetworkProfile:@
isEqualToNetworkProfileSelector :: Selector '[Id CWNetworkProfile] Bool
isEqualToNetworkProfileSelector = mkSelector "isEqualToNetworkProfile:"

-- | @Selector@ for @ssid@
ssidSelector :: Selector '[] RawId
ssidSelector = mkSelector "ssid"

-- | @Selector@ for @ssidData@
ssidDataSelector :: Selector '[] RawId
ssidDataSelector = mkSelector "ssidData"

-- | @Selector@ for @security@
securitySelector :: Selector '[] CWSecurity
securitySelector = mkSelector "security"

