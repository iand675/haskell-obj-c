{-# LANGUAGE PatternSynonyms #-}
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
  , networkProfileSelector
  , initSelector
  , initWithNetworkProfileSelector
  , networkProfileWithNetworkProfileSelector
  , isEqualToNetworkProfileSelector
  , ssidSelector
  , ssidDataSelector
  , securitySelector

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

-- | Convenience method for getting a CWNetworkProfile object.
--
-- ObjC selector: @+ networkProfile@
networkProfile :: IO (Id CWNetworkProfile)
networkProfile  =
  do
    cls' <- getRequiredClass "CWNetworkProfile"
    sendClassMsg cls' (mkSelector "networkProfile") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Initializes a CWNetworkProfile object.
--
-- ObjC selector: @- init@
init_ :: IsCWNetworkProfile cwNetworkProfile => cwNetworkProfile -> IO (Id CWNetworkProfile)
init_ cwNetworkProfile  =
    sendMsg cwNetworkProfile (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @networkProfile@ — A CWNetworkProfile object.
--
-- Returns: A CWNetworkProfile object.
--
-- Initializes a CWNetworkProfile object with the properties of an existing CWNetworkProfile object.
--
-- ObjC selector: @- initWithNetworkProfile:@
initWithNetworkProfile :: (IsCWNetworkProfile cwNetworkProfile, IsCWNetworkProfile networkProfile) => cwNetworkProfile -> networkProfile -> IO (Id CWNetworkProfile)
initWithNetworkProfile cwNetworkProfile  networkProfile =
  withObjCPtr networkProfile $ \raw_networkProfile ->
      sendMsg cwNetworkProfile (mkSelector "initWithNetworkProfile:") (retPtr retVoid) [argPtr (castPtr raw_networkProfile :: Ptr ())] >>= ownedObject . castPtr

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
    withObjCPtr networkProfile $ \raw_networkProfile ->
      sendClassMsg cls' (mkSelector "networkProfileWithNetworkProfile:") (retPtr retVoid) [argPtr (castPtr raw_networkProfile :: Ptr ())] >>= retainedObject . castPtr

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
isEqualToNetworkProfile cwNetworkProfile  networkProfile =
  withObjCPtr networkProfile $ \raw_networkProfile ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg cwNetworkProfile (mkSelector "isEqualToNetworkProfile:") retCULong [argPtr (castPtr raw_networkProfile :: Ptr ())]

-- | Returns the service set identifier (SSID) for the Wi-Fi network profile, encoded as a string.
--
-- Returns nil if the SSID can not be encoded as a valid UTF-8 or WinLatin1 string.
--
-- ObjC selector: @- ssid@
ssid :: IsCWNetworkProfile cwNetworkProfile => cwNetworkProfile -> IO RawId
ssid cwNetworkProfile  =
    fmap (RawId . castPtr) $ sendMsg cwNetworkProfile (mkSelector "ssid") (retPtr retVoid) []

-- | Returns the service set identifier (SSID) for the Wi-Fi network profile, encapsulated in an NSData object.
--
-- The SSID is 1-32 octets.
--
-- ObjC selector: @- ssidData@
ssidData :: IsCWNetworkProfile cwNetworkProfile => cwNetworkProfile -> IO RawId
ssidData cwNetworkProfile  =
    fmap (RawId . castPtr) $ sendMsg cwNetworkProfile (mkSelector "ssidData") (retPtr retVoid) []

-- | Returns the security type of the Wi-Fi network profile.
--
-- ObjC selector: @- security@
security :: IsCWNetworkProfile cwNetworkProfile => cwNetworkProfile -> IO CWSecurity
security cwNetworkProfile  =
    fmap (coerce :: CLong -> CWSecurity) $ sendMsg cwNetworkProfile (mkSelector "security") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @networkProfile@
networkProfileSelector :: Selector
networkProfileSelector = mkSelector "networkProfile"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithNetworkProfile:@
initWithNetworkProfileSelector :: Selector
initWithNetworkProfileSelector = mkSelector "initWithNetworkProfile:"

-- | @Selector@ for @networkProfileWithNetworkProfile:@
networkProfileWithNetworkProfileSelector :: Selector
networkProfileWithNetworkProfileSelector = mkSelector "networkProfileWithNetworkProfile:"

-- | @Selector@ for @isEqualToNetworkProfile:@
isEqualToNetworkProfileSelector :: Selector
isEqualToNetworkProfileSelector = mkSelector "isEqualToNetworkProfile:"

-- | @Selector@ for @ssid@
ssidSelector :: Selector
ssidSelector = mkSelector "ssid"

-- | @Selector@ for @ssidData@
ssidDataSelector :: Selector
ssidDataSelector = mkSelector "ssidData"

-- | @Selector@ for @security@
securitySelector :: Selector
securitySelector = mkSelector "security"

