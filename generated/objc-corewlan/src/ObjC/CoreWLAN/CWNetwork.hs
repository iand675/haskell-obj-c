{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Represents a device participating in a Wi-Fi network, providing accessors to various network attributes.
--
-- Generated bindings for @CWNetwork@.
module ObjC.CoreWLAN.CWNetwork
  ( CWNetwork
  , IsCWNetwork(..)
  , isEqualToNetwork
  , supportsSecurity
  , supportsPHYMode
  , rssiValue
  , noiseMeasurement
  , beaconInterval
  , ibss
  , isEqualToNetworkSelector
  , supportsSecuritySelector
  , supportsPHYModeSelector
  , rssiValueSelector
  , noiseMeasurementSelector
  , beaconIntervalSelector
  , ibssSelector

  -- * Enum types
  , CWPHYMode(CWPHYMode)
  , pattern KCWPHYModeNone
  , pattern KCWPHYMode11a
  , pattern KCWPHYMode11b
  , pattern KCWPHYMode11g
  , pattern KCWPHYMode11n
  , pattern KCWPHYMode11ac
  , pattern KCWPHYMode11ax
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

-- | @network@ — A CWNetwork object.
--
-- Returns: YES if the objects are equal, NO otherwise.
--
-- Determine CWNetwork equality.
--
-- CWNetwork objects are considered equal if their corresponding ssidData and bssid properties are equal.
--
-- ObjC selector: @- isEqualToNetwork:@
isEqualToNetwork :: (IsCWNetwork cwNetwork, IsCWNetwork network) => cwNetwork -> network -> IO Bool
isEqualToNetwork cwNetwork  network =
withObjCPtr network $ \raw_network ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg cwNetwork (mkSelector "isEqualToNetwork:") retCULong [argPtr (castPtr raw_network :: Ptr ())]

-- | @security@ — A CWSecurity type value.
--
-- Returns: YES if the Wi-Fi device supports the specified security type, NO otherwise.
--
-- Determine which security types a Wi-Fi device supports.
--
-- ObjC selector: @- supportsSecurity:@
supportsSecurity :: IsCWNetwork cwNetwork => cwNetwork -> CWSecurity -> IO Bool
supportsSecurity cwNetwork  security =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg cwNetwork (mkSelector "supportsSecurity:") retCULong [argCLong (coerce security)]

-- | @phyMode@ — A CWPHYMode type value.
--
-- Returns: YES if the Wi-Fi device supports the specified PHY mode, NO otherwise.
--
-- Determine which PHY modes a Wi-Fi device supports.
--
-- ObjC selector: @- supportsPHYMode:@
supportsPHYMode :: IsCWNetwork cwNetwork => cwNetwork -> CWPHYMode -> IO Bool
supportsPHYMode cwNetwork  phyMode =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg cwNetwork (mkSelector "supportsPHYMode:") retCULong [argCLong (coerce phyMode)]

-- | Returns the received signal strength indication (RSSI) measurement (dBm) for the Wi-Fi device.
--
-- ObjC selector: @- rssiValue@
rssiValue :: IsCWNetwork cwNetwork => cwNetwork -> IO CLong
rssiValue cwNetwork  =
  sendMsg cwNetwork (mkSelector "rssiValue") retCLong []

-- | Returns the noise measurement (dBm) for the Wi-Fi device.
--
-- ObjC selector: @- noiseMeasurement@
noiseMeasurement :: IsCWNetwork cwNetwork => cwNetwork -> IO CLong
noiseMeasurement cwNetwork  =
  sendMsg cwNetwork (mkSelector "noiseMeasurement") retCLong []

-- | Returns the beacon interval (ms) for the Wi-Fi device.
--
-- ObjC selector: @- beaconInterval@
beaconInterval :: IsCWNetwork cwNetwork => cwNetwork -> IO CLong
beaconInterval cwNetwork  =
  sendMsg cwNetwork (mkSelector "beaconInterval") retCLong []

-- | Returns: YES if the Wi-Fi device is part of an IBSS network, NO otherwise.
--
-- Indicates whether or not the Wi-Fi device is participating in an independent basic service set (IBSS), or ad-hoc Wi-Fi network.
--
-- ObjC selector: @- ibss@
ibss :: IsCWNetwork cwNetwork => cwNetwork -> IO Bool
ibss cwNetwork  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg cwNetwork (mkSelector "ibss") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @isEqualToNetwork:@
isEqualToNetworkSelector :: Selector
isEqualToNetworkSelector = mkSelector "isEqualToNetwork:"

-- | @Selector@ for @supportsSecurity:@
supportsSecuritySelector :: Selector
supportsSecuritySelector = mkSelector "supportsSecurity:"

-- | @Selector@ for @supportsPHYMode:@
supportsPHYModeSelector :: Selector
supportsPHYModeSelector = mkSelector "supportsPHYMode:"

-- | @Selector@ for @rssiValue@
rssiValueSelector :: Selector
rssiValueSelector = mkSelector "rssiValue"

-- | @Selector@ for @noiseMeasurement@
noiseMeasurementSelector :: Selector
noiseMeasurementSelector = mkSelector "noiseMeasurement"

-- | @Selector@ for @beaconInterval@
beaconIntervalSelector :: Selector
beaconIntervalSelector = mkSelector "beaconInterval"

-- | @Selector@ for @ibss@
ibssSelector :: Selector
ibssSelector = mkSelector "ibss"

