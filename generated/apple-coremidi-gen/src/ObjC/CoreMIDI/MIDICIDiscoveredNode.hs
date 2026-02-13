{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MIDICIDiscoveredNode@.
module ObjC.CoreMIDI.MIDICIDiscoveredNode
  ( MIDICIDiscoveredNode
  , IsMIDICIDiscoveredNode(..)
  , init_
  , destination
  , deviceInfo
  , supportsProfiles
  , supportsProperties
  , maximumSysExSize
  , destinationSelector
  , deviceInfoSelector
  , initSelector
  , maximumSysExSizeSelector
  , supportsProfilesSelector
  , supportsPropertiesSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreMIDI.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsMIDICIDiscoveredNode midiciDiscoveredNode => midiciDiscoveredNode -> IO (Id MIDICIDiscoveredNode)
init_ midiciDiscoveredNode =
  sendOwnedMessage midiciDiscoveredNode initSelector

-- | @- destination@
destination :: IsMIDICIDiscoveredNode midiciDiscoveredNode => midiciDiscoveredNode -> IO CUInt
destination midiciDiscoveredNode =
  sendMessage midiciDiscoveredNode destinationSelector

-- | @- deviceInfo@
deviceInfo :: IsMIDICIDiscoveredNode midiciDiscoveredNode => midiciDiscoveredNode -> IO (Id MIDICIDeviceInfo)
deviceInfo midiciDiscoveredNode =
  sendMessage midiciDiscoveredNode deviceInfoSelector

-- | @- supportsProfiles@
supportsProfiles :: IsMIDICIDiscoveredNode midiciDiscoveredNode => midiciDiscoveredNode -> IO Bool
supportsProfiles midiciDiscoveredNode =
  sendMessage midiciDiscoveredNode supportsProfilesSelector

-- | @- supportsProperties@
supportsProperties :: IsMIDICIDiscoveredNode midiciDiscoveredNode => midiciDiscoveredNode -> IO Bool
supportsProperties midiciDiscoveredNode =
  sendMessage midiciDiscoveredNode supportsPropertiesSelector

-- | @- maximumSysExSize@
maximumSysExSize :: IsMIDICIDiscoveredNode midiciDiscoveredNode => midiciDiscoveredNode -> IO (Id NSNumber)
maximumSysExSize midiciDiscoveredNode =
  sendMessage midiciDiscoveredNode maximumSysExSizeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MIDICIDiscoveredNode)
initSelector = mkSelector "init"

-- | @Selector@ for @destination@
destinationSelector :: Selector '[] CUInt
destinationSelector = mkSelector "destination"

-- | @Selector@ for @deviceInfo@
deviceInfoSelector :: Selector '[] (Id MIDICIDeviceInfo)
deviceInfoSelector = mkSelector "deviceInfo"

-- | @Selector@ for @supportsProfiles@
supportsProfilesSelector :: Selector '[] Bool
supportsProfilesSelector = mkSelector "supportsProfiles"

-- | @Selector@ for @supportsProperties@
supportsPropertiesSelector :: Selector '[] Bool
supportsPropertiesSelector = mkSelector "supportsProperties"

-- | @Selector@ for @maximumSysExSize@
maximumSysExSizeSelector :: Selector '[] (Id NSNumber)
maximumSysExSizeSelector = mkSelector "maximumSysExSize"

