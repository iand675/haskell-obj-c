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
  , initSelector
  , destinationSelector
  , deviceInfoSelector
  , supportsProfilesSelector
  , supportsPropertiesSelector
  , maximumSysExSizeSelector


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

import ObjC.CoreMIDI.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsMIDICIDiscoveredNode midiciDiscoveredNode => midiciDiscoveredNode -> IO (Id MIDICIDiscoveredNode)
init_ midiciDiscoveredNode  =
  sendMsg midiciDiscoveredNode (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- destination@
destination :: IsMIDICIDiscoveredNode midiciDiscoveredNode => midiciDiscoveredNode -> IO CUInt
destination midiciDiscoveredNode  =
  sendMsg midiciDiscoveredNode (mkSelector "destination") retCUInt []

-- | @- deviceInfo@
deviceInfo :: IsMIDICIDiscoveredNode midiciDiscoveredNode => midiciDiscoveredNode -> IO (Id MIDICIDeviceInfo)
deviceInfo midiciDiscoveredNode  =
  sendMsg midiciDiscoveredNode (mkSelector "deviceInfo") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- supportsProfiles@
supportsProfiles :: IsMIDICIDiscoveredNode midiciDiscoveredNode => midiciDiscoveredNode -> IO Bool
supportsProfiles midiciDiscoveredNode  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg midiciDiscoveredNode (mkSelector "supportsProfiles") retCULong []

-- | @- supportsProperties@
supportsProperties :: IsMIDICIDiscoveredNode midiciDiscoveredNode => midiciDiscoveredNode -> IO Bool
supportsProperties midiciDiscoveredNode  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg midiciDiscoveredNode (mkSelector "supportsProperties") retCULong []

-- | @- maximumSysExSize@
maximumSysExSize :: IsMIDICIDiscoveredNode midiciDiscoveredNode => midiciDiscoveredNode -> IO (Id NSNumber)
maximumSysExSize midiciDiscoveredNode  =
  sendMsg midiciDiscoveredNode (mkSelector "maximumSysExSize") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @destination@
destinationSelector :: Selector
destinationSelector = mkSelector "destination"

-- | @Selector@ for @deviceInfo@
deviceInfoSelector :: Selector
deviceInfoSelector = mkSelector "deviceInfo"

-- | @Selector@ for @supportsProfiles@
supportsProfilesSelector :: Selector
supportsProfilesSelector = mkSelector "supportsProfiles"

-- | @Selector@ for @supportsProperties@
supportsPropertiesSelector :: Selector
supportsPropertiesSelector = mkSelector "supportsProperties"

-- | @Selector@ for @maximumSysExSize@
maximumSysExSizeSelector :: Selector
maximumSysExSizeSelector = mkSelector "maximumSysExSize"

