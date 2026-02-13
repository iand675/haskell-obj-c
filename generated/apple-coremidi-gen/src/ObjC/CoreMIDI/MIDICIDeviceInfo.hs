{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MIDICIDeviceInfo@.
module ObjC.CoreMIDI.MIDICIDeviceInfo
  ( MIDICIDeviceInfo
  , IsMIDICIDeviceInfo(..)
  , init_
  , initWithDestination_manufacturer_family_model_revision
  , manufacturerID
  , family_
  , modelNumber
  , revisionLevel
  , midiDestination
  , familySelector
  , initSelector
  , initWithDestination_manufacturer_family_model_revisionSelector
  , manufacturerIDSelector
  , midiDestinationSelector
  , modelNumberSelector
  , revisionLevelSelector


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
init_ :: IsMIDICIDeviceInfo midiciDeviceInfo => midiciDeviceInfo -> IO (Id MIDICIDeviceInfo)
init_ midiciDeviceInfo =
  sendOwnedMessage midiciDeviceInfo initSelector

-- | @- initWithDestination:manufacturer:family:model:revision:@
initWithDestination_manufacturer_family_model_revision :: (IsMIDICIDeviceInfo midiciDeviceInfo, IsNSData manufacturer, IsNSData family_, IsNSData modelNumber, IsNSData revisionLevel) => midiciDeviceInfo -> CUInt -> manufacturer -> family_ -> modelNumber -> revisionLevel -> IO (Id MIDICIDeviceInfo)
initWithDestination_manufacturer_family_model_revision midiciDeviceInfo midiDestination manufacturer family_ modelNumber revisionLevel =
  sendOwnedMessage midiciDeviceInfo initWithDestination_manufacturer_family_model_revisionSelector midiDestination (toNSData manufacturer) (toNSData family_) (toNSData modelNumber) (toNSData revisionLevel)

-- | @- manufacturerID@
manufacturerID :: IsMIDICIDeviceInfo midiciDeviceInfo => midiciDeviceInfo -> IO (Id NSData)
manufacturerID midiciDeviceInfo =
  sendMessage midiciDeviceInfo manufacturerIDSelector

-- | @- family@
family_ :: IsMIDICIDeviceInfo midiciDeviceInfo => midiciDeviceInfo -> IO (Id NSData)
family_ midiciDeviceInfo =
  sendMessage midiciDeviceInfo familySelector

-- | @- modelNumber@
modelNumber :: IsMIDICIDeviceInfo midiciDeviceInfo => midiciDeviceInfo -> IO (Id NSData)
modelNumber midiciDeviceInfo =
  sendMessage midiciDeviceInfo modelNumberSelector

-- | @- revisionLevel@
revisionLevel :: IsMIDICIDeviceInfo midiciDeviceInfo => midiciDeviceInfo -> IO (Id NSData)
revisionLevel midiciDeviceInfo =
  sendMessage midiciDeviceInfo revisionLevelSelector

-- | @- midiDestination@
midiDestination :: IsMIDICIDeviceInfo midiciDeviceInfo => midiciDeviceInfo -> IO CUInt
midiDestination midiciDeviceInfo =
  sendMessage midiciDeviceInfo midiDestinationSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MIDICIDeviceInfo)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithDestination:manufacturer:family:model:revision:@
initWithDestination_manufacturer_family_model_revisionSelector :: Selector '[CUInt, Id NSData, Id NSData, Id NSData, Id NSData] (Id MIDICIDeviceInfo)
initWithDestination_manufacturer_family_model_revisionSelector = mkSelector "initWithDestination:manufacturer:family:model:revision:"

-- | @Selector@ for @manufacturerID@
manufacturerIDSelector :: Selector '[] (Id NSData)
manufacturerIDSelector = mkSelector "manufacturerID"

-- | @Selector@ for @family@
familySelector :: Selector '[] (Id NSData)
familySelector = mkSelector "family"

-- | @Selector@ for @modelNumber@
modelNumberSelector :: Selector '[] (Id NSData)
modelNumberSelector = mkSelector "modelNumber"

-- | @Selector@ for @revisionLevel@
revisionLevelSelector :: Selector '[] (Id NSData)
revisionLevelSelector = mkSelector "revisionLevel"

-- | @Selector@ for @midiDestination@
midiDestinationSelector :: Selector '[] CUInt
midiDestinationSelector = mkSelector "midiDestination"

