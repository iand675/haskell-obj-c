{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MIDI2DeviceInfo
--
-- An NSObject containing basic information about a MIDI 2.0 device. Used by				MIDIUMPEndpointPair and MIDICIDevice.
--
-- Generated bindings for @MIDI2DeviceInfo@.
module ObjC.CoreMIDI.MIDI2DeviceInfo
  ( MIDI2DeviceInfo
  , IsMIDI2DeviceInfo(..)
  , init_
  , family_
  , modelNumber
  , familySelector
  , initSelector
  , modelNumberSelector


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
init_ :: IsMIDI2DeviceInfo midI2DeviceInfo => midI2DeviceInfo -> IO (Id MIDI2DeviceInfo)
init_ midI2DeviceInfo =
  sendOwnedMessage midI2DeviceInfo initSelector

-- | family
--
-- The family of models to which the device belongs, up to 14 bits.
--
-- ObjC selector: @- family@
family_ :: IsMIDI2DeviceInfo midI2DeviceInfo => midI2DeviceInfo -> IO CUShort
family_ midI2DeviceInfo =
  sendMessage midI2DeviceInfo familySelector

-- | modelNumber
--
-- The specific model from the device manufacturer, up to 14 bits.
--
-- ObjC selector: @- modelNumber@
modelNumber :: IsMIDI2DeviceInfo midI2DeviceInfo => midI2DeviceInfo -> IO CUShort
modelNumber midI2DeviceInfo =
  sendMessage midI2DeviceInfo modelNumberSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MIDI2DeviceInfo)
initSelector = mkSelector "init"

-- | @Selector@ for @family@
familySelector :: Selector '[] CUShort
familySelector = mkSelector "family"

-- | @Selector@ for @modelNumber@
modelNumberSelector :: Selector '[] CUShort
modelNumberSelector = mkSelector "modelNumber"

