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
  , initSelector
  , familySelector
  , modelNumberSelector


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
init_ :: IsMIDI2DeviceInfo midI2DeviceInfo => midI2DeviceInfo -> IO (Id MIDI2DeviceInfo)
init_ midI2DeviceInfo  =
  sendMsg midI2DeviceInfo (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | family
--
-- The family of models to which the device belongs, up to 14 bits.
--
-- ObjC selector: @- family@
family_ :: IsMIDI2DeviceInfo midI2DeviceInfo => midI2DeviceInfo -> IO CUShort
family_ midI2DeviceInfo  =
  fmap fromIntegral $ sendMsg midI2DeviceInfo (mkSelector "family") retCUInt []

-- | modelNumber
--
-- The specific model from the device manufacturer, up to 14 bits.
--
-- ObjC selector: @- modelNumber@
modelNumber :: IsMIDI2DeviceInfo midI2DeviceInfo => midI2DeviceInfo -> IO CUShort
modelNumber midI2DeviceInfo  =
  fmap fromIntegral $ sendMsg midI2DeviceInfo (mkSelector "modelNumber") retCUInt []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @family@
familySelector :: Selector
familySelector = mkSelector "family"

-- | @Selector@ for @modelNumber@
modelNumberSelector :: Selector
modelNumberSelector = mkSelector "modelNumber"

