{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MIDICIDeviceManager
--
-- A singleton object that performs system-wide MIDI-CI Device bookkeeping.
--
-- MIDICIDeviceManager is used to retrieve information about MIDI-CI devices that				to MIDI-CI Discovery.
--
-- Generated bindings for @MIDICIDeviceManager@.
module ObjC.CoreMIDI.MIDICIDeviceManager
  ( MIDICIDeviceManager
  , IsMIDICIDeviceManager(..)
  , discoveredCIDevices
  , discoveredCIDevicesSelector


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

-- | discoveredCIDevices
--
-- A list of MIDICIDevices that responded to the last MIDI-CI discovery request.
--
-- ObjC selector: @- discoveredCIDevices@
discoveredCIDevices :: IsMIDICIDeviceManager midiciDeviceManager => midiciDeviceManager -> IO (Id NSArray)
discoveredCIDevices midiciDeviceManager  =
  sendMsg midiciDeviceManager (mkSelector "discoveredCIDevices") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @discoveredCIDevices@
discoveredCIDevicesSelector :: Selector
discoveredCIDevicesSelector = mkSelector "discoveredCIDevices"

