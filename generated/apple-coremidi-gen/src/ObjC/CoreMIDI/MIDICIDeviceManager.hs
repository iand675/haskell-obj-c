{-# LANGUAGE DataKinds #-}
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
  , sharedInstance
  , discoveredCIDevices
  , discoveredCIDevicesSelector
  , sharedInstanceSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreMIDI.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | sharedInstance
--
-- Retrieve the shared MIDI-CI device manager for the client process.
--
-- After the first access of the property, the client process may observe notifications which are				posted when the system-wide cache changes. In environments where virtual MIDI endpoint				creation is not allowed, callbacks are only invoked when the process is not suspended.				However, any suspended process will receive an updated copy of the cache when it				resumes its running state.
--
-- ObjC selector: @+ sharedInstance@
sharedInstance :: IO (Id MIDICIDeviceManager)
sharedInstance  =
  do
    cls' <- getRequiredClass "MIDICIDeviceManager"
    sendClassMessage cls' sharedInstanceSelector

-- | discoveredCIDevices
--
-- A list of MIDICIDevices that responded to the last MIDI-CI discovery request.
--
-- ObjC selector: @- discoveredCIDevices@
discoveredCIDevices :: IsMIDICIDeviceManager midiciDeviceManager => midiciDeviceManager -> IO (Id NSArray)
discoveredCIDevices midiciDeviceManager =
  sendMessage midiciDeviceManager discoveredCIDevicesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sharedInstance@
sharedInstanceSelector :: Selector '[] (Id MIDICIDeviceManager)
sharedInstanceSelector = mkSelector "sharedInstance"

-- | @Selector@ for @discoveredCIDevices@
discoveredCIDevicesSelector :: Selector '[] (Id NSArray)
discoveredCIDevicesSelector = mkSelector "discoveredCIDevices"

