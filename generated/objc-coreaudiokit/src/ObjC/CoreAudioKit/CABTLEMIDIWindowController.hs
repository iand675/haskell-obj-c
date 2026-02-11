{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | CABTLEMIDIWindowController
--
-- A window controller object that can present a window that displays nearby Bluetooth-based MIDI peripherals. The user can select one of those peripherals and pair it with their mac. Additionally, the user can advertise the mac as a Bluetooth-based MIDI peripheral.
--
-- To use this class, create an instance of the CABTLEMIDIWindowController, initialize it, and call showWindow: to display the UI.
--
-- Generated bindings for @CABTLEMIDIWindowController@.
module ObjC.CoreAudioKit.CABTLEMIDIWindowController
  ( CABTLEMIDIWindowController
  , IsCABTLEMIDIWindowController(..)


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

import ObjC.CoreAudioKit.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

