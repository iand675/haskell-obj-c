{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | CAInterDeviceAudioViewController
--
-- A view controller object that manages a view displaying iOS devices that are connected to the Mac and support inter-device audio. The user can select one of those peripherals and connect it to their mac. This class is only available in 64-bit runtimes.
--
-- To use this class, create an instance of the CAInterDeviceAudioController, get the view and add it as a subview of a NSWindow.
--
-- Generated bindings for @CAInterDeviceAudioViewController@.
module ObjC.CoreAudioKit.CAInterDeviceAudioViewController
  ( CAInterDeviceAudioViewController
  , IsCAInterDeviceAudioViewController(..)


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreAudioKit.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

