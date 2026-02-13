{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Configuration for a Mac graphics device.
--
-- This device can be used to attach a display to be shown in a VZVirtualMachineView.
--
-- Generated bindings for @VZMacGraphicsDeviceConfiguration@.
module ObjC.Virtualization.VZMacGraphicsDeviceConfiguration
  ( VZMacGraphicsDeviceConfiguration
  , IsVZMacGraphicsDeviceConfiguration(..)
  , init_
  , displays
  , setDisplays
  , displaysSelector
  , initSelector
  , setDisplaysSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Virtualization.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsVZMacGraphicsDeviceConfiguration vzMacGraphicsDeviceConfiguration => vzMacGraphicsDeviceConfiguration -> IO (Id VZMacGraphicsDeviceConfiguration)
init_ vzMacGraphicsDeviceConfiguration =
  sendOwnedMessage vzMacGraphicsDeviceConfiguration initSelector

-- | The displays to be attached to this graphics device.
--
-- Maximum of one display is supported.
--
-- ObjC selector: @- displays@
displays :: IsVZMacGraphicsDeviceConfiguration vzMacGraphicsDeviceConfiguration => vzMacGraphicsDeviceConfiguration -> IO (Id NSArray)
displays vzMacGraphicsDeviceConfiguration =
  sendMessage vzMacGraphicsDeviceConfiguration displaysSelector

-- | The displays to be attached to this graphics device.
--
-- Maximum of one display is supported.
--
-- ObjC selector: @- setDisplays:@
setDisplays :: (IsVZMacGraphicsDeviceConfiguration vzMacGraphicsDeviceConfiguration, IsNSArray value) => vzMacGraphicsDeviceConfiguration -> value -> IO ()
setDisplays vzMacGraphicsDeviceConfiguration value =
  sendMessage vzMacGraphicsDeviceConfiguration setDisplaysSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VZMacGraphicsDeviceConfiguration)
initSelector = mkSelector "init"

-- | @Selector@ for @displays@
displaysSelector :: Selector '[] (Id NSArray)
displaysSelector = mkSelector "displays"

-- | @Selector@ for @setDisplays:@
setDisplaysSelector :: Selector '[Id NSArray] ()
setDisplaysSelector = mkSelector "setDisplays:"

