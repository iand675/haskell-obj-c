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
  , initSelector
  , displaysSelector
  , setDisplaysSelector


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

import ObjC.Virtualization.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsVZMacGraphicsDeviceConfiguration vzMacGraphicsDeviceConfiguration => vzMacGraphicsDeviceConfiguration -> IO (Id VZMacGraphicsDeviceConfiguration)
init_ vzMacGraphicsDeviceConfiguration  =
  sendMsg vzMacGraphicsDeviceConfiguration (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The displays to be attached to this graphics device.
--
-- Maximum of one display is supported.
--
-- ObjC selector: @- displays@
displays :: IsVZMacGraphicsDeviceConfiguration vzMacGraphicsDeviceConfiguration => vzMacGraphicsDeviceConfiguration -> IO (Id NSArray)
displays vzMacGraphicsDeviceConfiguration  =
  sendMsg vzMacGraphicsDeviceConfiguration (mkSelector "displays") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The displays to be attached to this graphics device.
--
-- Maximum of one display is supported.
--
-- ObjC selector: @- setDisplays:@
setDisplays :: (IsVZMacGraphicsDeviceConfiguration vzMacGraphicsDeviceConfiguration, IsNSArray value) => vzMacGraphicsDeviceConfiguration -> value -> IO ()
setDisplays vzMacGraphicsDeviceConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg vzMacGraphicsDeviceConfiguration (mkSelector "setDisplays:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @displays@
displaysSelector :: Selector
displaysSelector = mkSelector "displays"

-- | @Selector@ for @setDisplays:@
setDisplaysSelector :: Selector
setDisplaysSelector = mkSelector "setDisplays:"

