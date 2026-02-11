{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Class representing a graphics display in a virtual machine.
--
-- VZGraphicsDisplay should not be instantiated directly.
--
-- Graphics displays are first configured on a VZGraphicsDeviceConfiguration subclass.    When a VZVirtualMachine is created from the configuration, the displays    are available through the VZGraphicsDevice's @displays@ property.
--
-- See: VZMacGraphicsDisplayConfiguration
--
-- See: VZVirtioGraphicsScanoutConfiguration
--
-- Generated bindings for @VZGraphicsDisplay@.
module ObjC.Virtualization.VZGraphicsDisplay
  ( VZGraphicsDisplay
  , IsVZGraphicsDisplay(..)
  , new
  , init_
  , reconfigureWithConfiguration_error
  , addObserver
  , removeObserver
  , newSelector
  , initSelector
  , reconfigureWithConfiguration_errorSelector
  , addObserverSelector
  , removeObserverSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Virtualization.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id VZGraphicsDisplay)
new  =
  do
    cls' <- getRequiredClass "VZGraphicsDisplay"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsVZGraphicsDisplay vzGraphicsDisplay => vzGraphicsDisplay -> IO (Id VZGraphicsDisplay)
init_ vzGraphicsDisplay  =
  sendMsg vzGraphicsDisplay (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Reconfigure this display.
--
-- @configuration@ — New display configuration.
--
-- @error@ — If not nil, assigned with an error describing why the new configuration is not valid.
--
-- Returns: YES if the reconfiguration is successful, NO otherwise.
--
-- The type of the configuration must match the corresponding type that caused this display to be created.
--
-- If successful, the new configuration will be passed to the guest but the guest may or may    not respond to parts of the configuration. The guest not using the new configuration does not    return an error.
--
-- Reconfiguration triggers a display state change which can be tracked by VZGraphicsDisplayObservers.
--
-- ObjC selector: @- reconfigureWithConfiguration:error:@
reconfigureWithConfiguration_error :: (IsVZGraphicsDisplay vzGraphicsDisplay, IsVZGraphicsDisplayConfiguration configuration, IsNSError error_) => vzGraphicsDisplay -> configuration -> error_ -> IO Bool
reconfigureWithConfiguration_error vzGraphicsDisplay  configuration error_ =
withObjCPtr configuration $ \raw_configuration ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg vzGraphicsDisplay (mkSelector "reconfigureWithConfiguration:error:") retCULong [argPtr (castPtr raw_configuration :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | Add an observer.
--
-- @observer@ — The new observer to be notified of display state changes.
--
-- ObjC selector: @- addObserver:@
addObserver :: IsVZGraphicsDisplay vzGraphicsDisplay => vzGraphicsDisplay -> RawId -> IO ()
addObserver vzGraphicsDisplay  observer =
  sendMsg vzGraphicsDisplay (mkSelector "addObserver:") retVoid [argPtr (castPtr (unRawId observer) :: Ptr ())]

-- | Remove an observer.
--
-- @observer@ — The observer to be removed.
--
-- ObjC selector: @- removeObserver:@
removeObserver :: IsVZGraphicsDisplay vzGraphicsDisplay => vzGraphicsDisplay -> RawId -> IO ()
removeObserver vzGraphicsDisplay  observer =
  sendMsg vzGraphicsDisplay (mkSelector "removeObserver:") retVoid [argPtr (castPtr (unRawId observer) :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @reconfigureWithConfiguration:error:@
reconfigureWithConfiguration_errorSelector :: Selector
reconfigureWithConfiguration_errorSelector = mkSelector "reconfigureWithConfiguration:error:"

-- | @Selector@ for @addObserver:@
addObserverSelector :: Selector
addObserverSelector = mkSelector "addObserver:"

-- | @Selector@ for @removeObserver:@
removeObserverSelector :: Selector
removeObserverSelector = mkSelector "removeObserver:"

