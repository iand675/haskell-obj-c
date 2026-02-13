{-# LANGUAGE DataKinds #-}
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
  , addObserverSelector
  , initSelector
  , newSelector
  , reconfigureWithConfiguration_errorSelector
  , removeObserverSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Virtualization.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id VZGraphicsDisplay)
new  =
  do
    cls' <- getRequiredClass "VZGraphicsDisplay"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsVZGraphicsDisplay vzGraphicsDisplay => vzGraphicsDisplay -> IO (Id VZGraphicsDisplay)
init_ vzGraphicsDisplay =
  sendOwnedMessage vzGraphicsDisplay initSelector

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
reconfigureWithConfiguration_error vzGraphicsDisplay configuration error_ =
  sendMessage vzGraphicsDisplay reconfigureWithConfiguration_errorSelector (toVZGraphicsDisplayConfiguration configuration) (toNSError error_)

-- | Add an observer.
--
-- @observer@ — The new observer to be notified of display state changes.
--
-- ObjC selector: @- addObserver:@
addObserver :: IsVZGraphicsDisplay vzGraphicsDisplay => vzGraphicsDisplay -> RawId -> IO ()
addObserver vzGraphicsDisplay observer =
  sendMessage vzGraphicsDisplay addObserverSelector observer

-- | Remove an observer.
--
-- @observer@ — The observer to be removed.
--
-- ObjC selector: @- removeObserver:@
removeObserver :: IsVZGraphicsDisplay vzGraphicsDisplay => vzGraphicsDisplay -> RawId -> IO ()
removeObserver vzGraphicsDisplay observer =
  sendMessage vzGraphicsDisplay removeObserverSelector observer

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id VZGraphicsDisplay)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VZGraphicsDisplay)
initSelector = mkSelector "init"

-- | @Selector@ for @reconfigureWithConfiguration:error:@
reconfigureWithConfiguration_errorSelector :: Selector '[Id VZGraphicsDisplayConfiguration, Id NSError] Bool
reconfigureWithConfiguration_errorSelector = mkSelector "reconfigureWithConfiguration:error:"

-- | @Selector@ for @addObserver:@
addObserverSelector :: Selector '[RawId] ()
addObserverSelector = mkSelector "addObserver:"

-- | @Selector@ for @removeObserver:@
removeObserverSelector :: Selector '[RawId] ()
removeObserverSelector = mkSelector "removeObserver:"

