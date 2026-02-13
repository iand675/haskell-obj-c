{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Virtio Console Port Configuration Array
--
-- This array stores a collection of port configurations for a VZVirtioConsoleConfiguration. The index in the array corresponds to the port index used in the virtual machine.
--
-- A maximumPortCount value may be set but must be larger than the highest indexed port. If no maximumPortCount value is set, the highest indexed port will be used.
--
-- See: VZVirtioConsoleConfiguration
--
-- See: VZVirtioConsolePortConfiguration
--
-- Generated bindings for @VZVirtioConsolePortConfigurationArray@.
module ObjC.Virtualization.VZVirtioConsolePortConfigurationArray
  ( VZVirtioConsolePortConfigurationArray
  , IsVZVirtioConsolePortConfigurationArray(..)
  , new
  , init_
  , objectAtIndexedSubscript
  , setObject_atIndexedSubscript
  , maximumPortCount
  , setMaximumPortCount
  , initSelector
  , maximumPortCountSelector
  , newSelector
  , objectAtIndexedSubscriptSelector
  , setMaximumPortCountSelector
  , setObject_atIndexedSubscriptSelector


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
new :: IO (Id VZVirtioConsolePortConfigurationArray)
new  =
  do
    cls' <- getRequiredClass "VZVirtioConsolePortConfigurationArray"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsVZVirtioConsolePortConfigurationArray vzVirtioConsolePortConfigurationArray => vzVirtioConsolePortConfigurationArray -> IO (Id VZVirtioConsolePortConfigurationArray)
init_ vzVirtioConsolePortConfigurationArray =
  sendOwnedMessage vzVirtioConsolePortConfigurationArray initSelector

-- | Get a port configuration at the specified index.
--
-- ObjC selector: @- objectAtIndexedSubscript:@
objectAtIndexedSubscript :: IsVZVirtioConsolePortConfigurationArray vzVirtioConsolePortConfigurationArray => vzVirtioConsolePortConfigurationArray -> CULong -> IO (Id VZVirtioConsolePortConfiguration)
objectAtIndexedSubscript vzVirtioConsolePortConfigurationArray portIndex =
  sendMessage vzVirtioConsolePortConfigurationArray objectAtIndexedSubscriptSelector portIndex

-- | Set a port configuration at the specified index.
--
-- ObjC selector: @- setObject:atIndexedSubscript:@
setObject_atIndexedSubscript :: (IsVZVirtioConsolePortConfigurationArray vzVirtioConsolePortConfigurationArray, IsVZVirtioConsolePortConfiguration configuration) => vzVirtioConsolePortConfigurationArray -> configuration -> CULong -> IO ()
setObject_atIndexedSubscript vzVirtioConsolePortConfigurationArray configuration portIndex =
  sendMessage vzVirtioConsolePortConfigurationArray setObject_atIndexedSubscriptSelector (toVZVirtioConsolePortConfiguration configuration) portIndex

-- | The maximum number of ports allocated by this device. The default is the number of ports attached to this device.
--
-- ObjC selector: @- maximumPortCount@
maximumPortCount :: IsVZVirtioConsolePortConfigurationArray vzVirtioConsolePortConfigurationArray => vzVirtioConsolePortConfigurationArray -> IO CUInt
maximumPortCount vzVirtioConsolePortConfigurationArray =
  sendMessage vzVirtioConsolePortConfigurationArray maximumPortCountSelector

-- | The maximum number of ports allocated by this device. The default is the number of ports attached to this device.
--
-- ObjC selector: @- setMaximumPortCount:@
setMaximumPortCount :: IsVZVirtioConsolePortConfigurationArray vzVirtioConsolePortConfigurationArray => vzVirtioConsolePortConfigurationArray -> CUInt -> IO ()
setMaximumPortCount vzVirtioConsolePortConfigurationArray value =
  sendMessage vzVirtioConsolePortConfigurationArray setMaximumPortCountSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id VZVirtioConsolePortConfigurationArray)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VZVirtioConsolePortConfigurationArray)
initSelector = mkSelector "init"

-- | @Selector@ for @objectAtIndexedSubscript:@
objectAtIndexedSubscriptSelector :: Selector '[CULong] (Id VZVirtioConsolePortConfiguration)
objectAtIndexedSubscriptSelector = mkSelector "objectAtIndexedSubscript:"

-- | @Selector@ for @setObject:atIndexedSubscript:@
setObject_atIndexedSubscriptSelector :: Selector '[Id VZVirtioConsolePortConfiguration, CULong] ()
setObject_atIndexedSubscriptSelector = mkSelector "setObject:atIndexedSubscript:"

-- | @Selector@ for @maximumPortCount@
maximumPortCountSelector :: Selector '[] CUInt
maximumPortCountSelector = mkSelector "maximumPortCount"

-- | @Selector@ for @setMaximumPortCount:@
setMaximumPortCountSelector :: Selector '[CUInt] ()
setMaximumPortCountSelector = mkSelector "setMaximumPortCount:"

