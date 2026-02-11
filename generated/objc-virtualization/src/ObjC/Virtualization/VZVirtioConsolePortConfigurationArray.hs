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
  , newSelector
  , initSelector
  , objectAtIndexedSubscriptSelector
  , setObject_atIndexedSubscriptSelector
  , maximumPortCountSelector
  , setMaximumPortCountSelector


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

-- | @+ new@
new :: IO (Id VZVirtioConsolePortConfigurationArray)
new  =
  do
    cls' <- getRequiredClass "VZVirtioConsolePortConfigurationArray"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsVZVirtioConsolePortConfigurationArray vzVirtioConsolePortConfigurationArray => vzVirtioConsolePortConfigurationArray -> IO (Id VZVirtioConsolePortConfigurationArray)
init_ vzVirtioConsolePortConfigurationArray  =
  sendMsg vzVirtioConsolePortConfigurationArray (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Get a port configuration at the specified index.
--
-- ObjC selector: @- objectAtIndexedSubscript:@
objectAtIndexedSubscript :: IsVZVirtioConsolePortConfigurationArray vzVirtioConsolePortConfigurationArray => vzVirtioConsolePortConfigurationArray -> CULong -> IO (Id VZVirtioConsolePortConfiguration)
objectAtIndexedSubscript vzVirtioConsolePortConfigurationArray  portIndex =
  sendMsg vzVirtioConsolePortConfigurationArray (mkSelector "objectAtIndexedSubscript:") (retPtr retVoid) [argCULong (fromIntegral portIndex)] >>= retainedObject . castPtr

-- | Set a port configuration at the specified index.
--
-- ObjC selector: @- setObject:atIndexedSubscript:@
setObject_atIndexedSubscript :: (IsVZVirtioConsolePortConfigurationArray vzVirtioConsolePortConfigurationArray, IsVZVirtioConsolePortConfiguration configuration) => vzVirtioConsolePortConfigurationArray -> configuration -> CULong -> IO ()
setObject_atIndexedSubscript vzVirtioConsolePortConfigurationArray  configuration portIndex =
withObjCPtr configuration $ \raw_configuration ->
    sendMsg vzVirtioConsolePortConfigurationArray (mkSelector "setObject:atIndexedSubscript:") retVoid [argPtr (castPtr raw_configuration :: Ptr ()), argCULong (fromIntegral portIndex)]

-- | The maximum number of ports allocated by this device. The default is the number of ports attached to this device.
--
-- ObjC selector: @- maximumPortCount@
maximumPortCount :: IsVZVirtioConsolePortConfigurationArray vzVirtioConsolePortConfigurationArray => vzVirtioConsolePortConfigurationArray -> IO CUInt
maximumPortCount vzVirtioConsolePortConfigurationArray  =
  sendMsg vzVirtioConsolePortConfigurationArray (mkSelector "maximumPortCount") retCUInt []

-- | The maximum number of ports allocated by this device. The default is the number of ports attached to this device.
--
-- ObjC selector: @- setMaximumPortCount:@
setMaximumPortCount :: IsVZVirtioConsolePortConfigurationArray vzVirtioConsolePortConfigurationArray => vzVirtioConsolePortConfigurationArray -> CUInt -> IO ()
setMaximumPortCount vzVirtioConsolePortConfigurationArray  value =
  sendMsg vzVirtioConsolePortConfigurationArray (mkSelector "setMaximumPortCount:") retVoid [argCUInt (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @objectAtIndexedSubscript:@
objectAtIndexedSubscriptSelector :: Selector
objectAtIndexedSubscriptSelector = mkSelector "objectAtIndexedSubscript:"

-- | @Selector@ for @setObject:atIndexedSubscript:@
setObject_atIndexedSubscriptSelector :: Selector
setObject_atIndexedSubscriptSelector = mkSelector "setObject:atIndexedSubscript:"

-- | @Selector@ for @maximumPortCount@
maximumPortCountSelector :: Selector
maximumPortCountSelector = mkSelector "maximumPortCount"

-- | @Selector@ for @setMaximumPortCount:@
setMaximumPortCountSelector :: Selector
setMaximumPortCountSelector = mkSelector "setMaximumPortCount:"

