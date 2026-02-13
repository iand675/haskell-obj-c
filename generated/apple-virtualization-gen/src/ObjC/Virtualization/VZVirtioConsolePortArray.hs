{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Virtio Console Port Array
--
-- This array stores a collection of ports configured for use by a VZVirtioConsoleDevice. VZVirtioConsolePort objects may be retrieved by index.
--
-- See: VZVirtioConsoleDevice
--
-- See: VZVirtioConsolePort
--
-- Generated bindings for @VZVirtioConsolePortArray@.
module ObjC.Virtualization.VZVirtioConsolePortArray
  ( VZVirtioConsolePortArray
  , IsVZVirtioConsolePortArray(..)
  , new
  , init_
  , objectAtIndexedSubscript
  , maximumPortCount
  , initSelector
  , maximumPortCountSelector
  , newSelector
  , objectAtIndexedSubscriptSelector


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
new :: IO (Id VZVirtioConsolePortArray)
new  =
  do
    cls' <- getRequiredClass "VZVirtioConsolePortArray"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsVZVirtioConsolePortArray vzVirtioConsolePortArray => vzVirtioConsolePortArray -> IO (Id VZVirtioConsolePortArray)
init_ vzVirtioConsolePortArray =
  sendOwnedMessage vzVirtioConsolePortArray initSelector

-- | Get a port at the specified index.
--
-- ObjC selector: @- objectAtIndexedSubscript:@
objectAtIndexedSubscript :: IsVZVirtioConsolePortArray vzVirtioConsolePortArray => vzVirtioConsolePortArray -> CULong -> IO (Id VZVirtioConsolePort)
objectAtIndexedSubscript vzVirtioConsolePortArray portIndex =
  sendMessage vzVirtioConsolePortArray objectAtIndexedSubscriptSelector portIndex

-- | The maximum number of ports allocated by this device.
--
-- ObjC selector: @- maximumPortCount@
maximumPortCount :: IsVZVirtioConsolePortArray vzVirtioConsolePortArray => vzVirtioConsolePortArray -> IO CUInt
maximumPortCount vzVirtioConsolePortArray =
  sendMessage vzVirtioConsolePortArray maximumPortCountSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id VZVirtioConsolePortArray)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VZVirtioConsolePortArray)
initSelector = mkSelector "init"

-- | @Selector@ for @objectAtIndexedSubscript:@
objectAtIndexedSubscriptSelector :: Selector '[CULong] (Id VZVirtioConsolePort)
objectAtIndexedSubscriptSelector = mkSelector "objectAtIndexedSubscript:"

-- | @Selector@ for @maximumPortCount@
maximumPortCountSelector :: Selector '[] CUInt
maximumPortCountSelector = mkSelector "maximumPortCount"

