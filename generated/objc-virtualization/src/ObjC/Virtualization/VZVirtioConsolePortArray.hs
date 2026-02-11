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
  , newSelector
  , initSelector
  , objectAtIndexedSubscriptSelector
  , maximumPortCountSelector


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
new :: IO (Id VZVirtioConsolePortArray)
new  =
  do
    cls' <- getRequiredClass "VZVirtioConsolePortArray"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsVZVirtioConsolePortArray vzVirtioConsolePortArray => vzVirtioConsolePortArray -> IO (Id VZVirtioConsolePortArray)
init_ vzVirtioConsolePortArray  =
  sendMsg vzVirtioConsolePortArray (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Get a port at the specified index.
--
-- ObjC selector: @- objectAtIndexedSubscript:@
objectAtIndexedSubscript :: IsVZVirtioConsolePortArray vzVirtioConsolePortArray => vzVirtioConsolePortArray -> CULong -> IO (Id VZVirtioConsolePort)
objectAtIndexedSubscript vzVirtioConsolePortArray  portIndex =
  sendMsg vzVirtioConsolePortArray (mkSelector "objectAtIndexedSubscript:") (retPtr retVoid) [argCULong (fromIntegral portIndex)] >>= retainedObject . castPtr

-- | The maximum number of ports allocated by this device.
--
-- ObjC selector: @- maximumPortCount@
maximumPortCount :: IsVZVirtioConsolePortArray vzVirtioConsolePortArray => vzVirtioConsolePortArray -> IO CUInt
maximumPortCount vzVirtioConsolePortArray  =
  sendMsg vzVirtioConsolePortArray (mkSelector "maximumPortCount") retCUInt []

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

-- | @Selector@ for @maximumPortCount@
maximumPortCountSelector :: Selector
maximumPortCountSelector = mkSelector "maximumPortCount"

