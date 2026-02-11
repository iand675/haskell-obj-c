{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Base class for a storage device configuration.
--
-- VZStorageDeviceConfiguration should not be instantiated directly.    One of its subclasses like VZVirtioBlockDeviceConfiguration should be used instead.
--
-- See: VZNVMExpressControllerDeviceConfiguration
--
-- See: VZUSBMassStorageDeviceConfiguration
--
-- See: VZVirtioBlockDeviceConfiguration
--
-- Generated bindings for @VZStorageDeviceConfiguration@.
module ObjC.Virtualization.VZStorageDeviceConfiguration
  ( VZStorageDeviceConfiguration
  , IsVZStorageDeviceConfiguration(..)
  , new
  , init_
  , attachment
  , newSelector
  , initSelector
  , attachmentSelector


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
new :: IO (Id VZStorageDeviceConfiguration)
new  =
  do
    cls' <- getRequiredClass "VZStorageDeviceConfiguration"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsVZStorageDeviceConfiguration vzStorageDeviceConfiguration => vzStorageDeviceConfiguration -> IO (Id VZStorageDeviceConfiguration)
init_ vzStorageDeviceConfiguration  =
  sendMsg vzStorageDeviceConfiguration (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Storage device attachment. Defines what local resource is exposed to the virtual machine as a disk.
--
-- See: VZDiskImageStorageDeviceAttachment
--
-- ObjC selector: @- attachment@
attachment :: IsVZStorageDeviceConfiguration vzStorageDeviceConfiguration => vzStorageDeviceConfiguration -> IO (Id VZStorageDeviceAttachment)
attachment vzStorageDeviceConfiguration  =
  sendMsg vzStorageDeviceConfiguration (mkSelector "attachment") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @attachment@
attachmentSelector :: Selector
attachmentSelector = mkSelector "attachment"

