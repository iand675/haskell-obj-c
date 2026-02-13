{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Configuration of a Virtio file system device.
--
-- This configuration creates a Virtio file system device which allows for exposing    directories on the host to a guest via a tag label.
--
-- Generated bindings for @VZVirtioFileSystemDeviceConfiguration@.
module ObjC.Virtualization.VZVirtioFileSystemDeviceConfiguration
  ( VZVirtioFileSystemDeviceConfiguration
  , IsVZVirtioFileSystemDeviceConfiguration(..)
  , initWithTag
  , validateTag_error
  , tag
  , setTag
  , share
  , setShare
  , macOSGuestAutomountTag
  , initWithTagSelector
  , macOSGuestAutomountTagSelector
  , setShareSelector
  , setTagSelector
  , shareSelector
  , tagSelector
  , validateTag_errorSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Virtualization.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Configuration of the Virtio file system device.
--
-- @tag@ — The label identifying this device in the guest.
--
-- The tag is presented as a label in the guest identifying this device for mounting. The tag must be valid, which can be checked with +[VZVirtioFileSystemDeviceConfiguration validateTag:error:].
--
-- See: +[VZVirtioFileSystemDeviceConfiguration validateTag:error:]
--
-- ObjC selector: @- initWithTag:@
initWithTag :: (IsVZVirtioFileSystemDeviceConfiguration vzVirtioFileSystemDeviceConfiguration, IsNSString tag) => vzVirtioFileSystemDeviceConfiguration -> tag -> IO (Id VZVirtioFileSystemDeviceConfiguration)
initWithTag vzVirtioFileSystemDeviceConfiguration tag =
  sendOwnedMessage vzVirtioFileSystemDeviceConfiguration initWithTagSelector (toNSString tag)

-- | Check if tag is a valid Virtio file system tag.
--
-- @tag@ — The tag to validate.
--
-- @error@ — If not nil, assigned with an error describing why the tag is not valid.
--
-- The tag must be non-empty and less than 36 bytes when encoded in UTF-8.
--
-- ObjC selector: @+ validateTag:error:@
validateTag_error :: (IsNSString tag, IsNSError error_) => tag -> error_ -> IO Bool
validateTag_error tag error_ =
  do
    cls' <- getRequiredClass "VZVirtioFileSystemDeviceConfiguration"
    sendClassMessage cls' validateTag_errorSelector (toNSString tag) (toNSError error_)

-- | The tag is a string identifying the device.
--
-- The tag is presented as a label in the guest identifying this device for mounting. The tag must be valid, which can be checked with +[VZVirtioFileSystemDeviceConfiguration validateTag:error:].
--
-- See: +[VZVirtioFileSystemDeviceConfiguration validateTag:error:]
--
-- ObjC selector: @- tag@
tag :: IsVZVirtioFileSystemDeviceConfiguration vzVirtioFileSystemDeviceConfiguration => vzVirtioFileSystemDeviceConfiguration -> IO (Id NSString)
tag vzVirtioFileSystemDeviceConfiguration =
  sendMessage vzVirtioFileSystemDeviceConfiguration tagSelector

-- | The tag is a string identifying the device.
--
-- The tag is presented as a label in the guest identifying this device for mounting. The tag must be valid, which can be checked with +[VZVirtioFileSystemDeviceConfiguration validateTag:error:].
--
-- See: +[VZVirtioFileSystemDeviceConfiguration validateTag:error:]
--
-- ObjC selector: @- setTag:@
setTag :: (IsVZVirtioFileSystemDeviceConfiguration vzVirtioFileSystemDeviceConfiguration, IsNSString value) => vzVirtioFileSystemDeviceConfiguration -> value -> IO ()
setTag vzVirtioFileSystemDeviceConfiguration value =
  sendMessage vzVirtioFileSystemDeviceConfiguration setTagSelector (toNSString value)

-- | Directory share. Defines how host resources are exposed to the guest virtual machine.
--
-- See: VZSingleDirectoryShare
--
-- See: VZMultipleDirectoryShare
--
-- See: VZLinuxRosettaDirectoryShare
--
-- ObjC selector: @- share@
share :: IsVZVirtioFileSystemDeviceConfiguration vzVirtioFileSystemDeviceConfiguration => vzVirtioFileSystemDeviceConfiguration -> IO (Id VZDirectoryShare)
share vzVirtioFileSystemDeviceConfiguration =
  sendMessage vzVirtioFileSystemDeviceConfiguration shareSelector

-- | Directory share. Defines how host resources are exposed to the guest virtual machine.
--
-- See: VZSingleDirectoryShare
--
-- See: VZMultipleDirectoryShare
--
-- See: VZLinuxRosettaDirectoryShare
--
-- ObjC selector: @- setShare:@
setShare :: (IsVZVirtioFileSystemDeviceConfiguration vzVirtioFileSystemDeviceConfiguration, IsVZDirectoryShare value) => vzVirtioFileSystemDeviceConfiguration -> value -> IO ()
setShare vzVirtioFileSystemDeviceConfiguration value =
  sendMessage vzVirtioFileSystemDeviceConfiguration setShareSelector (toVZDirectoryShare value)

-- | The macOS automount tag.
--
-- A device configured with this tag will be automatically mounted in a macOS guest.
--
-- ObjC selector: @+ macOSGuestAutomountTag@
macOSGuestAutomountTag :: IO (Id NSString)
macOSGuestAutomountTag  =
  do
    cls' <- getRequiredClass "VZVirtioFileSystemDeviceConfiguration"
    sendClassMessage cls' macOSGuestAutomountTagSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithTag:@
initWithTagSelector :: Selector '[Id NSString] (Id VZVirtioFileSystemDeviceConfiguration)
initWithTagSelector = mkSelector "initWithTag:"

-- | @Selector@ for @validateTag:error:@
validateTag_errorSelector :: Selector '[Id NSString, Id NSError] Bool
validateTag_errorSelector = mkSelector "validateTag:error:"

-- | @Selector@ for @tag@
tagSelector :: Selector '[] (Id NSString)
tagSelector = mkSelector "tag"

-- | @Selector@ for @setTag:@
setTagSelector :: Selector '[Id NSString] ()
setTagSelector = mkSelector "setTag:"

-- | @Selector@ for @share@
shareSelector :: Selector '[] (Id VZDirectoryShare)
shareSelector = mkSelector "share"

-- | @Selector@ for @setShare:@
setShareSelector :: Selector '[Id VZDirectoryShare] ()
setShareSelector = mkSelector "setShare:"

-- | @Selector@ for @macOSGuestAutomountTag@
macOSGuestAutomountTagSelector :: Selector '[] (Id NSString)
macOSGuestAutomountTagSelector = mkSelector "macOSGuestAutomountTag"

