{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | VZMacOSRestoreImage describes a version of macOS to be installed to a virtual machine.
--
-- A VZMacOSRestoreImage object can be created by loading an installation media file. A VZMacOSInstaller    object must be initialized with this VZMacOSRestoreImage object in order to install the operating    system onto a virtual machine.
--
-- Loading a restore image requires the app to have the "com.apple.security.virtualization" entitlement.
--
-- VZMacHardwareModel
--
-- VZMacOSInstaller
--
-- VZMacOSConfigurationRequirements
--
-- Generated bindings for @VZMacOSRestoreImage@.
module ObjC.Virtualization.VZMacOSRestoreImage
  ( VZMacOSRestoreImage
  , IsVZMacOSRestoreImage(..)
  , new
  , init_
  , loadFileURL_completionHandler
  , fetchLatestSupportedWithCompletionHandler
  , supported
  , url
  , buildVersion
  , mostFeaturefulSupportedConfiguration
  , newSelector
  , initSelector
  , loadFileURL_completionHandlerSelector
  , fetchLatestSupportedWithCompletionHandlerSelector
  , supportedSelector
  , urlSelector
  , buildVersionSelector
  , mostFeaturefulSupportedConfigurationSelector


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
new :: IO (Id VZMacOSRestoreImage)
new  =
  do
    cls' <- getRequiredClass "VZMacOSRestoreImage"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsVZMacOSRestoreImage vzMacOSRestoreImage => vzMacOSRestoreImage -> IO (Id VZMacOSRestoreImage)
init_ vzMacOSRestoreImage  =
  sendMsg vzMacOSRestoreImage (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Load a restore image from a file on the local file system.
--
-- @fileURL@ — A file URL indicating the macOS restore image to load.
--
-- @completionHandler@ — Block called after the restore image has successfully loaded or has failed to load.    The error parameter passed to the block is nil if the restore image was loaded successfully.    The completion handler will be invoked on an arbitrary thread.
--
-- VZMacOSRestoreImage can load IPSW installation media from a local file. If the fileURL parameter does not refer to    a local file, an exception will be raised.
--
-- ObjC selector: @+ loadFileURL:completionHandler:@
loadFileURL_completionHandler :: IsNSURL fileURL => fileURL -> Ptr () -> IO ()
loadFileURL_completionHandler fileURL completionHandler =
  do
    cls' <- getRequiredClass "VZMacOSRestoreImage"
    withObjCPtr fileURL $ \raw_fileURL ->
      sendClassMsg cls' (mkSelector "loadFileURL:completionHandler:") retVoid [argPtr (castPtr raw_fileURL :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | Fetch the latest restore image supported by this host from the network.
--
-- @completionHandler@ — Block called after the restore image fetch has succeeded or failed.    The error parameter passed to the block is nil if the restore image was fetched successfully.    The completion handler will be invoked on an arbitrary thread.
--
-- A VZMacOSInstaller object must be constructed with a VZMacOSRestoreImage loaded from a file on the local    filesystem. A VZMacOSRestoreImage fetched with the fetchLatestSupportedWithCompletionHandler method    will have a URL property referring to a restore image on the network. To use such a restore image, the    file referred to by the URL property should be downloaded locally (using NSURLSession or similar API). After    the restore image has been downloaded, a VZMacOSInstaller can be initialized using a URL referring to the    local file.
--
-- ObjC selector: @+ fetchLatestSupportedWithCompletionHandler:@
fetchLatestSupportedWithCompletionHandler :: Ptr () -> IO ()
fetchLatestSupportedWithCompletionHandler completionHandler =
  do
    cls' <- getRequiredClass "VZMacOSRestoreImage"
    sendClassMsg cls' (mkSelector "fetchLatestSupportedWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | Whether this restore image is supported on the current host.
--
-- ObjC selector: @- supported@
supported :: IsVZMacOSRestoreImage vzMacOSRestoreImage => vzMacOSRestoreImage -> IO Bool
supported vzMacOSRestoreImage  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg vzMacOSRestoreImage (mkSelector "supported") retCULong []

-- | The URL of this restore image.
--
-- If the restore image was loaded using +[VZMacOSRestoreImage loadFileURL:completionHandler:], the value of this property will be a file URL.    If the restore image was fetched using +[VZMacOSRestoreImage fetchLatestSupportedWithCompletionHandler:],    the value of this property will be a network URL referring to an installation media file.
--
-- ObjC selector: @- URL@
url :: IsVZMacOSRestoreImage vzMacOSRestoreImage => vzMacOSRestoreImage -> IO (Id NSURL)
url vzMacOSRestoreImage  =
  sendMsg vzMacOSRestoreImage (mkSelector "URL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The build version this restore image contains.
--
-- ObjC selector: @- buildVersion@
buildVersion :: IsVZMacOSRestoreImage vzMacOSRestoreImage => vzMacOSRestoreImage -> IO (Id NSString)
buildVersion vzMacOSRestoreImage  =
  sendMsg vzMacOSRestoreImage (mkSelector "buildVersion") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The configuration requirements for the most featureful configuration supported by the current host and by this restore image.
--
-- A VZMacOSRestoreImage can contain installation media for multiple Mac hardware models (VZMacHardwareModel). Some of these    hardware models may not be supported by the current host. The mostFeaturefulSupportedConfiguration property can be used to    determine the hardware model and configuration requirements that will provide the most complete feature set on the current    host. If none of the hardware models are supported on the current host, this property is nil.
--
-- ObjC selector: @- mostFeaturefulSupportedConfiguration@
mostFeaturefulSupportedConfiguration :: IsVZMacOSRestoreImage vzMacOSRestoreImage => vzMacOSRestoreImage -> IO (Id VZMacOSConfigurationRequirements)
mostFeaturefulSupportedConfiguration vzMacOSRestoreImage  =
  sendMsg vzMacOSRestoreImage (mkSelector "mostFeaturefulSupportedConfiguration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @loadFileURL:completionHandler:@
loadFileURL_completionHandlerSelector :: Selector
loadFileURL_completionHandlerSelector = mkSelector "loadFileURL:completionHandler:"

-- | @Selector@ for @fetchLatestSupportedWithCompletionHandler:@
fetchLatestSupportedWithCompletionHandlerSelector :: Selector
fetchLatestSupportedWithCompletionHandlerSelector = mkSelector "fetchLatestSupportedWithCompletionHandler:"

-- | @Selector@ for @supported@
supportedSelector :: Selector
supportedSelector = mkSelector "supported"

-- | @Selector@ for @URL@
urlSelector :: Selector
urlSelector = mkSelector "URL"

-- | @Selector@ for @buildVersion@
buildVersionSelector :: Selector
buildVersionSelector = mkSelector "buildVersion"

-- | @Selector@ for @mostFeaturefulSupportedConfiguration@
mostFeaturefulSupportedConfigurationSelector :: Selector
mostFeaturefulSupportedConfigurationSelector = mkSelector "mostFeaturefulSupportedConfiguration"

