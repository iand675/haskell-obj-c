{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Note: Using the workspace API requires the system extension entitlement
--
-- Generated bindings for @OSSystemExtensionsWorkspace@.
module ObjC.SystemExtensions.OSSystemExtensionsWorkspace
  ( OSSystemExtensionsWorkspace
  , IsOSSystemExtensionsWorkspace(..)
  , addObserver_error
  , removeObserver
  , systemExtensionsForApplicationWithBundleID_error
  , sharedWorkspace
  , addObserver_errorSelector
  , removeObserverSelector
  , systemExtensionsForApplicationWithBundleID_errorSelector
  , sharedWorkspaceSelector


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

import ObjC.SystemExtensions.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Start observing changes to System Extension(s) which are enabled or ready to be enabled.
--
-- ObjC selector: @- addObserver:error:@
addObserver_error :: (IsOSSystemExtensionsWorkspace osSystemExtensionsWorkspace, IsNSError error_) => osSystemExtensionsWorkspace -> RawId -> error_ -> IO Bool
addObserver_error osSystemExtensionsWorkspace  observer error_ =
withObjCPtr error_ $ \raw_error_ ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg osSystemExtensionsWorkspace (mkSelector "addObserver:error:") retCULong [argPtr (castPtr (unRawId observer) :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | Stop observing changes to System Extension(s).
--
-- ObjC selector: @- removeObserver:@
removeObserver :: IsOSSystemExtensionsWorkspace osSystemExtensionsWorkspace => osSystemExtensionsWorkspace -> RawId -> IO ()
removeObserver osSystemExtensionsWorkspace  observer =
  sendMsg osSystemExtensionsWorkspace (mkSelector "removeObserver:") retVoid [argPtr (castPtr (unRawId observer) :: Ptr ())]

-- | Get information about system extension(s) in an app with a bundle identifier
--
-- @bundleID@ — BundleIdentifier of the application containing the system extension(s)
--
-- @out_error@ — Error parameter to be populated with relevant error information
--
-- Returns: A set of system extension property objects on success, nil otherwise.
--
-- ObjC selector: @- systemExtensionsForApplicationWithBundleID:error:@
systemExtensionsForApplicationWithBundleID_error :: (IsOSSystemExtensionsWorkspace osSystemExtensionsWorkspace, IsNSString bundleID, IsNSError out_error) => osSystemExtensionsWorkspace -> bundleID -> out_error -> IO (Id NSSet)
systemExtensionsForApplicationWithBundleID_error osSystemExtensionsWorkspace  bundleID out_error =
withObjCPtr bundleID $ \raw_bundleID ->
  withObjCPtr out_error $ \raw_out_error ->
      sendMsg osSystemExtensionsWorkspace (mkSelector "systemExtensionsForApplicationWithBundleID:error:") (retPtr retVoid) [argPtr (castPtr raw_bundleID :: Ptr ()), argPtr (castPtr raw_out_error :: Ptr ())] >>= retainedObject . castPtr

-- | @+ sharedWorkspace@
sharedWorkspace :: IO (Id OSSystemExtensionsWorkspace)
sharedWorkspace  =
  do
    cls' <- getRequiredClass "OSSystemExtensionsWorkspace"
    sendClassMsg cls' (mkSelector "sharedWorkspace") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @addObserver:error:@
addObserver_errorSelector :: Selector
addObserver_errorSelector = mkSelector "addObserver:error:"

-- | @Selector@ for @removeObserver:@
removeObserverSelector :: Selector
removeObserverSelector = mkSelector "removeObserver:"

-- | @Selector@ for @systemExtensionsForApplicationWithBundleID:error:@
systemExtensionsForApplicationWithBundleID_errorSelector :: Selector
systemExtensionsForApplicationWithBundleID_errorSelector = mkSelector "systemExtensionsForApplicationWithBundleID:error:"

-- | @Selector@ for @sharedWorkspace@
sharedWorkspaceSelector :: Selector
sharedWorkspaceSelector = mkSelector "sharedWorkspace"

