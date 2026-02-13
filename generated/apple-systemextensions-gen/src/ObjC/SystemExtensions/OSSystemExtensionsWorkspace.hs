{-# LANGUAGE DataKinds #-}
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
  , sharedWorkspaceSelector
  , systemExtensionsForApplicationWithBundleID_errorSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SystemExtensions.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Start observing changes to System Extension(s) which are enabled or ready to be enabled.
--
-- ObjC selector: @- addObserver:error:@
addObserver_error :: (IsOSSystemExtensionsWorkspace osSystemExtensionsWorkspace, IsNSError error_) => osSystemExtensionsWorkspace -> RawId -> error_ -> IO Bool
addObserver_error osSystemExtensionsWorkspace observer error_ =
  sendMessage osSystemExtensionsWorkspace addObserver_errorSelector observer (toNSError error_)

-- | Stop observing changes to System Extension(s).
--
-- ObjC selector: @- removeObserver:@
removeObserver :: IsOSSystemExtensionsWorkspace osSystemExtensionsWorkspace => osSystemExtensionsWorkspace -> RawId -> IO ()
removeObserver osSystemExtensionsWorkspace observer =
  sendMessage osSystemExtensionsWorkspace removeObserverSelector observer

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
systemExtensionsForApplicationWithBundleID_error osSystemExtensionsWorkspace bundleID out_error =
  sendMessage osSystemExtensionsWorkspace systemExtensionsForApplicationWithBundleID_errorSelector (toNSString bundleID) (toNSError out_error)

-- | @+ sharedWorkspace@
sharedWorkspace :: IO (Id OSSystemExtensionsWorkspace)
sharedWorkspace  =
  do
    cls' <- getRequiredClass "OSSystemExtensionsWorkspace"
    sendClassMessage cls' sharedWorkspaceSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @addObserver:error:@
addObserver_errorSelector :: Selector '[RawId, Id NSError] Bool
addObserver_errorSelector = mkSelector "addObserver:error:"

-- | @Selector@ for @removeObserver:@
removeObserverSelector :: Selector '[RawId] ()
removeObserverSelector = mkSelector "removeObserver:"

-- | @Selector@ for @systemExtensionsForApplicationWithBundleID:error:@
systemExtensionsForApplicationWithBundleID_errorSelector :: Selector '[Id NSString, Id NSError] (Id NSSet)
systemExtensionsForApplicationWithBundleID_errorSelector = mkSelector "systemExtensionsForApplicationWithBundleID:error:"

-- | @Selector@ for @sharedWorkspace@
sharedWorkspaceSelector :: Selector '[] (Id OSSystemExtensionsWorkspace)
sharedWorkspaceSelector = mkSelector "sharedWorkspace"

