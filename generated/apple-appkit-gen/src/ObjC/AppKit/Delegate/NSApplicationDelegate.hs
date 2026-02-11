{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSApplicationDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newNSApplicationDelegate defaultNSApplicationDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AppKit.Delegate.NSApplicationDelegate
  ( NSApplicationDelegateOverrides(..)
  , defaultNSApplicationDelegateOverrides
  , newNSApplicationDelegate
  ) where

import Foreign.Ptr (Ptr, FunPtr, castPtr, nullPtr)
import Foreign.C.Types
import Foreign.StablePtr (newStablePtr, deRefStablePtr)
import System.IO.Unsafe (unsafePerformIO)
import Foreign.C.String (withCString)
import Foreign.LibFFI (retCULong, argPtr)

import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass, class_createInstance)
import ObjC.Runtime.ClassBuilder (objc_allocateClassPair, objc_registerClassPair)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.MsgSend (sendSuperMsg)
import ObjC.Runtime.StableIvar

-- | Overrides record for @\@protocol NSApplicationDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSApplicationDelegateOverrides = NSApplicationDelegateOverrides
  { _application_openURLs :: !(Maybe (RawId -> RawId -> IO ()))
  , _application_openFile :: !(Maybe (RawId -> RawId -> IO Bool))
  , _application_openFiles :: !(Maybe (RawId -> RawId -> IO ()))
  , _application_openTempFile :: !(Maybe (RawId -> RawId -> IO Bool))
  , _applicationShouldOpenUntitledFile :: !(Maybe (RawId -> IO Bool))
  , _applicationOpenUntitledFile :: !(Maybe (RawId -> IO Bool))
  , _application_openFileWithoutUI :: !(Maybe (RawId -> RawId -> IO Bool))
  , _application_printFile :: !(Maybe (RawId -> RawId -> IO Bool))
  , _applicationShouldTerminateAfterLastWindowClosed :: !(Maybe (RawId -> IO Bool))
  , _applicationShouldHandleReopen_hasVisibleWindows :: !(Maybe (RawId -> Bool -> IO Bool))
  , _applicationDockMenu :: !(Maybe (RawId -> IO RawId))
  , _application_willPresentError :: !(Maybe (RawId -> RawId -> IO RawId))
  , _application_didRegisterForRemoteNotificationsWithDeviceToken :: !(Maybe (RawId -> RawId -> IO ()))
  , _application_didFailToRegisterForRemoteNotificationsWithError :: !(Maybe (RawId -> RawId -> IO ()))
  , _application_didReceiveRemoteNotification :: !(Maybe (RawId -> RawId -> IO ()))
  , _applicationSupportsSecureRestorableState :: !(Maybe (RawId -> IO Bool))
  , _application_handlerForIntent :: !(Maybe (RawId -> RawId -> IO RawId))
  , _application_willEncodeRestorableState :: !(Maybe (RawId -> RawId -> IO ()))
  , _application_didDecodeRestorableState :: !(Maybe (RawId -> RawId -> IO ()))
  , _application_willContinueUserActivityWithType :: !(Maybe (RawId -> RawId -> IO Bool))
  , _application_continueUserActivity_restorationHandler :: !(Maybe (RawId -> RawId -> RawId -> IO Bool))
  , _application_didFailToContinueUserActivityWithType_error :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _application_didUpdateUserActivity :: !(Maybe (RawId -> RawId -> IO ()))
  , _application_userDidAcceptCloudKitShareWithMetadata :: !(Maybe (RawId -> RawId -> IO ()))
  , _application_delegateHandlesKey :: !(Maybe (RawId -> RawId -> IO Bool))
  , _applicationShouldAutomaticallyLocalizeKeyEquivalents :: !(Maybe (RawId -> IO Bool))
  , _applicationWillFinishLaunching :: !(Maybe (RawId -> IO ()))
  , _applicationDidFinishLaunching :: !(Maybe (RawId -> IO ()))
  , _applicationWillHide :: !(Maybe (RawId -> IO ()))
  , _applicationDidHide :: !(Maybe (RawId -> IO ()))
  , _applicationWillUnhide :: !(Maybe (RawId -> IO ()))
  , _applicationDidUnhide :: !(Maybe (RawId -> IO ()))
  , _applicationWillBecomeActive :: !(Maybe (RawId -> IO ()))
  , _applicationDidBecomeActive :: !(Maybe (RawId -> IO ()))
  , _applicationWillResignActive :: !(Maybe (RawId -> IO ()))
  , _applicationDidResignActive :: !(Maybe (RawId -> IO ()))
  , _applicationWillUpdate :: !(Maybe (RawId -> IO ()))
  , _applicationDidUpdate :: !(Maybe (RawId -> IO ()))
  , _applicationWillTerminate :: !(Maybe (RawId -> IO ()))
  , _applicationDidChangeScreenParameters :: !(Maybe (RawId -> IO ()))
  , _applicationDidChangeOcclusionState :: !(Maybe (RawId -> IO ()))
  , _applicationProtectedDataWillBecomeUnavailable :: !(Maybe (RawId -> IO ()))
  , _applicationProtectedDataDidBecomeAvailable :: !(Maybe (RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultNSApplicationDelegateOverrides :: NSApplicationDelegateOverrides
defaultNSApplicationDelegateOverrides = NSApplicationDelegateOverrides
  { _application_openURLs = Nothing
  , _application_openFile = Nothing
  , _application_openFiles = Nothing
  , _application_openTempFile = Nothing
  , _applicationShouldOpenUntitledFile = Nothing
  , _applicationOpenUntitledFile = Nothing
  , _application_openFileWithoutUI = Nothing
  , _application_printFile = Nothing
  , _applicationShouldTerminateAfterLastWindowClosed = Nothing
  , _applicationShouldHandleReopen_hasVisibleWindows = Nothing
  , _applicationDockMenu = Nothing
  , _application_willPresentError = Nothing
  , _application_didRegisterForRemoteNotificationsWithDeviceToken = Nothing
  , _application_didFailToRegisterForRemoteNotificationsWithError = Nothing
  , _application_didReceiveRemoteNotification = Nothing
  , _applicationSupportsSecureRestorableState = Nothing
  , _application_handlerForIntent = Nothing
  , _application_willEncodeRestorableState = Nothing
  , _application_didDecodeRestorableState = Nothing
  , _application_willContinueUserActivityWithType = Nothing
  , _application_continueUserActivity_restorationHandler = Nothing
  , _application_didFailToContinueUserActivityWithType_error = Nothing
  , _application_didUpdateUserActivity = Nothing
  , _application_userDidAcceptCloudKitShareWithMetadata = Nothing
  , _application_delegateHandlesKey = Nothing
  , _applicationShouldAutomaticallyLocalizeKeyEquivalents = Nothing
  , _applicationWillFinishLaunching = Nothing
  , _applicationDidFinishLaunching = Nothing
  , _applicationWillHide = Nothing
  , _applicationDidHide = Nothing
  , _applicationWillUnhide = Nothing
  , _applicationDidUnhide = Nothing
  , _applicationWillBecomeActive = Nothing
  , _applicationDidBecomeActive = Nothing
  , _applicationWillResignActive = Nothing
  , _applicationDidResignActive = Nothing
  , _applicationWillUpdate = Nothing
  , _applicationDidUpdate = Nothing
  , _applicationWillTerminate = Nothing
  , _applicationDidChangeScreenParameters = Nothing
  , _applicationDidChangeOcclusionState = Nothing
  , _applicationProtectedDataWillBecomeUnavailable = Nothing
  , _applicationProtectedDataDidBecomeAvailable = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_B_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsApplicationDelegateDelegateClass #-}
nsApplicationDelegateDelegateClass :: Class
nsApplicationDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSApplicationDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_application_openURLs = unSelector (mkSelector "application:openURLs:")
      sel_application_openFile = unSelector (mkSelector "application:openFile:")
      sel_application_openFiles = unSelector (mkSelector "application:openFiles:")
      sel_application_openTempFile = unSelector (mkSelector "application:openTempFile:")
      sel_applicationShouldOpenUntitledFile = unSelector (mkSelector "applicationShouldOpenUntitledFile:")
      sel_applicationOpenUntitledFile = unSelector (mkSelector "applicationOpenUntitledFile:")
      sel_application_openFileWithoutUI = unSelector (mkSelector "application:openFileWithoutUI:")
      sel_application_printFile = unSelector (mkSelector "application:printFile:")
      sel_applicationShouldTerminateAfterLastWindowClosed = unSelector (mkSelector "applicationShouldTerminateAfterLastWindowClosed:")
      sel_applicationShouldHandleReopen_hasVisibleWindows = unSelector (mkSelector "applicationShouldHandleReopen:hasVisibleWindows:")
      sel_applicationDockMenu = unSelector (mkSelector "applicationDockMenu:")
      sel_application_willPresentError = unSelector (mkSelector "application:willPresentError:")
      sel_application_didRegisterForRemoteNotificationsWithDeviceToken = unSelector (mkSelector "application:didRegisterForRemoteNotificationsWithDeviceToken:")
      sel_application_didFailToRegisterForRemoteNotificationsWithError = unSelector (mkSelector "application:didFailToRegisterForRemoteNotificationsWithError:")
      sel_application_didReceiveRemoteNotification = unSelector (mkSelector "application:didReceiveRemoteNotification:")
      sel_applicationSupportsSecureRestorableState = unSelector (mkSelector "applicationSupportsSecureRestorableState:")
      sel_application_handlerForIntent = unSelector (mkSelector "application:handlerForIntent:")
      sel_application_willEncodeRestorableState = unSelector (mkSelector "application:willEncodeRestorableState:")
      sel_application_didDecodeRestorableState = unSelector (mkSelector "application:didDecodeRestorableState:")
      sel_application_willContinueUserActivityWithType = unSelector (mkSelector "application:willContinueUserActivityWithType:")
      sel_application_continueUserActivity_restorationHandler = unSelector (mkSelector "application:continueUserActivity:restorationHandler:")
      sel_application_didFailToContinueUserActivityWithType_error = unSelector (mkSelector "application:didFailToContinueUserActivityWithType:error:")
      sel_application_didUpdateUserActivity = unSelector (mkSelector "application:didUpdateUserActivity:")
      sel_application_userDidAcceptCloudKitShareWithMetadata = unSelector (mkSelector "application:userDidAcceptCloudKitShareWithMetadata:")
      sel_application_delegateHandlesKey = unSelector (mkSelector "application:delegateHandlesKey:")
      sel_applicationShouldAutomaticallyLocalizeKeyEquivalents = unSelector (mkSelector "applicationShouldAutomaticallyLocalizeKeyEquivalents:")
      sel_applicationWillFinishLaunching = unSelector (mkSelector "applicationWillFinishLaunching:")
      sel_applicationDidFinishLaunching = unSelector (mkSelector "applicationDidFinishLaunching:")
      sel_applicationWillHide = unSelector (mkSelector "applicationWillHide:")
      sel_applicationDidHide = unSelector (mkSelector "applicationDidHide:")
      sel_applicationWillUnhide = unSelector (mkSelector "applicationWillUnhide:")
      sel_applicationDidUnhide = unSelector (mkSelector "applicationDidUnhide:")
      sel_applicationWillBecomeActive = unSelector (mkSelector "applicationWillBecomeActive:")
      sel_applicationDidBecomeActive = unSelector (mkSelector "applicationDidBecomeActive:")
      sel_applicationWillResignActive = unSelector (mkSelector "applicationWillResignActive:")
      sel_applicationDidResignActive = unSelector (mkSelector "applicationDidResignActive:")
      sel_applicationWillUpdate = unSelector (mkSelector "applicationWillUpdate:")
      sel_applicationDidUpdate = unSelector (mkSelector "applicationDidUpdate:")
      sel_applicationWillTerminate = unSelector (mkSelector "applicationWillTerminate:")
      sel_applicationDidChangeScreenParameters = unSelector (mkSelector "applicationDidChangeScreenParameters:")
      sel_applicationDidChangeOcclusionState = unSelector (mkSelector "applicationDidChangeOcclusionState:")
      sel_applicationProtectedDataWillBecomeUnavailable = unSelector (mkSelector "applicationProtectedDataWillBecomeUnavailable:")
      sel_applicationProtectedDataDidBecomeAvailable = unSelector (mkSelector "applicationProtectedDataDidBecomeAvailable:")
  -- application:openURLs:
  stub_0 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSApplicationDelegateOverrides
    case _application_openURLs rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "application:openURLs:" "v@:@@" stub_0

  -- application:openFile:
  stub_1 <- wrap_at_at_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSApplicationDelegateOverrides
    case _application_openFile rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "application:openFile:" "B@:@@" stub_1

  -- application:openFiles:
  stub_2 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSApplicationDelegateOverrides
    case _application_openFiles rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "application:openFiles:" "v@:@@" stub_2

  -- application:openTempFile:
  stub_3 <- wrap_at_at_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSApplicationDelegateOverrides
    case _application_openTempFile rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "application:openTempFile:" "B@:@@" stub_3

  -- applicationShouldOpenUntitledFile:
  stub_4 <- wrap_at_B $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSApplicationDelegateOverrides
    case _applicationShouldOpenUntitledFile rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0)
        pure (if r then 1 else 0)
  addObjCMethod cls "applicationShouldOpenUntitledFile:" "B@:@" stub_4

  -- applicationOpenUntitledFile:
  stub_5 <- wrap_at_B $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSApplicationDelegateOverrides
    case _applicationOpenUntitledFile rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0)
        pure (if r then 1 else 0)
  addObjCMethod cls "applicationOpenUntitledFile:" "B@:@" stub_5

  -- application:openFileWithoutUI:
  stub_6 <- wrap_at_at_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSApplicationDelegateOverrides
    case _application_openFileWithoutUI rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "application:openFileWithoutUI:" "B@:@@" stub_6

  -- application:printFile:
  stub_7 <- wrap_at_at_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSApplicationDelegateOverrides
    case _application_printFile rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "application:printFile:" "B@:@@" stub_7

  -- applicationShouldTerminateAfterLastWindowClosed:
  stub_8 <- wrap_at_B $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSApplicationDelegateOverrides
    case _applicationShouldTerminateAfterLastWindowClosed rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0)
        pure (if r then 1 else 0)
  addObjCMethod cls "applicationShouldTerminateAfterLastWindowClosed:" "B@:@" stub_8

  -- applicationShouldHandleReopen:hasVisibleWindows:
  stub_9 <- wrap_at_B_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSApplicationDelegateOverrides
    case _applicationShouldHandleReopen_hasVisibleWindows rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (arg1 /= 0)
        pure (if r then 1 else 0)
  addObjCMethod cls "applicationShouldHandleReopen:hasVisibleWindows:" "B@:@B" stub_9

  -- applicationDockMenu:
  stub_10 <- wrap_at_at $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSApplicationDelegateOverrides
    case _applicationDockMenu rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "applicationDockMenu:" "@@:@" stub_10

  -- application:willPresentError:
  stub_11 <- wrap_at_at_at $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSApplicationDelegateOverrides
    case _application_willPresentError rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "application:willPresentError:" "@@:@@" stub_11

  -- application:didRegisterForRemoteNotificationsWithDeviceToken:
  stub_12 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSApplicationDelegateOverrides
    case _application_didRegisterForRemoteNotificationsWithDeviceToken rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "application:didRegisterForRemoteNotificationsWithDeviceToken:" "v@:@@" stub_12

  -- application:didFailToRegisterForRemoteNotificationsWithError:
  stub_13 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSApplicationDelegateOverrides
    case _application_didFailToRegisterForRemoteNotificationsWithError rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "application:didFailToRegisterForRemoteNotificationsWithError:" "v@:@@" stub_13

  -- application:didReceiveRemoteNotification:
  stub_14 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSApplicationDelegateOverrides
    case _application_didReceiveRemoteNotification rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "application:didReceiveRemoteNotification:" "v@:@@" stub_14

  -- applicationSupportsSecureRestorableState:
  stub_15 <- wrap_at_B $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSApplicationDelegateOverrides
    case _applicationSupportsSecureRestorableState rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0)
        pure (if r then 1 else 0)
  addObjCMethod cls "applicationSupportsSecureRestorableState:" "B@:@" stub_15

  -- application:handlerForIntent:
  stub_16 <- wrap_at_at_at $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSApplicationDelegateOverrides
    case _application_handlerForIntent rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "application:handlerForIntent:" "@@:@@" stub_16

  -- application:willEncodeRestorableState:
  stub_17 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSApplicationDelegateOverrides
    case _application_willEncodeRestorableState rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "application:willEncodeRestorableState:" "v@:@@" stub_17

  -- application:didDecodeRestorableState:
  stub_18 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSApplicationDelegateOverrides
    case _application_didDecodeRestorableState rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "application:didDecodeRestorableState:" "v@:@@" stub_18

  -- application:willContinueUserActivityWithType:
  stub_19 <- wrap_at_at_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSApplicationDelegateOverrides
    case _application_willContinueUserActivityWithType rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "application:willContinueUserActivityWithType:" "B@:@@" stub_19

  -- application:continueUserActivity:restorationHandler:
  stub_20 <- wrap_at_at_at_B $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSApplicationDelegateOverrides
    case _application_continueUserActivity_restorationHandler rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2)
        pure (if r then 1 else 0)
  addObjCMethod cls "application:continueUserActivity:restorationHandler:" "B@:@@@" stub_20

  -- application:didFailToContinueUserActivityWithType:error:
  stub_21 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSApplicationDelegateOverrides
    case _application_didFailToContinueUserActivityWithType_error rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "application:didFailToContinueUserActivityWithType:error:" "v@:@@@" stub_21

  -- application:didUpdateUserActivity:
  stub_22 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSApplicationDelegateOverrides
    case _application_didUpdateUserActivity rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "application:didUpdateUserActivity:" "v@:@@" stub_22

  -- application:userDidAcceptCloudKitShareWithMetadata:
  stub_23 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSApplicationDelegateOverrides
    case _application_userDidAcceptCloudKitShareWithMetadata rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "application:userDidAcceptCloudKitShareWithMetadata:" "v@:@@" stub_23

  -- application:delegateHandlesKey:
  stub_24 <- wrap_at_at_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSApplicationDelegateOverrides
    case _application_delegateHandlesKey rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "application:delegateHandlesKey:" "B@:@@" stub_24

  -- applicationShouldAutomaticallyLocalizeKeyEquivalents:
  stub_25 <- wrap_at_B $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSApplicationDelegateOverrides
    case _applicationShouldAutomaticallyLocalizeKeyEquivalents rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0)
        pure (if r then 1 else 0)
  addObjCMethod cls "applicationShouldAutomaticallyLocalizeKeyEquivalents:" "B@:@" stub_25

  -- applicationWillFinishLaunching:
  stub_26 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSApplicationDelegateOverrides
    case _applicationWillFinishLaunching rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "applicationWillFinishLaunching:" "v@:@" stub_26

  -- applicationDidFinishLaunching:
  stub_27 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSApplicationDelegateOverrides
    case _applicationDidFinishLaunching rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "applicationDidFinishLaunching:" "v@:@" stub_27

  -- applicationWillHide:
  stub_28 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSApplicationDelegateOverrides
    case _applicationWillHide rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "applicationWillHide:" "v@:@" stub_28

  -- applicationDidHide:
  stub_29 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSApplicationDelegateOverrides
    case _applicationDidHide rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "applicationDidHide:" "v@:@" stub_29

  -- applicationWillUnhide:
  stub_30 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSApplicationDelegateOverrides
    case _applicationWillUnhide rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "applicationWillUnhide:" "v@:@" stub_30

  -- applicationDidUnhide:
  stub_31 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSApplicationDelegateOverrides
    case _applicationDidUnhide rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "applicationDidUnhide:" "v@:@" stub_31

  -- applicationWillBecomeActive:
  stub_32 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSApplicationDelegateOverrides
    case _applicationWillBecomeActive rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "applicationWillBecomeActive:" "v@:@" stub_32

  -- applicationDidBecomeActive:
  stub_33 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSApplicationDelegateOverrides
    case _applicationDidBecomeActive rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "applicationDidBecomeActive:" "v@:@" stub_33

  -- applicationWillResignActive:
  stub_34 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSApplicationDelegateOverrides
    case _applicationWillResignActive rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "applicationWillResignActive:" "v@:@" stub_34

  -- applicationDidResignActive:
  stub_35 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSApplicationDelegateOverrides
    case _applicationDidResignActive rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "applicationDidResignActive:" "v@:@" stub_35

  -- applicationWillUpdate:
  stub_36 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSApplicationDelegateOverrides
    case _applicationWillUpdate rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "applicationWillUpdate:" "v@:@" stub_36

  -- applicationDidUpdate:
  stub_37 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSApplicationDelegateOverrides
    case _applicationDidUpdate rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "applicationDidUpdate:" "v@:@" stub_37

  -- applicationWillTerminate:
  stub_38 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSApplicationDelegateOverrides
    case _applicationWillTerminate rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "applicationWillTerminate:" "v@:@" stub_38

  -- applicationDidChangeScreenParameters:
  stub_39 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSApplicationDelegateOverrides
    case _applicationDidChangeScreenParameters rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "applicationDidChangeScreenParameters:" "v@:@" stub_39

  -- applicationDidChangeOcclusionState:
  stub_40 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSApplicationDelegateOverrides
    case _applicationDidChangeOcclusionState rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "applicationDidChangeOcclusionState:" "v@:@" stub_40

  -- applicationProtectedDataWillBecomeUnavailable:
  stub_41 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSApplicationDelegateOverrides
    case _applicationProtectedDataWillBecomeUnavailable rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "applicationProtectedDataWillBecomeUnavailable:" "v@:@" stub_41

  -- applicationProtectedDataDidBecomeAvailable:
  stub_42 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSApplicationDelegateOverrides
    case _applicationProtectedDataDidBecomeAvailable rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "applicationProtectedDataDidBecomeAvailable:" "v@:@" stub_42

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSApplicationDelegateOverrides
    if queriedSel == sel_application_openURLs then pure (maybe 0 (const 1) (_application_openURLs rec_))
    else if queriedSel == sel_application_openFile then pure (maybe 0 (const 1) (_application_openFile rec_))
    else if queriedSel == sel_application_openFiles then pure (maybe 0 (const 1) (_application_openFiles rec_))
    else if queriedSel == sel_application_openTempFile then pure (maybe 0 (const 1) (_application_openTempFile rec_))
    else if queriedSel == sel_applicationShouldOpenUntitledFile then pure (maybe 0 (const 1) (_applicationShouldOpenUntitledFile rec_))
    else if queriedSel == sel_applicationOpenUntitledFile then pure (maybe 0 (const 1) (_applicationOpenUntitledFile rec_))
    else if queriedSel == sel_application_openFileWithoutUI then pure (maybe 0 (const 1) (_application_openFileWithoutUI rec_))
    else if queriedSel == sel_application_printFile then pure (maybe 0 (const 1) (_application_printFile rec_))
    else if queriedSel == sel_applicationShouldTerminateAfterLastWindowClosed then pure (maybe 0 (const 1) (_applicationShouldTerminateAfterLastWindowClosed rec_))
    else if queriedSel == sel_applicationShouldHandleReopen_hasVisibleWindows then pure (maybe 0 (const 1) (_applicationShouldHandleReopen_hasVisibleWindows rec_))
    else if queriedSel == sel_applicationDockMenu then pure (maybe 0 (const 1) (_applicationDockMenu rec_))
    else if queriedSel == sel_application_willPresentError then pure (maybe 0 (const 1) (_application_willPresentError rec_))
    else if queriedSel == sel_application_didRegisterForRemoteNotificationsWithDeviceToken then pure (maybe 0 (const 1) (_application_didRegisterForRemoteNotificationsWithDeviceToken rec_))
    else if queriedSel == sel_application_didFailToRegisterForRemoteNotificationsWithError then pure (maybe 0 (const 1) (_application_didFailToRegisterForRemoteNotificationsWithError rec_))
    else if queriedSel == sel_application_didReceiveRemoteNotification then pure (maybe 0 (const 1) (_application_didReceiveRemoteNotification rec_))
    else if queriedSel == sel_applicationSupportsSecureRestorableState then pure (maybe 0 (const 1) (_applicationSupportsSecureRestorableState rec_))
    else if queriedSel == sel_application_handlerForIntent then pure (maybe 0 (const 1) (_application_handlerForIntent rec_))
    else if queriedSel == sel_application_willEncodeRestorableState then pure (maybe 0 (const 1) (_application_willEncodeRestorableState rec_))
    else if queriedSel == sel_application_didDecodeRestorableState then pure (maybe 0 (const 1) (_application_didDecodeRestorableState rec_))
    else if queriedSel == sel_application_willContinueUserActivityWithType then pure (maybe 0 (const 1) (_application_willContinueUserActivityWithType rec_))
    else if queriedSel == sel_application_continueUserActivity_restorationHandler then pure (maybe 0 (const 1) (_application_continueUserActivity_restorationHandler rec_))
    else if queriedSel == sel_application_didFailToContinueUserActivityWithType_error then pure (maybe 0 (const 1) (_application_didFailToContinueUserActivityWithType_error rec_))
    else if queriedSel == sel_application_didUpdateUserActivity then pure (maybe 0 (const 1) (_application_didUpdateUserActivity rec_))
    else if queriedSel == sel_application_userDidAcceptCloudKitShareWithMetadata then pure (maybe 0 (const 1) (_application_userDidAcceptCloudKitShareWithMetadata rec_))
    else if queriedSel == sel_application_delegateHandlesKey then pure (maybe 0 (const 1) (_application_delegateHandlesKey rec_))
    else if queriedSel == sel_applicationShouldAutomaticallyLocalizeKeyEquivalents then pure (maybe 0 (const 1) (_applicationShouldAutomaticallyLocalizeKeyEquivalents rec_))
    else if queriedSel == sel_applicationWillFinishLaunching then pure (maybe 0 (const 1) (_applicationWillFinishLaunching rec_))
    else if queriedSel == sel_applicationDidFinishLaunching then pure (maybe 0 (const 1) (_applicationDidFinishLaunching rec_))
    else if queriedSel == sel_applicationWillHide then pure (maybe 0 (const 1) (_applicationWillHide rec_))
    else if queriedSel == sel_applicationDidHide then pure (maybe 0 (const 1) (_applicationDidHide rec_))
    else if queriedSel == sel_applicationWillUnhide then pure (maybe 0 (const 1) (_applicationWillUnhide rec_))
    else if queriedSel == sel_applicationDidUnhide then pure (maybe 0 (const 1) (_applicationDidUnhide rec_))
    else if queriedSel == sel_applicationWillBecomeActive then pure (maybe 0 (const 1) (_applicationWillBecomeActive rec_))
    else if queriedSel == sel_applicationDidBecomeActive then pure (maybe 0 (const 1) (_applicationDidBecomeActive rec_))
    else if queriedSel == sel_applicationWillResignActive then pure (maybe 0 (const 1) (_applicationWillResignActive rec_))
    else if queriedSel == sel_applicationDidResignActive then pure (maybe 0 (const 1) (_applicationDidResignActive rec_))
    else if queriedSel == sel_applicationWillUpdate then pure (maybe 0 (const 1) (_applicationWillUpdate rec_))
    else if queriedSel == sel_applicationDidUpdate then pure (maybe 0 (const 1) (_applicationDidUpdate rec_))
    else if queriedSel == sel_applicationWillTerminate then pure (maybe 0 (const 1) (_applicationWillTerminate rec_))
    else if queriedSel == sel_applicationDidChangeScreenParameters then pure (maybe 0 (const 1) (_applicationDidChangeScreenParameters rec_))
    else if queriedSel == sel_applicationDidChangeOcclusionState then pure (maybe 0 (const 1) (_applicationDidChangeOcclusionState rec_))
    else if queriedSel == sel_applicationProtectedDataWillBecomeUnavailable then pure (maybe 0 (const 1) (_applicationProtectedDataWillBecomeUnavailable rec_))
    else if queriedSel == sel_applicationProtectedDataDidBecomeAvailable then pure (maybe 0 (const 1) (_applicationProtectedDataDidBecomeAvailable rec_))
    else do
      let super_ = ObjCSuper (RawId self) superCls
      sendSuperMsg super_ (mkSelector "respondsToSelector:") retCULong
        [argPtr (castPtr queriedSel :: Ptr ())]
  addObjCMethod cls "respondsToSelector:" "B@::" rtsStub

  addStablePtrDeallocHandler cls
  objc_registerClassPair cls
  pure cls

-- | Create a new delegate implementing this protocol.
--
-- The returned 'RawId' can be used as a delegate or data source.
newNSApplicationDelegate :: NSApplicationDelegateOverrides -> IO RawId
newNSApplicationDelegate overrides = do
  inst <- class_createInstance nsApplicationDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
