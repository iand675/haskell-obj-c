{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSWindowDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newNSWindowDelegate defaultNSWindowDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AppKit.Delegate.NSWindowDelegate
  ( NSWindowDelegateOverrides(..)
  , defaultNSWindowDelegateOverrides
  , newNSWindowDelegate
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

-- | Overrides record for @\@protocol NSWindowDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSWindowDelegateOverrides = NSWindowDelegateOverrides
  { _windowShouldClose :: !(Maybe (RawId -> IO Bool))
  , _windowWillReturnFieldEditor_toObject :: !(Maybe (RawId -> RawId -> IO RawId))
  , _windowWillReturnUndoManager :: !(Maybe (RawId -> IO RawId))
  , _window_shouldPopUpDocumentPathMenu :: !(Maybe (RawId -> RawId -> IO Bool))
  , _customWindowsToEnterFullScreenForWindow :: !(Maybe (RawId -> IO RawId))
  , _window_startCustomAnimationToEnterFullScreenWithDuration :: !(Maybe (RawId -> Double -> IO ()))
  , _windowDidFailToEnterFullScreen :: !(Maybe (RawId -> IO ()))
  , _customWindowsToExitFullScreenForWindow :: !(Maybe (RawId -> IO RawId))
  , _window_startCustomAnimationToExitFullScreenWithDuration :: !(Maybe (RawId -> Double -> IO ()))
  , _customWindowsToEnterFullScreenForWindow_onScreen :: !(Maybe (RawId -> RawId -> IO RawId))
  , _window_startCustomAnimationToEnterFullScreenOnScreen_withDuration :: !(Maybe (RawId -> RawId -> Double -> IO ()))
  , _windowDidFailToExitFullScreen :: !(Maybe (RawId -> IO ()))
  , _window_willEncodeRestorableState :: !(Maybe (RawId -> RawId -> IO ()))
  , _window_didDecodeRestorableState :: !(Maybe (RawId -> RawId -> IO ()))
  , _previewRepresentableActivityItemsForWindow :: !(Maybe (RawId -> IO RawId))
  , _windowForSharingRequestFromWindow :: !(Maybe (RawId -> IO RawId))
  , _windowDidResize :: !(Maybe (RawId -> IO ()))
  , _windowDidExpose :: !(Maybe (RawId -> IO ()))
  , _windowWillMove :: !(Maybe (RawId -> IO ()))
  , _windowDidMove :: !(Maybe (RawId -> IO ()))
  , _windowDidBecomeKey :: !(Maybe (RawId -> IO ()))
  , _windowDidResignKey :: !(Maybe (RawId -> IO ()))
  , _windowDidBecomeMain :: !(Maybe (RawId -> IO ()))
  , _windowDidResignMain :: !(Maybe (RawId -> IO ()))
  , _windowWillClose :: !(Maybe (RawId -> IO ()))
  , _windowWillMiniaturize :: !(Maybe (RawId -> IO ()))
  , _windowDidMiniaturize :: !(Maybe (RawId -> IO ()))
  , _windowDidDeminiaturize :: !(Maybe (RawId -> IO ()))
  , _windowDidUpdate :: !(Maybe (RawId -> IO ()))
  , _windowDidChangeScreen :: !(Maybe (RawId -> IO ()))
  , _windowDidChangeScreenProfile :: !(Maybe (RawId -> IO ()))
  , _windowDidChangeBackingProperties :: !(Maybe (RawId -> IO ()))
  , _windowWillBeginSheet :: !(Maybe (RawId -> IO ()))
  , _windowDidEndSheet :: !(Maybe (RawId -> IO ()))
  , _windowWillStartLiveResize :: !(Maybe (RawId -> IO ()))
  , _windowDidEndLiveResize :: !(Maybe (RawId -> IO ()))
  , _windowWillEnterFullScreen :: !(Maybe (RawId -> IO ()))
  , _windowDidEnterFullScreen :: !(Maybe (RawId -> IO ()))
  , _windowWillExitFullScreen :: !(Maybe (RawId -> IO ()))
  , _windowDidExitFullScreen :: !(Maybe (RawId -> IO ()))
  , _windowWillEnterVersionBrowser :: !(Maybe (RawId -> IO ()))
  , _windowDidEnterVersionBrowser :: !(Maybe (RawId -> IO ()))
  , _windowWillExitVersionBrowser :: !(Maybe (RawId -> IO ()))
  , _windowDidExitVersionBrowser :: !(Maybe (RawId -> IO ()))
  , _windowDidChangeOcclusionState :: !(Maybe (RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultNSWindowDelegateOverrides :: NSWindowDelegateOverrides
defaultNSWindowDelegateOverrides = NSWindowDelegateOverrides
  { _windowShouldClose = Nothing
  , _windowWillReturnFieldEditor_toObject = Nothing
  , _windowWillReturnUndoManager = Nothing
  , _window_shouldPopUpDocumentPathMenu = Nothing
  , _customWindowsToEnterFullScreenForWindow = Nothing
  , _window_startCustomAnimationToEnterFullScreenWithDuration = Nothing
  , _windowDidFailToEnterFullScreen = Nothing
  , _customWindowsToExitFullScreenForWindow = Nothing
  , _window_startCustomAnimationToExitFullScreenWithDuration = Nothing
  , _customWindowsToEnterFullScreenForWindow_onScreen = Nothing
  , _window_startCustomAnimationToEnterFullScreenOnScreen_withDuration = Nothing
  , _windowDidFailToExitFullScreen = Nothing
  , _window_willEncodeRestorableState = Nothing
  , _window_didDecodeRestorableState = Nothing
  , _previewRepresentableActivityItemsForWindow = Nothing
  , _windowForSharingRequestFromWindow = Nothing
  , _windowDidResize = Nothing
  , _windowDidExpose = Nothing
  , _windowWillMove = Nothing
  , _windowDidMove = Nothing
  , _windowDidBecomeKey = Nothing
  , _windowDidResignKey = Nothing
  , _windowDidBecomeMain = Nothing
  , _windowDidResignMain = Nothing
  , _windowWillClose = Nothing
  , _windowWillMiniaturize = Nothing
  , _windowDidMiniaturize = Nothing
  , _windowDidDeminiaturize = Nothing
  , _windowDidUpdate = Nothing
  , _windowDidChangeScreen = Nothing
  , _windowDidChangeScreenProfile = Nothing
  , _windowDidChangeBackingProperties = Nothing
  , _windowWillBeginSheet = Nothing
  , _windowDidEndSheet = Nothing
  , _windowWillStartLiveResize = Nothing
  , _windowDidEndLiveResize = Nothing
  , _windowWillEnterFullScreen = Nothing
  , _windowDidEnterFullScreen = Nothing
  , _windowWillExitFullScreen = Nothing
  , _windowDidExitFullScreen = Nothing
  , _windowWillEnterVersionBrowser = Nothing
  , _windowDidEnterVersionBrowser = Nothing
  , _windowWillExitVersionBrowser = Nothing
  , _windowDidExitVersionBrowser = Nothing
  , _windowDidChangeOcclusionState = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_d_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> CDouble -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> CDouble -> IO ()))

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_d_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CDouble -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CDouble -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO CULong))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsWindowDelegateDelegateClass #-}
nsWindowDelegateDelegateClass :: Class
nsWindowDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSWindowDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_windowShouldClose = unSelector (mkSelector "windowShouldClose:")
      sel_windowWillReturnFieldEditor_toObject = unSelector (mkSelector "windowWillReturnFieldEditor:toObject:")
      sel_windowWillReturnUndoManager = unSelector (mkSelector "windowWillReturnUndoManager:")
      sel_window_shouldPopUpDocumentPathMenu = unSelector (mkSelector "window:shouldPopUpDocumentPathMenu:")
      sel_customWindowsToEnterFullScreenForWindow = unSelector (mkSelector "customWindowsToEnterFullScreenForWindow:")
      sel_window_startCustomAnimationToEnterFullScreenWithDuration = unSelector (mkSelector "window:startCustomAnimationToEnterFullScreenWithDuration:")
      sel_windowDidFailToEnterFullScreen = unSelector (mkSelector "windowDidFailToEnterFullScreen:")
      sel_customWindowsToExitFullScreenForWindow = unSelector (mkSelector "customWindowsToExitFullScreenForWindow:")
      sel_window_startCustomAnimationToExitFullScreenWithDuration = unSelector (mkSelector "window:startCustomAnimationToExitFullScreenWithDuration:")
      sel_customWindowsToEnterFullScreenForWindow_onScreen = unSelector (mkSelector "customWindowsToEnterFullScreenForWindow:onScreen:")
      sel_window_startCustomAnimationToEnterFullScreenOnScreen_withDuration = unSelector (mkSelector "window:startCustomAnimationToEnterFullScreenOnScreen:withDuration:")
      sel_windowDidFailToExitFullScreen = unSelector (mkSelector "windowDidFailToExitFullScreen:")
      sel_window_willEncodeRestorableState = unSelector (mkSelector "window:willEncodeRestorableState:")
      sel_window_didDecodeRestorableState = unSelector (mkSelector "window:didDecodeRestorableState:")
      sel_previewRepresentableActivityItemsForWindow = unSelector (mkSelector "previewRepresentableActivityItemsForWindow:")
      sel_windowForSharingRequestFromWindow = unSelector (mkSelector "windowForSharingRequestFromWindow:")
      sel_windowDidResize = unSelector (mkSelector "windowDidResize:")
      sel_windowDidExpose = unSelector (mkSelector "windowDidExpose:")
      sel_windowWillMove = unSelector (mkSelector "windowWillMove:")
      sel_windowDidMove = unSelector (mkSelector "windowDidMove:")
      sel_windowDidBecomeKey = unSelector (mkSelector "windowDidBecomeKey:")
      sel_windowDidResignKey = unSelector (mkSelector "windowDidResignKey:")
      sel_windowDidBecomeMain = unSelector (mkSelector "windowDidBecomeMain:")
      sel_windowDidResignMain = unSelector (mkSelector "windowDidResignMain:")
      sel_windowWillClose = unSelector (mkSelector "windowWillClose:")
      sel_windowWillMiniaturize = unSelector (mkSelector "windowWillMiniaturize:")
      sel_windowDidMiniaturize = unSelector (mkSelector "windowDidMiniaturize:")
      sel_windowDidDeminiaturize = unSelector (mkSelector "windowDidDeminiaturize:")
      sel_windowDidUpdate = unSelector (mkSelector "windowDidUpdate:")
      sel_windowDidChangeScreen = unSelector (mkSelector "windowDidChangeScreen:")
      sel_windowDidChangeScreenProfile = unSelector (mkSelector "windowDidChangeScreenProfile:")
      sel_windowDidChangeBackingProperties = unSelector (mkSelector "windowDidChangeBackingProperties:")
      sel_windowWillBeginSheet = unSelector (mkSelector "windowWillBeginSheet:")
      sel_windowDidEndSheet = unSelector (mkSelector "windowDidEndSheet:")
      sel_windowWillStartLiveResize = unSelector (mkSelector "windowWillStartLiveResize:")
      sel_windowDidEndLiveResize = unSelector (mkSelector "windowDidEndLiveResize:")
      sel_windowWillEnterFullScreen = unSelector (mkSelector "windowWillEnterFullScreen:")
      sel_windowDidEnterFullScreen = unSelector (mkSelector "windowDidEnterFullScreen:")
      sel_windowWillExitFullScreen = unSelector (mkSelector "windowWillExitFullScreen:")
      sel_windowDidExitFullScreen = unSelector (mkSelector "windowDidExitFullScreen:")
      sel_windowWillEnterVersionBrowser = unSelector (mkSelector "windowWillEnterVersionBrowser:")
      sel_windowDidEnterVersionBrowser = unSelector (mkSelector "windowDidEnterVersionBrowser:")
      sel_windowWillExitVersionBrowser = unSelector (mkSelector "windowWillExitVersionBrowser:")
      sel_windowDidExitVersionBrowser = unSelector (mkSelector "windowDidExitVersionBrowser:")
      sel_windowDidChangeOcclusionState = unSelector (mkSelector "windowDidChangeOcclusionState:")
  -- windowShouldClose:
  stub_0 <- wrap_at_B $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSWindowDelegateOverrides
    case _windowShouldClose rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0)
        pure (if r then 1 else 0)
  addObjCMethod cls "windowShouldClose:" "B@:@" stub_0

  -- windowWillReturnFieldEditor:toObject:
  stub_1 <- wrap_at_at_at $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSWindowDelegateOverrides
    case _windowWillReturnFieldEditor_toObject rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "windowWillReturnFieldEditor:toObject:" "@@:@@" stub_1

  -- windowWillReturnUndoManager:
  stub_2 <- wrap_at_at $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSWindowDelegateOverrides
    case _windowWillReturnUndoManager rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "windowWillReturnUndoManager:" "@@:@" stub_2

  -- window:shouldPopUpDocumentPathMenu:
  stub_3 <- wrap_at_at_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSWindowDelegateOverrides
    case _window_shouldPopUpDocumentPathMenu rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "window:shouldPopUpDocumentPathMenu:" "B@:@@" stub_3

  -- customWindowsToEnterFullScreenForWindow:
  stub_4 <- wrap_at_at $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSWindowDelegateOverrides
    case _customWindowsToEnterFullScreenForWindow rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "customWindowsToEnterFullScreenForWindow:" "@@:@" stub_4

  -- window:startCustomAnimationToEnterFullScreenWithDuration:
  stub_5 <- wrap_at_d_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSWindowDelegateOverrides
    case _window_startCustomAnimationToEnterFullScreenWithDuration rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (realToFrac arg1)
  addObjCMethod cls "window:startCustomAnimationToEnterFullScreenWithDuration:" "v@:@d" stub_5

  -- windowDidFailToEnterFullScreen:
  stub_6 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSWindowDelegateOverrides
    case _windowDidFailToEnterFullScreen rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "windowDidFailToEnterFullScreen:" "v@:@" stub_6

  -- customWindowsToExitFullScreenForWindow:
  stub_7 <- wrap_at_at $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSWindowDelegateOverrides
    case _customWindowsToExitFullScreenForWindow rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "customWindowsToExitFullScreenForWindow:" "@@:@" stub_7

  -- window:startCustomAnimationToExitFullScreenWithDuration:
  stub_8 <- wrap_at_d_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSWindowDelegateOverrides
    case _window_startCustomAnimationToExitFullScreenWithDuration rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (realToFrac arg1)
  addObjCMethod cls "window:startCustomAnimationToExitFullScreenWithDuration:" "v@:@d" stub_8

  -- customWindowsToEnterFullScreenForWindow:onScreen:
  stub_9 <- wrap_at_at_at $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSWindowDelegateOverrides
    case _customWindowsToEnterFullScreenForWindow_onScreen rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "customWindowsToEnterFullScreenForWindow:onScreen:" "@@:@@" stub_9

  -- window:startCustomAnimationToEnterFullScreenOnScreen:withDuration:
  stub_10 <- wrap_at_at_d_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSWindowDelegateOverrides
    case _window_startCustomAnimationToEnterFullScreenOnScreen_withDuration rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (realToFrac arg2)
  addObjCMethod cls "window:startCustomAnimationToEnterFullScreenOnScreen:withDuration:" "v@:@@d" stub_10

  -- windowDidFailToExitFullScreen:
  stub_11 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSWindowDelegateOverrides
    case _windowDidFailToExitFullScreen rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "windowDidFailToExitFullScreen:" "v@:@" stub_11

  -- window:willEncodeRestorableState:
  stub_12 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSWindowDelegateOverrides
    case _window_willEncodeRestorableState rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "window:willEncodeRestorableState:" "v@:@@" stub_12

  -- window:didDecodeRestorableState:
  stub_13 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSWindowDelegateOverrides
    case _window_didDecodeRestorableState rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "window:didDecodeRestorableState:" "v@:@@" stub_13

  -- previewRepresentableActivityItemsForWindow:
  stub_14 <- wrap_at_at $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSWindowDelegateOverrides
    case _previewRepresentableActivityItemsForWindow rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "previewRepresentableActivityItemsForWindow:" "@@:@" stub_14

  -- windowForSharingRequestFromWindow:
  stub_15 <- wrap_at_at $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSWindowDelegateOverrides
    case _windowForSharingRequestFromWindow rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "windowForSharingRequestFromWindow:" "@@:@" stub_15

  -- windowDidResize:
  stub_16 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSWindowDelegateOverrides
    case _windowDidResize rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "windowDidResize:" "v@:@" stub_16

  -- windowDidExpose:
  stub_17 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSWindowDelegateOverrides
    case _windowDidExpose rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "windowDidExpose:" "v@:@" stub_17

  -- windowWillMove:
  stub_18 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSWindowDelegateOverrides
    case _windowWillMove rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "windowWillMove:" "v@:@" stub_18

  -- windowDidMove:
  stub_19 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSWindowDelegateOverrides
    case _windowDidMove rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "windowDidMove:" "v@:@" stub_19

  -- windowDidBecomeKey:
  stub_20 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSWindowDelegateOverrides
    case _windowDidBecomeKey rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "windowDidBecomeKey:" "v@:@" stub_20

  -- windowDidResignKey:
  stub_21 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSWindowDelegateOverrides
    case _windowDidResignKey rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "windowDidResignKey:" "v@:@" stub_21

  -- windowDidBecomeMain:
  stub_22 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSWindowDelegateOverrides
    case _windowDidBecomeMain rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "windowDidBecomeMain:" "v@:@" stub_22

  -- windowDidResignMain:
  stub_23 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSWindowDelegateOverrides
    case _windowDidResignMain rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "windowDidResignMain:" "v@:@" stub_23

  -- windowWillClose:
  stub_24 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSWindowDelegateOverrides
    case _windowWillClose rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "windowWillClose:" "v@:@" stub_24

  -- windowWillMiniaturize:
  stub_25 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSWindowDelegateOverrides
    case _windowWillMiniaturize rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "windowWillMiniaturize:" "v@:@" stub_25

  -- windowDidMiniaturize:
  stub_26 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSWindowDelegateOverrides
    case _windowDidMiniaturize rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "windowDidMiniaturize:" "v@:@" stub_26

  -- windowDidDeminiaturize:
  stub_27 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSWindowDelegateOverrides
    case _windowDidDeminiaturize rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "windowDidDeminiaturize:" "v@:@" stub_27

  -- windowDidUpdate:
  stub_28 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSWindowDelegateOverrides
    case _windowDidUpdate rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "windowDidUpdate:" "v@:@" stub_28

  -- windowDidChangeScreen:
  stub_29 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSWindowDelegateOverrides
    case _windowDidChangeScreen rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "windowDidChangeScreen:" "v@:@" stub_29

  -- windowDidChangeScreenProfile:
  stub_30 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSWindowDelegateOverrides
    case _windowDidChangeScreenProfile rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "windowDidChangeScreenProfile:" "v@:@" stub_30

  -- windowDidChangeBackingProperties:
  stub_31 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSWindowDelegateOverrides
    case _windowDidChangeBackingProperties rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "windowDidChangeBackingProperties:" "v@:@" stub_31

  -- windowWillBeginSheet:
  stub_32 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSWindowDelegateOverrides
    case _windowWillBeginSheet rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "windowWillBeginSheet:" "v@:@" stub_32

  -- windowDidEndSheet:
  stub_33 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSWindowDelegateOverrides
    case _windowDidEndSheet rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "windowDidEndSheet:" "v@:@" stub_33

  -- windowWillStartLiveResize:
  stub_34 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSWindowDelegateOverrides
    case _windowWillStartLiveResize rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "windowWillStartLiveResize:" "v@:@" stub_34

  -- windowDidEndLiveResize:
  stub_35 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSWindowDelegateOverrides
    case _windowDidEndLiveResize rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "windowDidEndLiveResize:" "v@:@" stub_35

  -- windowWillEnterFullScreen:
  stub_36 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSWindowDelegateOverrides
    case _windowWillEnterFullScreen rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "windowWillEnterFullScreen:" "v@:@" stub_36

  -- windowDidEnterFullScreen:
  stub_37 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSWindowDelegateOverrides
    case _windowDidEnterFullScreen rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "windowDidEnterFullScreen:" "v@:@" stub_37

  -- windowWillExitFullScreen:
  stub_38 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSWindowDelegateOverrides
    case _windowWillExitFullScreen rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "windowWillExitFullScreen:" "v@:@" stub_38

  -- windowDidExitFullScreen:
  stub_39 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSWindowDelegateOverrides
    case _windowDidExitFullScreen rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "windowDidExitFullScreen:" "v@:@" stub_39

  -- windowWillEnterVersionBrowser:
  stub_40 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSWindowDelegateOverrides
    case _windowWillEnterVersionBrowser rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "windowWillEnterVersionBrowser:" "v@:@" stub_40

  -- windowDidEnterVersionBrowser:
  stub_41 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSWindowDelegateOverrides
    case _windowDidEnterVersionBrowser rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "windowDidEnterVersionBrowser:" "v@:@" stub_41

  -- windowWillExitVersionBrowser:
  stub_42 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSWindowDelegateOverrides
    case _windowWillExitVersionBrowser rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "windowWillExitVersionBrowser:" "v@:@" stub_42

  -- windowDidExitVersionBrowser:
  stub_43 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSWindowDelegateOverrides
    case _windowDidExitVersionBrowser rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "windowDidExitVersionBrowser:" "v@:@" stub_43

  -- windowDidChangeOcclusionState:
  stub_44 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSWindowDelegateOverrides
    case _windowDidChangeOcclusionState rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "windowDidChangeOcclusionState:" "v@:@" stub_44

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSWindowDelegateOverrides
    if queriedSel == sel_windowShouldClose then pure (maybe 0 (const 1) (_windowShouldClose rec_))
    else if queriedSel == sel_windowWillReturnFieldEditor_toObject then pure (maybe 0 (const 1) (_windowWillReturnFieldEditor_toObject rec_))
    else if queriedSel == sel_windowWillReturnUndoManager then pure (maybe 0 (const 1) (_windowWillReturnUndoManager rec_))
    else if queriedSel == sel_window_shouldPopUpDocumentPathMenu then pure (maybe 0 (const 1) (_window_shouldPopUpDocumentPathMenu rec_))
    else if queriedSel == sel_customWindowsToEnterFullScreenForWindow then pure (maybe 0 (const 1) (_customWindowsToEnterFullScreenForWindow rec_))
    else if queriedSel == sel_window_startCustomAnimationToEnterFullScreenWithDuration then pure (maybe 0 (const 1) (_window_startCustomAnimationToEnterFullScreenWithDuration rec_))
    else if queriedSel == sel_windowDidFailToEnterFullScreen then pure (maybe 0 (const 1) (_windowDidFailToEnterFullScreen rec_))
    else if queriedSel == sel_customWindowsToExitFullScreenForWindow then pure (maybe 0 (const 1) (_customWindowsToExitFullScreenForWindow rec_))
    else if queriedSel == sel_window_startCustomAnimationToExitFullScreenWithDuration then pure (maybe 0 (const 1) (_window_startCustomAnimationToExitFullScreenWithDuration rec_))
    else if queriedSel == sel_customWindowsToEnterFullScreenForWindow_onScreen then pure (maybe 0 (const 1) (_customWindowsToEnterFullScreenForWindow_onScreen rec_))
    else if queriedSel == sel_window_startCustomAnimationToEnterFullScreenOnScreen_withDuration then pure (maybe 0 (const 1) (_window_startCustomAnimationToEnterFullScreenOnScreen_withDuration rec_))
    else if queriedSel == sel_windowDidFailToExitFullScreen then pure (maybe 0 (const 1) (_windowDidFailToExitFullScreen rec_))
    else if queriedSel == sel_window_willEncodeRestorableState then pure (maybe 0 (const 1) (_window_willEncodeRestorableState rec_))
    else if queriedSel == sel_window_didDecodeRestorableState then pure (maybe 0 (const 1) (_window_didDecodeRestorableState rec_))
    else if queriedSel == sel_previewRepresentableActivityItemsForWindow then pure (maybe 0 (const 1) (_previewRepresentableActivityItemsForWindow rec_))
    else if queriedSel == sel_windowForSharingRequestFromWindow then pure (maybe 0 (const 1) (_windowForSharingRequestFromWindow rec_))
    else if queriedSel == sel_windowDidResize then pure (maybe 0 (const 1) (_windowDidResize rec_))
    else if queriedSel == sel_windowDidExpose then pure (maybe 0 (const 1) (_windowDidExpose rec_))
    else if queriedSel == sel_windowWillMove then pure (maybe 0 (const 1) (_windowWillMove rec_))
    else if queriedSel == sel_windowDidMove then pure (maybe 0 (const 1) (_windowDidMove rec_))
    else if queriedSel == sel_windowDidBecomeKey then pure (maybe 0 (const 1) (_windowDidBecomeKey rec_))
    else if queriedSel == sel_windowDidResignKey then pure (maybe 0 (const 1) (_windowDidResignKey rec_))
    else if queriedSel == sel_windowDidBecomeMain then pure (maybe 0 (const 1) (_windowDidBecomeMain rec_))
    else if queriedSel == sel_windowDidResignMain then pure (maybe 0 (const 1) (_windowDidResignMain rec_))
    else if queriedSel == sel_windowWillClose then pure (maybe 0 (const 1) (_windowWillClose rec_))
    else if queriedSel == sel_windowWillMiniaturize then pure (maybe 0 (const 1) (_windowWillMiniaturize rec_))
    else if queriedSel == sel_windowDidMiniaturize then pure (maybe 0 (const 1) (_windowDidMiniaturize rec_))
    else if queriedSel == sel_windowDidDeminiaturize then pure (maybe 0 (const 1) (_windowDidDeminiaturize rec_))
    else if queriedSel == sel_windowDidUpdate then pure (maybe 0 (const 1) (_windowDidUpdate rec_))
    else if queriedSel == sel_windowDidChangeScreen then pure (maybe 0 (const 1) (_windowDidChangeScreen rec_))
    else if queriedSel == sel_windowDidChangeScreenProfile then pure (maybe 0 (const 1) (_windowDidChangeScreenProfile rec_))
    else if queriedSel == sel_windowDidChangeBackingProperties then pure (maybe 0 (const 1) (_windowDidChangeBackingProperties rec_))
    else if queriedSel == sel_windowWillBeginSheet then pure (maybe 0 (const 1) (_windowWillBeginSheet rec_))
    else if queriedSel == sel_windowDidEndSheet then pure (maybe 0 (const 1) (_windowDidEndSheet rec_))
    else if queriedSel == sel_windowWillStartLiveResize then pure (maybe 0 (const 1) (_windowWillStartLiveResize rec_))
    else if queriedSel == sel_windowDidEndLiveResize then pure (maybe 0 (const 1) (_windowDidEndLiveResize rec_))
    else if queriedSel == sel_windowWillEnterFullScreen then pure (maybe 0 (const 1) (_windowWillEnterFullScreen rec_))
    else if queriedSel == sel_windowDidEnterFullScreen then pure (maybe 0 (const 1) (_windowDidEnterFullScreen rec_))
    else if queriedSel == sel_windowWillExitFullScreen then pure (maybe 0 (const 1) (_windowWillExitFullScreen rec_))
    else if queriedSel == sel_windowDidExitFullScreen then pure (maybe 0 (const 1) (_windowDidExitFullScreen rec_))
    else if queriedSel == sel_windowWillEnterVersionBrowser then pure (maybe 0 (const 1) (_windowWillEnterVersionBrowser rec_))
    else if queriedSel == sel_windowDidEnterVersionBrowser then pure (maybe 0 (const 1) (_windowDidEnterVersionBrowser rec_))
    else if queriedSel == sel_windowWillExitVersionBrowser then pure (maybe 0 (const 1) (_windowWillExitVersionBrowser rec_))
    else if queriedSel == sel_windowDidExitVersionBrowser then pure (maybe 0 (const 1) (_windowDidExitVersionBrowser rec_))
    else if queriedSel == sel_windowDidChangeOcclusionState then pure (maybe 0 (const 1) (_windowDidChangeOcclusionState rec_))
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
newNSWindowDelegate :: NSWindowDelegateOverrides -> IO RawId
newNSWindowDelegate overrides = do
  inst <- class_createInstance nsWindowDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
