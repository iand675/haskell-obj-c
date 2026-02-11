{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol IKCameraDeviceViewDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newIKCameraDeviceViewDelegate defaultIKCameraDeviceViewDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Quartz.Delegate.IKCameraDeviceViewDelegate
  ( IKCameraDeviceViewDelegateOverrides(..)
  , defaultIKCameraDeviceViewDelegateOverrides
  , newIKCameraDeviceViewDelegate
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

-- | Overrides record for @\@protocol IKCameraDeviceViewDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data IKCameraDeviceViewDelegateOverrides = IKCameraDeviceViewDelegateOverrides
  { _cameraDeviceViewSelectionDidChange :: !(Maybe (RawId -> IO ()))
  , _cameraDeviceView_didDownloadFile_location_fileData_error :: !(Maybe (RawId -> RawId -> RawId -> RawId -> RawId -> IO ()))
  , _cameraDeviceView_didEncounterError :: !(Maybe (RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultIKCameraDeviceViewDelegateOverrides :: IKCameraDeviceViewDelegateOverrides
defaultIKCameraDeviceViewDelegateOverrides = IKCameraDeviceViewDelegateOverrides
  { _cameraDeviceViewSelectionDidChange = Nothing
  , _cameraDeviceView_didDownloadFile_location_fileData_error = Nothing
  , _cameraDeviceView_didEncounterError = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_at_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE ikCameraDeviceViewDelegateDelegateClass #-}
ikCameraDeviceViewDelegateDelegateClass :: Class
ikCameraDeviceViewDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsIKCameraDeviceViewDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_cameraDeviceViewSelectionDidChange = unSelector (mkSelector "cameraDeviceViewSelectionDidChange:")
      sel_cameraDeviceView_didDownloadFile_location_fileData_error = unSelector (mkSelector "cameraDeviceView:didDownloadFile:location:fileData:error:")
      sel_cameraDeviceView_didEncounterError = unSelector (mkSelector "cameraDeviceView:didEncounterError:")
  -- cameraDeviceViewSelectionDidChange:
  stub_0 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IKCameraDeviceViewDelegateOverrides
    case _cameraDeviceViewSelectionDidChange rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "cameraDeviceViewSelectionDidChange:" "v@:@" stub_0

  -- cameraDeviceView:didDownloadFile:location:fileData:error:
  stub_1 <- wrap_at_at_at_at_at_v $ \self _cmd arg0 arg1 arg2 arg3 arg4 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IKCameraDeviceViewDelegateOverrides
    case _cameraDeviceView_didDownloadFile_location_fileData_error rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2) (RawId arg3) (RawId arg4)
  addObjCMethod cls "cameraDeviceView:didDownloadFile:location:fileData:error:" "v@:@@@@@" stub_1

  -- cameraDeviceView:didEncounterError:
  stub_2 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IKCameraDeviceViewDelegateOverrides
    case _cameraDeviceView_didEncounterError rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "cameraDeviceView:didEncounterError:" "v@:@@" stub_2

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IKCameraDeviceViewDelegateOverrides
    if queriedSel == sel_cameraDeviceViewSelectionDidChange then pure (maybe 0 (const 1) (_cameraDeviceViewSelectionDidChange rec_))
    else if queriedSel == sel_cameraDeviceView_didDownloadFile_location_fileData_error then pure (maybe 0 (const 1) (_cameraDeviceView_didDownloadFile_location_fileData_error rec_))
    else if queriedSel == sel_cameraDeviceView_didEncounterError then pure (maybe 0 (const 1) (_cameraDeviceView_didEncounterError rec_))
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
newIKCameraDeviceViewDelegate :: IKCameraDeviceViewDelegateOverrides -> IO RawId
newIKCameraDeviceViewDelegate overrides = do
  inst <- class_createInstance ikCameraDeviceViewDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
