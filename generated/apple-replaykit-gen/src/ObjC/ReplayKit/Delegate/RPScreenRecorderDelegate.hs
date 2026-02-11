{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol RPScreenRecorderDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newRPScreenRecorderDelegate defaultRPScreenRecorderDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.ReplayKit.Delegate.RPScreenRecorderDelegate
  ( RPScreenRecorderDelegateOverrides(..)
  , defaultRPScreenRecorderDelegateOverrides
  , newRPScreenRecorderDelegate
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

-- | Overrides record for @\@protocol RPScreenRecorderDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data RPScreenRecorderDelegateOverrides = RPScreenRecorderDelegateOverrides
  { _screenRecorder_didStopRecordingWithError_previewViewController :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _screenRecorder_didStopRecordingWithPreviewViewController_error :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _screenRecorderDidChangeAvailability :: !(Maybe (RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultRPScreenRecorderDelegateOverrides :: RPScreenRecorderDelegateOverrides
defaultRPScreenRecorderDelegateOverrides = RPScreenRecorderDelegateOverrides
  { _screenRecorder_didStopRecordingWithError_previewViewController = Nothing
  , _screenRecorder_didStopRecordingWithPreviewViewController_error = Nothing
  , _screenRecorderDidChangeAvailability = Nothing
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
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE rpScreenRecorderDelegateDelegateClass #-}
rpScreenRecorderDelegateDelegateClass :: Class
rpScreenRecorderDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsRPScreenRecorderDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_screenRecorder_didStopRecordingWithError_previewViewController = unSelector (mkSelector "screenRecorder:didStopRecordingWithError:previewViewController:")
      sel_screenRecorder_didStopRecordingWithPreviewViewController_error = unSelector (mkSelector "screenRecorder:didStopRecordingWithPreviewViewController:error:")
      sel_screenRecorderDidChangeAvailability = unSelector (mkSelector "screenRecorderDidChangeAvailability:")
  -- screenRecorder:didStopRecordingWithError:previewViewController:
  stub_0 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO RPScreenRecorderDelegateOverrides
    case _screenRecorder_didStopRecordingWithError_previewViewController rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "screenRecorder:didStopRecordingWithError:previewViewController:" "v@:@@@" stub_0

  -- screenRecorder:didStopRecordingWithPreviewViewController:error:
  stub_1 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO RPScreenRecorderDelegateOverrides
    case _screenRecorder_didStopRecordingWithPreviewViewController_error rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "screenRecorder:didStopRecordingWithPreviewViewController:error:" "v@:@@@" stub_1

  -- screenRecorderDidChangeAvailability:
  stub_2 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO RPScreenRecorderDelegateOverrides
    case _screenRecorderDidChangeAvailability rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "screenRecorderDidChangeAvailability:" "v@:@" stub_2

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO RPScreenRecorderDelegateOverrides
    if queriedSel == sel_screenRecorder_didStopRecordingWithError_previewViewController then pure (maybe 0 (const 1) (_screenRecorder_didStopRecordingWithError_previewViewController rec_))
    else if queriedSel == sel_screenRecorder_didStopRecordingWithPreviewViewController_error then pure (maybe 0 (const 1) (_screenRecorder_didStopRecordingWithPreviewViewController_error rec_))
    else if queriedSel == sel_screenRecorderDidChangeAvailability then pure (maybe 0 (const 1) (_screenRecorderDidChangeAvailability rec_))
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
newRPScreenRecorderDelegate :: RPScreenRecorderDelegateOverrides -> IO RawId
newRPScreenRecorderDelegate overrides = do
  inst <- class_createInstance rpScreenRecorderDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
