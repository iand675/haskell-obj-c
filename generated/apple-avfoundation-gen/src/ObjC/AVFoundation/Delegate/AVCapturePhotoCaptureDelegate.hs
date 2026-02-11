{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol AVCapturePhotoCaptureDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newAVCapturePhotoCaptureDelegate defaultAVCapturePhotoCaptureDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AVFoundation.Delegate.AVCapturePhotoCaptureDelegate
  ( AVCapturePhotoCaptureDelegateOverrides(..)
  , defaultAVCapturePhotoCaptureDelegateOverrides
  , newAVCapturePhotoCaptureDelegate
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

-- | Overrides record for @\@protocol AVCapturePhotoCaptureDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data AVCapturePhotoCaptureDelegateOverrides = AVCapturePhotoCaptureDelegateOverrides
  { _captureOutput_willBeginCaptureForResolvedSettings :: !(Maybe (RawId -> RawId -> IO ()))
  , _captureOutput_willCapturePhotoForResolvedSettings :: !(Maybe (RawId -> RawId -> IO ()))
  , _captureOutput_didCapturePhotoForResolvedSettings :: !(Maybe (RawId -> RawId -> IO ()))
  , _captureOutput_didFinishProcessingPhoto_error :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _captureOutput_didFinishCapturingDeferredPhotoProxy_error :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _captureOutput_didFinishRecordingLivePhotoMovieForEventualFileAtURL_resolvedSettings :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _captureOutput_didFinishCaptureForResolvedSettings_error :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultAVCapturePhotoCaptureDelegateOverrides :: AVCapturePhotoCaptureDelegateOverrides
defaultAVCapturePhotoCaptureDelegateOverrides = AVCapturePhotoCaptureDelegateOverrides
  { _captureOutput_willBeginCaptureForResolvedSettings = Nothing
  , _captureOutput_willCapturePhotoForResolvedSettings = Nothing
  , _captureOutput_didCapturePhotoForResolvedSettings = Nothing
  , _captureOutput_didFinishProcessingPhoto_error = Nothing
  , _captureOutput_didFinishCapturingDeferredPhotoProxy_error = Nothing
  , _captureOutput_didFinishRecordingLivePhotoMovieForEventualFileAtURL_resolvedSettings = Nothing
  , _captureOutput_didFinishCaptureForResolvedSettings_error = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE avCapturePhotoCaptureDelegateDelegateClass #-}
avCapturePhotoCaptureDelegateDelegateClass :: Class
avCapturePhotoCaptureDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsAVCapturePhotoCaptureDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_captureOutput_willBeginCaptureForResolvedSettings = unSelector (mkSelector "captureOutput:willBeginCaptureForResolvedSettings:")
      sel_captureOutput_willCapturePhotoForResolvedSettings = unSelector (mkSelector "captureOutput:willCapturePhotoForResolvedSettings:")
      sel_captureOutput_didCapturePhotoForResolvedSettings = unSelector (mkSelector "captureOutput:didCapturePhotoForResolvedSettings:")
      sel_captureOutput_didFinishProcessingPhoto_error = unSelector (mkSelector "captureOutput:didFinishProcessingPhoto:error:")
      sel_captureOutput_didFinishCapturingDeferredPhotoProxy_error = unSelector (mkSelector "captureOutput:didFinishCapturingDeferredPhotoProxy:error:")
      sel_captureOutput_didFinishRecordingLivePhotoMovieForEventualFileAtURL_resolvedSettings = unSelector (mkSelector "captureOutput:didFinishRecordingLivePhotoMovieForEventualFileAtURL:resolvedSettings:")
      sel_captureOutput_didFinishCaptureForResolvedSettings_error = unSelector (mkSelector "captureOutput:didFinishCaptureForResolvedSettings:error:")
  -- captureOutput:willBeginCaptureForResolvedSettings:
  stub_0 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVCapturePhotoCaptureDelegateOverrides
    case _captureOutput_willBeginCaptureForResolvedSettings rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "captureOutput:willBeginCaptureForResolvedSettings:" "v@:@@" stub_0

  -- captureOutput:willCapturePhotoForResolvedSettings:
  stub_1 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVCapturePhotoCaptureDelegateOverrides
    case _captureOutput_willCapturePhotoForResolvedSettings rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "captureOutput:willCapturePhotoForResolvedSettings:" "v@:@@" stub_1

  -- captureOutput:didCapturePhotoForResolvedSettings:
  stub_2 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVCapturePhotoCaptureDelegateOverrides
    case _captureOutput_didCapturePhotoForResolvedSettings rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "captureOutput:didCapturePhotoForResolvedSettings:" "v@:@@" stub_2

  -- captureOutput:didFinishProcessingPhoto:error:
  stub_3 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVCapturePhotoCaptureDelegateOverrides
    case _captureOutput_didFinishProcessingPhoto_error rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "captureOutput:didFinishProcessingPhoto:error:" "v@:@@@" stub_3

  -- captureOutput:didFinishCapturingDeferredPhotoProxy:error:
  stub_4 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVCapturePhotoCaptureDelegateOverrides
    case _captureOutput_didFinishCapturingDeferredPhotoProxy_error rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "captureOutput:didFinishCapturingDeferredPhotoProxy:error:" "v@:@@@" stub_4

  -- captureOutput:didFinishRecordingLivePhotoMovieForEventualFileAtURL:resolvedSettings:
  stub_5 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVCapturePhotoCaptureDelegateOverrides
    case _captureOutput_didFinishRecordingLivePhotoMovieForEventualFileAtURL_resolvedSettings rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "captureOutput:didFinishRecordingLivePhotoMovieForEventualFileAtURL:resolvedSettings:" "v@:@@@" stub_5

  -- captureOutput:didFinishCaptureForResolvedSettings:error:
  stub_6 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVCapturePhotoCaptureDelegateOverrides
    case _captureOutput_didFinishCaptureForResolvedSettings_error rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "captureOutput:didFinishCaptureForResolvedSettings:error:" "v@:@@@" stub_6

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVCapturePhotoCaptureDelegateOverrides
    if queriedSel == sel_captureOutput_willBeginCaptureForResolvedSettings then pure (maybe 0 (const 1) (_captureOutput_willBeginCaptureForResolvedSettings rec_))
    else if queriedSel == sel_captureOutput_willCapturePhotoForResolvedSettings then pure (maybe 0 (const 1) (_captureOutput_willCapturePhotoForResolvedSettings rec_))
    else if queriedSel == sel_captureOutput_didCapturePhotoForResolvedSettings then pure (maybe 0 (const 1) (_captureOutput_didCapturePhotoForResolvedSettings rec_))
    else if queriedSel == sel_captureOutput_didFinishProcessingPhoto_error then pure (maybe 0 (const 1) (_captureOutput_didFinishProcessingPhoto_error rec_))
    else if queriedSel == sel_captureOutput_didFinishCapturingDeferredPhotoProxy_error then pure (maybe 0 (const 1) (_captureOutput_didFinishCapturingDeferredPhotoProxy_error rec_))
    else if queriedSel == sel_captureOutput_didFinishRecordingLivePhotoMovieForEventualFileAtURL_resolvedSettings then pure (maybe 0 (const 1) (_captureOutput_didFinishRecordingLivePhotoMovieForEventualFileAtURL_resolvedSettings rec_))
    else if queriedSel == sel_captureOutput_didFinishCaptureForResolvedSettings_error then pure (maybe 0 (const 1) (_captureOutput_didFinishCaptureForResolvedSettings_error rec_))
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
newAVCapturePhotoCaptureDelegate :: AVCapturePhotoCaptureDelegateOverrides -> IO RawId
newAVCapturePhotoCaptureDelegate overrides = do
  inst <- class_createInstance avCapturePhotoCaptureDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
