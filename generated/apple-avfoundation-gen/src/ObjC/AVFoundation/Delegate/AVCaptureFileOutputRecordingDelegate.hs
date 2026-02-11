{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol AVCaptureFileOutputRecordingDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newAVCaptureFileOutputRecordingDelegate defaultAVCaptureFileOutputRecordingDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AVFoundation.Delegate.AVCaptureFileOutputRecordingDelegate
  ( AVCaptureFileOutputRecordingDelegateOverrides(..)
  , defaultAVCaptureFileOutputRecordingDelegateOverrides
  , newAVCaptureFileOutputRecordingDelegate
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

-- | Overrides record for @\@protocol AVCaptureFileOutputRecordingDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data AVCaptureFileOutputRecordingDelegateOverrides = AVCaptureFileOutputRecordingDelegateOverrides
  { _captureOutput_didStartRecordingToOutputFileAtURL_fromConnections :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _captureOutput_didPauseRecordingToOutputFileAtURL_fromConnections :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _captureOutput_didResumeRecordingToOutputFileAtURL_fromConnections :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _captureOutput_willFinishRecordingToOutputFileAtURL_fromConnections_error :: !(Maybe (RawId -> RawId -> RawId -> RawId -> IO ()))
  , _captureOutput_didFinishRecordingToOutputFileAtURL_fromConnections_error :: !(Maybe (RawId -> RawId -> RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultAVCaptureFileOutputRecordingDelegateOverrides :: AVCaptureFileOutputRecordingDelegateOverrides
defaultAVCaptureFileOutputRecordingDelegateOverrides = AVCaptureFileOutputRecordingDelegateOverrides
  { _captureOutput_didStartRecordingToOutputFileAtURL_fromConnections = Nothing
  , _captureOutput_didPauseRecordingToOutputFileAtURL_fromConnections = Nothing
  , _captureOutput_didResumeRecordingToOutputFileAtURL_fromConnections = Nothing
  , _captureOutput_willFinishRecordingToOutputFileAtURL_fromConnections_error = Nothing
  , _captureOutput_didFinishRecordingToOutputFileAtURL_fromConnections_error = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE avCaptureFileOutputRecordingDelegateDelegateClass #-}
avCaptureFileOutputRecordingDelegateDelegateClass :: Class
avCaptureFileOutputRecordingDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsAVCaptureFileOutputRecordingDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_captureOutput_didStartRecordingToOutputFileAtURL_fromConnections = unSelector (mkSelector "captureOutput:didStartRecordingToOutputFileAtURL:fromConnections:")
      sel_captureOutput_didPauseRecordingToOutputFileAtURL_fromConnections = unSelector (mkSelector "captureOutput:didPauseRecordingToOutputFileAtURL:fromConnections:")
      sel_captureOutput_didResumeRecordingToOutputFileAtURL_fromConnections = unSelector (mkSelector "captureOutput:didResumeRecordingToOutputFileAtURL:fromConnections:")
      sel_captureOutput_willFinishRecordingToOutputFileAtURL_fromConnections_error = unSelector (mkSelector "captureOutput:willFinishRecordingToOutputFileAtURL:fromConnections:error:")
      sel_captureOutput_didFinishRecordingToOutputFileAtURL_fromConnections_error = unSelector (mkSelector "captureOutput:didFinishRecordingToOutputFileAtURL:fromConnections:error:")
  -- captureOutput:didStartRecordingToOutputFileAtURL:fromConnections:
  stub_0 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVCaptureFileOutputRecordingDelegateOverrides
    case _captureOutput_didStartRecordingToOutputFileAtURL_fromConnections rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "captureOutput:didStartRecordingToOutputFileAtURL:fromConnections:" "v@:@@@" stub_0

  -- captureOutput:didPauseRecordingToOutputFileAtURL:fromConnections:
  stub_1 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVCaptureFileOutputRecordingDelegateOverrides
    case _captureOutput_didPauseRecordingToOutputFileAtURL_fromConnections rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "captureOutput:didPauseRecordingToOutputFileAtURL:fromConnections:" "v@:@@@" stub_1

  -- captureOutput:didResumeRecordingToOutputFileAtURL:fromConnections:
  stub_2 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVCaptureFileOutputRecordingDelegateOverrides
    case _captureOutput_didResumeRecordingToOutputFileAtURL_fromConnections rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "captureOutput:didResumeRecordingToOutputFileAtURL:fromConnections:" "v@:@@@" stub_2

  -- captureOutput:willFinishRecordingToOutputFileAtURL:fromConnections:error:
  stub_3 <- wrap_at_at_at_at_v $ \self _cmd arg0 arg1 arg2 arg3 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVCaptureFileOutputRecordingDelegateOverrides
    case _captureOutput_willFinishRecordingToOutputFileAtURL_fromConnections_error rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2) (RawId arg3)
  addObjCMethod cls "captureOutput:willFinishRecordingToOutputFileAtURL:fromConnections:error:" "v@:@@@@" stub_3

  -- captureOutput:didFinishRecordingToOutputFileAtURL:fromConnections:error:
  stub_4 <- wrap_at_at_at_at_v $ \self _cmd arg0 arg1 arg2 arg3 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVCaptureFileOutputRecordingDelegateOverrides
    case _captureOutput_didFinishRecordingToOutputFileAtURL_fromConnections_error rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2) (RawId arg3)
  addObjCMethod cls "captureOutput:didFinishRecordingToOutputFileAtURL:fromConnections:error:" "v@:@@@@" stub_4

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVCaptureFileOutputRecordingDelegateOverrides
    if queriedSel == sel_captureOutput_didStartRecordingToOutputFileAtURL_fromConnections then pure (maybe 0 (const 1) (_captureOutput_didStartRecordingToOutputFileAtURL_fromConnections rec_))
    else if queriedSel == sel_captureOutput_didPauseRecordingToOutputFileAtURL_fromConnections then pure (maybe 0 (const 1) (_captureOutput_didPauseRecordingToOutputFileAtURL_fromConnections rec_))
    else if queriedSel == sel_captureOutput_didResumeRecordingToOutputFileAtURL_fromConnections then pure (maybe 0 (const 1) (_captureOutput_didResumeRecordingToOutputFileAtURL_fromConnections rec_))
    else if queriedSel == sel_captureOutput_willFinishRecordingToOutputFileAtURL_fromConnections_error then pure (maybe 0 (const 1) (_captureOutput_willFinishRecordingToOutputFileAtURL_fromConnections_error rec_))
    else if queriedSel == sel_captureOutput_didFinishRecordingToOutputFileAtURL_fromConnections_error then pure (maybe 0 (const 1) (_captureOutput_didFinishRecordingToOutputFileAtURL_fromConnections_error rec_))
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
newAVCaptureFileOutputRecordingDelegate :: AVCaptureFileOutputRecordingDelegateOverrides -> IO RawId
newAVCaptureFileOutputRecordingDelegate overrides = do
  inst <- class_createInstance avCaptureFileOutputRecordingDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
