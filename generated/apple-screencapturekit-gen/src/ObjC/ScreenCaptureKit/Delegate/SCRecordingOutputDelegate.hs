{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol SCRecordingOutputDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newSCRecordingOutputDelegate defaultSCRecordingOutputDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.ScreenCaptureKit.Delegate.SCRecordingOutputDelegate
  ( SCRecordingOutputDelegateOverrides(..)
  , defaultSCRecordingOutputDelegateOverrides
  , newSCRecordingOutputDelegate
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

-- | Overrides record for @\@protocol SCRecordingOutputDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data SCRecordingOutputDelegateOverrides = SCRecordingOutputDelegateOverrides
  { _recordingOutputDidStartRecording :: !(Maybe (RawId -> IO ()))
  , _recordingOutput_didFailWithError :: !(Maybe (RawId -> RawId -> IO ()))
  , _recordingOutputDidFinishRecording :: !(Maybe (RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultSCRecordingOutputDelegateOverrides :: SCRecordingOutputDelegateOverrides
defaultSCRecordingOutputDelegateOverrides = SCRecordingOutputDelegateOverrides
  { _recordingOutputDidStartRecording = Nothing
  , _recordingOutput_didFailWithError = Nothing
  , _recordingOutputDidFinishRecording = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE scRecordingOutputDelegateDelegateClass #-}
scRecordingOutputDelegateDelegateClass :: Class
scRecordingOutputDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsSCRecordingOutputDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_recordingOutputDidStartRecording = unSelector (mkSelector "recordingOutputDidStartRecording:")
      sel_recordingOutput_didFailWithError = unSelector (mkSelector "recordingOutput:didFailWithError:")
      sel_recordingOutputDidFinishRecording = unSelector (mkSelector "recordingOutputDidFinishRecording:")
  -- recordingOutputDidStartRecording:
  stub_0 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCRecordingOutputDelegateOverrides
    case _recordingOutputDidStartRecording rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "recordingOutputDidStartRecording:" "v@:@" stub_0

  -- recordingOutput:didFailWithError:
  stub_1 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCRecordingOutputDelegateOverrides
    case _recordingOutput_didFailWithError rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "recordingOutput:didFailWithError:" "v@:@@" stub_1

  -- recordingOutputDidFinishRecording:
  stub_2 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCRecordingOutputDelegateOverrides
    case _recordingOutputDidFinishRecording rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "recordingOutputDidFinishRecording:" "v@:@" stub_2

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCRecordingOutputDelegateOverrides
    if queriedSel == sel_recordingOutputDidStartRecording then pure (maybe 0 (const 1) (_recordingOutputDidStartRecording rec_))
    else if queriedSel == sel_recordingOutput_didFailWithError then pure (maybe 0 (const 1) (_recordingOutput_didFailWithError rec_))
    else if queriedSel == sel_recordingOutputDidFinishRecording then pure (maybe 0 (const 1) (_recordingOutputDidFinishRecording rec_))
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
newSCRecordingOutputDelegate :: SCRecordingOutputDelegateOverrides -> IO RawId
newSCRecordingOutputDelegate overrides = do
  inst <- class_createInstance scRecordingOutputDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
