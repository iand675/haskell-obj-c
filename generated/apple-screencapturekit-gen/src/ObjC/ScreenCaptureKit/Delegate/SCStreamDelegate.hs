{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol SCStreamDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newSCStreamDelegate defaultSCStreamDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.ScreenCaptureKit.Delegate.SCStreamDelegate
  ( SCStreamDelegateOverrides(..)
  , defaultSCStreamDelegateOverrides
  , newSCStreamDelegate
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

-- | Overrides record for @\@protocol SCStreamDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data SCStreamDelegateOverrides = SCStreamDelegateOverrides
  { _stream_didStopWithError :: !(Maybe (RawId -> RawId -> IO ()))
  , _outputVideoEffectDidStartForStream :: !(Maybe (RawId -> IO ()))
  , _outputVideoEffectDidStopForStream :: !(Maybe (RawId -> IO ()))
  , _streamDidBecomeActive :: !(Maybe (RawId -> IO ()))
  , _streamDidBecomeInactive :: !(Maybe (RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultSCStreamDelegateOverrides :: SCStreamDelegateOverrides
defaultSCStreamDelegateOverrides = SCStreamDelegateOverrides
  { _stream_didStopWithError = Nothing
  , _outputVideoEffectDidStartForStream = Nothing
  , _outputVideoEffectDidStopForStream = Nothing
  , _streamDidBecomeActive = Nothing
  , _streamDidBecomeInactive = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE scStreamDelegateDelegateClass #-}
scStreamDelegateDelegateClass :: Class
scStreamDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsSCStreamDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_stream_didStopWithError = unSelector (mkSelector "stream:didStopWithError:")
      sel_outputVideoEffectDidStartForStream = unSelector (mkSelector "outputVideoEffectDidStartForStream:")
      sel_outputVideoEffectDidStopForStream = unSelector (mkSelector "outputVideoEffectDidStopForStream:")
      sel_streamDidBecomeActive = unSelector (mkSelector "streamDidBecomeActive:")
      sel_streamDidBecomeInactive = unSelector (mkSelector "streamDidBecomeInactive:")
  -- stream:didStopWithError:
  stub_0 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCStreamDelegateOverrides
    case _stream_didStopWithError rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "stream:didStopWithError:" "v@:@@" stub_0

  -- outputVideoEffectDidStartForStream:
  stub_1 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCStreamDelegateOverrides
    case _outputVideoEffectDidStartForStream rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "outputVideoEffectDidStartForStream:" "v@:@" stub_1

  -- outputVideoEffectDidStopForStream:
  stub_2 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCStreamDelegateOverrides
    case _outputVideoEffectDidStopForStream rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "outputVideoEffectDidStopForStream:" "v@:@" stub_2

  -- streamDidBecomeActive:
  stub_3 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCStreamDelegateOverrides
    case _streamDidBecomeActive rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "streamDidBecomeActive:" "v@:@" stub_3

  -- streamDidBecomeInactive:
  stub_4 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCStreamDelegateOverrides
    case _streamDidBecomeInactive rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "streamDidBecomeInactive:" "v@:@" stub_4

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCStreamDelegateOverrides
    if queriedSel == sel_stream_didStopWithError then pure (maybe 0 (const 1) (_stream_didStopWithError rec_))
    else if queriedSel == sel_outputVideoEffectDidStartForStream then pure (maybe 0 (const 1) (_outputVideoEffectDidStartForStream rec_))
    else if queriedSel == sel_outputVideoEffectDidStopForStream then pure (maybe 0 (const 1) (_outputVideoEffectDidStopForStream rec_))
    else if queriedSel == sel_streamDidBecomeActive then pure (maybe 0 (const 1) (_streamDidBecomeActive rec_))
    else if queriedSel == sel_streamDidBecomeInactive then pure (maybe 0 (const 1) (_streamDidBecomeInactive rec_))
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
newSCStreamDelegate :: SCStreamDelegateOverrides -> IO RawId
newSCStreamDelegate overrides = do
  inst <- class_createInstance scStreamDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
