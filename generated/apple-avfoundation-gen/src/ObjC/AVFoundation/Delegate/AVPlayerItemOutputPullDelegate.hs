{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol AVPlayerItemOutputPullDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newAVPlayerItemOutputPullDelegate defaultAVPlayerItemOutputPullDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AVFoundation.Delegate.AVPlayerItemOutputPullDelegate
  ( AVPlayerItemOutputPullDelegateOverrides(..)
  , defaultAVPlayerItemOutputPullDelegateOverrides
  , newAVPlayerItemOutputPullDelegate
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

-- | Overrides record for @\@protocol AVPlayerItemOutputPullDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data AVPlayerItemOutputPullDelegateOverrides = AVPlayerItemOutputPullDelegateOverrides
  { _outputMediaDataWillChange :: !(Maybe (RawId -> IO ()))
  , _outputSequenceWasFlushed :: !(Maybe (RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultAVPlayerItemOutputPullDelegateOverrides :: AVPlayerItemOutputPullDelegateOverrides
defaultAVPlayerItemOutputPullDelegateOverrides = AVPlayerItemOutputPullDelegateOverrides
  { _outputMediaDataWillChange = Nothing
  , _outputSequenceWasFlushed = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE avPlayerItemOutputPullDelegateDelegateClass #-}
avPlayerItemOutputPullDelegateDelegateClass :: Class
avPlayerItemOutputPullDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsAVPlayerItemOutputPullDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_outputMediaDataWillChange = unSelector (mkSelector "outputMediaDataWillChange:")
      sel_outputSequenceWasFlushed = unSelector (mkSelector "outputSequenceWasFlushed:")
  -- outputMediaDataWillChange:
  stub_0 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVPlayerItemOutputPullDelegateOverrides
    case _outputMediaDataWillChange rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "outputMediaDataWillChange:" "v@:@" stub_0

  -- outputSequenceWasFlushed:
  stub_1 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVPlayerItemOutputPullDelegateOverrides
    case _outputSequenceWasFlushed rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "outputSequenceWasFlushed:" "v@:@" stub_1

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVPlayerItemOutputPullDelegateOverrides
    if queriedSel == sel_outputMediaDataWillChange then pure (maybe 0 (const 1) (_outputMediaDataWillChange rec_))
    else if queriedSel == sel_outputSequenceWasFlushed then pure (maybe 0 (const 1) (_outputSequenceWasFlushed rec_))
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
newAVPlayerItemOutputPullDelegate :: AVPlayerItemOutputPullDelegateOverrides -> IO RawId
newAVPlayerItemOutputPullDelegate overrides = do
  inst <- class_createInstance avPlayerItemOutputPullDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
