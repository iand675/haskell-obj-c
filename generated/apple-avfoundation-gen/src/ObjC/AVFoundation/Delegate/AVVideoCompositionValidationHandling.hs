{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol AVVideoCompositionValidationHandling@.
--
-- Usage:
--
-- @
-- delegate <- newAVVideoCompositionValidationHandling defaultAVVideoCompositionValidationHandlingOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AVFoundation.Delegate.AVVideoCompositionValidationHandling
  ( AVVideoCompositionValidationHandlingOverrides(..)
  , defaultAVVideoCompositionValidationHandlingOverrides
  , newAVVideoCompositionValidationHandling
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

-- | Overrides record for @\@protocol AVVideoCompositionValidationHandling@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data AVVideoCompositionValidationHandlingOverrides = AVVideoCompositionValidationHandlingOverrides
  { _videoComposition_shouldContinueValidatingAfterFindingInvalidValueForKey :: !(Maybe (RawId -> RawId -> IO Bool))
  , _videoComposition_shouldContinueValidatingAfterFindingInvalidTimeRangeInInstruction :: !(Maybe (RawId -> RawId -> IO Bool))
  , _videoComposition_shouldContinueValidatingAfterFindingInvalidTrackIDInInstruction_layerInstruction_asset :: !(Maybe (RawId -> RawId -> RawId -> RawId -> IO Bool))
  }

-- | Default overrides with all methods unimplemented.
defaultAVVideoCompositionValidationHandlingOverrides :: AVVideoCompositionValidationHandlingOverrides
defaultAVVideoCompositionValidationHandlingOverrides = AVVideoCompositionValidationHandlingOverrides
  { _videoComposition_shouldContinueValidatingAfterFindingInvalidValueForKey = Nothing
  , _videoComposition_shouldContinueValidatingAfterFindingInvalidTimeRangeInInstruction = Nothing
  , _videoComposition_shouldContinueValidatingAfterFindingInvalidTrackIDInInstruction_layerInstruction_asset = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_at_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE avVideoCompositionValidationHandlingDelegateClass #-}
avVideoCompositionValidationHandlingDelegateClass :: Class
avVideoCompositionValidationHandlingDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsAVVideoCompositionValidationHandling" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_videoComposition_shouldContinueValidatingAfterFindingInvalidValueForKey = unSelector (mkSelector "videoComposition:shouldContinueValidatingAfterFindingInvalidValueForKey:")
      sel_videoComposition_shouldContinueValidatingAfterFindingInvalidTimeRangeInInstruction = unSelector (mkSelector "videoComposition:shouldContinueValidatingAfterFindingInvalidTimeRangeInInstruction:")
      sel_videoComposition_shouldContinueValidatingAfterFindingInvalidTrackIDInInstruction_layerInstruction_asset = unSelector (mkSelector "videoComposition:shouldContinueValidatingAfterFindingInvalidTrackIDInInstruction:layerInstruction:asset:")
  -- videoComposition:shouldContinueValidatingAfterFindingInvalidValueForKey:
  stub_0 <- wrap_at_at_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVVideoCompositionValidationHandlingOverrides
    case _videoComposition_shouldContinueValidatingAfterFindingInvalidValueForKey rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "videoComposition:shouldContinueValidatingAfterFindingInvalidValueForKey:" "B@:@@" stub_0

  -- videoComposition:shouldContinueValidatingAfterFindingInvalidTimeRangeInInstruction:
  stub_1 <- wrap_at_at_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVVideoCompositionValidationHandlingOverrides
    case _videoComposition_shouldContinueValidatingAfterFindingInvalidTimeRangeInInstruction rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "videoComposition:shouldContinueValidatingAfterFindingInvalidTimeRangeInInstruction:" "B@:@@" stub_1

  -- videoComposition:shouldContinueValidatingAfterFindingInvalidTrackIDInInstruction:layerInstruction:asset:
  stub_2 <- wrap_at_at_at_at_B $ \self _cmd arg0 arg1 arg2 arg3 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVVideoCompositionValidationHandlingOverrides
    case _videoComposition_shouldContinueValidatingAfterFindingInvalidTrackIDInInstruction_layerInstruction_asset rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2) (RawId arg3)
        pure (if r then 1 else 0)
  addObjCMethod cls "videoComposition:shouldContinueValidatingAfterFindingInvalidTrackIDInInstruction:layerInstruction:asset:" "B@:@@@@" stub_2

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVVideoCompositionValidationHandlingOverrides
    if queriedSel == sel_videoComposition_shouldContinueValidatingAfterFindingInvalidValueForKey then pure (maybe 0 (const 1) (_videoComposition_shouldContinueValidatingAfterFindingInvalidValueForKey rec_))
    else if queriedSel == sel_videoComposition_shouldContinueValidatingAfterFindingInvalidTimeRangeInInstruction then pure (maybe 0 (const 1) (_videoComposition_shouldContinueValidatingAfterFindingInvalidTimeRangeInInstruction rec_))
    else if queriedSel == sel_videoComposition_shouldContinueValidatingAfterFindingInvalidTrackIDInInstruction_layerInstruction_asset then pure (maybe 0 (const 1) (_videoComposition_shouldContinueValidatingAfterFindingInvalidTrackIDInInstruction_layerInstruction_asset rec_))
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
newAVVideoCompositionValidationHandling :: AVVideoCompositionValidationHandlingOverrides -> IO RawId
newAVVideoCompositionValidationHandling overrides = do
  inst <- class_createInstance avVideoCompositionValidationHandlingDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
