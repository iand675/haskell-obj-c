{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol MTL4MachineLearningCommandEncoder@.
--
-- Usage:
--
-- @
-- delegate <- newMTL4MachineLearningCommandEncoder defaultMTL4MachineLearningCommandEncoderOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Metal.Delegate.MTL4MachineLearningCommandEncoder
  ( MTL4MachineLearningCommandEncoderOverrides(..)
  , defaultMTL4MachineLearningCommandEncoderOverrides
  , newMTL4MachineLearningCommandEncoder
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

-- | Overrides record for @\@protocol MTL4MachineLearningCommandEncoder@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data MTL4MachineLearningCommandEncoderOverrides = MTL4MachineLearningCommandEncoderOverrides
  { _setPipelineState :: !(Maybe (RawId -> IO ()))
  , _setArgumentTable :: !(Maybe (RawId -> IO ()))
  , _dispatchNetworkWithIntermediatesHeap :: !(Maybe (RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultMTL4MachineLearningCommandEncoderOverrides :: MTL4MachineLearningCommandEncoderOverrides
defaultMTL4MachineLearningCommandEncoderOverrides = MTL4MachineLearningCommandEncoderOverrides
  { _setPipelineState = Nothing
  , _setArgumentTable = Nothing
  , _dispatchNetworkWithIntermediatesHeap = Nothing
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
{-# NOINLINE mtL4MachineLearningCommandEncoderDelegateClass #-}
mtL4MachineLearningCommandEncoderDelegateClass :: Class
mtL4MachineLearningCommandEncoderDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsMTL4MachineLearningCommandEncoder" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_setPipelineState = unSelector (mkSelector "setPipelineState:")
      sel_setArgumentTable = unSelector (mkSelector "setArgumentTable:")
      sel_dispatchNetworkWithIntermediatesHeap = unSelector (mkSelector "dispatchNetworkWithIntermediatesHeap:")
  -- setPipelineState:
  stub_0 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTL4MachineLearningCommandEncoderOverrides
    case _setPipelineState rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setPipelineState:" "v@:@" stub_0

  -- setArgumentTable:
  stub_1 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTL4MachineLearningCommandEncoderOverrides
    case _setArgumentTable rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setArgumentTable:" "v@:@" stub_1

  -- dispatchNetworkWithIntermediatesHeap:
  stub_2 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTL4MachineLearningCommandEncoderOverrides
    case _dispatchNetworkWithIntermediatesHeap rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "dispatchNetworkWithIntermediatesHeap:" "v@:@" stub_2

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTL4MachineLearningCommandEncoderOverrides
    if queriedSel == sel_setPipelineState then pure (maybe 0 (const 1) (_setPipelineState rec_))
    else if queriedSel == sel_setArgumentTable then pure (maybe 0 (const 1) (_setArgumentTable rec_))
    else if queriedSel == sel_dispatchNetworkWithIntermediatesHeap then pure (maybe 0 (const 1) (_dispatchNetworkWithIntermediatesHeap rec_))
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
newMTL4MachineLearningCommandEncoder :: MTL4MachineLearningCommandEncoderOverrides -> IO RawId
newMTL4MachineLearningCommandEncoder overrides = do
  inst <- class_createInstance mtL4MachineLearningCommandEncoderDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
