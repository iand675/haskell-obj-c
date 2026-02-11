{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol MPSNNLossCallback@.
--
-- Usage:
--
-- @
-- delegate <- newMPSNNLossCallback defaultMPSNNLossCallbackOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.MetalPerformanceShaders.Delegate.MPSNNLossCallback
  ( MPSNNLossCallbackOverrides(..)
  , defaultMPSNNLossCallbackOverrides
  , newMPSNNLossCallback
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

-- | Overrides record for @\@protocol MPSNNLossCallback@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data MPSNNLossCallbackOverrides = MPSNNLossCallbackOverrides
  { _scalarWeightForSourceImage_destinationImage :: !(Maybe (RawId -> RawId -> IO Float))
  }

-- | Default overrides with all methods unimplemented.
defaultMPSNNLossCallbackOverrides :: MPSNNLossCallbackOverrides
defaultMPSNNLossCallbackOverrides = MPSNNLossCallbackOverrides
  { _scalarWeightForSourceImage_destinationImage = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_f
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CFloat)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CFloat))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE mpsnnLossCallbackDelegateClass #-}
mpsnnLossCallbackDelegateClass :: Class
mpsnnLossCallbackDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsMPSNNLossCallback" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_scalarWeightForSourceImage_destinationImage = unSelector (mkSelector "scalarWeightForSourceImage:destinationImage:")
  -- scalarWeightForSourceImage:destinationImage:
  stub_0 <- wrap_at_at_f $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MPSNNLossCallbackOverrides
    case _scalarWeightForSourceImage_destinationImage rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (realToFrac r)
  addObjCMethod cls "scalarWeightForSourceImage:destinationImage:" "f@:@@" stub_0

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MPSNNLossCallbackOverrides
    if queriedSel == sel_scalarWeightForSourceImage_destinationImage then pure (maybe 0 (const 1) (_scalarWeightForSourceImage_destinationImage rec_))
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
newMPSNNLossCallback :: MPSNNLossCallbackOverrides -> IO RawId
newMPSNNLossCallback overrides = do
  inst <- class_createInstance mpsnnLossCallbackDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
