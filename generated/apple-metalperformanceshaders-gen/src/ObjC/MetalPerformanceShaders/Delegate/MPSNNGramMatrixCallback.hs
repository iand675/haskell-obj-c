{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol MPSNNGramMatrixCallback@.
--
-- Usage:
--
-- @
-- delegate <- newMPSNNGramMatrixCallback defaultMPSNNGramMatrixCallbackOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.MetalPerformanceShaders.Delegate.MPSNNGramMatrixCallback
  ( MPSNNGramMatrixCallbackOverrides(..)
  , defaultMPSNNGramMatrixCallbackOverrides
  , newMPSNNGramMatrixCallback
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

-- | Overrides record for @\@protocol MPSNNGramMatrixCallback@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data MPSNNGramMatrixCallbackOverrides = MPSNNGramMatrixCallbackOverrides
  { _alphaForSourceImage_destinationImage :: !(Maybe (RawId -> RawId -> IO Float))
  }

-- | Default overrides with all methods unimplemented.
defaultMPSNNGramMatrixCallbackOverrides :: MPSNNGramMatrixCallbackOverrides
defaultMPSNNGramMatrixCallbackOverrides = MPSNNGramMatrixCallbackOverrides
  { _alphaForSourceImage_destinationImage = Nothing
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
{-# NOINLINE mpsnnGramMatrixCallbackDelegateClass #-}
mpsnnGramMatrixCallbackDelegateClass :: Class
mpsnnGramMatrixCallbackDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsMPSNNGramMatrixCallback" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_alphaForSourceImage_destinationImage = unSelector (mkSelector "alphaForSourceImage:destinationImage:")
  -- alphaForSourceImage:destinationImage:
  stub_0 <- wrap_at_at_f $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MPSNNGramMatrixCallbackOverrides
    case _alphaForSourceImage_destinationImage rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (realToFrac r)
  addObjCMethod cls "alphaForSourceImage:destinationImage:" "f@:@@" stub_0

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MPSNNGramMatrixCallbackOverrides
    if queriedSel == sel_alphaForSourceImage_destinationImage then pure (maybe 0 (const 1) (_alphaForSourceImage_destinationImage rec_))
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
newMPSNNGramMatrixCallback :: MPSNNGramMatrixCallbackOverrides -> IO RawId
newMPSNNGramMatrixCallback overrides = do
  inst <- class_createInstance mpsnnGramMatrixCallbackDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
