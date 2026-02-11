{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol MTL4CommitFeedback@.
--
-- Usage:
--
-- @
-- delegate <- newMTL4CommitFeedback defaultMTL4CommitFeedbackOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Metal.Delegate.MTL4CommitFeedback
  ( MTL4CommitFeedbackOverrides(..)
  , defaultMTL4CommitFeedbackOverrides
  , newMTL4CommitFeedback
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

-- | Overrides record for @\@protocol MTL4CommitFeedback@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data MTL4CommitFeedbackOverrides = MTL4CommitFeedbackOverrides
  { _error :: !(Maybe (IO RawId))
  , _gpuStartTime :: !(Maybe (IO Double))
  , _gpuEndTime :: !(Maybe (IO Double))
  }

-- | Default overrides with all methods unimplemented.
defaultMTL4CommitFeedbackOverrides :: MTL4CommitFeedbackOverrides
defaultMTL4CommitFeedbackOverrides = MTL4CommitFeedbackOverrides
  { _error = Nothing
  , _gpuStartTime = Nothing
  , _gpuEndTime = Nothing
  }

foreign import ccall "wrapper"
  wrap_d
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CDouble)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CDouble))

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE mtL4CommitFeedbackDelegateClass #-}
mtL4CommitFeedbackDelegateClass :: Class
mtL4CommitFeedbackDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsMTL4CommitFeedback" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_error = unSelector (mkSelector "error")
      sel_gpuStartTime = unSelector (mkSelector "GPUStartTime")
      sel_gpuEndTime = unSelector (mkSelector "GPUEndTime")
  -- error
  stub_0 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTL4CommitFeedbackOverrides
    case _error rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "error" "@@:" stub_0

  -- GPUStartTime
  stub_1 <- wrap_d $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTL4CommitFeedbackOverrides
    case _gpuStartTime rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f 
        pure (realToFrac r)
  addObjCMethod cls "GPUStartTime" "d@:" stub_1

  -- GPUEndTime
  stub_2 <- wrap_d $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTL4CommitFeedbackOverrides
    case _gpuEndTime rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f 
        pure (realToFrac r)
  addObjCMethod cls "GPUEndTime" "d@:" stub_2

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTL4CommitFeedbackOverrides
    if queriedSel == sel_error then pure (maybe 0 (const 1) (_error rec_))
    else if queriedSel == sel_gpuStartTime then pure (maybe 0 (const 1) (_gpuStartTime rec_))
    else if queriedSel == sel_gpuEndTime then pure (maybe 0 (const 1) (_gpuEndTime rec_))
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
newMTL4CommitFeedback :: MTL4CommitFeedbackOverrides -> IO RawId
newMTL4CommitFeedback overrides = do
  inst <- class_createInstance mtL4CommitFeedbackDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
